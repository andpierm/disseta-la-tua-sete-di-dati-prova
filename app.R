# ==============================================================================
# DISSETA LA TUA SETE DI DATI - Maker Faire 2026
# Risposte salvate su Google Sheets (se non disponibile, su CSV locale)
# ==============================================================================

library(shiny)
library(shinyjs)
library(ape)
library(ggtree)
library(ggplot2)
library(pROC)
library(randomForest)
library(googlesheets4)

set.seed(123)

# --- 1. CONFIGURAZIONE -------------------------------------------------------
GS_SHEET_ID   <- "1HeKjUeP7e5qMwePfYOc9K3XOZqaW7lKCg1mUJMl7xR0"
GS_TAB        <- "risposte"               # nome della scheda nel foglio
CSV_FILE      <- "risposte_bevande.csv"   # usato solo se Google Sheets non è disponibile
PIN_OPERATORE <- "2026"                   # PIN della scheda operatore: cambialo prima dell'evento

# Ordine colonne: deve coincidere con la prima riga (intestazioni) del foglio
COLONNE <- c("timestamp", "nickname", "eta", "genere", "freq", "fortuna",
             "maratona_ny", "sabato_sera", "turno", "modalita",
             "bevanda_giusta", "bevanda_scelta", "scelta", "corretto", "id_partita", "lista")

# Chiave del service account, in ordine di priorità:
#  1) variabile d'ambiente GS_SERVICE_ACCOUNT_JSON (su shinyapps.io: contenuto del JSON)
#  2) file .json dentro la cartella .secrets/ del progetto (sul tuo computer)
trova_chiave <- function() {
  env <- Sys.getenv("GS_SERVICE_ACCOUNT_JSON")
  if (nzchar(env)) return(env)
  archivi <- list.files(".secrets", full.names = TRUE)
  json <- archivi[grepl("\\.json$", archivi, ignore.case = TRUE)]
  if (length(json) > 0) return(json[1])
  ""
}

GS_KEY <- trova_chiave()
USE_GS <- nzchar(GS_KEY) && nzchar(GS_SHEET_ID)

if (USE_GS) {
  tryCatch({
    if (grepl("^\\s*\\{", GS_KEY)) {
      tmp_json <- tempfile(fileext = ".json")
      writeLines(GS_KEY, tmp_json)
      gs4_auth(path = tmp_json)
    } else {
      gs4_auth(path = GS_KEY)
    }
    message("Salvataggio: Google Sheets attivo")
  }, error = function(e) {
    USE_GS <<- FALSE
    message("Autenticazione Google non riuscita, uso il CSV locale: ", e$message)
  })
} else {
  message("Salvataggio: Google Sheets non configurato, uso il CSV locale")
}

STATO_SALVATAGGIO <- if (USE_GS) "Google Sheets ✅" else "CSV locale ⚠️ (Google Sheets non configurato)"

# Scrive una riga. Restituisce NULL se è andata bene, altrimenti il messaggio d'errore.
salva_risposta <- function(riga) {
  tryCatch({
    riga <- riga[, COLONNE, drop = FALSE]
    if (USE_GS) {
      googlesheets4::sheet_append(GS_SHEET_ID, data = riga, sheet = GS_TAB)
    } else {
      scrivi_csv_locale(riga)
    }
    NULL
  }, error = function(e) {
    message("Errore salvataggio: ", e$message)
    e$message
  })
}

scrivi_csv_locale <- function(riga) {
  if (file.exists(CSV_FILE)) {
    intestazione <- tryCatch(names(read.csv(CSV_FILE, nrows = 1, stringsAsFactors = FALSE)),
                             error = function(e) character(0))
    if (!identical(intestazione, COLONNE)) {
      # intestazione diversa (file di una versione precedente): lo conservo come backup
      file.rename(CSV_FILE, paste0("backup_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", CSV_FILE))
    }
  }
  esiste <- file.exists(CSV_FILE)
  write.table(riga, file = CSV_FILE, append = esiste, sep = ",", dec = ".",
              row.names = FALSE, col.names = !esiste, qmethod = "double")
}

risposte_vuote <- function() {
  data.frame(
    timestamp = character(), nickname = character(), eta = numeric(),
    genere = character(), freq = numeric(), fortuna = character(),
    maratona_ny = character(), sabato_sera = character(), turno = numeric(),
    modalita = character(), bevanda_giusta = character(), bevanda_scelta = character(),
    scelta = character(), corretto = logical(), id_partita = character(),
    lista = character(), stringsAsFactors = FALSE
  )
}

pulisci_risposte <- function(grezzo) {
  df <- as.data.frame(grezzo, stringsAsFactors = FALSE)
  for (col in COLONNE) if (!col %in% names(df)) df[[col]] <- NA_character_
  df <- df[, COLONNE, drop = FALSE]
  df$eta      <- suppressWarnings(as.numeric(df$eta))
  df$freq     <- suppressWarnings(as.numeric(df$freq))
  df$turno    <- suppressWarnings(as.numeric(df$turno))
  df$corretto <- toupper(as.character(df$corretto)) %in% c("TRUE", "T", "1")
  rownames(df) <- NULL
  df
}

# Rilegge tutte le risposte (da Google Sheets o dal CSV locale)
leggi_risposte <- function() {
  tryCatch({
    grezzo <- if (USE_GS) {
      googlesheets4::read_sheet(GS_SHEET_ID, sheet = GS_TAB, col_types = "c")
    } else if (file.exists(CSV_FILE)) {
      read.csv(CSV_FILE, colClasses = "character", stringsAsFactors = FALSE)
    } else {
      NULL
    }
    dati <- if (is.null(grezzo) || nrow(grezzo) == 0) risposte_vuote() else pulisci_risposte(grezzo)
    list(dati = dati, errore = NULL)
  }, error = function(e) {
    message("Lettura risposte non riuscita: ", e$message)
    list(dati = risposte_vuote(), errore = e$message)
  })
}

# --- 2. ALBERO TASSONOMICO ---------------------------------------------------
beverage_tree_newick <- "((((coca_classic:1,coca_zero:1)Coca:1,(pepsi_classic:1,(coop_cola:1,conad_cola:1)StoreCola:1)PepsiOther:1)Cola:1,(chinotto:1,molecola:1)DarkBitter:1)DarkDrinks:1,(((fanta:1,fanta_zero:1)Fanta:1,conad_orange:1)Orange:1,(sprite:1,cedrata:1)ClearCitrus:1)Citrus:1,((estathe_limone:1,estathe_pesca:1)Estathe:1,sanbenedetto_te:1)Tea:1,(gatorade:1,powerade:1)Sports:1)Bibite;"

tree <- tryCatch(ape::read.tree(text = beverage_tree_newick), error = function(e) NULL)

# --- 3. BEVANDE --------------------------------------------------------------
# image: nome del file dentro la cartella www/ (controlla maiuscole ed estensione)
bevande <- data.frame(
  id = c("coca_classic", "coca_zero", "pepsi_classic", "coop_cola", "conad_cola",
         "chinotto", "molecola", "fanta", "fanta_zero", "conad_orange",
         "sprite", "cedrata", "estathe_limone", "estathe_pesca", "sanbenedetto_te",
         "gatorade", "powerade"),
  name = c("Coca-Cola Classica", "Coca-Cola Zero", "Pepsi Classica", "Coop Cola", "Conad Cola Zero",
           "Chinotto Sanpellegrino", "MoleCola", "Fanta Aranciata", "Fanta Zero", "Aranciata Conad",
           "Sprite", "Cedrata Tassoni", "Estathé Limone", "Estathé Pesca", "Tè San Benedetto",
           "Gatorade", "Powerade"),
  image = c("coca_classic.jpg", "coca_zero.jpg", "pepsi_classic.jpg", "coop_cola.jpg", "conad_cola.jpg",
            "chinotto.png", "molecola.jpg", "fanta.jpg", "fanta_zero.png", "conad_orange.jpeg",
            "sprite.jpg", "cedrata.jpg", "estathe_limone.jpg", "estathe_pesca.jpeg", "sanbenedetto_te.jpg",
            "gatorade.png", "powerade.jpg"),
  stringsAsFactors = FALSE
)

nome_bev     <- function(id) bevande$name[match(id, bevande$id)]
immagine_bev <- function(id) bevande$image[match(id, bevande$id)]

# Bicchiere neutro (vuoto, senza bevanda): incorporato nel codice, non serve un file in www/
BICCHIERE_NEUTRO <- paste0(
  "data:image/svg+xml;utf8,",
  "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 300 400'>",
  "<path d='M70 60 L230 60 L210 350 Q208 370 188 370 L112 370 Q92 370 90 350 Z' ",
  "fill='whitesmoke' stroke='darkgray' stroke-width='6' stroke-linejoin='round'/>",
  "<ellipse cx='150' cy='60' rx='80' ry='10' fill='gainsboro' stroke='darkgray' stroke-width='5'/>",
  "<path d='M95 110 L110 320' stroke='white' stroke-width='10' stroke-linecap='round' opacity='0.8'/>",
  "</svg>"
)

# --- 4. LISTE DI DOMANDE -----------------------------------------------------
# Ci sono 3 liste: a ogni partita ne viene estratta una a caso.
# Ogni lista ha 10 livelli, dal più facile (1) al più difficile (10).
# Ogni livello contiene:
#   coppia  -> 2 bicchieri (blind test): A e B
#   chiedi  -> quale dei due va riconosciuto (1 = A, 2 = B)
#   terna   -> 3 bicchieri (trova l'intruso): A, B e C
#   intruso -> quale dei tre è l'intruso (1 = A, 2 = B, 3 = C)
# Le posizioni sono fisse: la scheda operatore le riporta esattamente così.
livello <- function(coppia, chiedi, terna, intruso) {
  list(coppia = coppia, chiedi = chiedi, terna = terna, intruso = intruso)
}

LISTE <- list(
  
  # ---- LISTA 1 ----
  list(
    livello(c("coca_classic", "fanta"), 1, c("coca_classic", "pepsi_classic", "fanta"), 3),            # 1 facile
    livello(c("chinotto", "sprite"), 1, c("coca_classic", "sprite", "fanta"), 1),                     # 2 facile
    livello(c("cedrata", "gatorade"), 1, c("estathe_limone", "pepsi_classic", "sanbenedetto_te"), 2), # 3 medio-facile
    livello(c("chinotto", "coca_classic"), 2, c("fanta", "sprite", "cedrata"), 1),                    # 4 medio
    livello(c("fanta", "conad_orange"), 1, c("coca_zero", "coca_classic", "sprite"), 3),              # 5 medio
    livello(c("coca_classic", "pepsi_classic"), 2, c("coop_cola", "chinotto", "coca_classic"), 2),    # 6 medio-difficile
    livello(c("pepsi_classic", "coca_zero"), 2, c("fanta", "fanta_zero", "conad_orange"), 3),         # 7 difficile
    livello(c("estathe_limone", "estathe_pesca"), 2, c("sanbenedetto_te", "estathe_limone", "estathe_pesca"), 1), # 8 difficile
    livello(c("powerade", "gatorade"), 1, c("coop_cola", "chinotto", "conad_cola"), 2),               # 9 difficile: stesso colore
    livello(c("coop_cola", "conad_cola"), 2, c("gatorade", "powerade", "cedrata"), 3)                 # 10 il più difficile
  ),
  
  # ---- LISTA 2 ----
  list(
    livello(c("fanta", "chinotto"), 1, c("coca_classic", "pepsi_classic", "estathe_limone"), 3),      # 1 facile
    livello(c("coca_zero", "sprite"), 1, c("fanta", "conad_orange", "chinotto"), 3),                  # 2 facile
    livello(c("sanbenedetto_te", "pepsi_classic"), 1, c("coca_classic", "estathe_pesca", "sanbenedetto_te"), 1), # 3 medio-facile
    livello(c("fanta_zero", "cedrata"), 2, c("sprite", "cedrata", "fanta_zero"), 3),                  # 4 medio
    livello(c("coop_cola", "pepsi_classic"), 1, c("coop_cola", "fanta", "conad_cola"), 2),            # 5 medio
    livello(c("coca_zero", "chinotto"), 2, c("chinotto", "coca_zero", "coca_classic"), 1),            # 6 medio-difficile
    livello(c("cedrata", "sprite"), 2, c("conad_orange", "fanta", "fanta_zero"), 2),                  # 7 difficile
    livello(c("conad_orange", "fanta_zero"), 1, c("estathe_limone", "sanbenedetto_te", "estathe_pesca"), 3), # 8 difficile
    livello(c("sanbenedetto_te", "estathe_limone"), 2, c("conad_cola", "coca_zero", "coca_classic"), 1),    # 9 difficile
    livello(c("coca_classic", "conad_cola"), 2, c("coop_cola", "pepsi_classic", "conad_cola"), 2)     # 10 il più difficile
  ),
  
  # ---- LISTA 3 ----
  list(
    livello(c("coca_classic", "sprite"), 1, c("coca_classic", "sprite", "cedrata"), 1),               # 1 facile
    livello(c("sanbenedetto_te", "fanta"), 1, c("conad_orange", "fanta", "sanbenedetto_te"), 3),      # 2 facile
    livello(c("coca_zero", "estathe_pesca"), 2, c("estathe_limone", "coca_zero", "sanbenedetto_te"), 2), # 3 medio-facile
    livello(c("fanta", "sprite"), 1, c("cedrata", "fanta", "sprite"), 1),                             # 4 medio
    livello(c("conad_cola", "pepsi_classic"), 1, c("fanta_zero", "pepsi_classic", "conad_cola"), 1),  # 5 medio
    livello(c("pepsi_classic", "chinotto"), 2, c("coca_zero", "coca_classic", "pepsi_classic"), 3),   # 6 medio-difficile
    livello(c("fanta", "fanta_zero"), 2, c("conad_orange", "fanta_zero", "fanta"), 2),                # 7 difficile
    livello(c("estathe_pesca", "sanbenedetto_te"), 2, c("estathe_limone", "estathe_pesca", "sanbenedetto_te"), 1), # 8 difficile
    livello(c("coop_cola", "conad_cola"), 1, c("conad_cola", "coca_zero", "coop_cola"), 2),           # 9 difficile
    livello(c("powerade", "gatorade"), 2, c("coca_zero", "pepsi_classic", "conad_cola"), 3)           # 10 il più difficile
  )
)

# Controllo delle liste all'avvio: segnala nel terminale eventuali errori di scrittura
valida_liste <- function() {
  problemi <- character(0)
  for (l in seq_along(LISTE)) {
    if (length(LISTE[[l]]) != 10) {
      problemi <- c(problemi, paste("Lista", l, "non ha 10 domande"))
    }
    for (k in seq_along(LISTE[[l]])) {
      lv <- LISTE[[l]][[k]]
      if (!all(c(lv$coppia, lv$terna) %in% bevande$id)) {
        problemi <- c(problemi, paste("Lista", l, "domanda", k, ": nome bevanda sbagliato"))
      }
      if (length(unique(lv$coppia)) != 2 || length(unique(lv$terna)) != 3) {
        problemi <- c(problemi, paste("Lista", l, "domanda", k, ": bevande ripetute"))
      }
    }
  }
  if (length(problemi) > 0) message("ATTENZIONE liste domande:\n", paste(problemi, collapse = "\n"))
  invisible(problemi)
}
valida_liste()

scegli_modalita <- function(mode) {
  if (mode == "mix") sample(c(2, 3), 1) else as.numeric(mode)
}

# Restituisce le bevande del turno nell'ordine fisso dei bicchieri A, B, C
genera_turno <- function(turno, modalita, lista) {
  lv <- LISTE[[lista]][[turno]]
  if (modalita == 3) {
    list(modalita = 3, lista = lista, turno = turno, ids = lv$terna,
         correct_idx = lv$intruso, target = NULL)
  } else {
    list(modalita = 2, lista = lista, turno = turno, ids = lv$coppia,
         correct_idx = lv$chiedi, target = nome_bev(lv$coppia[lv$chiedi]))
  }
}

# Scheda operatore: per ogni lista e domanda, i bicchieri da preparare
tabella_scheda_operatore <- function() {
  righe <- list()
  for (l in seq_along(LISTE)) {
    for (k in seq_along(LISTE[[l]])) {
      lv <- LISTE[[l]][[k]]
      righe[[length(righe) + 1]] <- data.frame(
        Lista = l, Domanda = k, Bicchieri = 2,
        "Bicchiere A" = nome_bev(lv$coppia[1]),
        "Bicchiere B" = nome_bev(lv$coppia[2]),
        "Bicchiere C" = "-",
        "Risposta corretta" = paste0(LETTERS[lv$chiedi], ": ", nome_bev(lv$coppia[lv$chiedi])),
        check.names = FALSE, stringsAsFactors = FALSE
      )
      righe[[length(righe) + 1]] <- data.frame(
        Lista = l, Domanda = k, Bicchieri = 3,
        "Bicchiere A" = nome_bev(lv$terna[1]),
        "Bicchiere B" = nome_bev(lv$terna[2]),
        "Bicchiere C" = nome_bev(lv$terna[3]),
        "Risposta corretta" = paste0("intruso ", LETTERS[lv$intruso], ": ", nome_bev(lv$terna[lv$intruso])),
        check.names = FALSE, stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, righe)
}

# --- 5. GRAFICO ALBERO -------------------------------------------------------
# esito: vettore con nomi (id bevanda) -> "verde" (corretta) o "rosso" (errata)
colore_tip <- function(esito, id) {
  e <- esito[id]
  if (is.na(e)) "#6c757d" else if (e == "verde") "#28a745" else "#dc3545"
}

plot_albero <- function(visti, esito, size = 5.5) {
  tips <- intersect(tree$tip.label, visti)
  if (length(tips) < 2) tips <- tree$tip.label
  sub <- ape::keep.tip(tree, tips)
  
  dati <- data.frame(
    label = sub$tip.label,
    clean = nome_bev(sub$tip.label),
    color = unname(vapply(sub$tip.label, function(id) colore_tip(esito, id), character(1))),
    stringsAsFactors = FALSE
  )
  
  grafico <- ggtree(sub, size = 1.2) %<+% dati +
    geom_tiplab(aes(label = clean, color = color), size = size, align = TRUE,
                offset = 0.05, fontface = "bold") +
    geom_nodepoint(color = "#007bff", size = 3) +
    scale_color_identity() +
    theme_tree2() +
    theme(axis.text.x = element_text(size = 12, face = "bold"))
  
  xmax <- max(grafico$data$x, na.rm = TRUE)
  grafico + xlim(0, xmax * 1.45)
}

# --- 6. MODELLO RANDOM FOREST -----------------------------------------------
# Restituisce sempre una lista: ok = TRUE/FALSE, e un messaggio se non può girare
calcola_modello <- function(dati) {
  d <- dati[!is.na(dati$eta) & !is.na(dati$freq) & !is.na(dati$corretto), , drop = FALSE]
  if (nrow(d) < 5) {
    return(list(ok = FALSE, msg = paste0("Il modello serve almeno 5 risposte (per ora ce ne sono ", nrow(d), ").")))
  }
  if (length(unique(d$corretto)) < 2) {
    return(list(ok = FALSE, msg = "Il modello ha bisogno sia di risposte corrette sia di risposte sbagliate."))
  }
  if (length(unique(d$eta)) < 2 && length(unique(d$freq)) < 2) {
    return(list(ok = FALSE, msg = "Finora tutti hanno la stessa età e lo stesso consumo: il modello non può distinguere nessuno."))
  }
  d$y <- factor(ifelse(d$corretto, "Si", "No"), levels = c("No", "Si"))
  fit <- tryCatch(randomForest(y ~ eta + freq, data = d, ntree = 200), error = function(e) e)
  if (inherits(fit, "error")) {
    return(list(ok = FALSE, msg = paste("Errore nel modello:", fit$message)))
  }
  list(ok = TRUE, fit = fit, data = d, n = nrow(d))
}

# Probabilità stimata che il giocatore indovini, in base a età e consumo
probabilita_utente <- function(mod, eta, freq) {
  nd <- data.frame(eta = as.numeric(eta), freq = as.numeric(freq))
  as.numeric(predict(mod$fit, newdata = nd, type = "prob")[1, "Si"])
}

# Curva ROC sulle probabilità out-of-bag (ogni risposta valutata senza usarla per addestrare)
curva_roc <- function(mod) {
  prob <- mod$fit$votes[, "Si"]
  pROC::roc(mod$data$y, prob, levels = c("No", "Si"), direction = "<", quiet = TRUE)
}

# --- 7. CLASSIFICA -----------------------------------------------------------
# Per ogni partecipante: miglior punteggio tra le partite complete (10 risposte)
classifica_df <- function(dati) {
  d <- dati[!is.na(dati$id_partita) & nzchar(dati$id_partita) &
              !is.na(dati$nickname) & nzchar(dati$nickname), , drop = FALSE]
  if (nrow(d) == 0) return(NULL)
  
  # una riga per partita: nickname, risposte date, risposte corrette
  partite <- do.call(rbind, lapply(split(d, d$id_partita), function(p) {
    data.frame(nickname = p$nickname[1], risposte = nrow(p),
               corrette = sum(p$corretto), stringsAsFactors = FALSE)
  }))
  partite <- partite[partite$risposte == 10, , drop = FALSE]   # solo partite complete
  if (nrow(partite) == 0) return(NULL)
  
  # una riga per partecipante: miglior punteggio e numero di partite complete
  utenti <- do.call(rbind, lapply(split(partite, partite$nickname), function(u) {
    data.frame(nickname = u$nickname[1], punteggio = max(u$corrette),
               partite = nrow(u), stringsAsFactors = FALSE)
  }))
  utenti <- utenti[order(-utenti$punteggio, utenti$nickname), , drop = FALSE]
  
  cl <- data.frame(
    Posto = rank(-utenti$punteggio, ties.method = "min"),
    Nickname = utenti$nickname,
    "Miglior punteggio (su 10)" = utenti$punteggio,
    "Partite complete" = utenti$partite,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  rownames(cl) <- NULL
  cl
}

# --- 8. INTERFACCIA UTENTE ---------------------------------------------------
ui <- fluidPage(
  useShinyjs(),
  
  tags$head(
    tags$script(src = "https://cdn.jsdelivr.net/npm/canvas-confetti@1.6.0/dist/confetti.browser.min.js"),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('triggerConfetti', function(message) {
        if (typeof confetti === 'function') {
          confetti({ particleCount: 100, spread: 70, origin: { y: 0.6 } });
        }
      });
    ")),
    tags$style(HTML("
      body {
        background-image: url('Wallpaper.jpg');
        background-size: cover;
        background-repeat: no-repeat;
        background-attachment: fixed;
        min-height: 100vh;
        margin: 0;
        font-family: 'Poppins', sans-serif;
        color: #000000;
      }
      .well {
        background-color: rgba(255, 255, 255, 0.96) !important;
        border-radius: 15px;
        box-shadow: 0 4px 20px rgba(0,0,0,0.15);
        padding: 25px;
        margin: 20px auto;
      }
      .game-title {
        font-size: 38px;
        font-weight: 900;
        color: white;
        -webkit-text-stroke: 1.5px black;
        text-align: center;
        margin: 15px auto;
        width: 100%;
      }
      .panel-small { max-width: 680px; width: 90%; margin-top: 80px; }
      .panel-large { max-width: 1100px; width: 95%; margin: 80px auto 20px auto; }
      .panel-dashboard { max-width: 1200px; width: 95%; margin: 80px auto; }
      .drink-card-img {
        width: 100%;
        height: 260px;
        object-fit: contain;
        background-color: #f8f9fa;
        border-radius: 12px;
        padding: 10px;
        cursor: pointer;
        transition: transform 0.2s ease;
      }
      .drink-card-img:hover { transform: scale(1.03); }
      @media print {
        body { background: white !important; }
        #menu_button, #menu_dropdown, .no-print, .shiny-notification { display: none !important; }
        .well { box-shadow: none !important; }
      }
    "))
  ),
  
  tags$div(
    style = "position: fixed; top: 10px; right: 10px; z-index: 2000;",
    tags$button(
      id = "menu_button",
      HTML("🏠 Menù ▼"),
      style = "font-size:14px; padding:6px 12px; border-radius:8px; border:none; background-color: rgba(255,255,255,0.9); cursor:pointer; font-weight:bold;"
    ),
    tags$div(
      id = "menu_dropdown",
      style = "display:none; position:absolute; right:0; background-color: rgba(255,255,255,0.95); border-radius: 8px; margin-top:5px; padding:5px; min-width:180px; box-shadow: 0px 4px 8px rgba(0,0,0,0.2);",
      actionButton("go_dashboard_menu", "📊 Classifica & IA", style = "width:100%; border:none; background:none; text-align:left; padding:8px;"),
      actionButton("go_tree_menu", "🌳 Albero Tassonomico", style = "width:100%; border:none; background:none; text-align:left; padding:8px;"),
      actionButton("go_operatore", "🔒 Scheda operatore", style = "width:100%; border:none; background:none; text-align:left; padding:8px;")
    )
  ),
  
  tags$script(HTML("
    $(document).ready(function() {
      $('#menu_button').click(function() { $('#menu_dropdown').toggle(); });
      $(document).click(function(event) {
        if(!$(event.target).closest('#menu_button, #menu_dropdown').length) { $('#menu_dropdown').hide(); }
      });
    });
  ")),
  
  tags$img(
    src = "logo Sapienza (rgb).png",
    style = "position: fixed; bottom: 10px; left: 10px; width: 220px; height: auto; z-index: 1500;",
    onerror = "this.style.display='none'"
  ),
  
  uiOutput("main_ui")
)

# --- 9. LOGICA SERVER --------------------------------------------------------
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    visti = character(0),    # bevande mostrate nella partita (per l'albero)
    esito = character(0),    # "verde" o "rosso" per ogni bevanda
    aggiornamento = 0,       # aumenta quando bisogna rileggere le risposte
    operatore_ok = FALSE     # TRUE solo dopo il PIN corretto
  )
  
  game <- reactiveValues(
    stage = "start",
    id = NULL,
    nickname = NULL,
    eta = 20,
    genere = "Maschio",
    freq = 3,
    fortuna = "Molto",
    maratona_ny = "",
    sabato_sera = "",
    mode = "3",
    lista = NULL,
    turn = 1,
    correct = 0,
    current = NULL,
    choice_made = FALSE,
    selected = NULL
  )
  
  # Tutte le risposte, rilette dal foglio solo quando servono
  dati_globali <- reactive({
    rv$aggiornamento
    leggi_risposte()
  })
  
  # Modello random forest calcolato su tutti i partecipanti
  modello <- reactive({
    dg <- dati_globali()
    if (!is.null(dg$errore)) {
      return(list(ok = FALSE, msg = paste("Dati non disponibili:", dg$errore)))
    }
    calcola_modello(dg$dati)
  })
  
  output$main_ui <- renderUI({
    
    # --- 1. START ---
    if (game$stage == "start") {
      tagList(
        div(class = "game-title", "Disseta la tua sete di dati 🥤"),
        wellPanel(
          class = "panel-small",
          h3("Siete pronti alla sfida delle bibite?"),
          textInput("nickname", "🌟 Inserisci un nickname:"),
          numericInput("eta", "🎂 Quanti anni hai?", value = 20, min = 2, max = 100),
          
          radioButtons("genere", "👦👧🌈 Genere:",
                       choices = c("Maschio", "Femmina", "Non-binario", "Preferisco non dirlo"),
                       selected = "Maschio", inline = FALSE),
          
          sliderInput("freq", "🥤 Quante bibite gassate bevi a settimana?", min = 0, max = 14, value = 3),
          radioButtons("fortuna", "🍀 Quanto ti senti fortunat* oggi?", choices = c("Poco", "Molto"), inline = TRUE),
          
          hr(),
          h4("📝 Domande sullo stile di vita (Opzionali)"),
          textInput("maratona_ny", "🏃 Se ti invitassero a New York, faresti la maratona?",
                    placeholder = "es. Sì subito / Solo se posso fare foto / No..."),
          textInput("sabato_sera", "🍕 Descrivi il tuo sabato sera ideale:",
                    placeholder = "es. Divano e serie TV / Discoteca con amici / Cena fuori..."),
          
          hr(),
          radioButtons("mode_select", "🎯 Scegli la modalità del gioco:",
                       choices = c("Trova l'Intruso (3 Bicchieri)" = "3",
                                   "Assaggio alla Cieca (2 Bicchieri)" = "2",
                                   "Mista / Alternata (Casuale)" = "mix"),
                       selected = "3"),
          
          actionButton("start_game", "🚀 Inizia il gioco", class = "btn-primary btn-lg",
                       style = "width:100%; font-size:18px; padding:10px 30px;")
        )
      )
      
      # --- 2. GIOCO ---
    } else if (game$stage == "play") {
      curr <- game$current
      n <- length(curr$ids)
      col_w <- if (n == 3) 4 else 6
      
      wellPanel(
        class = "panel-large",
        div(style = "text-align:center;",
            h3(paste0("Lista ", curr$lista, " · Domanda #", game$turn, " di 10")),
            h4(if (n == 3) "Trova l'Intruso (3 Bicchieri): quale bevanda è l'intruso?"
               else paste0("Assaggio alla Cieca (2 Bicchieri): quale dei due bicchieri è ", curr$target, "?"))),
        br(),
        
        if (game$choice_made) {
          corretto <- game$selected == curr$correct_idx
          div(style = "text-align:center; padding:10px; margin-bottom:15px;",
              if (corretto) {
                tags$h3(paste0("✅ Complimenti, ", game$nickname, "! Hai indovinato!"),
                        style = "color:#155724; background:#d4edda; padding:12px; border-radius:10px;")
              } else {
                tags$h3(paste0("❌ Oh no ", game$nickname, ", non era la risposta corretta..."),
                        style = "color:#721c24; background:#f8d7da; padding:12px; border-radius:10px;")
              })
        },
        
        fluidRow(
          lapply(seq_len(n), function(i) {
            id <- curr$ids[i]
            # Blind test: prima della risposta il bicchiere è neutro, senza la foto della bevanda
            nascosto <- n == 2 && !game$choice_made
            etichetta <- if (nascosto) {
              paste("Bicchiere", LETTERS[i])
            } else {
              paste0("Bicchiere ", LETTERS[i], ": ", nome_bev(id))
            }
            
            bordo <- "#495057"
            if (game$choice_made) {
              if (i == game$selected) {
                bordo <- if (i == curr$correct_idx) "#28a745" else "#dc3545"
              } else if (i == curr$correct_idx) {
                bordo <- "#28a745"
              }
            }
            
            click <- if (!game$choice_made) sprintf("Shiny.setInputValue('choice_%d', Math.random());", i)
            
            immagine <- if (nascosto) {
              tags$img(
                class = "drink-card-img",
                src = BICCHIERE_NEUTRO,
                style = paste0("border: 4px solid ", bordo),
                onclick = click
              )
            } else {
              tags$img(
                class = "drink-card-img",
                src = immagine_bev(id),
                style = paste0("border: 4px solid ", bordo),
                onclick = click,
                onerror = "this.style.display='none'"
              )
            }
            
            column(
              col_w,
              div(
                style = "text-align:center; padding:10px; background:white; border-radius:15px; box-shadow:0 4px 10px rgba(0,0,0,0.08); margin-bottom:15px;",
                immagine,
                div(style = "margin-top:10px; font-weight:bold; font-size:18px;", etichetta)
              )
            )
          })
        ),
        
        if (game$choice_made) {
          div(style = "text-align:center; margin-top:20px;",
              actionButton("view_step_tree", "Vedi albero in costruzione 🌳 ➡️", class = "btn-success btn-lg",
                           style = "font-size:18px; padding:10px 40px;"))
        }
      )
      
      # --- 3. ALBERO IN COSTRUZIONE ---
    } else if (game$stage == "turn_tree") {
      wellPanel(
        class = "panel-large",
        h2("Il tuo albero finora 🌳"),
        plotOutput("turn_tree_plot", height = "480px"),
        div(style = "text-align:center; margin-top:20px;",
            actionButton("continue_after_tree",
                         if (game$turn == 10) "Vai ai risultati finali 🎉" else "Continua ➡️",
                         class = "btn-primary btn-lg",
                         style = "font-size:18px; padding:10px 40px;"))
      )
      
      # --- 4. FINE PARTITA: punteggio e classifica ---
    } else if (game$stage %in% c("lose", "win")) {
      wellPanel(
        class = "panel-small",
        h2(if (game$stage == "win") "Perfetto! 🎉" else "Partita Completata!"),
        h3(paste("Punteggio:", game$correct, "/ 10")),
        uiOutput("posto_utente"),
        h4("🏆 Classifica di tutti i partecipanti"),
        tableOutput("tabella_classifica"),
        br(),
        div(style = "text-align:center;",
            actionButton("go_tree_dashboard", "🌳 Albero finale", class = "btn-info btn-lg",
                         style = "font-size:16px; padding:10px 25px; margin:5px;"),
            actionButton("go_leaderboard", "📊 Dashboard & IA", class = "btn-primary btn-lg",
                         style = "font-size:16px; padding:10px 25px; margin:5px;"),
            actionButton("restart", "🔄 Nuova partita", class = "btn-warning btn-lg",
                         style = "font-size:16px; padding:10px 25px; margin:5px;"))
      )
      
      # --- 5. ALBERO FINALE ---
    } else if (game$stage == "tree_dashboard") {
      wellPanel(
        class = "panel-large",
        h2("Il tuo percorso nella Tassonomia delle Bibite 🌳"),
        plotOutput("final_tree", height = "480px"),
        div(style = "text-align:center; margin-top:20px;",
            actionButton("go_leaderboard", "📊 Classifica & Dashboard IA", class = "btn-primary btn-lg",
                         style = "font-size:16px; padding:10px 25px;"))
      )
      
      # --- 6. CLASSIFICA & DASHBOARD IA ---
    } else if (game$stage == "dashboard") {
      wellPanel(
        class = "panel-dashboard",
        h2("📊 Classifica & Dashboard IA"),
        p(style = "font-size:0.9em; color:#555;", paste("Salvataggio risposte:", STATO_SALVATAGGIO)),
        fluidRow(
          column(6,
                 h4("🏆 Classifica"),
                 tableOutput("tabella_classifica"),
                 br(),
                 actionButton("aggiorna", "🔄 Aggiorna dati", class = "btn-secondary")),
          column(6,
                 h4("🤖 Modello IA (Random Forest)"),
                 uiOutput("predizione_realtime_ui"),
                 br(),
                 h4("📈 Curva ROC"),
                 plotOutput("roc_combined", height = "300px"))
        ),
        hr(),
        h4("📋 Ultime risposte registrate"),
        tableOutput("tabella_risposte"),
        hr(),
        actionButton("restart", "🔄 Nuova partita", class = "btn-warning btn-lg",
                     style = "font-size:16px; padding:10px 30px;")
      )
      
      # --- 7. SCHEDA OPERATORE (riservata, con PIN) ---
    } else if (game$stage == "operatore") {
      if (!isTRUE(rv$operatore_ok)) {
        wellPanel(
          class = "panel-small",
          h2("🔒 Scheda operatore"),
          p("Pagina riservata: serve il PIN."),
          passwordInput("pin_operatore", "PIN:"),
          actionButton("entra_operatore", "Entra", class = "btn-primary"),
          actionButton("esci_operatore", "⬅ Indietro", class = "btn-default")
        )
      } else {
        wellPanel(
          class = "panel-large",
          div(class = "no-print", style = "text-align:right;",
              tags$button("🖨️ Stampa", onclick = "window.print()", class = "btn btn-default"),
              actionButton("esci_operatore", "⬅ Chiudi", class = "btn-default")),
          h2("Scheda operatore (riservata)"),
          uiOutput("in_corso"),
          p("Prepara i bicchieri come indicato. Non mostrare questa pagina ai partecipanti."),
          tableOutput("tabella_operatore")
        )
      }
    }
  })
  
  # Avvio gioco
  observeEvent(input$start_game, {
    nick <- trimws(input$nickname)
    req(nzchar(nick), !is.na(input$eta))
    
    game$id <- sprintf("%s_%06d", format(Sys.time(), "%Y%m%d%H%M%S"), sample.int(999999, 1))
    game$nickname <- nick
    game$eta <- input$eta
    game$genere <- input$genere
    game$freq <- input$freq
    game$fortuna <- input$fortuna
    game$maratona_ny <- if (!is.null(input$maratona_ny) && input$maratona_ny != "") input$maratona_ny else "Non specificato"
    game$sabato_sera <- if (!is.null(input$sabato_sera) && input$sabato_sera != "") input$sabato_sera else "Non specificato"
    game$mode <- input$mode_select
    game$lista <- sample(1:3, 1)
    game$turn <- 1
    game$correct <- 0
    
    rv$visti <- character(0)
    rv$esito <- character(0)
    
    game$current <- genera_turno(1, scegli_modalita(game$mode), game$lista)
    rv$visti <- unique(c(rv$visti, game$current$ids))
    game$choice_made <- FALSE
    game$selected <- NULL
    game$stage <- "play"
  })
  
  # Gestione selezione (fino a 3 bicchieri)
  lapply(1:3, function(i) {
    observeEvent(input[[paste0("choice_", i)]], {
      req(!game$choice_made, game$stage == "play")
      curr <- game$current
      if (i > length(curr$ids)) return()
      
      game$selected <- i
      game$choice_made <- TRUE
      
      is_correct <- i == curr$correct_idx
      id_scelto <- curr$ids[i]
      id_giusto <- curr$ids[curr$correct_idx]
      
      if (is_correct) {
        game$correct <- game$correct + 1
        session$sendCustomMessage("triggerConfetti", list())
      }
      
      # Verde per la bevanda giusta, rosso per tutte le altre della domanda
      for (id in curr$ids) {
        rv$esito[id] <- if (id == id_giusto) "verde" else "rosso"
      }
      
      nuovo <- data.frame(
        timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        nickname = game$nickname,
        eta = as.numeric(game$eta),
        genere = game$genere,
        freq = as.numeric(game$freq),
        fortuna = game$fortuna,
        maratona_ny = game$maratona_ny,
        sabato_sera = game$sabato_sera,
        turno = game$turn,
        modalita = curr$modalita,
        bevanda_giusta = id_giusto,
        bevanda_scelta = id_scelto,
        scelta = paste0("Opzione_", i),
        corretto = is_correct,
        id_partita = game$id,
        lista = curr$lista,
        stringsAsFactors = FALSE
      )
      
      errore <- salva_risposta(nuovo)
      if (!is.null(errore)) {
        showNotification(paste("ATTENZIONE: risposta NON salvata.", errore),
                         type = "error", duration = NULL)
      }
    })
  })
  
  observeEvent(input$view_step_tree, { game$stage <- "turn_tree" })
  
  observeEvent(input$continue_after_tree, {
    if (game$turn == 10) {
      rv$aggiornamento <- rv$aggiornamento + 1   # rilegge le risposte per la classifica
      game$stage <- if (game$correct >= 6) "win" else "lose"
    } else {
      game$turn <- game$turn + 1
      game$current <- genera_turno(game$turn, scegli_modalita(game$mode), game$lista)
      rv$visti <- unique(c(rv$visti, game$current$ids))
      game$choice_made <- FALSE
      game$selected <- NULL
      game$stage <- "play"
    }
  })
  
  observeEvent(input$go_tree_dashboard, { game$stage <- "tree_dashboard" })
  observeEvent(input$go_leaderboard, { game$stage <- "dashboard" })
  observeEvent(input$go_dashboard_menu, { game$stage <- "dashboard"; shinyjs::hide("menu_dropdown") })
  observeEvent(input$go_tree_menu, { game$stage <- "tree_dashboard"; shinyjs::hide("menu_dropdown") })
  observeEvent(input$restart, { game$stage <- "start" })
  observeEvent(input$aggiorna, { rv$aggiornamento <- rv$aggiornamento + 1 })
  
  # Scheda operatore: si apre solo con il PIN, e si chiude tornando alla schermata precedente
  observeEvent(input$go_operatore, {
    if (game$stage != "operatore") game$stage_prima <- game$stage
    rv$operatore_ok <- FALSE
    game$stage <- "operatore"
    shinyjs::hide("menu_dropdown")
  })
  
  observeEvent(input$entra_operatore, {
    if (identical(trimws(input$pin_operatore), PIN_OPERATORE)) {
      rv$operatore_ok <- TRUE
    } else {
      showNotification("PIN errato", type = "error")
    }
  })
  
  observeEvent(input$esci_operatore, {
    rv$operatore_ok <- FALSE
    game$stage <- if (is.null(game$stage_prima)) "start" else game$stage_prima
  })
  
  output$in_corso <- renderUI({
    curr <- game$current
    prima <- game$stage_prima
    if (!isTRUE(rv$operatore_ok) || is.null(curr) || is.null(prima) ||
        !(prima %in% c("play", "turn_tree"))) {
      return(NULL)
    }
    p(strong("In gioco adesso: "),
      paste0("Lista ", curr$lista, ", domanda #", game$turn, " (", curr$modalita, " bicchieri)"))
  })
  
  output$tabella_operatore <- renderTable({
    if (!isTRUE(rv$operatore_ok)) return(NULL)
    tabella_scheda_operatore()
  }, striped = TRUE, bordered = TRUE)
  
  output$turn_tree_plot <- renderPlot({
    req(length(rv$visti) > 0)
    plot_albero(rv$visti, rv$esito, size = 5.5)
  })
  
  output$final_tree <- renderPlot({
    req(length(rv$visti) > 0)
    plot_albero(rv$visti, rv$esito, size = 6)
  })
  
  # Posizione del giocatore in classifica
  output$posto_utente <- renderUI({
    dg <- dati_globali()
    if (!is.null(dg$errore)) {
      return(div(style = "color:#721c24;", paste("Classifica non disponibile:", dg$errore)))
    }
    cl <- classifica_df(dg$dati)
    if (is.null(cl) || is.null(game$nickname)) return(NULL)
    pos <- cl$Posto[match(game$nickname, cl$Nickname)]
    if (is.na(pos)) return(p("La tua partita non risulta ancora in classifica."))
    h4(paste0("Sei al posto ", pos, " su ", nrow(cl), " partecipanti"))
  })
  
  output$tabella_classifica <- renderTable({
    dg <- dati_globali()
    if (!is.null(dg$errore)) {
      return(data.frame(Messaggio = paste("Dati non disponibili:", dg$errore)))
    }
    cl <- classifica_df(dg$dati)
    if (is.null(cl)) {
      d <- dg$dati
      senza_id <- nrow(d) > 0 && all(is.na(d$id_partita))
      msg <- if (senza_id) {
        "Nel foglio manca la colonna id_partita: vedi le istruzioni."
      } else {
        "Nessuna partita completa ancora."
      }
      return(data.frame(Messaggio = msg))
    }
    cl
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  output$predizione_realtime_ui <- renderUI({
    mod <- modello()
    if (!isTRUE(mod$ok)) {
      return(div(style = "background:#fff3cd; padding:12px; border-radius:10px;", mod$msg))
    }
    p_si <- tryCatch(probabilita_utente(mod, game$eta, game$freq), error = function(e) NA_real_)
    if (is.na(p_si)) {
      return(div(style = "background:#fff3cd; padding:12px; border-radius:10px;",
                 "Inserisci età e consumo di bibite per vedere la predizione."))
    }
    nick <- if (is.null(game$nickname)) "" else game$nickname
    div(style = "background-color:#f0f8ff; padding:15px; border-radius:10px; border-left: 5px solid #007bff;",
        h5("🚀 Predizione Random Forest"),
        HTML(paste0("Probabilità stimata per <b>", htmltools::htmlEscape(nick),
                    "</b> di indovinare: <b>", round(p_si * 100, 1), "%</b>")),
        tags$br(),
        tags$small(paste0("Basato su ", mod$n, " risposte. Età ", game$eta,
                          ", bibite gassate a settimana: ", game$freq, ".")))
  })
  
  output$roc_combined <- renderPlot({
    mod <- modello()
    tryCatch({
      if (!isTRUE(mod$ok)) stop(mod$msg)
      roc_obj <- curva_roc(mod)
      plot(roc_obj, col = "#28a745", lwd = 2.5, main = "Curva ROC - Random Forest")
      abline(0, 1, lty = 2, col = "gray")
      legend("bottomright", legend = paste("AUC =", round(as.numeric(pROC::auc(roc_obj)), 2)),
             col = "#28a745", lwd = 2.5, bty = "n")
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste(strwrap(e$message, 45), collapse = "\n"), cex = 0.9, col = "#721c24")
    })
  })
  
  output$tabella_risposte <- renderTable({
    dg <- dati_globali()
    if (!is.null(dg$errore)) {
      return(data.frame(Messaggio = paste("Dati non disponibili:", dg$errore)))
    }
    d <- dg$dati
    if (nrow(d) == 0) return(data.frame(Messaggio = "Nessuna risposta registrata."))
    tail(d[, c("timestamp", "nickname", "eta", "genere", "corretto")], 5)
  })
}

shinyApp(ui = ui, server = server)
