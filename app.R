# ==============================================================================
# DISSETA LA TUA SETE DI DATI - Maker Faire 2026
# Risposte salvate su Google Sheets (se non disponibile, su CSV locale)
# ==============================================================================

library(shiny)
library(shinyjs)
library(ape)
library(ggtree)
library(ggplot2)
library(randomForest)
library(googlesheets4)

set.seed(123)

# --- 1. CONFIGURAZIONE -------------------------------------------------------
GS_SHEET_ID   <- "1HeKjUeP7e5qMwePfYOc9K3XOZqaW7lKCg1mUJMl7xR0"
GS_TAB        <- "risposte"               # nome della scheda nel foglio
CSV_FILE      <- "risposte_bevande.csv"   # usato solo se Google Sheets non è disponibile
PIN_OPERATORE <- "2026"                   # PIN della scheda operatore: cambialo prima dell'evento

N_DOMANDE       <- 3   # domande per partita
SOGLIA_VITTORIA <- 2   # risposte corrette minime per la schermata "win"

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
beverage_tree_newick <- "((((coca_classic:1,coca_zero:1)Coca:1,((pepsi_classic:1,pepsi_zero:1)Pepsi:1,(consilia_cola:1,conad_cola:1)StoreCola:1)OtherCola:1)Cola:1,(chinotto:1,molecola:1)DarkBitter:1)DarkDrinks:1,(((fanta:1,fanta_zero:1)Fanta:1,(conad_orange:1,sanpellegrino_aranciata:1)OtherOrange:1)Orange:1,((sprite:1,7up:1)LemonLime:1,cedrata_sanbenedetto:1)ClearCitrus:1)Citrus:1,((estathe_limone:1,estathe_pesca:1)Estathe:1,sanbenedetto_te:1)Tea:1,(gatorade:1,powerade:1)Sports:1)Bibite;"

tree <- tryCatch(ape::read.tree(text = beverage_tree_newick), error = function(e) NULL)

# --- 3. BEVANDE --------------------------------------------------------------
bevande <- data.frame(
  id = c("coca_classic", "coca_zero", "pepsi_classic", "consilia_cola", "conad_cola",
         "chinotto", "molecola", "fanta", "fanta_zero", "conad_orange",
         "sprite", "cedrata_sanbenedetto", "estathe_limone", "estathe_pesca", "sanbenedetto_te",
         "gatorade", "powerade", "pepsi_zero", "7up", "sanpellegrino_aranciata"),
  name = c("Coca-Cola Classica", "Coca-Cola Zero", "Pepsi Classica", "Consilia Cola", "Conad Cola",
           "Chinotto Sanpellegrino", "MoleCola", "Fanta Aranciata", "Fanta Zero", "Aranciata Conad",
           "Sprite", "Cedrata San Benedetto", "Estathé Limone", "Estathé Pesca", "Tè San Benedetto",
           "Gatorade", "Powerade", "Pepsi Zero", "7Up", "Aranciata Sanpellegrino"),
  image = c("coca_classic.jpg", "coca_zero.jpg", "pepsi_classic.jpg", "consilia_cola.jpg", "conad_cola.jpg",
            "chinotto.png", "molecola.jpg", "fanta.jpg", "fanta_zero.png", "conad_orange.jpeg",
            "sprite.jpg", "cedrata_sanbenedetto.jpg", "estathe_limone.jpg", "estathe_pesca.jpeg", "sanbenedetto_te.jpg",
            "gatorade.png", "powerade.jpg", "pepsi_zero.jpeg", "7up.jpg", "sanpellegrino_aranciata.png"),
  stringsAsFactors = FALSE
)

nome_bev     <- function(id) bevande$name[match(id, bevande$id)]
immagine_bev <- function(id) bevande$image[match(id, bevande$id)]

# Bicchiere neutro
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
# Ogni lista ha N_DOMANDE (3) domande, in ordine: 1 facile, 2 medio, 3 difficile.
# coppia(x, y, risposta): assaggio alla cieca, "risposta" = posizione della bevanda richiesta
# terna(x, y, z, risposta): trova l'intruso, "risposta" = posizione dell'intruso
coppia <- function(x, y, risposta) list(ids = c(x, y), risposta = risposta)
terna  <- function(x, y, z, risposta) list(ids = c(x, y, z), risposta = risposta)

# MODALITÀ 2 BICCHIERI (Trova la bevanda richiesta): 10 liste
LISTE_2 <- list(
  # ---- LISTA 1 ----
  list(
    coppia("coca_classic", "fanta", 1),                                 # 1 facile
    coppia("consilia_cola", "chinotto", 2),                             # 2 medio
    coppia("sprite", "7up", 2)                                          # 3 difficile: quasi identiche
  ),
  # ---- LISTA 2 ----
  list(
    coppia("chinotto", "sprite", 1),                                    # 1 facile
    coppia("fanta", "conad_orange", 1),                                 # 2 medio
    coppia("coca_classic", "pepsi_classic", 2)                          # 3 difficile: la sfida classica
  ),
  # ---- LISTA 3 ----
  list(
    coppia("gatorade", "cedrata_sanbenedetto", 2),                      # 1 facile
    coppia("coca_zero", "chinotto", 2),                                 # 2 medio
    coppia("fanta", "fanta_zero", 1)                                    # 3 difficile: zuccheri vs edulcoranti
  ),
  # ---- LISTA 4 ----
  list(
    coppia("sprite", "coca_classic", 2),                                # 1 facile
    coppia("fanta_zero", "cedrata_sanbenedetto", 1),                    # 2 medio
    coppia("consilia_cola", "conad_cola", 2)                            # 3 difficile: sottomarca vs sottomarca
  ),
  # ---- LISTA 5 ----
  list(
    coppia("estathe_pesca", "coca_zero", 1),                            # 1 facile
    coppia("pepsi_classic", "chinotto", 2),                             # 2 medio
    coppia("fanta", "sanpellegrino_aranciata", 1)                       # 3 difficile: dolce vs amara
  ),
  # ---- LISTA 6 ----
  list(
    coppia("sanbenedetto_te", "fanta", 2),                              # 1 facile
    coppia("conad_orange", "cedrata_sanbenedetto", 1),                  # 2 medio
    coppia("coca_zero", "pepsi_zero", 1)                                # 3 difficile: sfida tra le Zero
  ),
  # ---- LISTA 7 ----
  list(
    coppia("powerade", "pepsi_classic", 1),                             # 1 facile
    coppia("conad_cola", "coca_zero", 2),                               # 2 medio
    coppia("estathe_limone", "estathe_pesca", 2)                        # 3 difficile: stesso tè, due gusti
  ),
  # ---- LISTA 8 ----
  list(
    coppia("sanpellegrino_aranciata", "estathe_limone", 2),             # 1 facile
    coppia("sprite", "cedrata_sanbenedetto", 1),                        # 2 medio
    coppia("gatorade", "powerade", 1)                                   # 3 difficile: sfida tra sport drink
  ),
  # ---- LISTA 9 ----
  list(
    coppia("conad_cola", "sanbenedetto_te", 2),                         # 1 facile
    coppia("7up", "fanta", 1),                                          # 2 medio
    coppia("coca_classic", "coca_zero", 2)                              # 3 difficile: zucchero vs zero
  ),
  # ---- LISTA 10 ----
  list(
    coppia("cedrata_sanbenedetto", "pepsi_zero", 1),                    # 1 facile
    coppia("conad_orange", "7up", 2),                                   # 2 medio
    coppia("pepsi_classic", "pepsi_zero", 1)                            # 3 difficile: zucchero vs zero
  )
)

# MODALITÀ 3 BICCHIERI (Trova l'intruso): 10 liste
LISTE_3 <- list(
  # ---- LISTA 1 ----
  list(
    terna("gatorade", "coca_classic", "pepsi_classic", 1),              # 1 facile
    terna("sprite", "conad_orange", "cedrata_sanbenedetto", 2),         # 2 medio
    terna("coca_classic", "coca_zero", "pepsi_classic", 2)              # 3 difficile: unica senza zucchero
  ),
  # ---- LISTA 2 ----
  list(
    terna("coca_zero", "fanta", "pepsi_classic", 2),                    # 1 facile
    terna("sprite", "consilia_cola", "conad_cola", 1),                  # 2 medio
    terna("fanta", "sanpellegrino_aranciata", "fanta_zero", 2)          # 3 difficile: unica non Fanta
  ),
  # ---- LISTA 3 ----
  list(
    terna("gatorade", "powerade", "fanta", 3),                          # 1 facile
    terna("estathe_pesca", "coca_zero", "sanbenedetto_te", 2),          # 2 medio
    terna("cedrata_sanbenedetto", "sprite", "7up", 1)                   # 3 difficile: unica non Lemon-Lime
  ),
  # ---- LISTA 4 ----
  list(
    terna("cedrata_sanbenedetto", "fanta", "conad_orange", 1),          # 1 facile
    terna("pepsi_classic", "coca_classic", "chinotto", 3),              # 2 medio
    terna("consilia_cola", "pepsi_classic", "conad_cola", 2)            # 3 difficile: unica di marca
  ),
  # ---- LISTA 5 ----
  list(
    terna("estathe_limone", "sanbenedetto_te", "consilia_cola", 3),     # 1 facile
    terna("chinotto", "coca_zero", "conad_cola", 1),                    # 2 medio
    terna("coca_classic", "coca_zero", "pepsi_zero", 1)                 # 3 difficile: unica con zucchero
  ),
  # ---- LISTA 6 ----
  list(
    terna("sprite", "pepsi_classic", "cedrata_sanbenedetto", 2),        # 1 facile
    terna("conad_orange", "estathe_pesca", "estathe_limone", 1),        # 2 medio
    terna("estathe_limone", "estathe_pesca", "sanbenedetto_te", 3)      # 3 difficile: unica non Estathé
  ),
  # ---- LISTA 7 ----
  list(
    terna("estathe_pesca", "powerade", "gatorade", 1),                  # 1 facile
    terna("coca_zero", "pepsi_zero", "fanta_zero", 3),                  # 2 medio
    terna("coca_classic", "conad_cola", "pepsi_classic", 2)             # 3 difficile: unica sottomarca
  ),
  # ---- LISTA 8 ----
  list(
    terna("pepsi_zero", "gatorade", "coca_zero", 2),                    # 1 facile
    terna("fanta", "sanpellegrino_aranciata", "7up", 3),                # 2 medio
    terna("coca_classic", "coca_zero", "consilia_cola", 3)              # 3 difficile: unica non Coca-Cola
  ),
  # ---- LISTA 9 ----
  list(
    terna("fanta_zero", "estathe_limone", "fanta", 2),                  # 1 facile
    terna("gatorade", "coca_classic", "powerade", 2),                   # 2 medio
    terna("sprite", "7up", "fanta", 3)                                  # 3 difficile: intruso di gusto
  ),
  # ---- LISTA 10 ----
  list(
    terna("conad_cola", "sanpellegrino_aranciata", "coca_classic", 2),  # 1 facile
    terna("sprite", "cedrata_sanbenedetto", "chinotto", 3),             # 2 medio
    terna("estathe_limone", "sprite", "7up", 1)                         # 3 difficile: unica non gassata
  )
)

# MODALITÀ MISTA: 10 liste FISSE. In ogni lista il tipo di ogni domanda (2 o 3 bicchieri)
# è già deciso qui sotto, non viene estratto a caso.
LISTE_MIX <- list(
  # ---- LISTA M1 ----
  list(
    coppia("pepsi_zero", "estathe_pesca", 2),                           # 1 facile
    terna("pepsi_classic", "consilia_cola", "chinotto", 3),             # 2 medio
    coppia("coca_classic", "conad_cola", 1)                             # 3 difficile: marca vs sottomarca
  ),
  # ---- LISTA M2 ----
  list(
    terna("fanta", "estathe_limone", "conad_orange", 2),                # 1 facile
    coppia("7up", "cedrata_sanbenedetto", 2),                           # 2 medio
    terna("consilia_cola", "conad_cola", "coca_zero", 3)                # 3 difficile: unica di marca
  ),
  # ---- LISTA M3 ----
  list(
    coppia("gatorade", "chinotto", 1),                                  # 1 facile
    coppia("sanpellegrino_aranciata", "sprite", 1),                     # 2 medio
    terna("conad_cola", "consilia_cola", "pepsi_classic", 3)            # 3 difficile: unica di marca
  ),
  # ---- LISTA M4 ----
  list(
    terna("fanta", "sanbenedetto_te", "estathe_pesca", 1),              # 1 facile
    terna("sprite", "fanta_zero", "7up", 2),                            # 2 medio
    coppia("fanta_zero", "conad_orange", 2)                             # 3 difficile: zero vs zuccherata
  ),
  # ---- LISTA M5 ----
  list(
    coppia("sanbenedetto_te", "7up", 1),                                # 1 facile
    terna("chinotto", "pepsi_zero", "coca_zero", 1),                    # 2 medio
    terna("fanta", "fanta_zero", "sanpellegrino_aranciata", 3)              # 3 difficile: unica non Fanta
  ),
  # ---- LISTA M6 ----
  list(
    terna("powerade", "pepsi_classic", "gatorade", 2),                  # 1 facile
    coppia("pepsi_zero", "fanta_zero", 1),                              # 2 medio
    coppia("pepsi_classic", "consilia_cola", 2)                         # 3 difficile: marca vs sottomarca
  ),
  # ---- LISTA M7 ----
  list(
    coppia("estathe_limone", "fanta", 1),                               # 1 facile
    coppia("coca_zero", "7up", 2),                                      # 2 medio
    terna("estathe_pesca", "sanbenedetto_te", "estathe_limone", 2)      # 3 difficile: unica non Estathé
  ),
  # ---- LISTA M8 ----
  list(
    terna("conad_cola", "pepsi_classic", "powerade", 3),                # 1 facile
    terna("conad_orange", "sprite", "sanpellegrino_aranciata", 2),      # 2 medio
    coppia("conad_orange", "sanpellegrino_aranciata", 1)                # 3 difficile: due aranciate
  ),
  # ---- LISTA M9 ----
  list(
    coppia("chinotto", "estathe_limone", 1),                            # 1 facile
    terna("fanta", "7up", "sprite", 1),                                 # 2 medio
    coppia("pepsi_zero", "coca_classic", 2)                             # 3 difficile: zero vs classica
  ),
  # ---- LISTA M10 ----
  list(
    terna("gatorade", "estathe_pesca", "sanbenedetto_te", 1),           # 1 facile
    coppia("conad_cola", "chinotto", 2),                                # 2 medio
    terna("consilia_cola", "conad_cola", "pepsi_zero", 3)               # 3 difficile: unica di marca
  )
)

LISTE_PER_MODALITA <- list("2" = LISTE_2, "3" = LISTE_3, "mix" = LISTE_MIX)

MODALITA_NOMI <- c("2"   = "Assaggio alla cieca (2 bicchieri)",
                   "3"   = "Trova l'intruso (3 bicchieri)",
                   "mix" = "Mista")

# Prossima lista da usare per ogni modalità. Vive a livello di app (non di sessione):
# tutte le partite la condividono. Si parte dalla lista 1, poi 2, 3... fino all'ultima e si ricomincia.
# Si azzera se l'app viene riavviata.
PROSSIMA <- new.env()
PROSSIMA[["2"]]   <- 1
PROSSIMA[["3"]]   <- 1
PROSSIMA[["mix"]] <- 1

etichetta_lista <- function(mode, lista) if (mode == "mix") paste0("M", lista) else as.character(lista)

# Restituisce la lista da usare adesso e fa avanzare il contatore
assegna_lista <- function(mode) {
  l <- PROSSIMA[[mode]]
  PROSSIMA[[mode]] <- if (l >= length(LISTE_PER_MODALITA[[mode]])) 1 else l + 1
  l
}

# Riquadro "prossima lista" per la schermata finale: la modalità appena giocata in grassetto
prossime_liste_ui <- function(mode_corrente) {
  righe <- lapply(c("2", "3", "mix"), function(m) {
    testo <- paste0(MODALITA_NOMI[[m]], ": Lista ", etichetta_lista(m, PROSSIMA[[m]]))
    if (m == mode_corrente) {
      tags$p(tags$strong(testo), style = "margin:3px 0; font-size:1.15em;")
    } else {
      tags$p(testo, style = "margin:3px 0; color:#555;")
    }
  })
  div(style = "background:#e8f4fd; border-left:5px solid #007bff; padding:12px 15px; border-radius:10px; margin:15px 0;",
      h4("🥤 Prossima lista da preparare"),
      tagList(righe))
}

# Controllo delle liste all'avvio
valida_liste <- function() {
  problemi <- character(0)
  controlla <- function(nome, liste, n_ammessi) {
    if (length(liste) < 1) problemi <<- c(problemi, paste(nome, "- nessuna lista"))
    for (l in seq_along(liste)) {
      if (length(liste[[l]]) != N_DOMANDE) {
        problemi <<- c(problemi, paste(nome, "- lista", l, ": servono", N_DOMANDE, "domande"))
      }
      for (k in seq_along(liste[[l]])) {
        d <- liste[[l]][[k]]
        n <- length(d$ids)
        ok_ids <- n %in% n_ammessi && length(unique(d$ids)) == n && all(d$ids %in% bevande$id)
        if (!ok_ids) {
          problemi <<- c(problemi, paste(nome, "- lista", l, "domanda", k, ": bevande sbagliate o ripetute"))
        }
        if (!(d$risposta %in% seq_len(n))) {
          problemi <<- c(problemi, paste(nome, "- lista", l, "domanda", k, ": risposta fuori range"))
        }
      }
    }
  }
  controlla("Assaggio a 2 bicchieri", LISTE_2, 2)
  controlla("Trova l'intruso a 3 bicchieri", LISTE_3, 3)
  controlla("Modalità mista", LISTE_MIX, c(2, 3))
  if (length(problemi) > 0) message("ATTENZIONE liste domande:\n", paste(problemi, collapse = "\n"))
  invisible(problemi)
}
valida_liste()

# Restituisce le bevande del turno nell'ordine fisso dei bicchieri A, B, C.
# mode: "2", "3" oppure "mix"; il numero di bicchieri dipende dalla domanda della lista.
genera_turno <- function(turno, mode, lista) {
  d <- LISTE_PER_MODALITA[[mode]][[lista]][[turno]]
  n <- length(d$ids)
  list(modalita = n,
       lista = lista,
       lista_label = etichetta_lista(mode, lista),
       turno = turno,
       ids = d$ids,
       correct_idx = d$risposta,
       target = if (n == 2) nome_bev(d$ids[d$risposta]) else NULL)
}

# Scheda operatore: tutte le liste di tutte le modalità
tabella_scheda_operatore <- function() {
  righe <- list()
  aggiungi <- function(modalita_nome, liste, prefisso) {
    for (l in seq_along(liste)) {
      for (k in seq_along(liste[[l]])) {
        d <- liste[[l]][[k]]
        n <- length(d$ids)
        corretta <- nome_bev(d$ids[d$risposta])
        righe[[length(righe) + 1]] <<- data.frame(
          "Modalità" = modalita_nome,
          Lista = paste0(prefisso, l),
          Domanda = k,
          Bicchieri = n,
          "Bicchiere A" = nome_bev(d$ids[1]),
          "Bicchiere B" = nome_bev(d$ids[2]),
          "Bicchiere C" = if (n == 3) nome_bev(d$ids[3]) else "-",
          "Risposta corretta" = if (n == 3) {
            paste0("intruso ", LETTERS[d$risposta], ": ", corretta)
          } else {
            paste0(LETTERS[d$risposta], ": ", corretta)
          },
          check.names = FALSE, stringsAsFactors = FALSE
        )
      }
    }
  }
  aggiungi(MODALITA_NOMI[["2"]], LISTE_2, "")
  aggiungi(MODALITA_NOMI[["3"]], LISTE_3, "")
  aggiungi(MODALITA_NOMI[["mix"]], LISTE_MIX, "M")
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

# --- 7. INTERFACCIA UTENTE ---------------------------------------------------
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

      // Conteggio animato della probabilità: da 0 al valore finale
      window.animaProbabilita = function() {
        $('.prob-numero').each(function() {
          var el = $(this);
          var target = parseFloat(el.attr('data-target'));
          if (isNaN(target)) { return; }
          var inizio = null;
          var durata = 1800;
          function passo(ts) {
            if (inizio === null) { inizio = ts; }
            var p = Math.min((ts - inizio) / durata, 1);
            var ease = 1 - Math.pow(1 - p, 3);
            el.text(Math.round(target * ease) + '%');
            if (p < 1) { requestAnimationFrame(passo); }
          }
          requestAnimationFrame(passo);
        });
      };
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
      .panel-dashboard { max-width: 1000px; width: 95%; margin: 80px auto; }
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

      /* --- Probabilità di indovinare: grande, pulsante e lampeggiante --- */
      .prob-box {
        text-align: center;
        color: white;
        padding: 30px 20px;
        border-radius: 24px;
        margin: 10px 0 20px 0;
        box-shadow: 0 0 25px rgba(0,0,0,0.25);
        animation: probPulse 1.3s ease-in-out infinite, probEntra 0.6s ease-out;
      }
      .prob-alta  { background: linear-gradient(135deg, #11998e, #38ef7d); }
      .prob-media { background: linear-gradient(135deg, #f12711, #f5af19); }
      .prob-bassa { background: linear-gradient(135deg, #b31217, #e52d27); }
      .prob-titolo { font-size: 26px; font-weight: 800; }
      .prob-nome   { font-size: 20px; font-weight: 600; opacity: 0.95; min-height: 24px; }
      .prob-numero {
        font-size: 110px;
        font-weight: 900;
        line-height: 1.05;
        text-shadow: 0 0 18px rgba(255,255,255,0.85), 0 4px 10px rgba(0,0,0,0.3);
        animation: probLampeggia 0.9s steps(1, end) 4;
      }
      .prob-nota { font-size: 14px; opacity: 0.9; margin-top: 8px; }
      @keyframes probPulse {
        0%, 100% { transform: scale(1);     box-shadow: 0 0 0 0 rgba(255,255,255,0.55); }
        50%      { transform: scale(1.025); box-shadow: 0 0 35px 14px rgba(255,255,255,0); }
      }
      @keyframes probLampeggia {
        0%, 100% { opacity: 1; }
        50%      { opacity: 0.2; }
      }
      @keyframes probEntra {
        from { transform: scale(0.7); opacity: 0; }
        to   { transform: scale(1);   opacity: 1; }
      }

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
      actionButton("go_dashboard_menu", "📊 Dashboard IA", style = "width:100%; border:none; background:none; text-align:left; padding:8px;"),
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

# --- 8. LOGICA SERVER --------------------------------------------------------
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
                                   "Mista (2 e 3 Bicchieri alternati)" = "mix"),
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
            h3(paste0("Lista ", curr$lista_label, " · Domanda #", game$turn, " di ", N_DOMANDE)),
            h4(if (n == 3) "Trova l'Intruso (3 Bicchieri): quale bicchiere è l'intruso?"
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
            # Assaggio alla cieca in tutte le modalità: prima della risposta il bicchiere è neutro
            nascosto <- !game$choice_made
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
                         if (game$turn == N_DOMANDE) "Vai ai risultati finali 🎉" else "Continua ➡️",
                         class = "btn-primary btn-lg",
                         style = "font-size:18px; padding:10px 40px;"))
      )
      
      # --- 4. FINE PARTITA: solo punteggio e pulsanti ---
    } else if (game$stage %in% c("lose", "win")) {
      wellPanel(
        class = "panel-small",
        h2(if (game$stage == "win") "Ottimo lavoro! 🎉" else "Partita completata!"),
        h3(paste0("Punteggio: ", game$correct, " / ", N_DOMANDE)),
        prossime_liste_ui(game$mode),
        br(),
        div(style = "text-align:center;",
            actionButton("go_tree_dashboard", "🌳 Albero finale", class = "btn-info btn-lg",
                         style = "font-size:16px; padding:10px 25px; margin:5px;"),
            actionButton("go_dashboard", "📊 Dashboard IA", class = "btn-primary btn-lg",
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
            actionButton("go_dashboard", "📊 Dashboard IA", class = "btn-primary btn-lg",
                         style = "font-size:16px; padding:10px 25px;"))
      )
      
      # --- 6. DASHBOARD IA: probabilità di indovinare + ultime risposte ---
    } else if (game$stage == "dashboard") {
      wellPanel(
        class = "panel-dashboard",
        h2("📊 Dashboard IA"),
        p(style = "font-size:0.9em; color:#555;", paste("Salvataggio risposte:", STATO_SALVATAGGIO)),
        uiOutput("predizione_realtime_ui"),
        hr(),
        h4("📋 Ultime risposte registrate"),
        tableOutput("tabella_risposte"),
        br(),
        div(style = "text-align:center;",
            actionButton("aggiorna", "🔄 Aggiorna dati", class = "btn-secondary btn-lg",
                         style = "font-size:16px; padding:10px 25px; margin:5px;"),
            actionButton("restart", "🔄 Nuova partita", class = "btn-warning btn-lg",
                         style = "font-size:16px; padding:10px 25px; margin:5px;"))
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
    # la lista è la "prossima" della modalità scelta (si procede in ordine, vedi PROSSIMA)
    game$lista <- assegna_lista(game$mode)
    game$turn <- 1
    game$correct <- 0
    
    rv$visti <- character(0)
    rv$esito <- character(0)
    
    game$current <- genera_turno(1, game$mode, game$lista)
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
        modalita = curr$modalita,          # numero di bicchieri della domanda (2 o 3)
        bevanda_giusta = id_giusto,
        bevanda_scelta = id_scelto,
        scelta = paste0("Opzione_", i),
        corretto = is_correct,
        id_partita = game$id,
        lista = curr$lista_label,          # "1".."10" per 2 e 3 bicchieri, "M1".."M10" per la mista
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
    if (game$turn == N_DOMANDE) {
      rv$aggiornamento <- rv$aggiornamento + 1   # rilegge le risposte per la dashboard
      game$stage <- if (game$correct >= SOGLIA_VITTORIA) "win" else "lose"
    } else {
      game$turn <- game$turn + 1
      game$current <- genera_turno(game$turn, game$mode, game$lista)
      rv$visti <- unique(c(rv$visti, game$current$ids))
      game$choice_made <- FALSE
      game$selected <- NULL
      game$stage <- "play"
    }
  })
  
  observeEvent(input$go_tree_dashboard, { game$stage <- "tree_dashboard" })
  observeEvent(input$go_dashboard, { game$stage <- "dashboard" })
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
    p(tags$strong("In gioco adesso: "),
      paste0(MODALITA_NOMI[[game$mode]], ", Lista ", curr$lista_label,
             ", domanda #", game$turn, " (", curr$modalita, " bicchieri)"))
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
  
  # Probabilità di indovinare: grande, colorata, pulsante e lampeggiante
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
    perc <- round(p_si * 100)
    classe <- if (perc >= 66) "prob-alta" else if (perc >= 33) "prob-media" else "prob-bassa"
    nick <- if (is.null(game$nickname)) "" else game$nickname
    
    div(class = paste("prob-box", classe),
        div(class = "prob-titolo", "🚀 Probabilità di indovinare"),
        div(class = "prob-nome", if (nzchar(nick)) paste0("per ", nick)),
        div(class = "prob-numero", `data-target` = perc, paste0(perc, "%")),
        div(class = "prob-nota",
            paste0("Random Forest su ", mod$n, " risposte · età ", game$eta,
                   " · bibite gassate a settimana: ", game$freq)),
        tags$script("animaProbabilita();"))
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
