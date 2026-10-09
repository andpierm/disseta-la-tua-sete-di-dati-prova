# ==============================================================================
# DISSETA LA TUA SETE DI DATI - Maker Faire 2026
# App Shiny con Salvataggio Locale CSV, 17 Bevande e Metadati DEI + Stile di Vita
# ==============================================================================

library(shiny)
library(shinyjs)
library(ape)
library(ggtree)
library(ggplot2)
library(dplyr)
library(pROC)
library(randomForest)
library(bslib)

set.seed(123)

# --- 1. CONFIGURAZIONE SALVATAGGIO LOCALE CSV ---
CSV_FILE <- "risposte_bevande.csv"

salva_risposta_csv <- function(data_row, filename = CSV_FILE) {
  tryCatch({
    file_exists <- file.exists(filename)
    write.table(
      data_row,
      file = filename,
      append = TRUE,
      sep = ",",
      dec = ".",
      row.names = FALSE,
      col.names = !file_exists,
      qmethod = "double"
    )
  }, error = function(e) {
    message("Errore durante il salvataggio sul file CSV locale: ", e$message)
  })
}

# --- 2. ALBERO TASSONOMICO ESTESO ---
beverage_tree_newick <- "((((coca_classic:1,coca_zero:1)Coca:1,(pepsi_classic:1,(coop_cola:1,conad_cola:1)StoreCola:1)PepsiOther:1)Cola:1,(chinotto:1,molecola:1)DarkBitter:1)DarkDrinks:1,(((fanta:1,fanta_zero:1)Fanta:1,conad_orange:1)Orange:1,(sprite:1,cedrata:1)ClearCitrus:1)Citrus:1,((estathe_limone:1,estathe_pesca:1)Estathe:1,sanbenedetto_te:1)Tea:1,(gatorade:1,powerade:1)Sports:1)Bibite;"

tree <- tryCatch(ape::read.tree(text = beverage_tree_newick), error = function(e) NULL)

# --- 3. DATABASE BEVANDE ---
beverages_df <- data.frame(
  id = c("coca_classic", "coca_zero", "pepsi_classic", "coop_cola", "conad_cola", 
         "chinotto", "molecola", "fanta", "fanta_zero", "conad_orange", 
         "sprite", "cedrata", "estathe_limone", "estathe_pesca", "sanbenedetto_te", 
         "gatorade", "powerade"),
  
  name = c("Coca-Cola Classica", "Coca-Cola Zero", "Pepsi Classica", "Coop Cola", "Conad Cola", 
           "Chinotto Sanpellegrino", "MoleCola", "Fanta Aranciata", "Fanta Zero", "Aranciata Conad", 
           "Sprite", "Cedrata Tassoni", "Estathé Limone", "Estathé Pesca", "Tè San Benedetto", 
           "Gatorade", "Powerade"),
  
  tree_name = c("coca_classic", "coca_zero", "pepsi_classic", "coop_cola", "conad_cola", 
                "chinotto", "molecola", "fanta", "fanta_zero", "conad_orange", 
                "sprite", "cedrata", "estathe_limone", "estathe_pesca", "sanbenedetto_te", 
                "gatorade", "powerade"),
  
  group = c("Cola", "Cola", "Cola", "Cola", "Cola", 
            "DarkBitter", "Cola", "Citrus", "Citrus", "Citrus", 
            "Citrus", "Citrus", "Tea", "Tea", "Tea", 
            "Sports", "Sports"),
  
  color_look = c("Nera", "Nera", "Nera", "Nera", "Nera", 
                 "Nera", "Nera", "Arancione", "Arancione", "Arancione", 
                 "Trasparente", "Gialla", "Marrone", "Marrone", "Marrone", 
                 "Blu", "Blu"),
  
  image_url = c(
    "coca_classic.jpg",
    "coca_zero.jpg",
    "pepsi_classic.jpg",
    "coop_cola.jpg",
    "conad_cola.jpg",
    "chinotto.jpg",
    "molecola.jpg",
    "fanta.jpg",
    "fanta_zero.jpg",
    "conad_orange.jpeg",
    "sprite.jpg",
    "cedrata.jpg",
    "estathe_limone.jpg",
    "estathe_pesca.jpeg",
    "sanbenedetto_te.jpg",
    "gatorade.png",
    "powerade.jpg"
  ),
  
  stringsAsFactors = FALSE
)

safe_sample <- function(x, size = 1) {
  if (length(x) == 0) return(NULL)
  if (length(x) == 1) return(x)
  sample(x, size)
}

# --- GENERATORE TURNO 3 BICCHIERI (INTRUSO) ---
generate_turn_3 <- function(turn_num, tree, beverages_df) {
  if (turn_num <= 5) {
    easy_sets <- list(
      c("coca_classic", "pepsi_classic", "fanta"),
      c("coca_classic", "pepsi_classic", "estathe_limone"),
      c("estathe_limone", "sanbenedetto_te", "pepsi_classic"),
      c("fanta", "sprite", "coca_classic"),
      c("gatorade", "powerade", "cedrata")
    )
    idx <- ((turn_num - 1) %% length(easy_sets)) + 1
    chosen <- easy_sets[[idx]]
    focal <- chosen[1]
    similar <- chosen[2]
    intruder <- chosen[3]
  } else {
    hard_sets <- list(
      c("coca_classic", "molecola", "chinotto"),
      c("coca_classic", "coop_cola", "conad_cola"),
      c("fanta", "fanta_zero", "conad_orange"),
      c("estathe_limone", "estathe_pesca", "sanbenedetto_te"),
      c("chinotto", "molecola", "coop_cola")
    )
    idx <- ((turn_num - 6) %% length(hard_sets)) + 1
    chosen <- hard_sets[[idx]]
    focal <- chosen[1]
    similar <- chosen[2]
    intruder <- chosen[3]
  }
  
  species_raw <- sample(c(focal, similar, intruder))
  urls <- beverages_df$image_url[match(species_raw, beverages_df$tree_name)]
  names_clean <- beverages_df$name[match(species_raw, beverages_df$tree_name)]
  
  list(
    num_choices = 3,
    species_raw = species_raw,
    species = names_clean,
    intruder = which(species_raw == intruder),
    urls = urls,
    turn = turn_num
  )
}

# --- GENERATORE TURNO 2 BICCHIERI (ASSAGGIO ALLA CIECA) ---
generate_turn_2 <- function(turn_num, beverages_df) {
  if (turn_num <= 5) {
    easy_pairs <- list(
      c("coca_classic", "fanta"),
      c("pepsi_classic", "estathe_limone"),
      c("sprite", "chinotto"),
      c("cedrata", "coca_classic"),
      c("gatorade", "fanta")
    )
    idx <- ((turn_num - 1) %% length(easy_pairs)) + 1
    pair <- easy_pairs[[idx]]
  } else {
    hard_pairs <- list(
      c("coca_classic", "chinotto"),
      c("fanta", "conad_orange"),
      c("estathe_limone", "sanbenedetto_te"),
      c("coca_classic", "molecola"),
      c("chinotto", "coop_cola")
    )
    idx <- ((turn_num - 6) %% length(hard_pairs)) + 1
    pair <- hard_pairs[[idx]]
  }
  
  pair <- sample(pair)
  correct_idx <- sample(1:2, 1)
  urls <- beverages_df$image_url[match(pair, beverages_df$tree_name)]
  names_clean <- beverages_df$name[match(pair, beverages_df$tree_name)]
  
  list(
    num_choices = 2,
    species_raw = pair,
    species = names_clean,
    target_idx = correct_idx,
    urls = urls,
    turn = turn_num
  )
}

# --- 4. INTERFACCIA UTENTE (UI) ---
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
        background-image: url('wallpaper.jpg');
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
      .optional-label { font-size: 0.9em; color: #555; font-style: italic; }
    "))
  ),
  
  # Menù Navigazione Rapida
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
      actionButton("go_tree_menu", "🌳 Albero Tassonomico", style = "width:100%; border:none; background:none; text-align:left; padding:8px;")
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

# --- 5. LOGICA SERVER ---
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    seen_species = character(0),
    correct_species = character(0),
    wrong_species = character(0)
  )
  
  game <- reactiveValues(
    stage = "start",
    nickname = NULL,
    eta = 20,
    genere = "Maschio",
    freq = 3,
    fortuna = "Molto",
    maratona_ny = "",
    sabato_sera = "",
    turn = 1,
    correct = 0,
    current = NULL,
    choice_made = FALSE,
    selected = NULL,
    mode_selection = "3"
  )
  
  dati_iniziali <- data.frame(
    timestamp = rep(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), 5),
    nickname = c("Alice", "Marco", "Giulia", "Luca", "Sofia"),
    eta = c(22, 28, 19, 35, 24),
    genere = c("Femmina", "Maschio", "Non-binario", "Preferisco non dirlo", "Femmina"),
    freq = c(3, 5, 2, 7, 4),
    fortuna = c("Molto", "Poco", "Molto", "Poco", "Molto"),
    maratona_ny = c("Sì, subito!", "No, resto in hotel", "Solo per far foto", "Preferisco una pizza", "Forse"),
    sabato_sera = c("Uscita con amici", "Serie TV e divano", "Discoteca", "Cena tranquilla", "Gaming"),
    scelta = c("Opzione_1", "Opzione_2", "Opzione_1", "Opzione_1", "Opzione_2"),
    corretto = c(TRUE, FALSE, TRUE, TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  risposte <- reactiveVal(dati_iniziali)
  
  models_data <- reactive({
    df <- risposte()
    req(nrow(df) >= 3)
    df$corretto_f <- factor(ifelse(df$corretto, "Si", "No"), levels = c("No", "Si"))
    if (length(unique(df$corretto_f)) < 2) return(NULL)
    
    fit_rf <- tryCatch(randomForest(corretto_f ~ eta + freq, data = df, ntree = 50), error = function(e) NULL)
    list(fit_rf = fit_rf, df = df)
  })
  
  output$main_ui <- renderUI({
    
    # --- 1. START SCREEN ---
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
          textInput("maratona_ny", "🏃 Se ti invitassero a New York, faresti la maratona?", placeholder = "es. Sì subito / Solo se posso fare foto / No..."),
          textInput("sabato_sera", "🍕 Descrivi il tuo sabato sera ideale:", placeholder = "es. Divano e serie TV / Discoteca con amici / Cena fuori..."),
          
          hr(),
          radioButtons("mode_select", "🎯 Scegli la modalità del gioco:",
                       choices = c("Trova l'Intruso (3 Bicchieri)" = "3",
                                   "Assaggio alla Cieca (2 Bicchieri)" = "2",
                                   "Mista / Alternata (Casuale)" = "mix"),
                       selected = "3"),
          
          actionButton("start_game", "🚀 Inizia il gioco", class = "btn-primary btn-lg", style = "width:100%; font-size:18px; padding:10px 30px;")
        )
      )
      
      # --- 2. SCHERMATA DI GIOCO ---
    } else if (game$stage == "play") {
      curr <- game$current
      num_c <- curr$num_choices
      col_w <- if(num_c == 3) 4 else 6
      
      wellPanel(
        class = "panel-large",
        div(style="text-align:center;",
            h3(paste0("Turno ", game$turn, " di 10 — ", if(num_c == 3) "Trova l'Intruso (3 Bicchieri)" else "Assaggio alla Cieca (2 Bicchieri)"))
        ),
        br(),
        
        if(game$choice_made) {
          corretto <- if(num_c == 3) (game$selected == curr$intruder) else (game$selected == curr$target_idx)
          div(style = "text-align:center; padding:10px; margin-bottom:15px;",
              if(corretto) {
                tags$h3(paste0("✅ Complimenti, ", game$nickname, "! Hai indovinato!"), style = "color:#155724; background:#d4edda; padding:12px; border-radius:10px;")
              } else {
                tags$h3(paste0("❌ Oh no ", game$nickname, ", non era la risposta corretta..."), style = "color:#721c24; background:#f8d7da; padding:12px; border-radius:10px;")
              }
          )
        },
        
        fluidRow(
          lapply(1:num_c, function(i) {
            column(
              col_w,
              div(
                style = "text-align:center; padding:10px; background:white; border-radius:15px; box-shadow:0 4px 10px rgba(0,0,0,0.08); margin-bottom:15px;",
                tags$img(
                  class = "drink-card-img",
                  src = curr$urls[i],
                  style = paste0(
                    "border: 4px solid ",
                    if(game$choice_made && i == game$selected) {
                      is_win <- if(num_c == 3) (i == curr$intruder) else (i == curr$target_idx)
                      if(is_win) "#28a745" else "#dc3545"
                    } else if(game$choice_made && ((num_c == 3 && i == curr$intruder) || (num_c == 2 && i == curr$target_idx))) {
                      "#28a745"
                    } else {
                      "#495057"
                    }
                  ),
                  onclick = if(!game$choice_made) sprintf("Shiny.setInputValue('choice_%d', Math.random());", i),
                  onerror = "this.src='https://images.unsplash.com/photo-1527960471264-932f39eb5846?w=400';"
                ),
                div(style = "margin-top:10px; font-weight:bold; font-size:18px;", curr$species[i])
              )
            )
          })
        ),
        
        if(game$choice_made) {
          div(style = "text-align:center; margin-top:20px;",
              actionButton("view_step_tree", "Vedi albero in costruzione 🌳 ➡️", class = "btn-success btn-lg", style = "font-size:18px; padding:10px 40px;")
          )
        }
      )
      
      # --- 3. ALBERO IN COSTRUZIONE ---
    } else if (game$stage == "turn_tree") {
      wellPanel(
        class = "panel-large",
        h2("Il tuo albero finora 🌳"),
        plotOutput("turn_tree_plot", height = "480px"),
        div(
          style = "text-align:center; margin-top:20px;",
          actionButton(
            "continue_after_tree",
            if (game$turn == 10) "Vai ai risultati finali 🎉" else "Continua ➡️",
            class = "btn-primary btn-lg",
            style = "font-size:18px; padding:10px 40px;"
          )
        )
      )
      
      # --- 4. SCHERMATA FINE GAME ---
    } else if (game$stage %in% c("lose", "win")) {
      wellPanel(
        class = "panel-small",
        h2(if(game$stage == "win") "Perfetto! 🎉" else "Partita Completata!"),
        h3(paste("Punteggio:", game$correct, "/ 10")),
        actionButton("go_tree_dashboard", "Scopri la Tassonomia Finale e la Classifica", class = "btn-info btn-lg", style = "width:100%; font-size:16px; padding:10px 25px;")
      )
      
      # --- 5. ALBERO FINALE ---
    } else if (game$stage == "tree_dashboard") {
      wellPanel(
        class = "panel-large",
        h2("Il tuo percorso nella Tassonomia delle Bibite 🌳"),
        plotOutput("final_tree", height = "480px"),
        div(style = "text-align:center; margin-top:20px;",
            actionButton("go_leaderboard", "Vai alla Dashboard & IA", class = "btn-primary btn-lg", style = "font-size:16px; padding:10px 25px;")
        )
      )
      
      # --- 6. DASHBOARD & IA ---
    } else if (game$stage == "dashboard") {
      wellPanel(
        class = "panel-dashboard",
        h2("📊 Dashboard & Predizioni IA in Tempo Reale"),
        fluidRow(
          column(6,
                 h4("🤖 Modelli IA (Random Forest)"),
                 uiOutput("predizione_realtime_ui"),
                 br(),
                 h4("📋 Ultime Risposte Registrate"),
                 tableOutput("tabella_risposte")
          ),
          column(6,
                 h4("📈 Curva ROC del Modello"),
                 plotOutput("roc_combined", height = "300px")
          )
        ),
        hr(),
        actionButton("restart", "Nuova Partita 🔄", class = "btn-warning btn-lg", style = "font-size:16px; padding:10px 30px;")
      )
    }
  })
  
  # Avvio Gioco
  observeEvent(input$start_game, {
    req(input$nickname)
    game$nickname <- input$nickname
    game$eta <- input$eta
    game$genere <- input$genere
    game$freq <- input$freq
    game$fortuna <- input$fortuna
    game$maratona_ny <- if(!is.null(input$maratona_ny) && input$maratona_ny != "") input$maratona_ny else "Non specificato"
    game$sabato_sera <- if(!is.null(input$sabato_sera) && input$sabato_sera != "") input$sabato_sera else "Non specificato"
    game$mode_selection <- input$mode_select
    game$turn <- 1
    game$correct <- 0
    
    rv$seen_species <- character(0)
    rv$correct_species <- character(0)
    rv$wrong_species <- character(0)
    
    next_mode <- if(game$mode_selection == "mix") safe_sample(c(2, 3), 1) else as.numeric(game$mode_selection)
    game$current <- if(next_mode == 3) {
      generate_turn_3(1, tree, beverages_df)
    } else {
      generate_turn_2(1, beverages_df)
    }
    
    game$choice_made <- FALSE
    game$stage <- "play"
    rv$seen_species <- unique(c(rv$seen_species, game$current$species_raw))
  })
  
  # Gestione Selezione
  lapply(1:3, function(i) {
    observeEvent(input[[paste0("choice_", i)]], {
      req(!game$choice_made, game$stage == "play")
      num_c <- game$current$num_choices
      if(i <= num_c) {
        game$selected <- i
        game$choice_made <- TRUE
        
        is_correct <- if(num_c == 3) (i == game$current$intruder) else (i == game$current$target_idx)
        chosen_sp <- game$current$species_raw[i]
        
        if (is_correct) {
          game$correct <- game$correct + 1
          session$sendCustomMessage("triggerConfetti", list())
          rv$correct_species <- unique(c(rv$correct_species, game$current$species_raw))
        } else {
          rv$wrong_species <- unique(c(rv$wrong_species, chosen_sp))
          rv$correct_species <- unique(c(rv$correct_species, setdiff(game$current$species_raw, chosen_sp)))
        }
        
        nuovo <- data.frame(
          timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          nickname = game$nickname,
          eta = game$eta,
          genere = game$genere,
          freq = game$freq,
          fortuna = game$fortuna,
          maratona_ny = game$maratona_ny,
          sabato_sera = game$sabato_sera,
          scelta = paste0("Opzione_", i),
          corretto = is_correct,
          stringsAsFactors = FALSE
        )
        
        risposte(rbind(risposte(), nuovo))
        
        # Salvataggio nel file CSV locale
        salva_risposta_csv(nuovo)
      }
    })
  })
  
  observeEvent(input$view_step_tree, { game$stage <- "turn_tree" })
  
  observeEvent(input$continue_after_tree, {
    if (game$turn == 10) {
      game$stage <- if(game$correct >= 6) "win" else "lose"
    } else {
      game$turn <- game$turn + 1
      next_mode <- if(game$mode_selection == "mix") safe_sample(c(2, 3), 1) else as.numeric(game$mode_selection)
      
      game$current <- if(next_mode == 3) {
        generate_turn_3(game$turn, tree, beverages_df)
      } else {
        generate_turn_2(game$turn, beverages_df)
      }
      
      rv$seen_species <- unique(c(rv$seen_species, game$current$species_raw))
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
  
  # PLOT ALBERO PASSO-PASSO
  output$turn_tree_plot <- renderPlot({
    req(length(rv$seen_species) > 0)
    valid_tips <- intersect(tree$tip.label, rv$seen_species)
    if (length(valid_tips) < 2) valid_tips <- tree$tip.label
    
    subtree <- ape::keep.tip(tree, valid_tips)
    
    tip_colors <- sapply(subtree$tip.label, function(sp) {
      if (sp %in% rv$wrong_species) "#dc3545"
      else if (sp %in% rv$correct_species) "#28a745"
      else "#6c757d"
    })
    
    clean_names <- beverages_df$name[match(subtree$tip.label, beverages_df$tree_name)]
    
    tip_data <- data.frame(
      label = subtree$tip.label,
      clean_label = clean_names,
      color = tip_colors,
      stringsAsFactors = FALSE
    )
    
    p <- ggtree(subtree, size = 1.2) %<+% tip_data +
      geom_tiplab(aes(label = clean_label, color = color), size = 5.5, align = TRUE, offset = 0.05, fontface = "bold") +
      geom_nodepoint(color = "#007bff", size = 3) +
      scale_color_identity() +
      theme_tree2() +
      theme(axis.text.x = element_text(size = 12, face = "bold"))
    
    xmax <- max(p$data$x, na.rm = TRUE)
    p + xlim(0, xmax * 1.45)
  })
  
  # PLOT ALBERO FINALE
  output$final_tree <- renderPlot({
    req(length(rv$seen_species) > 0)
    valid_tips <- intersect(tree$tip.label, rv$seen_species)
    if(length(valid_tips) < 2) valid_tips <- tree$tip.label
    
    subtree <- ape::keep.tip(tree, valid_tips)
    
    tip_colors <- sapply(subtree$tip.label, function(sp) {
      if (sp %in% rv$wrong_species) "#dc3545"
      else if (sp %in% rv$correct_species) "#28a745"
      else "#6c757d"
    })
    
    clean_names <- beverages_df$name[match(subtree$tip.label, beverages_df$tree_name)]
    
    tip_data <- data.frame(
      label = subtree$tip.label,
      clean_label = clean_names,
      color = tip_colors,
      stringsAsFactors = FALSE
    )
    
    p <- ggtree(subtree, size = 1.2) %<+% tip_data +
      geom_tiplab(aes(label = clean_label, color = color), size = 6, align = TRUE, offset = 0.05, fontface = "bold") +
      geom_nodepoint(color = "#007bff", size = 3) +
      scale_color_identity() +
      theme_tree2() +
      theme(axis.text.x = element_text(size = 12, face = "bold"))
    
    xmax <- max(p$data$x, na.rm = TRUE)
    p + xlim(0, xmax * 1.45)
  })
  
  output$predizione_realtime_ui <- renderUI({
    mdl <- models_data()
    if(is.null(mdl) || is.null(mdl$fit_rf)) return(HTML("<p>Modello IA in fase di addestramento...</p>"))
    
    df_pred <- data.frame(eta = game$eta, freq = game$freq)
    prob_rf <- tryCatch(predict(mdl$fit_rf, newdata = df_pred, type = "prob")[, "Si"], error = function(e) 0.5)
    
    HTML(paste0(
      "<div style='background-color:#f0f8ff; padding:15px; border-radius:10px; border-left: 5px solid #007bff;'>",
      "<h5>🚀 Predizione Random Forest</h5>",
      "Probabilità stimata per <b>", game$nickname, "</b> di indovinare: <b>", round(prob_rf * 100, 1), "%</b><br>",
      "<small>Basato su età (", game$eta, ") e consumo settimanale (", game$freq, ").</small>",
      "</div>"
    ))
  })
  
  output$tabella_risposte <- renderTable({
    tail(risposte()[, c("timestamp", "nickname", "eta", "genere", "corretto")], 5)
  })
  
  output$roc_combined <- renderPlot({
    mdl <- models_data()
    if (is.null(mdl) || is.null(mdl$fit_rf)) {
      plot(c(0, 1), c(0, 1), type = "n", xlab = "1 - Specificità", ylab = "Sensitività", main = "Curva ROC IA")
      text(0.5, 0.5, "In attesa di dati per la Curva ROC", cex = 1.1)
      return()
    }
    
    prob_rf <- predict(mdl$fit_rf, type = "prob")[, "Si"]
    roc_rf <- tryCatch(pROC::roc(mdl$df$corretto_f, prob_rf, levels = c("No", "Si")), error = function(e) NULL)
    
    plot(c(0, 1), c(0, 1), type = "n", xlab = "1 - Specificità", ylab = "Sensitività", main = "Curva ROC - Random Forest")
    abline(0, 1, lty = 2, col = "gray")
    
    if (!is.null(roc_rf)) {
      plot(roc_rf, add = TRUE, col = "#28a745", lwd = 2.5)
      legend("bottomright", legend = paste("AUC =", round(pROC::auc(roc_rf), 2)), col = "#28a745", lwd = 2.5, bty = "n")
    }
  })
}

shinyApp(ui = ui, server = server)