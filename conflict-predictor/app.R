# ============================================================================
#  Shiny App 2 — Conflict Outcome Predictor (v7)
#  STAT 3280 · Final Project · Raymond Santiago Flores · 2026
#
#  v7 memory fixes over v6:
#    - Slider inputs debounced (400 ms) → cuts per-keystroke RF calls
#    - Z_DATA comparator matrix pre-computed once at startup instead of
#      rebuilding it on every render (was: sweep() on full matrix each time)
#    - outputOptions(..., suspendWhenHidden = TRUE) set explicitly for all
#      four heavy outputs so hidden tabs never re-render
#    - user_row_d() (debounced) used for all expensive computations;
#      fast scoreline still reads undebounced user_row() for responsiveness
# ============================================================================

suppressPackageStartupMessages({
  library(shiny);        library(bslib);       library(dplyr);   library(tidyr)
  library(ggplot2);      library(plotly);       library(scales);  library(stringr)
  library(forcats);      library(DT);           library(purrr)
  library(randomForest)
})

# ── DESIGN TOKENS ─────────────────────────────────────────────────────────────
PAPER  <- "#FAFAFA"
INK    <- "#1F1F1D"
INK2   <- "#4A4A47"
RULE   <- "#E2E0DA"
ACCENT <- "#B8312F"

outcome_colors <- c(
  "Peace Agreement" = "#1F7A5A",
  "Ceasefire"       = "#4A8FBF",
  "Victory"         = "#B8312F",
  "Low Activity"    = "#DD8452",
  "Other/Unclear"   = "#888780"
)

theme_conflict <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor      = element_blank(),
      panel.grid.major.x    = element_blank(),
      panel.grid.major.y    = element_line(color = "grey92", linewidth = 0.4),
      plot.title            = element_text(face = "bold", size = 15,
                                           color = INK, hjust = 0,
                                           margin = ggplot2::margin(b = 4)),
      plot.title.position   = "plot",
      plot.subtitle         = element_text(color = INK2, size = 10.5,
                                           hjust = 0, margin = ggplot2::margin(b = 14)),
      plot.caption          = element_text(color = INK2, size = 8.5,
                                           margin = ggplot2::margin(t = 12), hjust = 1),
      plot.caption.position = "plot",
      axis.title            = element_text(color = INK2, size = 10),
      axis.title.x          = element_text(margin = ggplot2::margin(t = 8)),
      axis.title.y          = element_text(margin = ggplot2::margin(r = 8)),
      axis.text             = element_text(color = INK, size = 9.5),
      legend.position       = "bottom",
      legend.title          = element_text(color = INK2, size = 9),
      legend.text           = element_text(color = INK,  size = 9),
      panel.background      = element_rect(fill = PAPER, color = NA),
      plot.background       = element_rect(fill = PAPER, color = NA),
      strip.text            = element_text(face = "bold", color = INK, size = 11)
    )
}
# ── DATA LOAD ─────────────────────────────────────────────────────────────────
rf_model <- readRDS("rf_model.rds")
rf_data  <- readRDS("rf_data.rds")
imp      <- readRDS("imp.rds")

# ── PRE-COMPUTE COMPARATOR MATRIX (once at startup, not per render) ───────────
# Avoids rebuilding sweep(as.matrix(...)) on every comp_table render.
COMP_FEATS <- c("c_ep_dur", "intensity_level", "log_deaths", "log_gdp", "democracy")
COMP_SDS   <- sapply(rf_data[, COMP_FEATS], sd, na.rm = TRUE)
COMP_SDS[COMP_SDS == 0] <- 1
Z_DATA     <- sweep(as.matrix(rf_data[, COMP_FEATS]), 2, COMP_SDS, "/")

# ── SAFE PREDICT HELPER ───────────────────────────────────────────────────────
rf_predict_prob <- function(newdata) {
  nd <- data.frame(
    type_of_conflict = as.integer(newdata$type_of_conflict),
    region           = as.integer(newdata$region),
    has_support      = as.integer(newdata$has_support),
    c_ep_dur         = as.numeric(newdata$c_ep_dur),
    intensity_level  = as.integer(newdata$intensity_level),
    log_deaths       = as.numeric(newdata$log_deaths),
    incompatibility  = as.integer(newdata$incompatibility),
    log_gdp          = as.numeric(newdata$log_gdp),
    democracy        = as.numeric(newdata$democracy)
  )
  randomForest:::predict.randomForest(rf_model, newdata = nd, type = "prob")
}

# ── UI ────────────────────────────────────────────────────────────────────────
app_theme <- bs_theme(
  version = 5, bg = PAPER, fg = INK, primary = ACCENT, font_scale = 1.0
)

ui <- fluidPage(
  theme = app_theme,
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com",
              crossorigin = ""),
    tags$link(href = paste0("https://fonts.googleapis.com/css2?",
                            "family=Lora:wght@400;600;700&",
                            "family=Source+Sans+3:wght@300;400;600&display=swap"),
              rel = "stylesheet"),
    tags$style(HTML(sprintf("
      body { background:%s; color:%s;
             font-family:'Source Sans 3',system-ui,sans-serif; }
      h1,h2,h3,h4 { font-family:'Lora',Georgia,serif; color:%s; }
      .app-title  { font-family:'Lora',Georgia,serif; font-weight:700;
                    font-size:1.9rem; color:%s; margin:.2em 0 .05em 0; }
      .app-deck   { font-style:italic; color:%s; font-size:1.05rem;
                    margin-bottom:1.6em; max-width:760px; }
      .well,.form-control,.selectize-input {
                    background-color:#FFFFFF !important;
                    border-color:%s !important; color:%s; }
      .nav-tabs .nav-link        { color:%s; font-family:'Source Sans 3'; }
      .nav-tabs .nav-link.active { color:%s; font-weight:600;
                    border-bottom:2px solid %s !important; }
      .takeaway   { background:#FFFFFF; border-left:3px solid %s;
                    padding:.6em .9em; margin:0 0 1em 0; color:%s;
                    font-family:'Lora',Georgia,serif; font-style:italic;
                    font-size:.98rem; }
      .scoreline  { font-family:'Lora',Georgia,serif; font-size:1.1rem;
                    color:%s; margin:.2em 0 .8em 0; }
      .scoreline b { color:%s; }
    ", PAPER, INK, INK, INK, INK2, RULE, INK,
       INK2, INK, ACCENT, ACCENT, INK, INK, ACCENT)))
  ),

  div(class = "app-title", "What does the model predict for this conflict?"),
  div(class = "app-deck",
      "Choose nine structural conditions on the left. The random-forest ",
      "model — trained on every terminated conflict in the UCDP record — ",
      "returns the probability for each of five termination outcomes, ",
      "the sensitivity of predictions to each input, the most influential ",
      "predictors, and the closest historical analogues. GDP per capita and ",
      "democracy scores are matched at the country level; episodes with no ",
      "available data (~18%) use the dataset median."),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Conflict profile"),
      selectInput("type_of_conflict", "Conflict type",
                  choices  = c("Extrasystemic"    = 1, "Interstate"       = 2,
                               "Intrastate"       = 3, "Intl. Intrastate" = 4),
                  selected = 3),
      selectInput("region", "Region",
                  choices  = c("Europe" = 1, "Middle East" = 2,
                               "Asia"   = 3, "Africa"      = 4,
                               "Americas" = 5),
                  selected = 4),
      radioButtons("has_support", "External support?",
                   choices = c("No" = 0, "Yes" = 1), selected = 0,
                   inline  = TRUE),
      sliderInput("c_ep_dur", "Episode duration (years)",
                  min = 1, max = 30, value = 5),
      sliderInput("intensity_level", "Peak intensity",
                  min = 1, max = 2, value = 2, step = 1),
      selectInput("incompatibility", "Issue at stake",
                  choices  = c("Territory" = 1, "Government" = 2, "Both" = 3),
                  selected = 2),
      sliderInput("log_gdp", "GDP per capita (log\u2081\u2080, USD)",
                  min = 2, max = 5, step = 0.1,
                  value = round(median(rf_data$log_gdp, na.rm = TRUE), 1)),
      sliderInput("democracy", "Democracy score (V-Dem polyarchy)",
                  min = 0, max = 1, step = 0.01,
                  value = round(median(rf_data$democracy, na.rm = TRUE), 2)),
      sliderInput("log_deaths", "Battle deaths (log\u2081\u2080)",
                  min = 0, max = 6, step = 0.1,
                  value = round(median(rf_data$log_deaths, na.rm = TRUE), 1)),
      hr(),
      helpText(tags$small("Defaults = median terminated conflict in the data."))
    ),

    mainPanel(
      width = 9,
      uiOutput("scoreline"),
      tabsetPanel(
        id = "tabs", type = "tabs",

        # ── Tab 1: Prediction ────────────────────────────────────────────────
        tabPanel("Prediction",
          div(class = "takeaway",
              "Bar length = model probability. The most likely outcome is ",
              "highlighted in red; others recede to neutral so the focal ",
              "comparison reads first."),
          plotlyOutput("prob_plot", height = "440px")),

        # ── Tab 2: Sensitivity ───────────────────────────────────────────────
        tabPanel("Sensitivity",
          div(class = "takeaway",
              "Each panel sweeps one continuous predictor across its full range ",
              "while holding all others fixed at your current sidebar values. ",
              "The dashed rule marks your current setting."),
          selectInput("sens_outcome", "Show outcome:",
                      choices  = names(outcome_colors),
                      selected = "Peace Agreement",
                      width    = "260px"),
          plotlyOutput("sens_plot", height = "500px")),

        # ── Tab 3: Drivers ───────────────────────────────────────────────────
        tabPanel("Drivers",
          div(class = "takeaway",
              "Variable importance split into structural conditions (politics, ",
              "economy, support) versus battlefield conditions (duration, ",
              "intensity, deaths). Larger bar = more predictive."),
          plotlyOutput("imp_plot", height = "440px")),

        # ── Tab 4: Support Effect ────────────────────────────────────────────
        tabPanel("Support Effect",
          div(class = "takeaway",
              "Predicted probabilities for this profile with external support ",
              "absent (grey) versus present (red). Bar-length difference is the ",
              "model's estimated marginal effect of outside backing, holding ",
              "all other inputs constant."),
          plotlyOutput("support_plot", height = "440px")),

        # ── Tab 5: Comparators ───────────────────────────────────────────────
        tabPanel("Comparators",
          div(class = "takeaway",
              "The 15 historical conflicts whose feature vectors are nearest ",
              "to your selected profile, sorted by similarity. Actual outcomes ",
              "are shown — compare with the model's prediction on the left."),
          DT::DTOutput("comp_table"))
      )
    )
  ),

  hr(),
  tags$p(class = "text-muted small",
         "Model: random forest, 500 trees · exploratory, not validated for ",
         "out-of-sample prediction · ",
         "Source: UCDP/PRIO v25.1 + Termination v4-2024 + ESD + WDI + V-Dem · ",
         "Visualization: R. S. Flores, 2026.")
)

# ── SERVER ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── Raw (undebounced) profile — used only for the cheap scoreline ──────────
  user_row <- reactive({
    data.frame(
      type_of_conflict = as.integer(input$type_of_conflict),
      region           = as.integer(input$region),
      has_support      = as.integer(input$has_support),
      c_ep_dur         = as.numeric(input$c_ep_dur),
      intensity_level  = as.integer(input$intensity_level),
      log_deaths       = as.numeric(input$log_deaths),
      incompatibility  = as.integer(input$incompatibility),
      log_gdp          = as.numeric(input$log_gdp),
      democracy        = as.numeric(input$democracy)
    )
  })

  # ── Debounced profile (400 ms) — used for all expensive computations ───────
  # Prevents rapid-fire RF calls while the user is still dragging a slider.
  user_row_d <- debounce(user_row, 400)

  # pred_probs reads the debounced profile
  pred_probs <- reactive({
    p <- rf_predict_prob(user_row_d())
    data.frame(outcome = colnames(p), prob = as.numeric(p[1, ])) %>%
      mutate(outcome = factor(outcome, levels = names(outcome_colors)))
  })

  # ── Scoreline headline (reads undebounced → stays snappy) ─────────────────
  output$scoreline <- renderUI({
    pp  <- pred_probs()
    top <- pp$outcome[which.max(pp$prob)]
    HTML(sprintf(
      '<div class="scoreline">Most likely outcome: <b>%s</b> &nbsp;&middot;&nbsp; %s probability</div>',
      top, percent(pp$prob[which.max(pp$prob)], accuracy = 1)
    ))
  })

  # ── Tab 1: Prediction bars ───────────────────────────────────────────────
  output$prob_plot <- renderPlotly({
    pp  <- pred_probs()
    top <- pp$outcome[which.max(pp$prob)]
    pp  <- pp %>%
      mutate(
        fill_col = if_else(outcome == top, ACCENT, "#BFBDB7"),
        label    = percent(prob, accuracy = 1)
      )
    p <- ggplot(pp, aes(x = prob, y = fct_rev(outcome),
                        text = paste0("<b>", outcome, "</b>: ", label))) +
      geom_col(aes(fill = fill_col), width = 0.7) +
      geom_text(aes(label = label, x = prob + 0.012),
                hjust = 0, size = 3.6, color = INK, fontface = "bold") +
      scale_fill_identity() +
      scale_x_continuous(limits = c(0, max(pp$prob) * 1.28),
                         labels = percent_format(accuracy = 1)) +
      labs(title    = paste0("Most likely: ", top),
           subtitle = "Predicted probability for each termination outcome",
           x = NULL, y = NULL) +
      theme_conflict() +
      theme(panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_line(color = "grey92"),
            axis.text.y        = element_text(size = 11, face = "bold"))
    ggplotly(p, tooltip = "text") %>% plotly::config(displayModeBar = FALSE)
  })

  # ── Tab 2: Sensitivity (all four continuous vars, one outcome) ────────────
  output$sens_plot <- renderPlotly({
    base    <- user_row_d()          # debounced — only fires after pause
    outcome <- input$sens_outcome

    cont_vars <- list(
      c_ep_dur   = list(label = "Duration (years)",    from = 1, to = 30, n = 40),
      log_deaths = list(label = "Battle deaths (log)", from = 0, to = 6,  n = 40),
      log_gdp    = list(label = "GDP p.c. (log)",      from = 2, to = 5,  n = 40),
      democracy  = list(label = "Democracy score",     from = 0, to = 1,  n = 40)
    )

    sweep_df <- map_dfr(names(cont_vars), function(col) {
      meta    <- cont_vars[[col]]
      sweep_x <- seq(meta$from, meta$to, length.out = meta$n)
      rows    <- base[rep(1, meta$n), , drop = FALSE]
      rows[[col]] <- sweep_x
      probs_mat <- rf_predict_prob(rows)
      data.frame(var  = meta$label,
                 x    = sweep_x,
                 prob = probs_mat[, outcome, drop = TRUE])
    })

    vline_df <- data.frame(
      var = c(cont_vars$c_ep_dur$label, cont_vars$log_deaths$label,
              cont_vars$log_gdp$label,  cont_vars$democracy$label),
      x   = c(base$c_ep_dur, base$log_deaths, base$log_gdp, base$democracy)
    )

    p <- ggplot(sweep_df, aes(x = x, y = prob,
                              text = paste0(round(x, 2), " \u2192 ",
                                            percent(prob, accuracy = 1)))) +
      geom_line(color = ACCENT, linewidth = 1) +
      geom_vline(data = vline_df, aes(xintercept = x),
                 linetype = "dashed", color = INK2, linewidth = 0.5) +
      facet_wrap(~ var, scales = "free_x", ncol = 2) +
      scale_y_continuous(labels = percent_format(accuracy = 1),
                         limits = c(0, NA),
                         expand = expansion(mult = c(0, 0.08))) +
      labs(title    = paste0("Sensitivity: ", outcome),
           subtitle = "Each panel sweeps one predictor \u00b7 dashed rule = your current value \u00b7 others held fixed",
           x = NULL, y = "Predicted probability") +
      theme_conflict() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(color = "grey92"))

    ggplotly(p, tooltip = "text") %>% plotly::config(displayModeBar = FALSE)
  })

  # ── Tab 3: Variable importance (static — depends only on imp) ────────────
  output$imp_plot <- renderPlotly({
    df <- imp %>%
      mutate(label  = fct_reorder(label, MeanDecreaseGini),
             family = factor(family, levels = c("Structural", "Battlefield")))

    p <- ggplot(df, aes(x = MeanDecreaseGini, y = label,
                        text = paste0("<b>", label, "</b><br>",
                                      "Mean \u0394 Gini: ",
                                      round(MeanDecreaseGini, 1)))) +
      geom_col(fill = "#8A8178", width = 0.7) +
      facet_wrap(~ family, scales = "free_y", ncol = 2) +
      labs(title    = "What does the model lean on?",
           subtitle = "Mean decrease in Gini \u00b7 larger = more predictive",
           x = "Importance", y = NULL) +
      theme_conflict() +
      theme(panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_line(color = "grey92"),
            axis.text.y        = element_text(size = 10.5))

    ggplotly(p, tooltip = "text") %>% plotly::config(displayModeBar = FALSE)
  })

  # ── Tab 4: Support Effect ────────────────────────────────────────────────
  output$support_plot <- renderPlotly({
    base <- user_row_d()             # debounced
    row0 <- base; row0$has_support <- 0L
    row1 <- base; row1$has_support <- 1L
    p0   <- rf_predict_prob(row0)
    p1   <- rf_predict_prob(row1)

    sup_df <- bind_rows(
      data.frame(outcome = colnames(p0), prob = as.numeric(p0[1, ]),
                 support = "No external support"),
      data.frame(outcome = colnames(p1), prob = as.numeric(p1[1, ]),
                 support = "External support")
    ) %>%
      mutate(
        outcome = factor(outcome, levels = rev(names(outcome_colors))),
        support = factor(support,
                         levels = c("No external support", "External support"))
      )

    p <- ggplot(sup_df,
                aes(x = prob, y = outcome, fill = support,
                    text = paste0("<b>", outcome, "</b> \u00b7 ", support,
                                  "<br>", percent(prob, accuracy = 1)))) +
      geom_col(position = position_dodge(width = 0.7), width = 0.6) +
      scale_fill_manual(values = c("No external support" = "#B5ADA3",
                                   "External support"    = ACCENT),
                        name = NULL) +
      scale_x_continuous(labels = percent_format(accuracy = 1),
                         expand = expansion(mult = c(0, 0.1))) +
      labs(title    = "Effect of external support on predicted outcomes",
           subtitle = "All other inputs held at your selected values",
           x = "Predicted probability", y = NULL,
           caption  = paste0("Source: UCDP/PRIO Termination v4-2024 + ESD \u00b7 ",
                             "RF model (n = 500 trees)")) +
      theme_conflict() +
      theme(panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_line(color = "grey92"),
            legend.position    = "top",
            axis.text.y        = element_text(size = 11))

    ggplotly(p, tooltip = "text") %>%
      plotly::config(displayModeBar = FALSE) %>%
      plotly::layout(barmode = "group",
                     legend  = list(orientation = "h", x = 0, y = -0.15))
  })

  # ── Tab 5: Comparators ───────────────────────────────────────────────────
  # Z_DATA (the normalized feature matrix) is pre-computed at startup above —
  # only the per-user distance vector is computed here.
  output$comp_table <- DT::renderDT({
    u      <- user_row_d()[, COMP_FEATS]   # debounced
    z_user <- as.numeric(u) / COMP_SDS    # uses pre-computed SDS

    cat_match <- (rf_data$type_of_conflict == as.integer(input$type_of_conflict)) +
                 (rf_data$region           == as.integer(input$region))           +
                 (rf_data$has_support      == as.integer(input$has_support))      +
                 (rf_data$incompatibility  == as.integer(input$incompatibility))

    # Z_DATA already normalized; only user-specific part computed per render
    dist <- sqrt(rowSums(sweep(Z_DATA, 2, z_user, "-")^2)) - 0.6 * cat_match

    out <- rf_data %>%
      mutate(distance = dist) %>%
      arrange(distance) %>%
      slice(1:15) %>%
      mutate(
        Type = case_when(
          type_of_conflict == 1 ~ "Extrasystemic",
          type_of_conflict == 2 ~ "Interstate",
          type_of_conflict == 3 ~ "Intrastate",
          type_of_conflict == 4 ~ "Intl. Intrastate"),
        Region = case_when(
          region == 1 ~ "Europe",      region == 2 ~ "Middle East",
          region == 3 ~ "Asia",        region == 4 ~ "Africa",
          region == 5 ~ "Americas"),
        Support          = if_else(has_support == 1, "Yes", "No"),
        `Duration (yrs)` = c_ep_dur,
        Outcome          = as.character(c_outcome_label)
      ) %>%
      select(Conflict = location, Year = c_ep_endyear, Type, Region,
             Support, `Duration (yrs)`, Outcome)

    DT::datatable(out, rownames = FALSE,
                  options = list(pageLength = 15, dom = "tip"),
                  class   = "compact stripe hover") %>%
      DT::formatStyle("Outcome",
        target     = "cell",
        color      = DT::styleEqual(names(outcome_colors), outcome_colors),
        fontWeight = "bold")
  })


  outputOptions(output, "sens_plot",    suspendWhenHidden = TRUE)
  outputOptions(output, "imp_plot",     suspendWhenHidden = TRUE)
  outputOptions(output, "support_plot", suspendWhenHidden = TRUE)
  outputOptions(output, "comp_table",   suspendWhenHidden = TRUE)
}

shinyApp(ui = ui, server = server)
