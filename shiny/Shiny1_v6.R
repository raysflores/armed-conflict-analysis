suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
  library(forcats)
  library(stringr)
  library(survival)
  library(DT)
  library(ggrepel)
})

# designs
PAPER  <- "#FAFAFA"
INK    <- "#1F1F1D"
INK2   <- "#4A4A47"
RULE   <- "#E2E0DA"
ACCENT <- "#B8312F"

conflict_colors <- c(
  "Extrasystemic"      = "#B5ADA3",
  "Interstate"         = "#8A8178",
  "Intrastate"         = "#5A524A",
  "Intrastate (Intl.)" = "#C4AD8E"
)

intensity_colors <- c(
  "Minor Conflict" = "#B5ADA3",
  "War"            = "#5A524A"
)

support_km_colors <- c(
  "External Support"    = ACCENT,
  "No External Support" = "#4A8FBF"
)

theme_conflict <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(
        color = "grey92", linewidth = 0.4
      ),
      plot.title = element_text(
        hjust = 0,
        face = "bold",
        size = 14
      ),
      plot.title.position = "plot",
      plot.subtitle = element_text(
        color = INK2,
        size = 10.5,
        hjust = 0,
        margin = ggplot2::margin(b = 14)
      ),
      plot.caption = element_text(
        color = INK2,
        size = 8.5,
        margin = ggplot2::margin(t = 12),
        hjust = 1
      ),
      plot.caption.position = "plot",
      axis.title = element_text(color = INK, size = 10),
      axis.title.x = element_text(
        face = "bold",
        color = INK,
        size = 10
      ),
      axis.title.y = element_text(
        face = "bold",
        color = INK,
        size = 10
      ),
      axis.text = element_text(size = 9),
      legend.position = "bottom",
      legend.title = element_text(color = INK2, size = 9),
      legend.text = element_text(color = INK, size = 9),
      panel.background = element_rect(fill = PAPER, color = NA),
      plot.background = element_rect(fill = PAPER, color = NA),
      strip.text = element_text(
        face = "bold",
        color = INK,
        size = 11
      )
    )
}

# PRECOMPUTED DATA
conflict_data <- readRDS("conflict_data.rds")
km_base       <- readRDS("km_base.rds")

# UI
app_theme <- bs_theme(
  version    = 5,
  bg         = PAPER, fg = INK,
  primary    = ACCENT,
  font_scale = 1.0
)

# Need to have arial since tableau is using this type face
ui <- fluidPage(
  theme = app_theme,
  tags$head(
    tags$style(HTML(sprintf("
      body { background: %s; color: %s;
             font-family: Arial, Helvetica, sans-serif; }
      h1,h2,h3,h4 { font-family: Arial, Helvetica, sans-serif; color: %s; }
      .app-title  { font-family: Arial, Helvetica, sans-serif; font-weight: 700;
                    font-size: 1.9rem; color: %s; margin: .2em 0 .05em 0; }
      .app-deck   { font-style: italic; color: %s; font-size: 1.05rem;
                    margin-bottom: 1.6em; max-width: 760px; }
      .well, .form-control, .selectize-input {
                    background-color: #FFFFFF !important;
                    border-color: %s !important; color: %s; }
      .nav-tabs .nav-link        { color: %s; font-family: Arial, Helvetica, sans-serif; }
      .nav-tabs .nav-link.active { color: %s; font-weight: 600;
                    border-bottom: 2px solid %s !important; }
      .kpi-box { background: #FFFFFF; border-left: 3px solid %s;
                 padding: .55em .85em; margin: 0 0 .7em 0;
                 font-family: Arial, Helvetica, sans-serif; }
      .kpi-box .kpi-val { font-family: Arial, Helvetica, sans-serif;
                          font-size: 1.5rem; font-weight: 700; color: %s; }
      .kpi-box .kpi-lbl { font-size: .82rem; color: %s; }
    ", PAPER, INK, INK, INK, INK2, RULE, INK,
                            INK2, INK, ACCENT, ACCENT, ACCENT, INK2)))
  ),
  div(class = "app-title", "Historical Conflict Dashboard"),
  div(class = "app-deck",
      "Filter decades of armed conflict by period, region, type, ",
      "All five tabs update together."),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Filters"),
      sliderInput("year_range", "Year Range",
                  min = 1946, max = 2024,
                  value = c(1970, 2024), sep = ""),
      selectInput("region", "Focus region",
                  choices  = c("All", "Africa", "Americas",
                               "Asia", "Europe", "Middle East"),
                  selected = "All"),
      checkboxGroupInput("conflict_type", "Conflict type",
                         choices  = c( "Interstate",
                                      "Intrastate", "Intrastate (Intl.)"),
                         selected = c("Intrastate", "Intrastate (Intl.)")),
      radioButtons("intensity", "Intensity",
                   choices  = c("Both", "Minor Conflict", "War"),
                   selected = "Both"),
      numericInput("min_deaths", "Minimum battle deaths",
                   value = 0, min = 0, step = 100),
      hr(),
      uiOutput("kpi_boxes")
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "tabs", type = "tabs",
        tabPanel("Temporal Heatmap",
                 br(),
                 p(style = paste0("font-family: Arial, Helvetica, sans-serif;",
                                  "font-size:.95rem;",
                                  "color:", INK2, ";border-left:3px solid ",
                                  ACCENT, ";padding:.5em .8em;",
                                  "background:#fff;margin-bottom:1em;"),
                   "Darker cells = more active conflicts in that year. ",
                   "Intrastate conflict has dominated since the Cold War's end."),
                 plotOutput("heat_map", height = "300px")),
        tabPanel("Fatality Distribution",
                 br(),
                 p(style = paste0("font-family: Arial, Helvetica, sans-serif;",
                                  "font-size:.95rem;",
                                  "color:", INK2, ";border-left:3px solid ",
                                  ACCENT, ";padding:.5em .8em;",
                                  "background:#fff;margin-bottom:1em;"),
                   "Log-scale density of battle deaths by conflict type. ",
                   "The long right tail reflects the rare catastrophic wars."),
                 plotOutput("dist_plot", height = "400px")),
        tabPanel("Conflicts Over Time",
                 br(),
                 p(style = paste0("font-family: Arial, Helvetica, sans-serif;",
                                  "font-size:.95rem;",
                                  "color:", INK2, ";border-left:3px solid ",
                                  ACCENT, ";padding:.5em .8em;",
                                  "background:#fff;margin-bottom:1em;"),
                   "Annual count of active conflicts, split by type. ",
                   "Intrastate wars peak in the mid-1990s and remain elevated."),
                 plotOutput("trend_plot", height = "420px")),
        tabPanel("Survival (KM)",
                 br(),
                 p(style = paste0("font-family: Arial, Helvetica, sans-serif;",
                                  "font-size:.95rem;",
                                  "color:", INK2, ";border-left:3px solid ",
                                  ACCENT, ";padding:.5em .8em;",
                                  "background:#fff;margin-bottom:1em;"),
                   "Kaplan–Meier survivorship curves for terminated conflict ",
                   "episodes, stratified by external support status. ",
                   "The curve shows the proportion of episodes still active at ",
                   "each duration. Dashed verticals mark the median. ",
                   "Filter by conflict type to isolate a subset."),
                 plotOutput("km_plot", height = "440px")),
        tabPanel("Conflict List",
                 br(),
                 DT::dataTableOutput("conflict_table", width = "100%"))
      )
    )
  ),
  hr(),
  tags$p(class = "text-muted small",
         "Source: UCDP/PRIO Armed Conflict Dataset v25.1 + ",
         "Battle-Related Deaths Dataset v25.1 + ",
         "Conflict Termination Dataset v4-2024 + ESD")
)

# Server
server <- function(input, output, session) {
  filtered <- reactive({
    d <- conflict_data %>%
      filter(
        year          >= input$year_range[1],
        year          <= input$year_range[2],
        conflict_type %in% input$conflict_type,
        battle_deaths >= input$min_deaths
      )
    if (input$region    != "All")  d <- d %>% filter(region_label    == input$region)
    if (input$intensity != "Both") d <- d %>% filter(intensity_label == input$intensity)
    d
  })

  # KM filter
  km_filtered <- reactive({
    km_base %>% filter(conflict_type %in% input$conflict_type)
  })

  # ── KPI boxes
  output$kpi_boxes <- renderUI({
    d <- filtered()
    n_conflicts <- length(unique(d$conflict_id))
    n_deaths    <- sum(d$battle_deaths, na.rm = TRUE)
    tagList(
      div(class = "kpi-box",
          div(class = "kpi-val", comma(n_conflicts)),
          div(class = "kpi-lbl", "unique conflicts")),
      div(class = "kpi-box",
          div(class = "kpi-val", label_number(scale_cut = cut_short_scale())(n_deaths)),
          div(class = "kpi-lbl", "battle deaths"))
    )
  })

  # Heatmap
  output$heat_map <- renderPlot(bg = PAPER, {
    heat_data <- filtered() %>%
      count(year, conflict_type) %>%
      filter(!is.na(conflict_type))
    validate(need(nrow(heat_data) > 0, "Not enough data — adjust your filters."))
    ggplot(heat_data, aes(x = year, y = conflict_type, fill = n)) +
      geom_tile(color = "white", linewidth = 0.25) +
      scale_fill_gradient(low = "#EDE9E3", high = ACCENT,
                          name = "Active conflicts") +
      scale_x_continuous(breaks = seq(1946, 2024, by = 10)) +
      scale_y_discrete(limits = rev(names(conflict_colors))) +
      labs(x = "Year", y = NULL,
           caption = "Source: UCDP/PRIO Armed Conflict Dataset v25.1") +
      theme_conflict() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_blank(),
            legend.position    = "right")
  })

  # Fatality density
  output$dist_plot <- renderPlot(bg = PAPER, {
    d <- filtered() %>% filter(battle_deaths > 0)
    validate(need(nrow(d) > 5, "Not enough data — broaden your filters."))
    ggplot(d, aes(x = battle_deaths,
                  fill = conflict_type, color = conflict_type)) +
      geom_density(alpha = 0.45, linewidth = 0.7) +
      scale_x_log10(
        breaks = 10^(0:6),
        labels = c("1", "10", "100", "1 thousand", "10 thousand", "100 thousand", "1 million")
      ) +
      scale_fill_manual(values  = conflict_colors, name = NULL) +
      scale_color_manual(values = conflict_colors, name = NULL) +
      labs(x       = "Battle deaths (log scale)",
           y       = "Density",
           caption = "Conflicts with 0 recorded deaths excluded · Source: UCDP/PRIO v25.1") +
      theme_conflict()
  })

  # ── Conflicts over time ──────────────────────────────────────────────────────
  output$trend_plot <- renderPlot(bg = PAPER, {
    d <- filtered()
    validate(need(nrow(d) > 0, "No conflicts match the current filters."))
    trend <- d %>%
      group_by(year, conflict_type) %>%
      summarise(n = n_distinct(conflict_id), .groups = "drop")
    end_labels <- trend %>%
      group_by(conflict_type) %>%
      slice_max(year, n = 1, with_ties = FALSE) %>%
      ungroup()
    ggplot(trend, aes(x = year, y = n,
                      color = conflict_type, group = conflict_type)) +
      geom_line(linewidth = 0.95, alpha = 0.9) +
      geom_point(size = 1.8, alpha = 0.75) +
      ggrepel::geom_text_repel(
        data          = end_labels,
        aes(label     = conflict_type),
        hjust         = 0, nudge_x = 0.8,
        size          = 3, fontface = "bold",
        segment.color = "grey70", segment.size = 0.3,
        direction     = "y", max.overlaps = 10,
        show.legend   = FALSE
      ) +
      scale_color_manual(values = conflict_colors, name = NULL) +
      scale_x_continuous(breaks = seq(1946, 2024, by = 10),
                         expand = expansion(mult = c(0.01, 0.18))) +
      labs(x = "Year", y = "Active conflicts",
           caption = "Source: UCDP/PRIO Armed Conflict Dataset v25.1") +
      theme_conflict() +
      theme(legend.position = "none")
  })

# Kaplan–Meier 
  output$km_plot <- renderPlot(bg = PAPER, {
    d <- km_filtered()
    validate(need(
      nrow(d) >= 10,
      "Not enough terminated episodes for this conflict type."
    ))

    km_fit      <- survfit(Surv(duration_yr, event) ~ support_label, data = d)
    strata_names <- sub("support_label=", "", names(km_fit$strata))

    km_df <- bind_rows(
      data.frame(t = 0, surv = 1,
                 support_label = strata_names,
                 stringsAsFactors = FALSE),
      data.frame(t            = km_fit$time,
                 surv         = km_fit$surv,
                 support_label = rep(strata_names, km_fit$strata),
                 stringsAsFactors = FALSE)
    )

    km_table <- summary(km_fit)$table
    medians  <- data.frame(
      support_label = sub("support_label=", "", rownames(km_table)),
      med           = km_table[, "median"],
      stringsAsFactors = FALSE
    ) %>% filter(is.finite(med))

    ggplot(km_df, aes(x = t, y = surv, color = support_label)) +
      geom_step(linewidth = 0.95) +
      { if (nrow(medians) > 0)
          geom_vline(data = medians,
                     aes(xintercept = med, color = support_label),
                     linetype = "dashed", linewidth = 0.6, alpha = 0.7)
      } +
      scale_color_manual(values = support_km_colors, name = NULL) +
      scale_y_continuous(labels = percent_format(accuracy = 1),
                         limits = c(0, 1), expand = c(0, 0)) +
      scale_x_continuous(breaks = seq(0, 50, by = 5),
                         expand = expansion(mult = c(0, 0.02))) +
      labs(
        title    = "Conflicts with external support last longer",
        subtitle = "Kaplan–Meier survivorship of terminated episodes by support status · dashed lines = median duration",
        x        = "Episode duration (years)",
        y        = "Proportion still active",
        caption  = "Source: UCDP Termination Dataset v4-2024 + ESD"
      ) +
      theme_conflict() +
      theme(legend.position = "bottom")
  })

# Conflict list
  output$conflict_table <- DT::renderDataTable({
    d <- filtered() %>%
      group_by(location, region_label, conflict_type, intensity_label) %>%
      summarise(
        `Years active`  = paste(min(year), "–", max(year)),
        `Battle deaths` = comma(sum(battle_deaths, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      rename(Conflict  = location,
             Region    = region_label,
             Type      = conflict_type,
             Intensity = intensity_label) %>%
      arrange(Region, Type)
    validate(need(nrow(d) > 0, "No conflicts match these parameters."))
    d
  },
  options  = list(pageLength = 15, scrollX = TRUE,
                  dom = "ftp", language = list(search = "Search:")),
  rownames = FALSE,
  class    = "compact stripe hover")
}

shinyApp(ui = ui, server = server)
