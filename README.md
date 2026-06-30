# Armed Conflict Analysis & Visualization

**Raymond Santiago Flores · STAT 3280 Data Visualization · Spring 2026 · University of Virginia**

An end-to-end data visualization project exploring patterns, dynamics, and outcomes of armed conflicts worldwide using the [UCDP/PRIO Armed Conflict Dataset](https://ucdp.uu.se/). The project combines exploratory analysis, time-series modeling, interactive Shiny dashboards, and a deployed conflict outcome predictor.

---

## Overview

This project asks: *What structural patterns distinguish conflicts that end in peace agreements from those that end in military victory, ceasefire, or prolonged stalemate?* Using four decades of conflict event data, it builds a suite of visualizations and a Random Forest–based predictive model to explore that question.

---

## Key Deliverables

| Deliverable | Description |
|---|---|
| `DVis-Proj-fin.qmd` | Full Quarto report — EDA, time-series analysis, outcome classification visualizations |
| `shiny/app.R` (App 1) | Interactive conflict explorer — filter by type, intensity, region, and time period |
| `conflict-predictor/app.R` (App 2) | Deployed Random Forest outcome predictor — user inputs conflict characteristics, model returns predicted resolution probability |

https://htmlpreview.github.io/?
https://github.com/raysflores/armed-conflict-analysis/blob/main/armed-conflict-prediction.html


---

## Data Sources

- **UCDP/PRIO Armed Conflict Dataset v25.1** — conflict-level panel data (1946–2024)
- **UCDP Battle-Related Deaths Dataset v25.1** — annual fatality estimates by conflict
- **UCDP Georeferenced Event Dataset (GED) v25.1** — geolocated conflict incidents
- **UCDP Conflict Termination Dataset v4 (2024)** — resolution outcomes by conflict episode
- **UCDP External Support Dataset** — third-party state involvement
- **World Bank World Development Indicators (WDI)** — country-level economic covariates
- **Peace Agreement Dataset** — coded peace agreement characteristics

---

## Methods & Technical Stack

- **Languages:** R
- **Key packages:** `tidyverse`, `ggplot2`, `plotly`, `shiny`, `bslib`, `survival`, `forecast`, `patchwork`, `DT`, `randomForest`
- **Reporting:** Quarto (`.qmd`)
- **Deployment:** Shiny apps deployed to [shinyapps.io](https://www.shinyapps.io/) under account `raysflores`
- **Statistical methods:** Time-series decomposition, survival analysis (conflict duration), Random Forest classification (outcome prediction), comparative visualization

---

## Conflict Outcome Predictor (App 2)

App 2 is the project's flagship deliverable. A trained Random Forest model accepts user-defined conflict characteristics as inputs (conflict type, intensity level, external support, duration, fatality count, geographic region) and returns:

- Predicted outcome probabilities across five categories: Peace Agreement, Ceasefire, Victory, Low Activity, Ongoing
- A comparator panel showing where the user-defined conflict sits relative to historical analogues in the training data
- A feature importance visualization

Performance optimizations include debounced slider inputs, pre-computed comparator matrices, and `suspendWhenHidden` on all heavy outputs.

---

## Conflict Types Analyzed

- **Extrasystemic** — colonial/imperial conflicts
- **Interstate** — between recognized states
- **Intrastate** — internal armed conflict (with and without foreign intervention)

---

## Repository Structure

```
├── DVis-Proj-fin.qmd          # Main Quarto report
├── Data/
│   ├── UcdpPrioConflict_v25_1.rds
│   ├── BattleDeaths_v25_1_conf.rds
│   ├── GEDEvent_v25_1.rds
│   ├── UCDPConflictTerminationDataset_v4_2024_Conflict.csv
│   ├── External Support Dataset (ESD).xlsx
│   └── Peace Agreement.xlsx
├── shiny/                     # App 1 — Conflict Explorer
│   └── app.R
└── conflict-predictor/        # App 2 — Outcome Predictor (RF)
    ├── app.R
    ├── rf_model.rds
    └── precompute_app2_v7.R
```

---

## About

Completed as the capstone project for STAT 3280: Data Visualization at the University of Virginia. All data is publicly available from the Uppsala Conflict Data Program (UCDP) and the World Bank.

**Author:** Raymond Santiago Flores | [raymondosf@gmail.com](mailto:raymondosf@gmail.com)
