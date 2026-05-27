suppressPackageStartupMessages({
  library(dplyr);       library(tidyr);    library(stringr)
  library(readxl);      library(readr);    library(forcats)
  library(randomForest); library(countrycode)
})

outcome_colors <- c(
  "Peace Agreement" = "#1F7A5A",
  "Ceasefire"       = "#4A8FBF",
  "Victory"         = "#B8312F",
  "Low Activity"    = "#DD8452",
  "Other/Unclear"   = "#888780"
)

conflict    <- readRDS("Data/UcdpPrioConflict_v25_1.rds")
deaths_conf <- readRDS("Data/BattleDeaths_v25_1_conf.rds")
termination <- read_csv("Data/UCDPConflictTerminationDataset_v4_2024_Conflict.csv",
                        show_col_types = FALSE)
esd         <- read_excel("Data/External Support Dataset (ESD).xlsx")


esd_simple <- esd %>%
  group_by(conflict_id) %>%
  summarize(has_support = as.integer(any(ext_sup == 1, na.rm = TRUE)),
            .groups = "drop")

deaths_perconf <- deaths_conf %>%
  group_by(conflict_id) %>%
  summarize(total_deaths = sum(bd_best, na.rm = TRUE), .groups = "drop") %>%
  mutate(log_deaths = log10(pmax(total_deaths, 1)))

incompat_flag <- conflict %>%
  group_by(conflict_id) %>%
  summarize(incompatibility_type = first(incompatibility), .groups = "drop")

conflict_iso <- conflict %>%
  select(conflict_id, gwno_loc) %>%
  distinct() %>%
  mutate(
    gwno_primary = as.integer(str_extract(as.character(gwno_loc), "^[0-9]+")),
    iso2c = suppressWarnings(
      countrycode(gwno_primary, origin = "cown", destination = "iso2c")
    )
  ) %>%
  select(conflict_id, iso2c) %>%
  filter(!is.na(iso2c))

wb_country <- WDI::WDI(indicator = "NY.GDP.PCAP.CD",
                       start = 1960, end = 2024, extra = FALSE) %>%
  rename(gdp_per_cap = NY.GDP.PCAP.CD) %>%
  filter(!is.na(gdp_per_cap)) %>%
  select(iso2c, year, gdp_per_cap)

wb_summary <- termination %>%
  filter(c_epterm == 1) %>%
  select(conflict_id, end_year = c_ep_endyear) %>%
  left_join(conflict_iso, by = "conflict_id") %>%
  left_join(wb_country, by = c("iso2c", "end_year" = "year")) %>%
  group_by(conflict_id) %>%
  summarize(gdp_per_cap = mean(gdp_per_cap, na.rm = TRUE), .groups = "drop") %>%
  mutate(log_gdp = log10(pmax(gdp_per_cap, 1, na.rm = TRUE)))

vdem_country <- vdemdata::vdem %>%
  select(country_text_id, year, v2x_polyarchy) %>%
  filter(!is.na(v2x_polyarchy)) %>%
  mutate(iso2c = suppressWarnings(
    countrycode(country_text_id, origin = "iso3c", destination = "iso2c")
  )) %>%
  filter(!is.na(iso2c))

vdem_summary <- termination %>%
  filter(c_epterm == 1) %>%
  select(conflict_id, end_year = c_ep_endyear) %>%
  left_join(conflict_iso,  by = "conflict_id") %>%
  left_join(vdem_country,  by = c("iso2c", "end_year" = "year")) %>%
  group_by(conflict_id) %>%
  summarize(democracy = mean(v2x_polyarchy, na.rm = TRUE), .groups = "drop")

rf_data <- termination %>%
  filter(c_epterm == 1) %>%
  distinct(conflict_id, .keep_all = TRUE) %>%
  left_join(esd_simple,     by = "conflict_id") %>%
  left_join(deaths_perconf, by = "conflict_id") %>%
  left_join(incompat_flag,  by = "conflict_id") %>%
  left_join(wb_summary,     by = "conflict_id") %>%
  left_join(vdem_summary,   by = "conflict_id") %>%
  mutate(
    has_support     = replace_na(has_support, 0L),
    log_deaths      = replace_na(log_deaths, 0),
    incompatibility = replace_na(as.integer(incompatibility_type), 2L),
    log_gdp   = if_else(is.na(log_gdp)   | !is.finite(log_gdp),
                        median(log_gdp,   na.rm = TRUE), log_gdp),
    democracy = if_else(is.na(democracy)  | !is.finite(democracy),
                        median(democracy, na.rm = TRUE), democracy),
    c_outcome_label = case_when(
      c_outcome == 1 ~ "Peace Agreement", c_outcome == 2 ~ "Ceasefire",
      c_outcome == 3 ~ "Victory",         c_outcome == 4 ~ "Low Activity",
      c_outcome == 5 ~ "Other/Unclear",   TRUE ~ NA_character_)
  ) %>%
  filter(!is.na(c_outcome_label), !is.na(type_of_conflict),
         !is.na(region), !is.na(c_ep_dur)) %>%
  mutate(c_outcome_label = factor(c_outcome_label,
                                  levels = names(outcome_colors))) %>%
  select(type_of_conflict, region, has_support, c_ep_dur,
         intensity_level, log_deaths, incompatibility,
         log_gdp, democracy, c_outcome_label,
         conflict_id, location, c_ep_endyear)
set.seed(42)
rf_model <- randomForest::randomForest(
  c_outcome_label ~ type_of_conflict + region +
    has_support + c_ep_dur + intensity_level +
    log_deaths + incompatibility + log_gdp + democracy,
  data = rf_data, ntree = 500, importance = TRUE
)
rf_model$y         <- NULL   # copy of training response vector
rf_model$oob.times <- NULL   # per-observation OOB bootstrap counts
rf_model$votes     <- NULL   # OOB vote matrix (nrow x nclass)
rf_model$err.rate  <- NULL   # per-tree OOB error-rate trace (ntree x nclass+1)
rf_model$confusion <- NULL   # OOB confusion matrix + class-error column

imp <- as.data.frame(randomForest::importance(rf_model)) %>%
  tibble::rownames_to_column("variable") %>%
  mutate(
    label = case_when(
      variable == "type_of_conflict" ~ "Conflict type",
      variable == "region"           ~ "Region",
      variable == "has_support"      ~ "External support",
      variable == "c_ep_dur"         ~ "Episode duration",
      variable == "intensity_level"  ~ "Peak intensity",
      variable == "log_deaths"       ~ "Battle deaths (log)",
      variable == "incompatibility"  ~ "Issue (territory/govt)",
      variable == "log_gdp"          ~ "GDP per capita (log)",
      variable == "democracy"        ~ "Democracy score"
    ),
    family = if_else(variable %in% c("intensity_level", "log_deaths", "c_ep_dur"),
                     "Battlefield", "Structural")
  )

saveRDS(rf_model, "rf_model.rds")
saveRDS(rf_data,  "rf_data.rds")
saveRDS(imp,      "imp.rds")

