source("R/utils.R")

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(tidyr)
  library(stringr)
})

cfg <- load_config()
period_suffix <- paste0(min(cfg$project$years), "_", max(cfg$project$years))
parquet_path <- root_path("data", "final", sprintf("pooled_harmonized_births_%s.parquet", period_suffix))
sub_dir <- root_path("submission")
tab_supp <- file.path(sub_dir, "tables", "supplementary")
out_dir <- root_path("outputs", "validation")
dir.create(tab_supp, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

set.seed(20260501)
sample_per_country_year <- 20000L
pool_per_country_year <- 50000L
countries <- c("Brazil", "United States")
years <- min(cfg$project$years):max(cfg$project$years)
ds <- arrow::open_dataset(parquet_path)

sample_one <- function(country_name, year_value) {
  pooled <- ds |>
    filter(country == country_name, birth_year == year_value, plurality_cat == "singleton") |>
    select(
      country,
      birth_year,
      age_risk,
      inadequate_prenatal_care,
      low_education,
      preterm_birth,
      low_birth_weight,
      parity_or_birth_order,
      newborn_sex
    ) |>
    head(pool_per_country_year) |>
    collect()
  slice_sample(pooled, n = min(sample_per_country_year, nrow(pooled)))
}

validation_sample <- bind_rows(lapply(countries, function(country_name) {
  bind_rows(lapply(years, function(year_value) sample_one(country_name, year_value)))
})) |>
  mutate(
    risk_score3 = as.integer(age_risk) + as.integer(inadequate_prenatal_care) + as.integer(low_education),
    risk_score3_cat = factor(as.character(risk_score3), levels = c("0", "1", "2", "3")),
    parity_or_birth_order = as.numeric(parity_or_birth_order),
    newborn_sex = factor(newborn_sex)
  )

fit_individual <- function(data, outcome) {
  model_df <- data |>
    filter(
      !is.na(.data[[outcome]]),
      !is.na(risk_score3_cat),
      !is.na(parity_or_birth_order),
      !is.na(newborn_sex),
      as.character(newborn_sex) != "unknown"
    )
  fit <- glm(
    as.formula(paste0(outcome, " ~ risk_score3_cat + parity_or_birth_order + newborn_sex + factor(birth_year)")),
    data = model_df,
    family = poisson(link = "log")
  )
  tibble(
    term = names(coef(fit)),
    individual_log = unname(coef(fit)),
    individual_arr = exp(individual_log),
    sample_n = nrow(model_df)
  )
}

fit_grouped <- function(data, outcome) {
  model_df <- data |>
    filter(
      !is.na(.data[[outcome]]),
      !is.na(risk_score3_cat),
      !is.na(parity_or_birth_order),
      !is.na(newborn_sex),
      as.character(newborn_sex) != "unknown"
    ) |>
    mutate(event = as.integer(.data[[outcome]])) |>
    group_by(risk_score3_cat, parity_or_birth_order, newborn_sex, birth_year) |>
    summarise(events = sum(event, na.rm = TRUE), total = n(), .groups = "drop")
  fit <- glm(
    events ~ risk_score3_cat + parity_or_birth_order + newborn_sex + factor(birth_year) + offset(log(total)),
    data = model_df,
    family = poisson(link = "log")
  )
  tibble(
    term = names(coef(fit)),
    grouped_log = unname(coef(fit)),
    grouped_arr = exp(grouped_log),
    grouped_cells = nrow(model_df)
  )
}

term_labels <- c(
  risk_score3_cat1 = "1 domain",
  risk_score3_cat2 = "2 domains",
  risk_score3_cat3 = "3 domains"
)
outcome_labels <- c(
  preterm_birth = "Preterm birth",
  low_birth_weight = "Low birth weight"
)
validation_table <- bind_rows(lapply(countries, function(country_name) {
  country_data <- validation_sample |> filter(country == country_name)
  bind_rows(lapply(names(outcome_labels), function(outcome) {
    fit_individual(country_data, outcome) |>
      inner_join(fit_grouped(country_data, outcome), by = "term") |>
      filter(term %in% names(term_labels)) |>
      transmute(
        Outcome = outcome_labels[[outcome]],
        Country = country_name,
        `Risk score` = term_labels[term],
        `Individual-record aRR` = sprintf("%.4f", individual_arr),
        `Grouped-count aRR` = sprintf("%.4f", grouped_arr),
        `Absolute difference` = sprintf("%.6f", abs(individual_arr - grouped_arr)),
        `Validation sample N` = format(sample_n, big.mark = ",", scientific = FALSE),
        `Grouped covariate-pattern cells` = format(grouped_cells, big.mark = ",", scientific = FALSE),
        Note = "Country-year stratified validation subsample; point estimates compare identical model specifications fitted to individual records and grouped covariate-pattern counts."
      )
  }))
}))

write.table(
  validation_table,
  file.path(tab_supp, "Supplementary_Table_19_grouped_count_validation.csv"),
  sep = ",",
  row.names = FALSE,
  col.names = TRUE,
  quote = TRUE,
  qmethod = "double",
  na = ""
)
write.table(
  validation_table,
  file.path(out_dir, "Supplementary_Table_19_grouped_count_validation.csv"),
  sep = ",",
  row.names = FALSE,
  col.names = TRUE,
  quote = TRUE,
  qmethod = "double",
  na = ""
)

message("Grouped-count validation table written.")
