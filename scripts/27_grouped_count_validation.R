source("R/utils.R")

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(sandwich)
})

# Dataset fragment scans can otherwise complete in thread-dependent order,
# changing which records enter the fixed country-year validation pools even
# when R's random seed is fixed.
arrow::set_cpu_count(1L)

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
  individual_vcov <- sandwich::vcovHC(fit, type = "HC0")
  tibble(
    term = names(coef(fit)),
    individual_log = unname(coef(fit)),
    individual_arr = exp(individual_log),
    individual_hc0_se = sqrt(diag(individual_vcov)),
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
  grouped_pattern_vcov <- sandwich::vcovHC(fit, type = "HC0")
  grouped_exact_vcov <- grouped_binary_hc0(fit, model_df$events, model_df$total)
  tibble(
    term = names(coef(fit)),
    grouped_log = unname(coef(fit)),
    grouped_arr = exp(grouped_log),
    grouped_pattern_hc0_se = sqrt(diag(grouped_pattern_vcov)),
    grouped_exact_hc0_se = sqrt(diag(grouped_exact_vcov)),
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
        `Individual-record HC0 SE` = sprintf("%.8f", individual_hc0_se),
        `Naive grouped-pattern HC0 SE` = sprintf("%.8f", grouped_pattern_hc0_se),
        `Reconstructed individual-record HC0 SE` = sprintf("%.8f", grouped_exact_hc0_se),
        `Absolute reconstructed SE difference` = sprintf("%.10f", abs(individual_hc0_se - grouped_exact_hc0_se)),
        `Validation sample N` = format(sample_n, big.mark = ",", scientific = FALSE),
        `Grouped covariate-pattern cells` = format(grouped_cells, big.mark = ",", scientific = FALSE)
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

standardized_risk_validation <- function(data, outcome, country_name) {
  model_df <- data |>
    filter(
      country == country_name,
      !is.na(.data[[outcome]]),
      !is.na(risk_score3_cat),
      !is.na(parity_or_birth_order),
      !is.na(newborn_sex),
      as.character(newborn_sex) != "unknown"
    )
  formula_individual <- as.formula(
    paste0(outcome, " ~ risk_score3_cat + parity_or_birth_order + newborn_sex + factor(birth_year)")
  )
  individual_fit <- glm(formula_individual, data = model_df, family = poisson(link = "log"))
  individual_vcov <- sandwich::vcovHC(individual_fit, type = "HC0")

  grouped_df <- model_df |>
    mutate(event = as.integer(.data[[outcome]])) |>
    group_by(risk_score3_cat, parity_or_birth_order, newborn_sex, birth_year) |>
    summarise(events = sum(event, na.rm = TRUE), total = n(), .groups = "drop")
  grouped_fit <- glm(
    events ~ risk_score3_cat + parity_or_birth_order + newborn_sex + factor(birth_year) + offset(log(total)),
    data = grouped_df,
    family = poisson(link = "log")
  )
  grouped_vcov <- grouped_binary_hc0(grouped_fit, grouped_df$events, grouped_df$total)

  levels_exposure <- levels(model_df$risk_score3_cat)
  individual_risks <- purrr::map_dfr(levels_exposure, function(level_value) {
    newdata <- model_df
    newdata$risk_score3_cat <- factor(level_value, levels = levels_exposure)
    mm <- model.matrix(delete.response(terms(individual_fit)), newdata)
    mm <- mm[, names(coef(individual_fit)), drop = FALSE]
    predicted <- as.numeric(exp(mm %*% coef(individual_fit)))
    gradient <- colMeans(mm * predicted) * 1000
    tibble(
      risk_score = level_value,
      individual_risk = mean(predicted) * 1000,
      individual_gradient = list(gradient)
    )
  })
  grouped_risks <- purrr::map_dfr(levels_exposure, function(level_value) {
    newdata <- grouped_df
    newdata$risk_score3_cat <- factor(level_value, levels = levels_exposure)
    mm <- model.matrix(delete.response(terms(grouped_fit)), newdata)
    mm <- mm[, names(coef(grouped_fit)), drop = FALSE]
    predicted_events <- as.numeric(exp(mm %*% coef(grouped_fit) + log(newdata$total)))
    gradient <- colSums(mm * predicted_events) / sum(newdata$total) * 1000
    tibble(
      risk_score = level_value,
      grouped_risk = sum(predicted_events) / sum(newdata$total) * 1000,
      grouped_gradient = list(gradient)
    )
  })

  joined <- left_join(individual_risks, grouped_risks, by = "risk_score")
  individual_reference_gradient <- joined$individual_gradient[[1]]
  grouped_reference_gradient <- joined$grouped_gradient[[1]]
  individual_reference_risk <- joined$individual_risk[[1]]
  grouped_reference_risk <- joined$grouped_risk[[1]]
  joined |>
    mutate(
      individual_risk_se = purrr::map_dbl(
        individual_gradient,
        ~ sqrt(as.numeric(t(.x) %*% individual_vcov %*% .x))
      ),
      grouped_risk_se = purrr::map_dbl(
        grouped_gradient,
        ~ sqrt(as.numeric(t(.x) %*% grouped_vcov %*% .x))
      ),
      individual_risk_difference = individual_risk - individual_reference_risk,
      grouped_risk_difference = grouped_risk - grouped_reference_risk,
      individual_risk_difference_se = purrr::map_dbl(
        individual_gradient,
        ~ {
          gradient <- .x - individual_reference_gradient
          sqrt(as.numeric(t(gradient) %*% individual_vcov %*% gradient))
        }
      ),
      grouped_risk_difference_se = purrr::map_dbl(
        grouped_gradient,
        ~ {
          gradient <- .x - grouped_reference_gradient
          sqrt(as.numeric(t(gradient) %*% grouped_vcov %*% gradient))
        }
      ),
      country = country_name,
      outcome = outcome
    ) |>
    transmute(
      country,
      outcome,
      risk_score,
      individual_risk,
      grouped_risk,
      absolute_risk_difference = abs(individual_risk - grouped_risk),
      individual_risk_se,
      grouped_risk_se,
      absolute_risk_se_difference = abs(individual_risk_se - grouped_risk_se),
      individual_risk_difference,
      grouped_risk_difference,
      absolute_difference_in_risk_difference = abs(individual_risk_difference - grouped_risk_difference),
      individual_risk_difference_se,
      grouped_risk_difference_se,
      absolute_risk_difference_se_difference = abs(individual_risk_difference_se - grouped_risk_difference_se)
    )
}

absolute_risk_validation <- bind_rows(lapply(countries, function(country_name) {
  bind_rows(lapply(names(outcome_labels), function(outcome) {
    standardized_risk_validation(validation_sample, outcome, country_name)
  }))
}))
write.table(
  absolute_risk_validation,
  file.path(out_dir, "absolute_risk_delta_validation.csv"),
  sep = ",",
  row.names = FALSE,
  col.names = TRUE,
  quote = TRUE,
  qmethod = "double",
  na = ""
)

message("Grouped-count coefficient and absolute-risk validation tables written.")
