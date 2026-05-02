source("R/utils.R")

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(sandwich)
  library(lmtest)
  library(broom)
  library(stringr)
  library(tibble)
})

cfg <- load_config()
period_label <- paste0(min(cfg$project$years), "-", max(cfg$project$years))
period_suffix <- gsub("-", "_", period_label)

out_dir <- root_path("outputs", "additional_sensitivity", "tables")
sub_supp <- root_path("submission", "tables", "supplementary")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(sub_supp, recursive = TRUE, showWarnings = FALSE)

fmt_rr <- function(e, l, h) sprintf("%.2f (%.2f-%.2f)", as.numeric(e), as.numeric(l), as.numeric(h))
fmt_p <- function(p) ifelse(is.na(p), "", ifelse(as.numeric(p) < 0.001, "<0.001", sprintf("%.3f", as.numeric(p))))
fmt_n <- function(x) format(as.numeric(x), big.mark = ",", scientific = FALSE, trim = TRUE)

complete_model_data <- function(data, outcome, exposure, covariates) {
  vars <- unique(c(outcome, exposure, covariates, "country"))
  data |>
    select(any_of(vars)) |>
    filter(!is.na(.data[[outcome]]), !is.na(.data[[exposure]])) |>
    filter(if_all(any_of(covariates), ~ !is.na(.x) & as.character(.x) != "unknown"))
}

aggregate_model_data <- function(data, outcome, predictors) {
  data |>
    mutate(.event = as.integer(.data[[outcome]])) |>
    group_by(across(all_of(unique(c(predictors, "country"))))) |>
    summarise(events = sum(.event, na.rm = TRUE), total = n(), .groups = "drop") |>
    filter(total > 0)
}

fit_poisson_by_country <- function(data, outcome, exposure) {
  covariates <- c("parity_or_birth_order", "newborn_sex", "birth_year")
  purrr::imap_dfr(split(data, data$country), function(piece, country_name) {
    model_df <- complete_model_data(piece, outcome, exposure, covariates)
    if (!nrow(model_df) || dplyr::n_distinct(model_df[[outcome]], na.rm = TRUE) < 2) {
      return(tibble())
    }
    active_covariates <- covariates[vapply(covariates, function(v) dplyr::n_distinct(model_df[[v]], na.rm = TRUE) > 1, logical(1))]
    rhs <- paste(
      c(exposure, active_covariates[!active_covariates %in% "birth_year"], if ("birth_year" %in% active_covariates) "factor(birth_year)" else NULL),
      collapse = " + "
    )
    predictors <- c(exposure, active_covariates)
    agg <- aggregate_model_data(model_df, outcome, predictors)
    fit <- glm(as.formula(paste0("events ~ ", rhs, " + offset(log(total))")), data = agg, family = poisson(link = "log"))
    robust <- sandwich::vcovHC(fit, type = "HC0")
    broom::tidy(lmtest::coeftest(fit, vcov. = robust)) |>
      mutate(
        log_estimate = estimate,
        estimate = exp(log_estimate),
        conf.low = exp(log_estimate - 1.96 * std.error),
        conf.high = exp(log_estimate + 1.96 * std.error),
        outcome = outcome,
        n = sum(agg$total),
        country = country_name
      )
  })
}

format_score_rows <- function(tab, exposure_prefix, score_label, analysis_label) {
  tab |>
    filter(stringr::str_starts(term, exposure_prefix)) |>
    transmute(
      Analysis = analysis_label,
      Country = country,
      Outcome = dplyr::recode(outcome, preterm_birth = "Preterm birth", low_birth_weight = "Low birth weight"),
      `Risk score definition` = score_label,
      `Risk score` = paste0(stringr::str_remove(term, exposure_prefix), " domain", ifelse(stringr::str_remove(term, exposure_prefix) == "1", "", "s")),
      `aRR (95% CI)` = fmt_rr(estimate, conf.low, conf.high),
      `P value` = fmt_p(p.value),
      `Model N` = fmt_n(n)
    )
}

df <- arrow::read_parquet(root_path("data", "final", sprintf("pooled_harmonized_births_%s.parquet", period_suffix))) |>
  mutate(
    singleton_primary = plurality_cat == "singleton",
    analysis_period = case_when(
      birth_year <= 2019 ~ "2017-2019",
      birth_year <= 2021 ~ "2020-2021",
      birth_year <= 2024 ~ "2022-2024",
      TRUE ~ NA_character_
    ),
    risk_score3 = as.integer(age_risk) + as.integer(inadequate_prenatal_care) + as.integer(low_education),
    risk_score3_cat = factor(as.character(risk_score3), levels = c("0", "1", "2", "3")),
    risk_score_age_education = as.integer(age_risk) + as.integer(low_education),
    risk_score_age_education_cat = factor(as.character(risk_score_age_education), levels = c("0", "1", "2"))
  ) |>
  filter(singleton_primary)

primary <- c("preterm_birth", "low_birth_weight")

age25_models <- purrr::map_dfr(primary, function(outcome) {
  fit_poisson_by_country(
    df |> filter(maternal_age >= 25),
    outcome,
    "risk_score_age_education_cat"
  )
})

period_models <- purrr::map_dfr(sort(unique(na.omit(df$analysis_period))), function(period) {
  purrr::map_dfr(primary, function(outcome) {
    fit_poisson_by_country(
      df |> filter(analysis_period == period),
      outcome,
      "risk_score3_cat"
    ) |>
      mutate(analysis_period = period)
  })
})

exclude_2024_models <- purrr::map_dfr(primary, function(outcome) {
  fit_poisson_by_country(
    df |> filter(birth_year <= 2023),
    outcome,
    "risk_score3_cat"
  )
})

formatted_age25 <- format_score_rows(
  age25_models,
  "risk_score_age_education_cat",
  "Age + education only",
  "Restricted to maternal age >=25 years"
)

formatted_period <- period_models |>
  filter(stringr::str_starts(term, "risk_score3_cat")) |>
  transmute(
    Analysis = paste0("Birth period ", analysis_period),
    Country = country,
    Outcome = dplyr::recode(outcome, preterm_birth = "Preterm birth", low_birth_weight = "Low birth weight"),
    `Risk score definition` = "Main 3-domain score",
    `Risk score` = paste0(stringr::str_remove(term, "risk_score3_cat"), " domain", ifelse(stringr::str_remove(term, "risk_score3_cat") == "1", "", "s")),
    `aRR (95% CI)` = fmt_rr(estimate, conf.low, conf.high),
    `P value` = fmt_p(p.value),
    `Model N` = fmt_n(n)
  )

formatted_exclude_2024 <- format_score_rows(
  exclude_2024_models,
  "risk_score3_cat",
  "Main 3-domain score",
  "Excluding 2024"
)

s1_table <- bind_rows(formatted_age25, formatted_period, formatted_exclude_2024)

write_csv_safe(s1_table, file.path(out_dir, "supplementary_table_additional_sensitivity.csv"))
write_csv_safe(s1_table, file.path(sub_supp, "Supplementary_Table_11_additional_sensitivity.csv"))

message("Additional sensitivity table written.")
