suppressPackageStartupMessages({
  library(dplyr)
  library(broom)
  library(sandwich)
  library(lmtest)
  library(marginaleffects)
  library(tibble)
})

source("R/utils.R")

model_covariates <- c(
  "maternal_age_cat", "maternal_education_cat", "prenatal_care_cat",
  "plurality_cat", "parity_or_birth_order", "newborn_sex", "birth_year"
)

complete_model_data <- function(df, outcome, exposure, covariates = model_covariates) {
  vars <- unique(c(outcome, exposure, covariates, "country"))
  df |>
    select(any_of(vars)) |>
    filter(!is.na(.data[[outcome]]), !is.na(.data[[exposure]])) |>
    filter(if_all(any_of(covariates), ~ !is.na(.x) & .x != "unknown"))
}

aggregate_model_data <- function(data, outcome, predictors) {
  group_vars <- unique(c(predictors, "country"))
  data |>
    mutate(.event = as.integer(.data[[outcome]])) |>
    group_by(across(all_of(group_vars))) |>
    summarise(
      events = sum(.event, na.rm = TRUE),
      total = dplyr::n(),
      .groups = "drop"
    ) |>
    filter(total > 0)
}

fit_modified_poisson <- function(data, outcome, formula_rhs, predictors) {
  agg <- aggregate_model_data(data, outcome, predictors)
  formula <- stats::as.formula(sprintf("events ~ %s + offset(log(total))", formula_rhs))
  fit <- stats::glm(formula, data = agg, family = poisson(link = "log"))
  robust <- sandwich::vcovHC(fit, type = "HC0")
  broom::tidy(lmtest::coeftest(fit, vcov. = robust), conf.int = FALSE) |>
    mutate(
      log_estimate = estimate,
      estimate = exp(log_estimate),
      conf.low = exp(log_estimate - 1.96 * std.error),
      conf.high = exp(log_estimate + 1.96 * std.error),
      outcome = outcome,
      n = sum(agg$total),
      cells = nrow(agg)
    )
}

fit_country_risk_score_models <- function(df, outcomes) {
  purrr::map_dfr(outcomes, function(outcome) {
    purrr::map_dfr(split(df, df$country), function(country_df) {
      model_df <- complete_model_data(country_df, outcome, "risk_score_cat")
      if (nrow(model_df) == 0 || length(unique(model_df[[outcome]])) < 2) {
        return(tibble())
      }
      covariates <- c("maternal_age_cat", "maternal_education_cat", "prenatal_care_cat", "plurality_cat", "parity_or_birth_order", "newborn_sex", "birth_year")
      covariates <- covariates[vapply(covariates, function(v) dplyr::n_distinct(model_df[[v]], na.rm = TRUE) > 1, logical(1))]
      rhs <- paste(c("risk_score_cat", covariates[!covariates %in% "birth_year"], if ("birth_year" %in% covariates) "factor(birth_year)" else NULL), collapse = " + ")
      predictors <- c("risk_score_cat", covariates)
      fit_modified_poisson(
        model_df,
        outcome,
        rhs,
        predictors
      ) |>
        mutate(country = unique(country_df$country))
    })
  })
}

fit_pooled_risk_score_models <- function(df, outcomes) {
  purrr::map_dfr(outcomes, function(outcome) {
    model_df <- complete_model_data(df, outcome, "risk_score_cat")
    if (nrow(model_df) == 0 || length(unique(model_df[[outcome]])) < 2) {
      return(tibble())
    }
    covariates <- c("maternal_age_cat", "maternal_education_cat", "prenatal_care_cat", "plurality_cat", "parity_or_birth_order", "newborn_sex", "birth_year")
    covariates <- covariates[vapply(covariates, function(v) dplyr::n_distinct(model_df[[v]], na.rm = TRUE) > 1, logical(1))]
    rhs <- paste(c("risk_score_cat * country", covariates[!covariates %in% "birth_year"], if ("birth_year" %in% covariates) "factor(birth_year)" else NULL), collapse = " + ")
    predictors <- c("risk_score_cat", "country", covariates)
    fit_modified_poisson(
      model_df,
      outcome,
      rhs,
      predictors
    ) |>
      mutate(country = "Pooled")
  })
}

fit_phenotype_models <- function(df, outcomes) {
  purrr::map_dfr(outcomes, function(outcome) {
    model_df <- complete_model_data(df, outcome, "risk_phenotype", covariates = c("parity_or_birth_order", "newborn_sex", "birth_year"))
    if (nrow(model_df) == 0 || length(unique(model_df[[outcome]])) < 2) {
      return(tibble())
    }
    covariates <- c("parity_or_birth_order", "newborn_sex", "birth_year")
    covariates <- covariates[vapply(covariates, function(v) dplyr::n_distinct(model_df[[v]], na.rm = TRUE) > 1, logical(1))]
    rhs <- paste(c("risk_phenotype * country", covariates[!covariates %in% "birth_year"], if ("birth_year" %in% covariates) "factor(birth_year)" else NULL), collapse = " + ")
    predictors <- c("risk_phenotype", "country", covariates)
    fit_modified_poisson(
      model_df,
      outcome,
      rhs,
      predictors
    ) |>
      mutate(country = "Pooled")
  })
}

predict_absolute_risk <- function(df, outcome) {
  model_df <- complete_model_data(df, outcome, "risk_phenotype", covariates = c("parity_or_birth_order", "newborn_sex", "birth_year"))
  if (nrow(model_df) == 0 || length(unique(model_df[[outcome]])) < 2) {
    return(tibble())
  }
  predictors <- c("risk_phenotype", "country", "parity_or_birth_order", "newborn_sex", "birth_year")
  agg <- aggregate_model_data(model_df, outcome, predictors)
  fit <- stats::glm(
    events ~ risk_phenotype * country + parity_or_birth_order + newborn_sex + factor(birth_year) + offset(log(total)),
    data = agg,
    family = poisson(link = "log")
  )
  countries <- sort(unique(agg$country))
  phenotypes <- levels(model_df$risk_phenotype)
  purrr::map_dfr(countries, function(cty) {
    base <- agg |> filter(country == cty)
    purrr::map_dfr(phenotypes, function(pheno) {
      newdata <- base |> mutate(risk_phenotype = factor(pheno, levels = levels(model_df$risk_phenotype)))
      predicted_events <- stats::predict(fit, newdata = newdata, type = "response")
      tibble(
        outcome = outcome,
        country = cty,
        risk_phenotype = pheno,
        adjusted_risk_per_1000 = sum(predicted_events, na.rm = TRUE) / sum(newdata$total, na.rm = TRUE) * 1000,
        conf.low = NA_real_,
        conf.high = NA_real_
      )
    })
  }) |>
    group_by(outcome, country) |>
    mutate(
      reference_risk = adjusted_risk_per_1000[risk_phenotype == "low_risk"][1],
      risk_difference_per_1000 = adjusted_risk_per_1000 - reference_risk
    ) |>
    ungroup()
}

calculate_paf <- function(df, outcomes) {
  purrr::map_dfr(outcomes, function(outcome) {
    df |>
      filter(!is.na(.data[[outcome]]), !is.na(risk_phenotype)) |>
      group_by(country) |>
      summarise(
        outcome_rate = mean(.data[[outcome]], na.rm = TRUE),
        exposed_outcome_rate = mean(.data[[outcome]][risk_phenotype != "low_risk"], na.rm = TRUE),
        exposed_fraction_among_cases = mean(risk_phenotype[.data[[outcome]]] != "low_risk", na.rm = TRUE),
        paf = if_else(outcome_rate > 0, (outcome_rate - mean(.data[[outcome]][risk_phenotype == "low_risk"], na.rm = TRUE)) / outcome_rate, NA_real_),
        .groups = "drop"
      ) |>
      mutate(outcome = outcome)
  })
}

