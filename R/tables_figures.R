suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(readr)
  library(scales)
})

source("R/utils.R")

make_table1 <- function(df) {
  df |>
    group_by(country) |>
    summarise(
      births = n(),
      maternal_age_mean = mean(maternal_age, na.rm = TRUE),
      age_risk_pct = mean(age_risk, na.rm = TRUE) * 100,
      low_education_pct = mean(low_education, na.rm = TRUE) * 100,
      inadequate_prenatal_care_pct = mean(inadequate_prenatal_care, na.rm = TRUE) * 100,
      multiple_gestation_pct = mean(multiple_gestation, na.rm = TRUE) * 100,
      preterm_birth_pct = mean(preterm_birth, na.rm = TRUE) * 100,
      low_birth_weight_pct = mean(low_birth_weight, na.rm = TRUE) * 100,
      cesarean_delivery_pct = mean(cesarean_delivery, na.rm = TRUE) * 100,
      .groups = "drop"
    )
}

make_table2 <- function(df) {
  df |>
    group_by(country, birth_year) |>
    summarise(
      births = n(),
      preterm_birth_rate_per_1000 = mean(preterm_birth, na.rm = TRUE) * 1000,
      low_birth_weight_rate_per_1000 = mean(low_birth_weight, na.rm = TRUE) * 1000,
      cesarean_delivery_rate_per_1000 = mean(cesarean_delivery, na.rm = TRUE) * 1000,
      low_apgar5_rate_per_1000 = mean(low_apgar5, na.rm = TRUE) * 1000,
      congenital_anomaly_rate_per_1000 = mean(congenital_anomaly, na.rm = TRUE) * 1000,
      .groups = "drop"
    )
}

make_missing_table <- function(df) {
  vars <- c(
    "maternal_age", "maternal_education_cat", "prenatal_care_cat", "plurality_cat",
    "parity_or_birth_order", "gestational_age", "birth_weight", "delivery_mode",
    "newborn_sex", "apgar5", "congenital_anomaly"
  )
  df |>
    group_by(country, birth_year) |>
    summarise(
      across(all_of(vars), ~ mean(is.na(.x) | as.character(.x) == "unknown") * 100, .names = "{.col}_missing_pct"),
      births = n(),
      .groups = "drop"
    )
}

plot_outcome_trends <- function(table2) {
  table2 |>
    select(country, birth_year, ends_with("_rate_per_1000")) |>
    pivot_longer(ends_with("_rate_per_1000"), names_to = "outcome", values_to = "rate_per_1000") |>
    filter(outcome %in% c("preterm_birth_rate_per_1000", "low_birth_weight_rate_per_1000", "cesarean_delivery_rate_per_1000")) |>
    mutate(outcome = recode(
      outcome,
      preterm_birth_rate_per_1000 = "Preterm birth",
      low_birth_weight_rate_per_1000 = "Low birth weight",
      cesarean_delivery_rate_per_1000 = "Cesarean delivery"
    )) |>
    ggplot(aes(x = birth_year, y = rate_per_1000, color = country)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.8) +
    facet_wrap(~ outcome, scales = "free_y") +
    labs(x = "Birth year", y = "Rate per 1000 live births", color = "Country") +
    theme_minimal(base_size = 12)
}

plot_model_rr <- function(model_table) {
  model_table |>
    filter(str_detect(term, "risk_score_cat")) |>
    ggplot(aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high, color = country)) +
    geom_vline(xintercept = 1, linetype = "dashed") +
    geom_pointrange(position = position_dodge(width = 0.6)) +
    facet_wrap(~ outcome, scales = "free_y") +
    scale_x_log10() +
    labs(x = "Adjusted risk ratio", y = NULL, color = "Model") +
    theme_minimal(base_size = 12)
}

plot_absolute_risk <- function(abs_risk) {
  abs_risk |>
    ggplot(aes(x = risk_phenotype, y = adjusted_risk_per_1000, fill = country)) +
    geom_col(position = position_dodge(width = 0.8)) +
    facet_wrap(~ outcome, scales = "free_y") +
    coord_flip() +
    labs(x = "Risk phenotype", y = "Adjusted risk per 1000 live births", fill = "Country") +
    theme_minimal(base_size = 12)
}

plot_paf <- function(paf) {
  paf |>
    ggplot(aes(x = outcome, y = paf * 100, fill = country)) +
    geom_col(position = position_dodge(width = 0.8)) +
    coord_flip() +
    labs(x = "Outcome", y = "Population attributable fraction (%)", fill = "Country") +
    theme_minimal(base_size = 12)
}

