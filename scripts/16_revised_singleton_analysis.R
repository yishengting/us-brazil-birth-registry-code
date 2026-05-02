source("R/utils.R")

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(sandwich)
  library(lmtest)
  library(broom)
  library(stringr)
})

out_dir <- root_path("outputs", "revision")
table_dir <- file.path(out_dir, "tables")
figure_dir <- file.path(out_dir, "figures")
manuscript_dir <- file.path(out_dir, "manuscript")
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(manuscript_dir, recursive = TRUE, showWarnings = FALSE)
cfg <- load_config()
period_label <- paste0(min(cfg$project$years), "-", max(cfg$project$years))
period_display <- gsub("-", "–", period_label)
period_suffix <- gsub("-", "_", period_label)
year_breaks <- seq(min(cfg$project$years), max(cfg$project$years))

fmt_n <- function(x) format(round(as.numeric(x)), big.mark = ",", scientific = FALSE)
fmt1 <- function(x) sprintf("%.1f", as.numeric(x))
fmt2 <- function(x) sprintf("%.2f", as.numeric(x))
fmt_p <- function(p) ifelse(is.na(p), "", ifelse(as.numeric(p) < 0.001, "<0.001", sprintf("%.3f", as.numeric(p))))
fmt_rr <- function(e, l, h) sprintf("%.2f (%.2f–%.2f)", as.numeric(e), as.numeric(l), as.numeric(h))

palette <- c("Brazil" = "#0072B2", "United States" = "#D55E00")
outcome_labels <- c(
  preterm_birth = "Preterm birth",
  low_birth_weight = "Low birth weight",
  very_preterm_birth = "Very preterm birth",
  very_low_birth_weight = "Very low birth weight",
  cesarean_delivery = "Cesarean delivery",
  low_apgar5 = "Low 5-minute Apgar",
  congenital_anomaly = "Congenital anomaly"
)
score_labels <- c("1" = "1 risk domain", "2" = "2 risk domains", "3" = "3 risk domains")
phenotype_labels <- c(
  low_risk = "Low risk",
  age_only = "Age risk only",
  inadequate_care_only = "Inadequate prenatal care only",
  low_education_only = "Low education only",
  age_inadequate = "Age risk + inadequate care",
  age_low_education = "Age risk + low education",
  education_inadequate = "Low education + inadequate care",
  all_three = "All three domains"
)

theme_pub <- function(base_size = 10) {
  theme_classic(base_size = base_size, base_family = "Helvetica") +
    theme(
      plot.title = element_text(face = "bold", hjust = 0),
      plot.subtitle = element_text(color = "grey25"),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(color = "grey15"),
      legend.position = "top",
      legend.title = element_blank(),
      strip.background = element_rect(fill = "grey92", color = "grey65", linewidth = 0.3),
      strip.text = element_text(face = "bold"),
      panel.grid.major.y = element_line(color = "grey90", linewidth = 0.25)
    )
}

save_fig <- function(plot, name, width, height) {
  ggsave(file.path(figure_dir, paste0(name, ".png")), plot, width = width, height = height, dpi = 600, bg = "white")
  ggsave(file.path(figure_dir, paste0(name, ".pdf")), plot, width = width, height = height, device = cairo_pdf, bg = "white")
  ggsave(file.path(figure_dir, paste0(name, ".tiff")), plot, width = width, height = height, dpi = 600, compression = "lzw", bg = "white")
}

write_csv_safe <- function(x, path, append = FALSE) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.table(x, path, sep = ",", row.names = FALSE, col.names = !append, append = append, quote = TRUE, qmethod = "double", na = "")
}

pooled_path <- file.path("data", "final", sprintf("pooled_harmonized_births_%s.parquet", period_suffix))
df <- arrow::read_parquet(pooled_path)

df <- df |>
  mutate(
    singleton_primary = plurality_cat == "singleton",
    risk_score3 = as.integer(age_risk) + as.integer(inadequate_prenatal_care) + as.integer(low_education),
    risk_score3_cat = factor(as.character(risk_score3), levels = c("0", "1", "2", "3")),
    risk_phenotype3 = case_when(
      is.na(risk_score3) ~ NA_character_,
      !age_risk & !inadequate_prenatal_care & !low_education ~ "low_risk",
      age_risk & !inadequate_prenatal_care & !low_education ~ "age_only",
      !age_risk & inadequate_prenatal_care & !low_education ~ "inadequate_care_only",
      !age_risk & !inadequate_prenatal_care & low_education ~ "low_education_only",
      age_risk & inadequate_prenatal_care & !low_education ~ "age_inadequate",
      age_risk & !inadequate_prenatal_care & low_education ~ "age_low_education",
      !age_risk & inadequate_prenatal_care & low_education ~ "education_inadequate",
      age_risk & inadequate_prenatal_care & low_education ~ "all_three",
      TRUE ~ NA_character_
    ),
    risk_phenotype3 = factor(risk_phenotype3, levels = names(phenotype_labels)),
    no_prenatal_care = prenatal_visits == 0
  )

singleton <- df |> filter(singleton_primary)
primary_outcomes <- c("preterm_birth", "low_birth_weight")
secondary_outcomes <- c("very_preterm_birth", "very_low_birth_weight", "low_apgar5", "cesarean_delivery")

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

fit_poisson <- function(data, outcome, exposure, interaction = FALSE) {
  covariates <- c("parity_or_birth_order", "newborn_sex", "birth_year")
  model_df <- complete_model_data(data, outcome, exposure, covariates)
  covariates <- covariates[vapply(covariates, function(v) n_distinct(model_df[[v]], na.rm = TRUE) > 1, logical(1))]
  rhs_main <- paste(c(exposure, covariates[!covariates %in% "birth_year"], if ("birth_year" %in% covariates) "factor(birth_year)" else NULL), collapse = " + ")
  rhs <- if (interaction) sub(exposure, paste0(exposure, " * country"), rhs_main, fixed = TRUE) else rhs_main
  predictors <- c(exposure, if (interaction) "country", covariates)
  if (interaction) {
    pieces <- list(model_df)
  } else {
    pieces <- split(model_df, model_df$country)
  }
  purrr::imap_dfr(pieces, function(piece, nm) {
    agg <- aggregate_model_data(piece, outcome, predictors)
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
        cells = nrow(agg),
        country = if (interaction) "Pooled" else nm
      )
  })
}

fit_models <- function(data, exposure, outcomes) {
  purrr::map_dfr(outcomes, ~ bind_rows(
    fit_poisson(data, .x, exposure, interaction = FALSE),
    fit_poisson(data, .x, exposure, interaction = TRUE)
  ))
}

score_models <- fit_models(singleton, "risk_score3_cat", c(primary_outcomes, secondary_outcomes))
phenotype_models <- fit_models(singleton, "risk_phenotype3", primary_outcomes)

predict_abs <- function(data, outcome, exposure) {
  covariates <- c("parity_or_birth_order", "newborn_sex", "birth_year")
  model_df <- complete_model_data(data, outcome, exposure, covariates)
  reference_level <- levels(model_df[[exposure]])[1]
  purrr::map_dfr(split(model_df, model_df$country), function(piece) {
    agg <- aggregate_model_data(piece, outcome, c(exposure, covariates))
    fit <- glm(
      as.formula(paste0("events ~ ", exposure, " + parity_or_birth_order + newborn_sex + factor(birth_year) + offset(log(total))")),
      data = agg,
      family = poisson(link = "log")
    )
    levels_exp <- levels(piece[[exposure]])
    purrr::map_dfr(levels_exp, function(level_value) {
      newdata <- agg
      newdata[[exposure]] <- factor(level_value, levels = levels_exp)
      pred <- predict(fit, newdata = newdata, type = "response")
      tibble(
        country = unique(piece$country),
        outcome = outcome,
        exposure = level_value,
        adjusted_risk_per_1000 = sum(pred, na.rm = TRUE) / sum(newdata$total, na.rm = TRUE) * 1000
      )
    })
  }) |>
    group_by(country, outcome) |>
    mutate(
      reference_risk = adjusted_risk_per_1000[exposure == reference_level][1],
      risk_difference_per_1000 = adjusted_risk_per_1000 - reference_risk
    ) |>
    ungroup()
}

abs_score <- purrr::map_dfr(primary_outcomes, ~ predict_abs(singleton, .x, "risk_score3_cat"))
abs_pheno <- purrr::map_dfr(primary_outcomes, ~ predict_abs(singleton, .x, "risk_phenotype3"))

burden_fraction <- singleton |>
  filter(!is.na(risk_phenotype3), risk_phenotype3 != "low_risk") |>
  summarise(.by = c(country), exposed_births = n())

excess_burden <- purrr::map_dfr(primary_outcomes, function(outcome) {
  singleton |>
    filter(!is.na(.data[[outcome]]), !is.na(risk_phenotype3)) |>
    group_by(country) |>
    summarise(
      observed_rate = mean(.data[[outcome]], na.rm = TRUE),
      low_risk_rate = mean(.data[[outcome]][risk_phenotype3 == "low_risk"], na.rm = TRUE),
      excess_burden_fraction = (observed_rate - low_risk_rate) / observed_rate,
      .groups = "drop"
    ) |>
    mutate(outcome = outcome)
})

table1_revised <- singleton |>
  summarise(
    .by = country,
    births = n(),
    maternal_age_mean = mean(maternal_age, na.rm = TRUE),
    age_risk_pct = mean(age_risk, na.rm = TRUE) * 100,
    low_education_pct = mean(low_education, na.rm = TRUE) * 100,
    inadequate_prenatal_care_pct = mean(inadequate_prenatal_care, na.rm = TRUE) * 100,
    no_prenatal_care_pct = mean(no_prenatal_care, na.rm = TRUE) * 100,
    male_pct = mean(newborn_sex == "male", na.rm = TRUE) * 100,
    preterm_birth_pct = mean(preterm_birth, na.rm = TRUE) * 100,
    low_birth_weight_pct = mean(low_birth_weight, na.rm = TRUE) * 100,
    very_preterm_birth_pct = mean(very_preterm_birth, na.rm = TRUE) * 100,
    very_low_birth_weight_pct = mean(very_low_birth_weight, na.rm = TRUE) * 100,
    cesarean_delivery_pct = mean(cesarean_delivery, na.rm = TRUE) * 100
  )

table2_revised <- singleton |>
  summarise(
    .by = c(country, birth_year),
    births = n(),
    preterm_birth_rate_per_1000 = mean(preterm_birth, na.rm = TRUE) * 1000,
    low_birth_weight_rate_per_1000 = mean(low_birth_weight, na.rm = TRUE) * 1000,
    very_preterm_birth_rate_per_1000 = mean(very_preterm_birth, na.rm = TRUE) * 1000,
    very_low_birth_weight_rate_per_1000 = mean(very_low_birth_weight, na.rm = TRUE) * 1000,
    cesarean_delivery_rate_per_1000 = mean(cesarean_delivery, na.rm = TRUE) * 1000
  ) |>
  arrange(country, birth_year)

missingness <- singleton |>
  summarise(
    .by = c(country, birth_year),
    births = n(),
    gestational_age_missing_pct = mean(is.na(gestational_age)) * 100,
    birth_weight_missing_pct = mean(is.na(birth_weight)) * 100,
    education_unknown_pct = mean(maternal_education_cat == "unknown" | is.na(maternal_education_cat)) * 100,
    prenatal_care_unknown_pct = mean(prenatal_care_cat == "unknown" | is.na(prenatal_care_cat)) * 100,
    delivery_mode_unknown_pct = mean(delivery_mode == "unknown" | is.na(delivery_mode)) * 100,
    apgar5_missing_pct = mean(is.na(apgar5)) * 100
  ) |>
  arrange(country, birth_year)

flow_counts <- tibble(
  metric = c("all_births", "singleton_births", "preterm_model_n", "lbw_model_n", "cesarean_model_n"),
  n = c(
    nrow(df),
    nrow(singleton),
    sum(complete_model_data(singleton, "preterm_birth", "risk_score3_cat", c("parity_or_birth_order", "newborn_sex", "birth_year")) |> count() |> pull(n)),
    sum(complete_model_data(singleton, "low_birth_weight", "risk_score3_cat", c("parity_or_birth_order", "newborn_sex", "birth_year")) |> count() |> pull(n)),
    sum(complete_model_data(singleton, "cesarean_delivery", "risk_score3_cat", c("parity_or_birth_order", "newborn_sex", "birth_year")) |> count() |> pull(n))
  )
)

write_csv_safe(table1_revised, file.path(table_dir, "table1_singleton_baseline.csv"))
write_csv_safe(table2_revised, file.path(table_dir, "table2_singleton_outcome_rates.csv"))
write_csv_safe(score_models, file.path(table_dir, "table3_singleton_risk_score_models.csv"))
write_csv_safe(phenotype_models, file.path(table_dir, "table4_singleton_phenotype_models.csv"))
write_csv_safe(abs_pheno, file.path(table_dir, "table5_singleton_absolute_risks.csv"))
write_csv_safe(excess_burden, file.path(table_dir, "table_excess_burden_fraction.csv"))
write_csv_safe(missingness, file.path(table_dir, "supplementary_table_missingness.csv"))
write_csv_safe(flow_counts, file.path(table_dir, "flow_counts.csv"))

trend_plot <- table2_revised |>
  select(country, birth_year, preterm_birth_rate_per_1000, low_birth_weight_rate_per_1000, very_preterm_birth_rate_per_1000, very_low_birth_weight_rate_per_1000) |>
  pivot_longer(ends_with("_per_1000"), names_to = "outcome", values_to = "rate") |>
  mutate(
    outcome = recode(
      outcome,
      preterm_birth_rate_per_1000 = "Preterm birth",
      low_birth_weight_rate_per_1000 = "Low birth weight",
      very_preterm_birth_rate_per_1000 = "Very preterm birth",
      very_low_birth_weight_rate_per_1000 = "Very low birth weight"
    ),
    outcome = factor(outcome, levels = c("Preterm birth", "Low birth weight", "Very preterm birth", "Very low birth weight"))
  ) |>
  ggplot(aes(birth_year, rate, color = country, group = country)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  facet_wrap(~ outcome, scales = "free_y", nrow = 2) +
  scale_color_manual(values = palette) +
  scale_x_continuous(breaks = year_breaks) +
  labs(title = "Figure 2. Outcome trends among singleton births", subtitle = paste0("Rates per 1,000 singleton live births, ", period_display), x = "Birth year", y = "Rate per 1,000 singleton live births") +
  theme_pub()
save_fig(trend_plot, "figure2_singleton_outcome_trends", 8.5, 6.2)

forest_plot <- score_models |>
  filter(country %in% names(palette), outcome %in% primary_outcomes, term %in% paste0("risk_score3_cat", c("1", "2", "3"))) |>
  mutate(
    risk_score = factor(score_labels[str_remove(term, "risk_score3_cat")], levels = rev(unname(score_labels))),
    outcome = factor(outcome_labels[outcome], levels = c("Preterm birth", "Low birth weight"))
  ) |>
  ggplot(aes(estimate, risk_score, xmin = conf.low, xmax = conf.high, color = country)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey40") +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), orientation = "y", height = 0.16, position = position_dodge(width = 0.55)) +
  geom_point(position = position_dodge(width = 0.55), size = 2) +
  facet_wrap(~ outcome, nrow = 1) +
  scale_color_manual(values = palette) +
  scale_x_log10(breaks = c(0.75, 1, 1.5, 2, 3, 4), labels = c("0.75", "1", "1.5", "2", "3", "4")) +
  labs(title = "Figure 3. Maternal risk score and primary outcomes among singleton births", subtitle = "Reference group: 0 risk domains; models adjusted for birth year, parity/birth order, and newborn sex", x = "Adjusted risk ratio (log scale)", y = NULL) +
  theme_pub()
save_fig(forest_plot, "figure3_singleton_risk_score_forest", 8.5, 4.4)

abs_plot <- abs_pheno |>
  filter(outcome %in% primary_outcomes) |>
  mutate(
    outcome = factor(outcome_labels[outcome], levels = c("Preterm birth", "Low birth weight")),
    phenotype = factor(phenotype_labels[exposure], levels = rev(unname(phenotype_labels)))
  ) |>
  ggplot(aes(adjusted_risk_per_1000, phenotype, fill = country)) +
  geom_col(position = position_dodge(width = 0.72), width = 0.62) +
  facet_wrap(~ outcome, scales = "free_x", nrow = 1) +
  scale_fill_manual(values = palette) +
  labs(title = "Figure 4. Adjusted absolute risk by singleton maternal risk phenotype", subtitle = "Model-standardized risks per 1,000 singleton live births", x = "Adjusted risk per 1,000 singleton live births", y = NULL) +
  theme_pub(9)
save_fig(abs_plot, "figure4_singleton_absolute_risk", 10, 5.2)

burden_plot <- excess_burden |>
  mutate(outcome = factor(outcome_labels[outcome], levels = c("Preterm birth", "Low birth weight"))) |>
  ggplot(aes(excess_burden_fraction * 100, outcome, fill = country)) +
  geom_col(position = position_dodge(width = 0.72), width = 0.62) +
  scale_fill_manual(values = palette) +
  labs(title = "Supplementary Figure. Risk-stratified excess burden fraction", subtitle = "Non-low-risk singleton phenotypes compared with the low-risk phenotype", x = "Excess burden fraction (%)", y = NULL) +
  theme_pub()
save_fig(burden_plot, "supplementary_figure_excess_burden_fraction", 7, 3.8)

flow_plot <- ggplot() +
  annotate("rect", xmin = 0.06, xmax = 0.94, ymin = 3.5, ymax = 4.15, fill = "white", color = "grey20") +
  annotate("rect", xmin = 0.06, xmax = 0.94, ymin = 2.35, ymax = 3.0, fill = "white", color = "grey20") +
  annotate("rect", xmin = 0.06, xmax = 0.94, ymin = 1.2, ymax = 1.85, fill = "white", color = "grey20") +
  annotate("rect", xmin = 0.06, xmax = 0.94, ymin = 0.05, ymax = 0.7, fill = "white", color = "grey20") +
  annotate("segment", x = 0.5, xend = 0.5, y = 3.5, yend = 3.0, arrow = arrow(length = grid::unit(0.18, "cm"))) +
  annotate("segment", x = 0.5, xend = 0.5, y = 2.35, yend = 1.85, arrow = arrow(length = grid::unit(0.18, "cm"))) +
  annotate("segment", x = 0.5, xend = 0.5, y = 1.2, yend = 0.7, arrow = arrow(length = grid::unit(0.18, "cm"))) +
  annotate("text", x = 0.5, y = 3.825, label = paste0("Public live birth registry records, ", period_display, "\nN = ", fmt_n(flow_counts$n[flow_counts$metric == "all_births"])), size = 3.5, lineheight = 0.95) +
  annotate("text", x = 0.5, y = 2.675, label = paste0("Primary analysis restricted to singleton births\nN = ", fmt_n(flow_counts$n[flow_counts$metric == "singleton_births"])), size = 3.5, lineheight = 0.95) +
  annotate("text", x = 0.5, y = 1.525, label = paste0("Outcome-specific complete records\nPreterm birth: ", fmt_n(flow_counts$n[flow_counts$metric == "preterm_model_n"]), "\nLow birth weight: ", fmt_n(flow_counts$n[flow_counts$metric == "lbw_model_n"])), size = 3.4, lineheight = 0.95) +
  annotate("text", x = 0.5, y = 0.375, label = paste0("Secondary health-system-sensitive outcome\nCesarean delivery model N = ", fmt_n(flow_counts$n[flow_counts$metric == "cesarean_model_n"])), size = 3.4, lineheight = 0.95) +
  coord_cartesian(xlim = c(0, 1), ylim = c(-0.1, 4.3), expand = FALSE) +
  labs(title = "Figure 1. Study population flow") +
  theme_void(base_family = "Helvetica") +
  theme(plot.title = element_text(face = "bold", size = 12, hjust = 0))
save_fig(flow_plot, "figure1_revised_study_flow", 7, 7)

writeLines("Revised singleton-primary analysis completed.", file.path(out_dir, "revision_analysis_complete.txt"))
message("Revised singleton-primary analysis completed.")
