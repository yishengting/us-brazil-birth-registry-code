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

rev_root <- root_path("outputs", "revision2")
table_dir <- file.path(rev_root, "tables")
figure_dir <- file.path(rev_root, "figures")
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
cfg <- load_config()
period_label <- paste0(min(cfg$project$years), "-", max(cfg$project$years))
period_suffix <- gsub("-", "_", period_label)

write_csv_safe <- function(x, path, append = FALSE) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.table(x, path, sep = ",", row.names = FALSE, col.names = !append, append = append, quote = TRUE, qmethod = "double", na = "")
}

fmt_rr <- function(e, l, h) sprintf("%.2f (%.2f–%.2f)", e, l, h)
palette <- c("Brazil" = "#0072B2", "United States" = "#D55E00")
outcome_labels <- c(
  preterm_birth = "Preterm birth",
  low_birth_weight = "Low birth weight",
  term_low_birth_weight = "Term low birth weight",
  very_preterm_birth = "Very preterm birth",
  very_low_birth_weight = "Very low birth weight"
)
phenotype_labels <- c(
  low_risk = "Low risk",
  age_only = "Age risk only",
  inadequate_care_only = "Low prenatal-visit count only",
  low_education_only = "Low education only",
  age_inadequate = "Age risk + low visit count",
  age_low_education = "Age risk + low education",
  education_inadequate = "Low education + low visit count",
  all_three = "All three domains"
)

theme_pub <- function(base_size = 10) {
  theme_classic(base_size = base_size, base_family = "Helvetica") +
    theme(
      plot.title = element_text(face = "bold", hjust = 0),
      plot.subtitle = element_text(color = "grey25"),
      axis.title = element_text(face = "bold"),
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
  pieces <- if (interaction) list(Pooled = model_df) else split(model_df, model_df$country)
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
        country = nm
      )
  })
}

standardized_abs_ci <- function(data, outcome, exposure) {
  covariates <- c("parity_or_birth_order", "newborn_sex", "birth_year")
  model_df <- complete_model_data(data, outcome, exposure, covariates)
  ref <- levels(model_df[[exposure]])[1]
  purrr::map_dfr(split(model_df, model_df$country), function(piece) {
    agg <- aggregate_model_data(piece, outcome, c(exposure, covariates))
    fit <- glm(
      as.formula(paste0("events ~ ", exposure, " + parity_or_birth_order + newborn_sex + factor(birth_year) + offset(log(total))")),
      data = agg,
      family = poisson(link = "log")
    )
    vc <- sandwich::vcovHC(fit, type = "HC0")
    levs <- levels(piece[[exposure]])
    risks <- purrr::map_dfr(levs, function(level_value) {
      newdata <- agg
      newdata[[exposure]] <- factor(level_value, levels = levs)
      mm <- model.matrix(delete.response(terms(fit)), newdata)
      pred <- as.numeric(exp(mm %*% coef(fit) + log(newdata$total)))
      total <- sum(newdata$total)
      risk <- sum(pred) / total * 1000
      grad <- colSums(mm * pred) / total * 1000
      se <- sqrt(as.numeric(t(grad) %*% vc %*% grad))
      tibble(
        country = unique(piece$country),
        outcome = outcome,
        exposure = level_value,
        adjusted_risk_per_1000 = risk,
        risk_ci_low = risk - 1.96 * se,
        risk_ci_high = risk + 1.96 * se
      )
    })
    ref_row <- risks |> filter(exposure == ref)
    risks |>
      mutate(
        reference_risk = ref_row$adjusted_risk_per_1000[1],
        risk_difference_per_1000 = adjusted_risk_per_1000 - reference_risk,
        risk_difference_ci_low = risk_ci_low - ref_row$risk_ci_high[1],
        risk_difference_ci_high = risk_ci_high - ref_row$risk_ci_low[1]
      )
  })
}

df <- arrow::read_parquet(file.path("data", "final", sprintf("pooled_harmonized_births_%s.parquet", period_suffix))) |>
  mutate(
    singleton_primary = plurality_cat == "singleton",
    risk_score3 = as.integer(age_risk) + as.integer(inadequate_prenatal_care) + as.integer(low_education),
    risk_score3_cat = factor(as.character(risk_score3), levels = c("0", "1", "2", "3")),
    risk_score_no_care = as.integer(age_risk) + as.integer(no_prenatal_care <- prenatal_visits == 0) + as.integer(low_education),
    risk_score_no_care_cat = factor(as.character(risk_score_no_care), levels = c("0", "1", "2", "3")),
    risk_score_age_education = as.integer(age_risk) + as.integer(low_education),
    risk_score_age_education_cat = factor(as.character(risk_score_age_education), levels = c("0", "1", "2")),
    teenage_mother = maternal_age < 20,
    advanced_maternal_age = maternal_age >= 35,
    very_advanced_maternal_age = maternal_age >= 40,
    age_subtype = case_when(
      maternal_age < 20 ~ "teenage_mother",
      maternal_age >= 40 ~ "very_advanced_maternal_age",
      maternal_age >= 35 ~ "advanced_maternal_age_35_39",
      maternal_age >= 20 & maternal_age < 35 ~ "age_20_34",
      TRUE ~ NA_character_
    ),
    age_subtype = factor(age_subtype, levels = c("age_20_34", "teenage_mother", "advanced_maternal_age_35_39", "very_advanced_maternal_age")),
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
    risk_phenotype3 = factor(risk_phenotype3, levels = names(phenotype_labels))
  )

singleton <- df |> filter(singleton_primary)
primary <- c("preterm_birth", "low_birth_weight")

term_lbw_models <- fit_poisson(singleton, "term_low_birth_weight", "risk_score3_cat", interaction = FALSE)
no_care_models <- purrr::map_dfr(primary, ~ fit_poisson(singleton, .x, "risk_score_no_care_cat", interaction = FALSE))
age_education_models <- purrr::map_dfr(primary, ~ fit_poisson(singleton, .x, "risk_score_age_education_cat", interaction = FALSE))
age_subtype_models <- purrr::map_dfr(primary, ~ fit_poisson(singleton, .x, "age_subtype", interaction = FALSE))

score_models <- read.csv(root_path("outputs", "revision", "tables", "table3_singleton_risk_score_models.csv"), stringsAsFactors = FALSE)
interaction_table <- score_models |>
  filter(country == "Pooled", outcome %in% primary, str_detect(term, ":countryUnited States")) |>
  transmute(
    outcome,
    risk_score = str_replace(str_remove(term, ":countryUnited States"), "risk_score3_cat", ""),
    ratio_of_aRR_US_vs_Brazil = estimate,
    conf.low,
    conf.high,
    p_interaction = p.value
  )

abs_ci <- purrr::map_dfr(primary, ~ standardized_abs_ci(singleton, .x, "risk_phenotype3"))

education_harmonization <- tibble::tribble(
  ~harmonized_category, ~us_source_variable, ~us_source_coding, ~brazil_source_variable, ~brazil_source_coding, ~final_decision,
  "Low education", "meduc", "Codes 1-2: 8th grade or less; 9th-12th grade with no diploma", "ESCMAE2010", "Codes 0-2: no schooling; fundamental I; fundamental II", "Used as registry marker approximating less than completed high school or country-specific secondary schooling",
  "Middle education", "meduc", "Codes 3-6: high school/GED; some college; associate degree", "ESCMAE2010", "Codes 3-4: secondary education; incomplete higher education", "Used as intermediate education category; exact equivalence may differ across countries",
  "High education", "meduc", "Codes 7-8: bachelor's degree; master's/professional/doctorate degree", "ESCMAE2010", "Code 5: completed higher education", "Used as highest education category",
  "Unknown", "meduc", "Not stated or non-reporting", "ESCMAE/ESCMAE2010", "Ignored, unknown, or missing", "Retained as unknown; not recoded as low risk"
)

prenatal_harmonization <- tibble::tribble(
  ~harmonized_variable, ~us_source_variable, ~us_definition, ~brazil_source_variable, ~brazil_definition, ~interpretation,
  "Low recorded prenatal-visit count (<4 visits)", "previs", "Total number of prenatal visits", "CONSULTAS", "Categorical prenatal visit count", "Main registry marker; may be affected by gestational length",
  "No prenatal care", "previs", "0 visits", "CONSULTAS", "No prenatal visits category", "Sensitivity marker less affected by shortened gestation than visit-count thresholds",
  "Strict low visit-count marker (<7 visits)", "previs", "Total visits <7", "CONSULTAS", "Categories below 7 visits", "Sensitivity threshold"
)

write_csv_safe(term_lbw_models, file.path(table_dir, "supplementary_table_term_low_birth_weight_models.csv"))
write_csv_safe(no_care_models, file.path(table_dir, "supplementary_table_no_prenatal_care_sensitivity.csv"))
write_csv_safe(age_education_models, file.path(table_dir, "supplementary_table_age_education_only_sensitivity.csv"))
write_csv_safe(age_subtype_models, file.path(table_dir, "supplementary_table_age_subtype_models.csv"))
write_csv_safe(interaction_table, file.path(table_dir, "table3_country_interaction_ratios.csv"))
write_csv_safe(abs_ci, file.path(table_dir, "table5_absolute_risks_with_ci.csv"))
write_csv_safe(education_harmonization, file.path(table_dir, "supplementary_table_education_harmonization.csv"))
write_csv_safe(prenatal_harmonization, file.path(table_dir, "supplementary_table_prenatal_care_harmonization.csv"))

abs_plot <- abs_ci |>
  filter(outcome %in% primary) |>
  mutate(
    outcome = factor(outcome_labels[outcome], levels = c("Preterm birth", "Low birth weight")),
    phenotype = factor(phenotype_labels[exposure], levels = rev(unname(phenotype_labels)))
  ) |>
  ggplot(aes(adjusted_risk_per_1000, phenotype, color = country)) +
  geom_errorbar(aes(xmin = risk_ci_low, xmax = risk_ci_high), orientation = "y", height = 0.16, position = position_dodge(width = 0.55), linewidth = 0.45) +
  geom_point(position = position_dodge(width = 0.55), size = 2) +
  facet_wrap(~ outcome, scales = "free_x", nrow = 1) +
  scale_color_manual(values = palette) +
  labs(
    title = "Figure 4. Adjusted absolute risk by singleton registry risk-marker profile",
    subtitle = "Model-standardized risks per 1,000 singleton live births; error bars show delta-method 95% CIs",
    x = "Adjusted risk per 1,000 singleton live births",
    y = NULL
  ) +
  theme_pub(9)
save_fig(abs_plot, "figure4_singleton_absolute_risk_with_ci", 10, 5.2)

flow <- read.csv(root_path("outputs", "revision", "tables", "flow_counts.csv"), stringsAsFactors = FALSE)
country_all <- read.csv(root_path("outputs", "logs", "final_birth_counts_by_country_year.csv"), stringsAsFactors = FALSE) |>
  group_by(country) |>
  summarise(all_births = sum(births, na.rm = TRUE), .groups = "drop")
country_singleton <- read.csv(root_path("outputs", "revision", "tables", "table1_singleton_baseline.csv"), stringsAsFactors = FALSE) |>
  transmute(country, singleton_births = as.numeric(births))
country_flow <- country_all |>
  left_join(country_singleton, by = "country") |>
  mutate(excluded_multiple = all_births - singleton_births)

all_n <- flow$n[flow$metric == "all_births"]
singleton_n <- flow$n[flow$metric == "singleton_births"]
multiple_n <- all_n - singleton_n
preterm_n <- flow$n[flow$metric == "preterm_model_n"]
lbw_n <- flow$n[flow$metric == "lbw_model_n"]
cesarean_n <- flow$n[flow$metric == "cesarean_model_n"]
us_all_n <- country_flow$all_births[country_flow$country == "United States"]
br_all_n <- country_flow$all_births[country_flow$country == "Brazil"]
us_singleton_n <- country_flow$singleton_births[country_flow$country == "United States"]
br_singleton_n <- country_flow$singleton_births[country_flow$country == "Brazil"]
us_multiple_n <- country_flow$excluded_multiple[country_flow$country == "United States"]
br_multiple_n <- country_flow$excluded_multiple[country_flow$country == "Brazil"]
flow_plot <- ggplot() +
  annotate("rect", xmin = 0.07, xmax = 0.93, ymin = 3.95, ymax = 4.68, fill = "#F8FAFC", color = "#334155", linewidth = 0.55) +
  annotate("rect", xmin = 0.07, xmax = 0.93, ymin = 2.25, ymax = 2.98, fill = "#F8FAFC", color = "#334155", linewidth = 0.55) +
  annotate("rect", xmin = 0.07, xmax = 0.93, ymin = 0.55, ymax = 1.45, fill = "#F8FAFC", color = "#334155", linewidth = 0.55) +
  annotate("segment", x = 0.5, xend = 0.5, y = 3.95, yend = 2.98, linewidth = 0.55, color = "#334155", arrow = arrow(type = "closed", length = grid::unit(0.17, "cm"))) +
  annotate("segment", x = 0.5, xend = 0.5, y = 2.25, yend = 1.45, linewidth = 0.55, color = "#334155", arrow = arrow(type = "closed", length = grid::unit(0.17, "cm"))) +
  annotate("rect", xmin = 0.62, xmax = 0.95, ymin = 3.18, ymax = 3.88, fill = "#EEF2F7", color = "#64748B", linewidth = 0.45) +
  annotate("segment", x = 0.5, xend = 0.62, y = 3.53, yend = 3.53, linewidth = 0.45, color = "#64748B") +
  annotate("text", x = 0.5, y = 4.50, label = "All eligible live-birth registry records", size = 3.2, fontface = "bold", color = "#111827") +
  annotate("text", x = 0.5, y = 4.30, label = period_label, size = 2.9, color = "#1F2937") +
  annotate("text", x = 0.5, y = 4.12, label = paste0("N = ", format(all_n, big.mark = ","), " (United States: ", format(us_all_n, big.mark = ","), "; Brazil: ", format(br_all_n, big.mark = ","), ")"), size = 2.85, color = "#1F2937") +
  annotate("text", x = 0.78, y = 3.69, label = "Excluded from primary analysis", size = 2.7, fontface = "bold", color = "#1F2937") +
  annotate("text", x = 0.78, y = 3.52, label = paste0("Multiple births: n = ", format(multiple_n, big.mark = ",")), size = 2.62, color = "#1F2937") +
  annotate("text", x = 0.78, y = 3.35, label = paste0("(United States: ", format(us_multiple_n, big.mark = ","), "; Brazil: ", format(br_multiple_n, big.mark = ","), ")"), size = 2.52, color = "#1F2937") +
  annotate("text", x = 0.5, y = 2.70, label = "Singleton births included in primary analysis", size = 3.15, fontface = "bold", color = "#111827") +
  annotate("text", x = 0.5, y = 2.50, label = paste0("N = ", format(singleton_n, big.mark = ","), " (United States: ", format(us_singleton_n, big.mark = ","), "; Brazil: ", format(br_singleton_n, big.mark = ","), ")"), size = 2.85, color = "#1F2937") +
  annotate("text", x = 0.5, y = 1.30, label = "Outcome-specific complete-case records used in adjusted models", size = 3.05, fontface = "bold", color = "#111827") +
  annotate("text", x = 0.5, y = 1.08, label = paste0("Preterm birth: ", format(preterm_n, big.mark = ","), "   |   Low birth weight: ", format(lbw_n, big.mark = ",")), size = 2.72, color = "#1F2937") +
  annotate("text", x = 0.5, y = 0.89, label = paste0("Cesarean delivery: ", format(cesarean_n, big.mark = ",")), size = 2.72, color = "#1F2937") +
  coord_cartesian(xlim = c(0, 1), ylim = c(0.35, 4.85), expand = FALSE) +
  labs(title = "Figure 1. Study population flow") +
  theme_void(base_family = "Helvetica") +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0, color = "#111827"),
    plot.margin = margin(8, 10, 8, 8)
  )
save_fig(flow_plot, "figure1_revised2_study_flow", 7, 7)

message("Revision 2 addenda completed.")
