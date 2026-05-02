source("R/utils.R")

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(stringr)
})

submission_root <- Sys.getenv("SUBMISSION_ROOT", unset = "")
sub_dir <- if (nzchar(submission_root)) submission_root else root_path("submission")
cfg <- load_config()
period_label <- paste0(min(cfg$project$years), "-", max(cfg$project$years))
period_suffix <- gsub("-", "_", period_label)
fig_png <- file.path(sub_dir, "figures", "png")
fig_pdf <- file.path(sub_dir, "figures", "pdf")
fig_tiff <- file.path(sub_dir, "figures", "tiff")
tab_main <- file.path(sub_dir, "tables", "main")
tab_supp <- file.path(sub_dir, "tables", "supplementary")
dir.create(fig_png, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_pdf, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_tiff, recursive = TRUE, showWarnings = FALSE)
dir.create(tab_supp, recursive = TRUE, showWarnings = FALSE)

write_csv_safe <- function(x, path) {
  utils::write.table(x, path, sep = ",", row.names = FALSE, col.names = TRUE, quote = TRUE, qmethod = "double", na = "")
}
fmt_rr <- function(e, l, h) sprintf("%.2f (%.2f–%.2f)", as.numeric(e), as.numeric(l), as.numeric(h))
fmt1 <- function(x) sprintf("%.1f", as.numeric(x))

palette <- c("Brazil" = "#0072B2", "United States" = "#D55E00")
theme_pub <- function(base_size = 10) {
  theme_classic(base_size = base_size, base_family = "sans") +
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
save_all <- function(plot, name, width, height) {
  tmp_dir <- file.path(tempdir(), "submission_figure_write")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  tmp_png <- file.path(tmp_dir, paste0(name, ".png"))
  tmp_pdf <- file.path(tmp_dir, paste0(name, ".pdf"))
  tmp_tiff <- file.path(tmp_dir, paste0(name, ".tiff"))
  ggsave(tmp_png, plot, width = width, height = height, dpi = 600, bg = "white")
  ggsave(tmp_pdf, plot, width = width, height = height, device = cairo_pdf, bg = "white")
  ggsave(tmp_tiff, plot, width = width, height = height, dpi = 600, compression = "lzw", bg = "white")
  file.copy(tmp_png, file.path(fig_png, basename(tmp_png)), overwrite = TRUE)
  file.copy(tmp_pdf, file.path(fig_pdf, basename(tmp_pdf)), overwrite = TRUE)
  file.copy(tmp_tiff, file.path(fig_tiff, basename(tmp_tiff)), overwrite = TRUE)
}

table3 <- read.csv(file.path(tab_main, "table3_singleton_risk_score_models.csv"), stringsAsFactors = FALSE)
read_first_csv <- function(...) {
  paths <- c(...)
  existing <- paths[file.exists(paths)]
  if (!length(existing)) {
    stop("None of these files exist: ", paste(paths, collapse = ", "), call. = FALSE)
  }
  read.csv(existing[[1]], stringsAsFactors = FALSE)
}
no_care <- read_first_csv(
  file.path(tab_supp, "supplementary_table_no_prenatal_care_sensitivity.csv"),
  file.path(tab_supp, "Supplementary_Table_7_no_prenatal_care_sensitivity.csv")
)
age_education <- read_first_csv(
  file.path(tab_supp, "supplementary_table_age_education_only_sensitivity.csv"),
  file.path(tab_supp, "Supplementary_Table_8_age_education_only_sensitivity.csv")
)
term_lbw <- read_first_csv(
  file.path(tab_supp, "supplementary_table_term_low_birth_weight_models.csv"),
  file.path(tab_supp, "Supplementary_Table_4_term_low_birth_weight.csv")
)
abs_risk <- read.csv(file.path(tab_main, "table5_absolute_risks_with_ci.csv"), stringsAsFactors = FALSE)

sensitivity_forest_data <- bind_rows(
  table3 |>
    filter(country %in% names(palette), outcome %in% c("preterm_birth", "low_birth_weight"), term == "risk_score3_cat3") |>
    mutate(analysis = "Main 3-domain score", display_outcome = recode(outcome, preterm_birth = "Preterm birth", low_birth_weight = "Low birth weight")),
  no_care |>
    filter(country %in% names(palette), outcome %in% c("preterm_birth", "low_birth_weight"), term == "risk_score_no_care_cat3") |>
    mutate(analysis = "No prenatal care only", display_outcome = recode(outcome, preterm_birth = "Preterm birth", low_birth_weight = "Low birth weight")),
  age_education |>
    filter(country %in% names(palette), outcome %in% c("preterm_birth", "low_birth_weight"), term == "risk_score_age_education_cat2") |>
    mutate(analysis = "Age + education only", display_outcome = recode(outcome, preterm_birth = "Preterm birth", low_birth_weight = "Low birth weight")),
  term_lbw |>
    filter(country %in% names(palette), outcome == "term_low_birth_weight", term == "risk_score3_cat3") |>
    mutate(analysis = "Main 3-domain score", display_outcome = "Term low birth weight")
) |>
  transmute(
    analysis,
    country,
    outcome = display_outcome,
    estimate,
    conf.low,
    conf.high,
    aRR_95_CI = fmt_rr(estimate, conf.low, conf.high)
  )
write_csv_safe(sensitivity_forest_data, file.path(tab_supp, "supplementary_table_sensitivity_forest_data.csv"))

sens_plot <- sensitivity_forest_data |>
  mutate(
    analysis = factor(analysis, levels = rev(c("Main 3-domain score", "No prenatal care only", "Age + education only"))),
    outcome = factor(outcome, levels = c("Preterm birth", "Low birth weight", "Term low birth weight"))
  ) |>
  ggplot(aes(estimate, analysis, xmin = conf.low, xmax = conf.high, color = country)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey45") +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), orientation = "y", height = 0.16, position = position_dodge(width = 0.55)) +
  geom_point(position = position_dodge(width = 0.55), size = 2) +
  facet_wrap(~ outcome, scales = "free_x", nrow = 1) +
  scale_color_manual(values = palette) +
  scale_x_log10() +
  labs(
    title = "Supplementary Figure 2. Robustness of risk-score associations",
    subtitle = "Highest available risk category versus no risk domains under alternative exposure definitions",
    x = "Adjusted risk ratio, log scale",
    y = NULL
  ) +
  theme_pub(9)
save_all(sens_plot, "supplementary_figure2_sensitivity_forest", 10.5, 4.8)

df <- arrow::read_parquet(file.path("data", "final", sprintf("pooled_harmonized_births_%s.parquet", period_suffix))) |>
  mutate(
    singleton_primary = plurality_cat == "singleton",
    risk_score3 = as.integer(age_risk) + as.integer(inadequate_prenatal_care) + as.integer(low_education),
    risk_phenotype3 = case_when(
      is.na(risk_score3) ~ NA_character_,
      !age_risk & !inadequate_prenatal_care & !low_education ~ "Low risk",
      age_risk & !inadequate_prenatal_care & !low_education ~ "Age risk only",
      !age_risk & inadequate_prenatal_care & !low_education ~ "Inadequate prenatal care only",
      !age_risk & !inadequate_prenatal_care & low_education ~ "Low education only",
      age_risk & inadequate_prenatal_care & !low_education ~ "Age risk + inadequate care",
      age_risk & !inadequate_prenatal_care & low_education ~ "Age risk + low education",
      !age_risk & inadequate_prenatal_care & low_education ~ "Low education + inadequate care",
      age_risk & inadequate_prenatal_care & low_education ~ "All three domains",
      TRUE ~ NA_character_
    )
  ) |>
  filter(singleton_primary, !is.na(risk_phenotype3))

phenotype_levels <- c(
  "Low risk",
  "Age risk only",
  "Inadequate prenatal care only",
  "Low education only",
  "Age risk + inadequate care",
  "Age risk + low education",
  "Low education + inadequate care",
  "All three domains"
)
phenotype_key <- tibble::tibble(
  exposure = c(
    "low_risk",
    "age_only",
    "inadequate_care_only",
    "low_education_only",
    "age_inadequate",
    "age_low_education",
    "education_inadequate",
    "all_three"
  ),
  risk_phenotype3 = phenotype_levels
)
phenotype_palette <- c(
  "Low risk" = "#009E73",
  "Age risk only" = "#0072B2",
  "Inadequate prenatal care only" = "#E69F00",
  "Low education only" = "#D55E00",
  "Age risk + inadequate care" = "#56B4E9",
  "Age risk + low education" = "#CC79A7",
  "Low education + inadequate care" = "#999933",
  "All three domains" = "#666666"
)
phenotype_prev <- df |>
  count(country, risk_phenotype3, name = "births") |>
  group_by(country) |>
  mutate(total_births = sum(births), percent = births / total_births * 100) |>
  ungroup() |>
  mutate(risk_phenotype3 = factor(risk_phenotype3, levels = phenotype_levels))
write_csv_safe(phenotype_prev, file.path(tab_supp, "supplementary_table_phenotype_prevalence.csv"))

prev_plot <- phenotype_prev |>
  ggplot(aes(country, percent, fill = risk_phenotype3)) +
  geom_col(width = 0.58, color = "white", linewidth = 0.2) +
  scale_fill_manual(values = phenotype_palette, name = "Phenotype", drop = FALSE) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 100, 20)) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(
    title = "Supplementary Figure 3. Distribution of singleton maternal risk phenotypes",
    subtitle = "Percent distribution by country",
    x = NULL,
    y = "Percent of singleton live births"
  ) +
  theme_pub(9) +
  theme(
    legend.position = "right",
    legend.key.height = grid::unit(0.5, "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#E5E7EB", linewidth = 0.3),
    axis.line.x = element_line(color = "#9CA3AF", linewidth = 0.25),
    axis.line.y = element_line(color = "#9CA3AF", linewidth = 0.25)
  )
save_all(prev_plot, "supplementary_figure3_phenotype_prevalence", 8, 5)

standardization <- abs_risk |>
  filter(outcome %in% c("preterm_birth", "low_birth_weight")) |>
  select(country, outcome, exposure, adjusted_risk_per_1000) |>
  left_join(phenotype_key, by = "exposure") |>
  left_join(
    phenotype_prev |> select(country, risk_phenotype3, percent),
    by = c("country", "risk_phenotype3")
  )
observed <- standardization |>
  group_by(country, outcome) |>
  summarise(observed_rate_per_1000 = sum(adjusted_risk_per_1000 * percent / 100, na.rm = TRUE), .groups = "drop")
br_dist <- phenotype_prev |> filter(country == "Brazil") |> select(risk_phenotype3, br_percent = percent)
us_dist <- phenotype_prev |> filter(country == "United States") |> select(risk_phenotype3, us_percent = percent)
std_table <- abs_risk |>
  filter(outcome %in% c("preterm_birth", "low_birth_weight")) |>
  select(country, outcome, exposure, adjusted_risk_per_1000) |>
  left_join(phenotype_key, by = "exposure") |>
  left_join(br_dist, by = "risk_phenotype3") |>
  left_join(us_dist, by = "risk_phenotype3") |>
  group_by(country, outcome) |>
  summarise(
    rate_standardized_to_brazil_distribution = sum(adjusted_risk_per_1000 * br_percent / 100, na.rm = TRUE),
    rate_standardized_to_us_distribution = sum(adjusted_risk_per_1000 * us_percent / 100, na.rm = TRUE),
    .groups = "drop"
  ) |>
  left_join(observed, by = c("country", "outcome")) |>
  transmute(
    country,
    outcome,
    observed_rate_per_1000 = fmt1(observed_rate_per_1000),
    rate_standardized_to_brazil_distribution = fmt1(rate_standardized_to_brazil_distribution),
    rate_standardized_to_us_distribution = fmt1(rate_standardized_to_us_distribution)
  )
write_csv_safe(std_table, file.path(tab_supp, "supplementary_table_cross_national_standardization.csv"))

harmonization_full <- tibble::tribble(
  ~harmonized_variable, ~us_source_variable, ~us_coding, ~brazil_source_variable, ~brazil_coding, ~final_harmonized_category, ~notes,
  "Maternal age", "mager", "Single years of age", "IDADEMAE", "Single years of age", "<20, 20–34, 35–39, ≥40; age risk <20 or ≥35", "Used consistently across countries",
  "Maternal education", "meduc", "Codes 1–2 low, 3–6 middle, 7–8 high, 9 unknown", "ESCMAE2010", "Codes 0–2 low, 3–4 middle, 5 high, 9 unknown", "low/middle/high/unknown", "Harmonized as registry marker; exact schooling equivalence may differ",
  "Prenatal visits", "previs", "Total number of visits", "CONSULTAS", "Categorical visits", "<4 visits main marker; no care and <7 visits sensitivity", "Total visits may be affected by gestational length",
  "Gestational age", "oegest_comb", "Obstetric estimate in weeks", "SEMAGESTAC", "Gestational age in weeks", "<37, <32, term ≥37", "Categorical fallback used only where week variable unavailable",
  "Birth weight", "dbwt", "Edited grams", "PESO", "Grams", "<2500 g, <1500 g, term LBW", "Implausible values set missing",
  "Parity/birth order", "lbo_rec", "Live birth order recode", "QTDFILVIVO", "Previous live births", "model covariate", "Not perfectly equivalent but captures parity/birth order",
  "Newborn sex", "sex", "M/F", "SEXO", "1/2 categories", "male/female/unknown", "Model covariate",
  "Cesarean delivery", "dmeth_rec", "Delivery method recode", "PARTO", "Delivery mode", "cesarean/vaginal/unknown", "Secondary health-system-sensitive outcome",
  "Low 5-minute Apgar", "apgar5", "0–10 score", "APGAR5", "0–10 score", "<7", "Secondary outcome",
  "Congenital anomaly", "ca_* indicators", "Certificate anomaly indicators", "CODANOMAL", "ICD anomaly code", "exploratory only", "Ascertainment differs substantially across countries"
)
write_csv_safe(harmonization_full, file.path(tab_supp, "supplementary_table_full_variable_harmonization.csv"))

message("Final submission enhancements completed.")
