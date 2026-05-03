source("R/utils.R")

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(stringr)
  library(patchwork)
})

submission_root <- Sys.getenv("SUBMISSION_ROOT", unset = "")
sub_dir <- if (nzchar(submission_root)) submission_root else root_path("submission")
fig_png <- file.path(sub_dir, "figures", "png")
fig_pdf <- file.path(sub_dir, "figures", "pdf")
fig_tiff <- file.path(sub_dir, "figures", "tiff")
tab_supp <- file.path(sub_dir, "tables", "supplementary")
tab_ready <- file.path(tab_supp, "publication_ready")
out_dir <- root_path("outputs", "publication_ready_supplementary")
dir.create(fig_png, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_pdf, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_tiff, recursive = TRUE, showWarnings = FALSE)
dir.create(tab_ready, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

country_palette <- c("Brazil" = "#0072B2", "United States" = "#D55E00")
country_shapes <- c("Brazil" = 16, "United States" = 17)
outcome_labels <- c(
  preterm_birth = "Preterm birth",
  low_birth_weight = "Low birth weight",
  term_low_birth_weight = "Term low birth weight"
)

fmt1 <- function(x) sprintf("%.1f", as.numeric(x))
fmt_rr <- function(est, lo, hi) sprintf("%.2f (%.2f-%.2f)", est, lo, hi)
parse_num <- function(x) as.numeric(gsub(",", "", as.character(x), fixed = FALSE))

theme_journal <- function(base_size = 9) {
  theme_classic(base_size = base_size, base_family = "Helvetica") +
    theme(
      axis.title = element_text(face = "bold", color = "#111827"),
      axis.text = element_text(color = "#1F2937"),
      axis.line = element_line(color = "#6B7280", linewidth = 0.25),
      axis.ticks = element_line(color = "#6B7280", linewidth = 0.25),
      legend.position = "top",
      legend.title = element_blank(),
      legend.key.width = grid::unit(0.55, "cm"),
      strip.background = element_rect(fill = "#F3F4F6", color = "#9CA3AF", linewidth = 0.25),
      strip.text = element_text(face = "bold", color = "#111827"),
      panel.grid.major.y = element_line(color = "#E5E7EB", linewidth = 0.25),
      panel.grid.minor = element_blank(),
      plot.margin = margin(8, 12, 8, 8)
    )
}

save_all <- function(plot, name, width, height) {
  png_path <- file.path(fig_png, paste0(name, ".png"))
  pdf_path <- file.path(fig_pdf, paste0(name, ".pdf"))
  tiff_path <- file.path(fig_tiff, paste0(name, ".tiff"))
  out_png <- file.path(out_dir, paste0(name, ".png"))
  out_pdf <- file.path(out_dir, paste0(name, ".pdf"))
  out_tiff <- file.path(out_dir, paste0(name, ".tiff"))
  ggsave(png_path, plot, width = width, height = height, dpi = 600, bg = "white")
  ggsave(pdf_path, plot, width = width, height = height, device = grDevices::cairo_pdf, bg = "white")
  ggsave(tiff_path, plot, width = width, height = height, dpi = 600, compression = "lzw", bg = "white")
  invisible(file.copy(png_path, out_png, overwrite = TRUE))
  invisible(file.copy(pdf_path, out_pdf, overwrite = TRUE))
  invisible(file.copy(tiff_path, out_tiff, overwrite = TRUE))
}

table3 <- read.csv(root_path("outputs", "revision", "tables", "table3_singleton_risk_score_models.csv"), stringsAsFactors = FALSE)
no_care <- read.csv(root_path("outputs", "revision2", "tables", "supplementary_table_no_prenatal_care_sensitivity.csv"), stringsAsFactors = FALSE)
age_education <- read.csv(root_path("outputs", "revision2", "tables", "supplementary_table_age_education_only_sensitivity.csv"), stringsAsFactors = FALSE)
term_lbw <- read.csv(root_path("outputs", "revision2", "tables", "supplementary_table_term_low_birth_weight_models.csv"), stringsAsFactors = FALSE)
profile_prev <- read.csv(file.path(tab_supp, "Supplementary_Table_9_risk_profile_prevalence.csv"), stringsAsFactors = FALSE, check.names = FALSE)

obsolete_figures <- c(
  "supplementary_figure_excess_burden_fraction",
  "supplementary_figure1_excess_burden_fraction",
  "supplementary_figure2_sensitivity_forest",
  "supplementary_figure3_risk_profile_prevalence",
  "supplementary_figure4_outcome_trends"
)
for (dir in c(fig_png, fig_pdf, fig_tiff, out_dir)) {
  ext <- if (basename(dir) %in% c("png", "pdf", "tiff")) basename(dir) else NULL
  if (is.null(ext)) {
    stale <- file.path(dir, paste0(rep(obsolete_figures, each = 3), ".", c("png", "pdf", "tiff")))
  } else {
    stale <- file.path(dir, paste0(obsolete_figures, ".", ext))
  }
  file.remove(stale[file.exists(stale)])
}

sensitivity_data <- bind_rows(
  table3 |>
    filter(country %in% names(country_palette), outcome %in% c("preterm_birth", "low_birth_weight"), term == "risk_score3_cat3") |>
    mutate(analysis = "Main 3-domain score"),
  no_care |>
    filter(country %in% names(country_palette), outcome %in% c("preterm_birth", "low_birth_weight"), term == "risk_score_no_care_cat3") |>
    mutate(analysis = "No prenatal care only"),
  age_education |>
    filter(country %in% names(country_palette), outcome %in% c("preterm_birth", "low_birth_weight"), term == "risk_score_age_education_cat2") |>
    mutate(analysis = "Age + education only"),
  term_lbw |>
    filter(country %in% names(country_palette), outcome == "term_low_birth_weight", term == "risk_score3_cat3") |>
    mutate(analysis = "Main 3-domain score")
) |>
  transmute(
    outcome = outcome_labels[outcome],
    analysis,
    country,
    estimate,
    conf.low,
    conf.high,
    rr_label = fmt_rr(estimate, conf.low, conf.high)
  )
row_map <- tibble::tribble(
  ~outcome, ~analysis, ~y,
  "Preterm birth", "Main 3-domain score", 7.05,
  "Preterm birth", "No prenatal care only", 6.42,
  "Preterm birth", "Age + education only", 5.79,
  "Low birth weight", "Main 3-domain score", 4.45,
  "Low birth weight", "No prenatal care only", 3.82,
  "Low birth weight", "Age + education only", 3.19,
  "Term low birth weight", "Main 3-domain score", 1.85
)
sensitivity_data <- sensitivity_data |>
  left_join(row_map, by = c("outcome", "analysis")) |>
  mutate(y_plot = y + if_else(country == "Brazil", 0.08, -0.08))
sensitivity_text <- row_map |>
  left_join(
    sensitivity_data |>
      select(outcome, analysis, country, rr_label) |>
      pivot_wider(names_from = country, values_from = rr_label),
    by = c("outcome", "analysis")
  )
ylims <- c(1.25, 8.05)
sens_label_panel <- ggplot() +
  annotate("text", x = 0.02, y = 7.42, label = "Preterm birth", hjust = 0, fontface = "bold", size = 3.05, color = "#111827") +
  annotate("text", x = 0.02, y = 4.82, label = "Low birth weight", hjust = 0, fontface = "bold", size = 3.05, color = "#111827") +
  annotate("text", x = 0.02, y = 2.22, label = "Term low birth weight", hjust = 0, fontface = "bold", size = 3.05, color = "#111827") +
  geom_text(data = sensitivity_text, aes(x = 0.07, y = y, label = analysis), hjust = 0, size = 2.75, color = "#1F2937") +
  coord_cartesian(xlim = c(0, 1), ylim = ylims, expand = FALSE) +
  theme_void(base_family = "Helvetica") +
  theme(plot.margin = margin(5, 2, 28, 2))
sens_forest_panel <- ggplot(sensitivity_data, aes(estimate, y_plot, xmin = conf.low, xmax = conf.high, color = country, shape = country)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "#6B7280", linewidth = 0.32) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), orientation = "y", width = 0.08, linewidth = 0.48) +
  geom_point(size = 2.05) +
  scale_color_manual(values = country_palette) +
  scale_shape_manual(values = country_shapes) +
  scale_x_log10(breaks = c(1, 1.5, 2, 3, 4), labels = c("1", "1.5", "2", "3", "4"), limits = c(0.95, 4.05)) +
  coord_cartesian(ylim = ylims, expand = FALSE) +
  labs(x = "Adjusted risk ratio", y = NULL) +
  theme_journal(8.7) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_line(color = "#F3F4F6", linewidth = 0.25),
    legend.position = "top",
    plot.margin = margin(5, 4, 5, 4)
  )
sens_value_panel <- ggplot(sensitivity_text) +
  annotate("text", x = 0.04, y = 7.74, label = "Brazil aRR (95% CI)", hjust = 0, fontface = "bold", size = 2.65, color = "#111827") +
  annotate("text", x = 0.52, y = 7.74, label = "US aRR (95% CI)", hjust = 0, fontface = "bold", size = 2.65, color = "#111827") +
  geom_text(aes(x = 0.04, y = y, label = Brazil), hjust = 0, size = 2.65, color = "#1F2937") +
  geom_text(aes(x = 0.52, y = y, label = `United States`), hjust = 0, size = 2.65, color = "#1F2937") +
  coord_cartesian(xlim = c(0, 1), ylim = ylims, expand = FALSE) +
  theme_void(base_family = "Helvetica") +
  theme(plot.margin = margin(8, 2, 28, 2))
sens_plot <- sens_label_panel + sens_forest_panel + sens_value_panel +
  plot_layout(widths = c(1.0, 1.35, 1.35), guides = "collect") &
  theme(legend.position = "top")
save_all(sens_plot, "supplementary_figure1_sensitivity_forest", 10.5, 5.0)

profile_order <- c(
  "Low risk",
  "Age risk only",
  "Low prenatal-visit count only",
  "Low education only",
  "Age risk + low visit count",
  "Age risk + low education",
  "Low education + low visit count",
  "All three domains"
)
prev_long <- profile_prev |>
  rename(country = Country, profile = `Risk profile`, percent = Percent) |>
  mutate(percent = as.numeric(percent)) |>
  left_join(tibble::tibble(profile = profile_order, y = seq(length(profile_order), 1)), by = "profile")
prev_wide <- prev_long |>
  select(country, profile, percent, y) |>
  pivot_wider(names_from = country, values_from = percent) |>
  mutate(
    diff = Brazil - `United States`,
    diff_label = sprintf("%+.1f", diff),
    low = pmin(Brazil, `United States`),
    high = pmax(Brazil, `United States`)
  )
prev_plot <- ggplot() +
  geom_segment(data = prev_wide, aes(x = low, xend = high, y = y, yend = y), color = "#9CA3AF", linewidth = 0.45) +
  geom_point(data = prev_long, aes(x = percent, y = y, color = country, shape = country), size = 2.15) +
  geom_text(data = prev_wide, aes(x = 72, y = y, label = fmt1(Brazil)), hjust = 0.5, size = 2.65, color = "#1F2937") +
  geom_text(data = prev_wide, aes(x = 79, y = y, label = fmt1(`United States`)), hjust = 0.5, size = 2.65, color = "#1F2937") +
  geom_text(data = prev_wide, aes(x = 86, y = y, label = diff_label), hjust = 0.5, size = 2.65, color = "#1F2937") +
  annotate("text", x = 72, y = 8.65, label = "Brazil %", fontface = "bold", size = 2.65, color = "#111827") +
  annotate("text", x = 79, y = 8.65, label = "US %", fontface = "bold", size = 2.65, color = "#111827") +
  annotate("text", x = 86, y = 8.65, label = "Difference", fontface = "bold", size = 2.65, color = "#111827") +
  annotate("segment", x = 69, xend = 69, y = 0.45, yend = 8.45, color = "#D1D5DB", linewidth = 0.35) +
  scale_color_manual(values = country_palette) +
  scale_shape_manual(values = country_shapes) +
  scale_y_continuous(breaks = seq(length(profile_order), 1), labels = profile_order) +
  scale_x_continuous(breaks = seq(0, 70, 10)) +
  coord_cartesian(xlim = c(0, 90), ylim = c(0.45, 8.75), expand = FALSE, clip = "off") +
  labs(x = "Profile-classifiable births (%)", y = NULL) +
  theme_journal(9) +
  theme(
    panel.grid.major.x = element_line(color = "#F3F4F6", linewidth = 0.25),
    panel.grid.major.y = element_line(color = "#E5E7EB", linewidth = 0.25),
    legend.position = "top",
    plot.margin = margin(8, 10, 8, 8)
  )
save_all(prev_plot, "supplementary_figure2_risk_profile_prevalence", 8.8, 5.6)

table_titles <- c(
  "Supplementary_Table_1_missingness.csv" = "Supplementary Table 1. Missing or unknown values among singleton live births by country and year",
  "Supplementary_Table_2_country_interaction_ratios.csv" = "Supplementary Table 2. Country interaction ratios for risk-score associations",
  "Supplementary_Table_3_variable_harmonization.csv" = "Supplementary Table 3. Cross-national variable harmonisation framework",
  "Supplementary_Table_4_term_low_birth_weight.csv" = "Supplementary Table 4. Risk-score associations with term low birth weight among singleton births",
  "Supplementary_Table_5_age_subtype_models.csv" = "Supplementary Table 5. Age-subtype associations with primary outcomes among singleton births",
  "Supplementary_Table_6_education_harmonization.csv" = "Supplementary Table 6. Detailed education harmonisation mapping across the United States and Brazil",
  "Supplementary_Table_7_no_prenatal_care_sensitivity.csv" = "Supplementary Table 7. Sensitivity analysis using no prenatal care as the visit-count sensitivity domain",
  "Supplementary_Table_8_age_education_only_sensitivity.csv" = "Supplementary Table 8. Sensitivity analysis using age plus education risk-score domains only",
  "Supplementary_Table_9_risk_profile_prevalence.csv" = "Supplementary Table 9. Distribution of singleton registry risk-marker profiles by country",
  "Supplementary_Table_10_cross_national_standardization.csv" = "Supplementary Table 10. Cross-national standardisation of risk-profile-distribution-adjusted outcome rates",
  "Supplementary_Table_11_additional_sensitivity.csv" = "Supplementary Table 11. Additional sensitivity analyses for age-education coupling and temporal heterogeneity",
  "Supplementary_Table_12_complete_case_derivation.csv" = "Supplementary Table 12. Complete-case derivation for primary outcome models among singleton births",
  "Supplementary_Table_13_registry_algorithms.csv" = "Supplementary Table 13. Registry variable algorithms, unknown handling, and grouped-count modelling implementation",
  "Supplementary_Table_14_risk_profile_interaction_p_values.csv" = "Supplementary Table 14. Country interaction P values for singleton maternal risk-profile associations"
)
table_notes <- c(
  "Supplementary_Table_1_missingness.csv" = "Singleton births denotes singleton records in each country-year stratum.",
  "Supplementary_Table_2_country_interaction_ratios.csv" = "Ratio of aRRs represents the multiplicative difference in association strength (United States vs Brazil).",
  "Supplementary_Table_3_variable_harmonization.csv" = "Harmonisation reflects registry-based comparability and may not represent perfect equivalence of source coding across countries.",
  "Supplementary_Table_4_term_low_birth_weight.csv" = "Reference group is 0 risk domains. Models adjusted for birth year, parity or birth order, and newborn sex.",
  "Supplementary_Table_5_age_subtype_models.csv" = "Age subtype categories were modelled with adjustment for birth year, parity or birth order, and newborn sex.",
  "Supplementary_Table_6_education_harmonization.csv" = "Education categories were used as harmonised registry markers. The table provides source-code-level mapping; unknown categories were retained and not recoded as low risk.",
  "Supplementary_Table_7_no_prenatal_care_sensitivity.csv" = "Reference group is 0 risk domains under the no-prenatal-care definition.",
  "Supplementary_Table_8_age_education_only_sensitivity.csv" = "Prenatal visit count was excluded from this score definition to evaluate robustness to gestational-length-related bias.",
  "Supplementary_Table_9_risk_profile_prevalence.csv" = "Percentages were calculated among profile-classifiable singleton births with non-missing maternal age, prenatal visit-count category, and maternal education fields. Denominators therefore differ from the full singleton cohort.",
  "Supplementary_Table_10_cross_national_standardization.csv" = "Descriptive standardisation only. Standardised rate = sum(profile prevalence x profile-specific adjusted risk). This analysis is descriptive and not interpreted causally.",
  "Supplementary_Table_11_additional_sensitivity.csv" = "Age-restricted models used the age-plus-education score among births to mothers aged >=25 years. Period-specific models used the main 3-domain score in 2017-2019, 2020-2021, and 2022-2024. Models excluding 2024 births used the main 3-domain score after removing births from the most recent registry year.",
  "Supplementary_Table_12_complete_case_derivation.csv" = "Steps are sequential within country and outcome. Profile-classifiable records required non-missing maternal age risk, education domain, and prenatal visit-count domain. Model covariates were parity or birth order and newborn sex.",
  "Supplementary_Table_13_registry_algorithms.csv" = "Rules are provided for reproducibility and RECORD-style reporting. Prenatal visit count is interpreted as a registry risk marker and not as a causal care-quality exposure. Grouped covariate-pattern counts document exact categorical aggregation used for modified Poisson models.",
  "Supplementary_Table_14_risk_profile_interaction_p_values.csv" = "P values correspond to the pooled risk profile by country interaction terms shown as ratios of aRRs in Supplementary Table 17. Main-text interpretation emphasises ratio estimates and 95% confidence intervals."
)
column_maps <- list(
  "Supplementary_Table_1_missingness.csv" = c(country = "Country", birth_year = "Birth year", singleton_births = "Singleton births, n", gestational_age_missing_pct = "Gestational age missing, %", birth_weight_missing_pct = "Birth weight missing, %", education_unknown_pct = "Education unknown, %", prenatal_care_unknown_pct = "Prenatal visit count unknown, %", prenatal_visit_count_unknown_pct = "Prenatal visit count unknown, %", delivery_mode_unknown_pct = "Delivery mode unknown, %", apgar5_missing_pct = "Apgar 5 missing, %"),
  "Supplementary_Table_3_variable_harmonization.csv" = c(harmonised_variable = "Harmonised variable", us_source_variable = "US variable", us_coding = "US coding", brazil_source_variable = "Brazil variable", brazil_coding = "Brazil coding", final_harmonised_category = "Harmonised category", notes = "Notes"),
  "Supplementary_Table_6_education_harmonization.csv" = c(harmonised_category = "Category", us_source_variable = "US variable", us_source_coding_detail = "US coding", brazil_source_variable = "Brazil variable", brazil_source_coding_detail = "Brazil coding", final_decision = "Decision"),
  "Supplementary_Table_13_registry_algorithms.csv" = c(Domain = "Domain", `Source variables` = "Source variables", `Rule used in primary analysis` = "Primary rule", `Unknown/implausible handling` = "Unknown/implausible handling", `Reviewer-facing rationale` = "Rationale")
)
pretty_names <- function(nm) {
  generic <- nm |>
    str_replace_all("_", " ") |>
    str_replace_all("pct", "%") |>
    str_replace_all("aRR", "aRR") |>
    str_squish()
  generic <- tools::toTitleCase(generic)
  generic <- str_replace_all(generic, "\\bUs\\b", "US")
  generic <- str_replace_all(generic, "\\bArr\\b", "aRR")
  generic <- str_replace_all(generic, "\\bCi\\b", "CI")
  generic <- str_replace_all(generic, "\\bPct\\b", "%")
  generic
}
standardise_table <- function(path) {
  file_name <- basename(path)
  x <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  names(x) <- if (file_name %in% names(column_maps)) {
    map <- column_maps[[file_name]]
    unname(ifelse(names(x) %in% names(map), map[names(x)], pretty_names(names(x))))
  } else {
    pretty_names(names(x))
  }
  x[] <- lapply(x, function(col) {
    col <- as.character(col)
    col <- str_replace_all(col, "Harmonized", "Harmonised")
    col <- str_replace_all(col, "harmonized", "harmonised")
    col
  })
  x
}
md_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("\\|", "\\\\|", x)
  x <- gsub("\n", " ", x, fixed = TRUE)
  x
}
md_table <- function(x) {
  x <- as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
  header <- paste(md_escape(names(x)), collapse = " | ")
  divider <- paste(rep("---", ncol(x)), collapse = " | ")
  rows <- apply(x, 1, function(row) paste(md_escape(row), collapse = " | "))
  paste(c(paste0("| ", header, " |"), paste0("| ", divider, " |"), paste0("| ", rows, " |")), collapse = "\n")
}
write_table_md <- function(file_name, x) {
  paste0("## ", table_titles[[file_name]], "\n\n", md_table(x), "\n\n", "*Note.* ", table_notes[[file_name]], "\n")
}
supp_files <- file.path(tab_supp, names(table_titles))
table_objects <- stats::setNames(lapply(supp_files, standardise_table), names(table_titles))
combined_md <- paste(
  "# Publication-ready supplementary tables\n",
  paste(vapply(names(table_objects), function(nm) write_table_md(nm, table_objects[[nm]]), character(1)), collapse = "\n\\newpage\n\n"),
  sep = "\n"
)
combined_md_path <- file.path(tab_ready, "supplementary_tables_publication_ready.md")
writeLines(combined_md, combined_md_path, useBytes = TRUE)
for (file_name in names(table_objects)) {
  table_id <- str_extract(file_name, "Supplementary_Table_[0-9]+")
  out_base <- paste0(table_id, "_publication_ready")
  writeLines(write_table_md(file_name, table_objects[[file_name]]), file.path(tab_ready, paste0(out_base, ".md")), useBytes = TRUE)
}
pandoc <- Sys.which("pandoc")
if (nzchar(pandoc)) {
  system2(pandoc, c(combined_md_path, "-o", file.path(tab_ready, "supplementary_tables_publication_ready.docx")))
  for (file_name in names(table_objects)) {
    table_id <- str_extract(file_name, "Supplementary_Table_[0-9]+")
    out_base <- paste0(table_id, "_publication_ready")
    system2(pandoc, c(file.path(tab_ready, paste0(out_base, ".md")), "-o", file.path(tab_ready, paste0(out_base, ".docx"))))
  }
} else {
  warning("Pandoc not found; Markdown tables were written but DOCX tables were not generated.")
}

index_lines <- c(
  "# Supplementary Material Index",
  "",
  "This index is organised for reviewer navigation. Supplementary Tables 1 and 12 document missingness and complete-case derivation; Tables 3, 6, and 13 document harmonisation, unknown handling, and modelling algorithms; Tables 2, 4, 5, 7, 8, 11, 14, 16, and 17 report relative-effect, interaction, and sensitivity models; Tables 9, 10, 15, and 18 report profile prevalence, standardisation, annual rates, and adjusted absolute risks; Table 19 validates grouped-count modelling against individual-record Poisson models.",
  "",
  "## Supplementary Figures",
  "",
  "| Item | File stem | Purpose |",
  "| --- | --- | --- |",
  "| Supplementary Figure 1 | supplementary_figure1_sensitivity_forest | Robustness of largest available risk-score contrasts across alternative exposure definitions and term low birth weight. |",
  "| Supplementary Figure 2 | supplementary_figure2_risk_profile_prevalence | Distribution of profile-classifiable singleton registry risk-marker profiles by country, with percentage-point differences. |",
  "",
  "## Supplementary Tables",
  "",
  "| Item | File | Purpose |",
  "| --- | --- | --- |"
)
table_purposes <- c(
  "Missingness checks by country and year.",
  "Country interaction ratios for risk-score models.",
  "Variable harmonisation framework across registry systems.",
  "Term low birth weight sensitivity analysis.",
  "Maternal age subtype models.",
  "Source-code-level education harmonisation.",
  "No-prenatal-care-only sensitivity analysis.",
  "Age-plus-education-only sensitivity analysis excluding prenatal visit count.",
  "Risk-profile prevalence and denominators.",
  "Cross-national standardisation by risk-profile distribution.",
  "Additional age-restricted, period-specific, and analyses excluding 2024 births.",
  "Complete-case derivation for primary outcome models.",
  "Registry algorithms, unknown handling, and grouped-count modelling implementation.",
  "Risk-profile country-interaction P values."
)
index_lines <- c(
  index_lines,
  sprintf(
    "| Supplementary Table %s | %s | %s |",
    seq_along(names(table_titles)),
    names(table_titles),
    table_purposes
  ),
  "",
  "Editable publication-ready supplementary tables are provided in `tables/supplementary/publication_ready/` as one combined DOCX file and as individual DOCX/Markdown files."
)
writeLines(index_lines, file.path(tab_supp, "SUPPLEMENTARY_MATERIAL_INDEX.md"), useBytes = TRUE)
writeLines(index_lines, file.path(tab_ready, "SUPPLEMENTARY_MATERIAL_INDEX.md"), useBytes = TRUE)

message("Supplementary figures, publication-ready tables, and index upgraded.")
