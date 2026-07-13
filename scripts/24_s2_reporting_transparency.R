source("R/utils.R")

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(stringr)
})

cfg <- load_config()
period_label <- paste0(min(cfg$project$years), "-", max(cfg$project$years))
period_suffix <- gsub("-", "_", period_label)

sub_dir <- root_path("submission")
tab_main <- file.path(sub_dir, "tables", "main")
tab_supp <- file.path(sub_dir, "tables", "supplementary")
fig_png <- file.path(sub_dir, "figures", "png")
fig_pdf <- file.path(sub_dir, "figures", "pdf")
fig_tiff <- file.path(sub_dir, "figures", "tiff")
out_dir <- root_path("outputs", "s2_reporting_transparency")
dir.create(tab_supp, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_png, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_pdf, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_tiff, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

fmt_n <- function(x) format(round(as.numeric(x)), big.mark = ",", scientific = FALSE, trim = TRUE)
fmt1 <- function(x) sprintf("%.1f", as.numeric(x))
parse_num <- function(x) as.numeric(gsub(",", "", as.character(x), fixed = FALSE))

write_csv_safe <- function(x, path) {
  utils::write.table(x, path, sep = ",", row.names = FALSE, col.names = TRUE, quote = TRUE, qmethod = "double", na = "")
}

df <- arrow::read_parquet(root_path("data", "final", sprintf("pooled_harmonized_births_%s.parquet", period_suffix))) |>
  mutate(
    singleton_primary = plurality_cat == "singleton",
    low_prenatal_visit_count = inadequate_prenatal_care,
    profile_classifiable = !is.na(age_risk) & !is.na(low_education) & !is.na(low_prenatal_visit_count),
    model_covariates_complete = profile_classifiable &
      !is.na(parity_or_birth_order) & as.character(parity_or_birth_order) != "unknown" &
      !is.na(newborn_sex) & as.character(newborn_sex) != "unknown"
  )

singleton <- df |> filter(singleton_primary)

table1 <- singleton |>
  group_by(country) |>
  summarise(
    `Singleton births, n` = n(),
    `Maternal age, mean years` = mean(maternal_age, na.rm = TRUE),
    `Age risk, %` = mean(age_risk, na.rm = TRUE) * 100,
    `Low education, %` = mean(low_education, na.rm = TRUE) * 100,
    `Low prenatal-visit count (<4), %` = mean(low_prenatal_visit_count, na.rm = TRUE) * 100,
    `No prenatal care recorded, %` = mean(prenatal_visits == 0, na.rm = TRUE) * 100,
    `Maternal education unknown, %` = mean(is.na(low_education)) * 100,
    `Prenatal visit count unknown, %` = mean(is.na(low_prenatal_visit_count)) * 100,
    `Gestational age missing/implausible, %` = mean(is.na(gestational_age)) * 100,
    `Birth weight missing/implausible, %` = mean(is.na(birth_weight)) * 100,
    `Male newborn, %` = mean(newborn_sex == "male", na.rm = TRUE) * 100,
    `Preterm birth, %` = mean(preterm_birth, na.rm = TRUE) * 100,
    `Low birth weight, %` = mean(low_birth_weight, na.rm = TRUE) * 100,
    `Very preterm birth, %` = mean(very_preterm_birth, na.rm = TRUE) * 100,
    `Very low birth weight, %` = mean(very_low_birth_weight, na.rm = TRUE) * 100,
    `Caesarean delivery, %` = mean(cesarean_delivery, na.rm = TRUE) * 100,
    .groups = "drop"
  ) |>
  pivot_longer(-country, names_to = "Characteristic", values_to = "value") |>
  mutate(
    value = if_else(
      Characteristic == "Singleton births, n",
      fmt_n(value),
      fmt1(value)
    )
  ) |>
  pivot_wider(names_from = country, values_from = value) |>
  select(Characteristic, Brazil, `United States`)
write_csv_safe(table1, file.path(tab_main, "table1_singleton_baseline.csv"))

step_table_for <- function(data, country_name, outcome, outcome_label) {
  x <- data |> filter(country == country_name)
  start_n <- nrow(x)
  steps <- list(
    list("Singleton live births", rep(TRUE, start_n), "Primary analytic population after excluding multiple births."),
    list("Maternal age domain classifiable", !is.na(x$age_risk), "Maternal age restricted to 10-55 years before domain definition."),
    list("Education domain classifiable", !is.na(x$age_risk) & !is.na(x$low_education), "Unknown education retained as missing for profile-classifiable analyses."),
    list("Prenatal visit-count domain classifiable", !is.na(x$age_risk) & !is.na(x$low_education) & !is.na(x$low_prenatal_visit_count), "Low visit-count marker defined as <4 recorded visits."),
    list("Model covariates complete", x$model_covariates_complete, "Parity or birth order and newborn sex non-missing and not unknown."),
    list(paste0(outcome_label, " model complete case"), x$model_covariates_complete & !is.na(x[[outcome]]), "Outcome-specific complete-case model denominator.")
  )
  retained <- vapply(steps, function(s) sum(s[[2]], na.rm = TRUE), numeric(1))
  excluded <- c(NA_real_, head(retained, -1) - tail(retained, -1))
  tibble::tibble(
    Outcome = outcome_label,
    Country = country_name,
    Step = vapply(steps, `[[`, character(1), 1),
    `Records retained` = fmt_n(retained),
    `Records excluded at step` = if_else(is.na(excluded), "", fmt_n(excluded)),
    `Percent of singleton births retained` = fmt1(retained / start_n * 100),
    Note = vapply(steps, `[[`, character(1), 3)
  )
}

complete_case <- bind_rows(
  step_table_for(singleton, "Brazil", "preterm_birth", "Preterm birth"),
  step_table_for(singleton, "United States", "preterm_birth", "Preterm birth"),
  step_table_for(singleton, "Brazil", "low_birth_weight", "Low birth weight"),
  step_table_for(singleton, "United States", "low_birth_weight", "Low birth weight")
)
write_csv_safe(complete_case, file.path(tab_supp, "Supplementary_Table_12_complete_case_derivation.csv"))
write_csv_safe(complete_case, file.path(out_dir, "Supplementary_Table_12_complete_case_derivation.csv"))

algorithm_appendix <- tibble::tribble(
  ~Domain, ~`Source variables`, ~`Rule used in primary analysis`, ~`Unknown/implausible handling`, ~`Reviewer-facing rationale`,
  "Study population", "Plurality category from each registry", "Primary analysis restricted to singleton live births.", "Multiple births excluded; unknown plurality not included in singleton primary analysis.", "Prevents the dominant multiple-gestation pathway from driving registry risk profiles.",
  "Maternal age domain", "US mager; Brazil IDADEMAE", "Age risk = maternal age <20 or >=35 years.", "Ages outside 10-55 years set to missing.", "Transparent reproductive-age marker available in both systems.",
  "Education domain", "US meduc; Brazil ESCMAE2010", "Low education approximates less than completed high school or closest country-specific equivalent.", "Unknown or ignored categories retained as unknown, not recoded as low risk.", "Interpreted as harmonised registry social-risk marker, not identical socioeconomic position.",
  "Prenatal visit-count domain", "US previs; Brazil CONSULTAS-derived visit category", "Low prenatal-visit count marker = <4 recorded visits.", "Unknown visit counts retained as missing for profile-classifiable analyses.", "Neutral wording avoids interpreting total visit count as care quality; count can be affected by gestational length.",
  "Primary outcomes", "Gestational age; birth weight", "Preterm birth <37 completed weeks; low birth weight <2,500 g.", "Gestational age <22 or >44 weeks and birth weight <300 or >7,000 g set to missing.", "Defines registry endpoints using standard thresholds and transparent plausibility rules.",
  "Model covariates", "Birth year; parity or birth order; newborn sex", "Adjusted for birth year, parity or birth order, and newborn sex.", "Unknown parity/order or sex excluded from adjusted complete-case models.", "Common covariate set available in both national registry systems.",
  "Grouped-count modelling", "Outcome, risk score/profile, country, birth year, parity/order, newborn sex", "Modified Poisson models fit to grouped event counts with log offset for total births in each covariate pattern. Individual-record HC0 covariance was reconstructed from event and non-event score contributions within each pattern.", "Outcome-specific complete records used for each model; direct HC0 calculation on grouped rows was retained only as a validation diagnostic.", "Targets the individual-record coefficient and HC0 covariance without expanding the grouped data; both were checked against individual-record models.",
  "Grouped covariate-pattern counts", "Primary outcome model aggregation tables", "Risk-score primary-outcome models used 576-1,105 country-specific grouped covariate patterns and 1,675-1,681 pooled patterns. Risk-profile primary-outcome models used 1,152-1,983 country-specific patterns and 3,126-3,135 pooled patterns.", "Counts vary by outcome, country, and exposure specification.", "Documents that grouped-count models retained all complete records through exact categorical aggregation rather than subsampling.",
  "Geography", "Brazil UF retained during download; NCHS public-use natality microdata", "No geographic fixed-effect model in the primary revision.", "US public-use files do not include state/county identifiers; restricted-use access would be required for comparable US subnational modelling.", "Explains why Brazil UF alone was not used for an asymmetric cross-national fixed-effect sensitivity."
)
write_csv_safe(algorithm_appendix, file.path(tab_supp, "Supplementary_Table_13_registry_algorithms.csv"))
write_csv_safe(algorithm_appendix, file.path(out_dir, "Supplementary_Table_13_registry_algorithms.csv"))

flow_summary <- singleton |>
  summarise(
    all_births = nrow(df),
    singleton_births = n(),
    multiple_excluded = all_births - singleton_births,
    profile_classifiable = sum(profile_classifiable, na.rm = TRUE),
    profile_missing = singleton_births - profile_classifiable,
    covariate_complete = sum(model_covariates_complete, na.rm = TRUE),
    covariate_missing = profile_classifiable - covariate_complete,
    preterm_model_n = sum(model_covariates_complete & !is.na(preterm_birth), na.rm = TRUE),
    lbw_model_n = sum(model_covariates_complete & !is.na(low_birth_weight), na.rm = TRUE),
    preterm_outcome_missing = covariate_complete - preterm_model_n,
    lbw_outcome_missing = covariate_complete - lbw_model_n
  )

flow_plot <- ggplot() +
  annotate("rect", xmin = 0.08, xmax = 0.92, ymin = 4.55, ymax = 5.20, fill = "#F8FAFC", color = "#334155", linewidth = 0.55) +
  annotate("rect", xmin = 0.08, xmax = 0.92, ymin = 3.45, ymax = 4.10, fill = "#F8FAFC", color = "#334155", linewidth = 0.55) +
  annotate("rect", xmin = 0.08, xmax = 0.92, ymin = 2.35, ymax = 3.00, fill = "#F8FAFC", color = "#334155", linewidth = 0.55) +
  annotate("rect", xmin = 0.08, xmax = 0.92, ymin = 1.25, ymax = 1.90, fill = "#F8FAFC", color = "#334155", linewidth = 0.55) +
  annotate("rect", xmin = 0.08, xmax = 0.44, ymin = 0.15, ymax = 0.80, fill = "#F8FAFC", color = "#334155", linewidth = 0.55) +
  annotate("rect", xmin = 0.56, xmax = 0.92, ymin = 0.15, ymax = 0.80, fill = "#F8FAFC", color = "#334155", linewidth = 0.55) +
  annotate("segment", x = 0.5, xend = 0.5, y = 4.55, yend = 4.10, arrow = arrow(type = "closed", length = grid::unit(0.15, "cm")), color = "#334155", linewidth = 0.5) +
  annotate("segment", x = 0.5, xend = 0.5, y = 3.45, yend = 3.00, arrow = arrow(type = "closed", length = grid::unit(0.15, "cm")), color = "#334155", linewidth = 0.5) +
  annotate("segment", x = 0.5, xend = 0.5, y = 2.35, yend = 1.90, arrow = arrow(type = "closed", length = grid::unit(0.15, "cm")), color = "#334155", linewidth = 0.5) +
  annotate("segment", x = 0.5, xend = 0.26, y = 1.25, yend = 0.80, arrow = arrow(type = "closed", length = grid::unit(0.15, "cm")), color = "#334155", linewidth = 0.5) +
  annotate("segment", x = 0.5, xend = 0.74, y = 1.25, yend = 0.80, arrow = arrow(type = "closed", length = grid::unit(0.15, "cm")), color = "#334155", linewidth = 0.5) +
  annotate("text", x = 0.5, y = 5.02, label = paste0("All public live-birth registry records, ", period_label), fontface = "bold", size = 3.0, color = "#111827") +
  annotate("text", x = 0.5, y = 4.82, label = paste0("N = ", fmt_n(flow_summary$all_births)), size = 2.85, color = "#1F2937") +
  annotate("text", x = 0.5, y = 3.88, label = "Singleton births included in primary analysis", fontface = "bold", size = 3.0, color = "#111827") +
  annotate("text", x = 0.5, y = 3.68, label = paste0("N = ", fmt_n(flow_summary$singleton_births), "; multiple births excluded: ", fmt_n(flow_summary$multiple_excluded)), size = 2.75, color = "#1F2937") +
  annotate("text", x = 0.5, y = 2.78, label = "Risk-profile-classifiable singleton births", fontface = "bold", size = 3.0, color = "#111827") +
  annotate("text", x = 0.5, y = 2.58, label = paste0("N = ", fmt_n(flow_summary$profile_classifiable), "; missing age/education/visit-count domains: ", fmt_n(flow_summary$profile_missing)), size = 2.65, color = "#1F2937") +
  annotate("text", x = 0.5, y = 1.68, label = "Complete common model covariates", fontface = "bold", size = 3.0, color = "#111827") +
  annotate("text", x = 0.5, y = 1.48, label = paste0("N = ", fmt_n(flow_summary$covariate_complete), "; missing parity/order or sex: ", fmt_n(flow_summary$covariate_missing)), size = 2.65, color = "#1F2937") +
  annotate("text", x = 0.26, y = 0.58, label = "Preterm birth model", fontface = "bold", size = 2.9, color = "#111827") +
  annotate("text", x = 0.26, y = 0.38, label = paste0("N = ", fmt_n(flow_summary$preterm_model_n), "\nmissing/implausible GA: ", fmt_n(flow_summary$preterm_outcome_missing)), size = 2.55, lineheight = 0.95, color = "#1F2937") +
  annotate("text", x = 0.74, y = 0.58, label = "Low birth weight model", fontface = "bold", size = 2.9, color = "#111827") +
  annotate("text", x = 0.74, y = 0.38, label = paste0("N = ", fmt_n(flow_summary$lbw_model_n), "\nmissing/implausible weight: ", fmt_n(flow_summary$lbw_outcome_missing)), size = 2.55, lineheight = 0.95, color = "#1F2937") +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 5.35), expand = FALSE) +
  theme_void(base_family = "sans") +
  theme(plot.margin = margin(8, 10, 8, 8))

save_all <- function(plot, name, width, height) {
  ggsave(file.path(fig_png, paste0(name, ".png")), plot, width = width, height = height, dpi = 600, bg = "white")
  ggsave(file.path(fig_pdf, paste0(name, ".pdf")), plot, width = width, height = height, device = cairo_pdf, bg = "white")
  ggsave(file.path(fig_tiff, paste0(name, ".tiff")), plot, width = width, height = height, dpi = 600, compression = "lzw", bg = "white")
  ggsave(file.path(out_dir, paste0(name, ".png")), plot, width = width, height = height, dpi = 600, bg = "white")
}
save_all(flow_plot, "figure1_study_flow", 7.2, 7.4)

message("S2 reporting transparency tables and Figure 1 written.")
