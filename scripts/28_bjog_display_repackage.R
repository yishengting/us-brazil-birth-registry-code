source("R/utils.R")

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
})

sub_dir <- root_path("submission")
tab_main <- file.path(sub_dir, "tables", "main")
tab_supp <- file.path(sub_dir, "tables", "supplementary")
main_ready <- file.path(tab_main, "publication_ready")
supp_ready <- file.path(tab_supp, "publication_ready")
fig_dirs <- file.path(sub_dir, "figures", c("png", "pdf", "tiff"))
dir.create(main_ready, recursive = TRUE, showWarnings = FALSE)
dir.create(supp_ready, recursive = TRUE, showWarnings = FALSE)

copy_if_exists <- function(from, to) {
  if (file.exists(from)) {
    file.copy(from, to, overwrite = TRUE)
  }
}
move_main_table_to_supp <- function(main_file, supp_file) {
  main_path <- file.path(tab_main, main_file)
  supp_path <- file.path(tab_supp, supp_file)
  if (file.exists(main_path)) {
    file.copy(main_path, supp_path, overwrite = TRUE)
    file.remove(main_path)
  } else if (!file.exists(supp_path)) {
    stop("Neither main nor supplementary source table exists: ", main_file)
  }
}

move_main_table_to_supp("table2_singleton_outcome_rates.csv", "Supplementary_Table_15_annual_outcome_rates.csv")
move_main_table_to_supp("table3_singleton_risk_score_models.csv", "Supplementary_Table_16_risk_score_models.csv")
move_main_table_to_supp("table4_singleton_risk_profile_models.csv", "Supplementary_Table_17_risk_profile_models.csv")
move_main_table_to_supp("table5_absolute_risks_with_ci.csv", "Supplementary_Table_18_absolute_risks_with_ci.csv")
legacy_main_tables <- file.path(tab_main, "table4_singleton_phenotype_models.csv")
file.remove(legacy_main_tables[file.exists(legacy_main_tables)])

for (dir in fig_dirs) {
  ext <- basename(dir)
  copy_if_exists(file.path(dir, paste0("figure2_outcome_trends.", ext)), file.path(dir, paste0("supplementary_figure3_outcome_trends.", ext)))
  copy_if_exists(file.path(dir, paste0("figure3_risk_score_forest.", ext)), file.path(dir, paste0("figure2_risk_score_forest.", ext)))
  copy_if_exists(file.path(dir, paste0("figure4_absolute_risk_with_ci.", ext)), file.path(dir, paste0("figure3_absolute_risk_with_ci.", ext)))
  stale_figures <- file.path(
    dir,
    paste0(
      c("figure2_outcome_trends", "figure3_risk_score_forest", "figure4_absolute_risk_with_ci", "supplementary_figure4_outcome_trends"),
      ".",
      ext
    )
  )
  file.remove(stale_figures[file.exists(stale_figures)])
}

legacy_supp_figures <- unlist(lapply(fig_dirs, function(dir) {
  ext <- basename(dir)
  file.path(
    dir,
    paste0(
      c(
        "supplementary_figure2_sensitivity_forest",
        "supplementary_figure3_phenotype_prevalence",
        "supplementary_figure3_risk_profile_prevalence",
        "supplementary_figure_excess_burden_fraction"
      ),
      ".",
      ext
    )
  )
}))
file.remove(legacy_supp_figures[file.exists(legacy_supp_figures)])

journal_upload <- file.path(sub_dir, "figures", "journal_upload")
dir.create(journal_upload, recursive = TRUE, showWarnings = FALSE)
journal_figure_map <- c(
  "Figure_1.tiff" = "figure1_study_flow.tiff",
  "Figure_2.tiff" = "figure2_risk_score_forest.tiff",
  "Figure_3.tiff" = "figure3_absolute_risk_with_ci.tiff",
  "Supplementary_Figure_1.tiff" = "supplementary_figure1_sensitivity_forest.tiff",
  "Supplementary_Figure_2.tiff" = "supplementary_figure2_risk_profile_prevalence.tiff",
  "Supplementary_Figure_3.tiff" = "supplementary_figure3_outcome_trends.tiff"
)
journal_copy_status <- vapply(names(journal_figure_map), function(destination_name) {
  source_path <- file.path(sub_dir, "figures", "tiff", journal_figure_map[[destination_name]])
  if (!file.exists(source_path)) stop("Missing journal figure source: ", source_path, call. = FALSE)
  isTRUE(file.copy(source_path, file.path(journal_upload, destination_name), overwrite = TRUE))
}, logical(1))
if (!all(journal_copy_status)) stop("One or more journal-upload TIFF copies failed.", call. = FALSE)

legacy_supp_tables <- list.files(
  tab_supp,
  pattern = "^supplementary_table_.*[.]csv$",
  full.names = TRUE
)
file.remove(legacy_supp_tables[file.exists(legacy_supp_tables)])

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
write_table_md <- function(title, data, note, abbreviations = "") {
  footnotes <- paste0("*Note.* ", note, "\n")
  if (nzchar(abbreviations)) {
    footnotes <- paste0(footnotes, "\n*Abbreviations.* ", abbreviations, "\n")
  }
  paste0("## ", title, "\n\n", md_table(data), "\n\n", footnotes)
}

main_title <- "Table 1. Baseline characteristics of singleton births by country"
main_note <- "Values are percentages unless otherwise indicated and are calculated within country among singleton live births from 2017 to 2024. Maternal age risk was defined as age <20 or ≥35 years; low education was defined as less than completed high school or the closest country-specific equivalent; low recorded prenatal-visit count was defined as <4 visits. Missing or implausible gestational-age and birthweight values used the plausibility rules described in Methods. Caesarean delivery is shown as a contextual service-delivery indicator."
main_abbreviations <- "US, United States."
main_table <- read.csv(file.path(tab_main, "table1_singleton_baseline.csv"), stringsAsFactors = FALSE, check.names = FALSE)
main_md <- paste0("# Publication-ready main table\n\n", write_table_md(main_title, main_table, main_note, main_abbreviations))
writeLines(main_md, file.path(main_ready, "main_tables_publication_ready.md"), useBytes = TRUE)
writeLines(write_table_md(main_title, main_table, main_note, main_abbreviations), file.path(main_ready, "table1_publication_ready.md"), useBytes = TRUE)
stale_main_ready <- file.path(
  main_ready,
  c(
    "table2_publication_ready.md", "table2_publication_ready.docx",
    "table3_publication_ready.md", "table3_publication_ready.docx",
    "table4_publication_ready.md", "table4_publication_ready.docx",
    "table5_publication_ready.md", "table5_publication_ready.docx"
  )
)
file.remove(stale_main_ready[file.exists(stale_main_ready)])

pandoc <- Sys.which("pandoc")
if (nzchar(pandoc)) {
  run_pandoc_docx <- function(input, output) {
    status <- system2(pandoc, c(shQuote(input), "-o", shQuote(output)))
    if (!identical(status, 0L)) stop("Pandoc failed for ", input, call. = FALSE)
  }
  run_pandoc_docx(
    file.path(main_ready, "main_tables_publication_ready.md"),
    file.path(main_ready, "main_tables_publication_ready.docx")
  )
  run_pandoc_docx(
    file.path(main_ready, "table1_publication_ready.md"),
    file.path(main_ready, "table1_publication_ready.docx")
  )
}

supp_titles <- c(
  "1" = "Table S1. Missing or unknown values among singleton live births by country and year",
  "2" = "Table S2. Country interaction ratios for risk-score associations",
  "3" = "Table S3. Cross-national variable harmonisation framework",
  "4" = "Table S4. Risk-score associations with term low birth weight among singleton births",
  "5" = "Table S5. Age-subtype associations with primary outcomes among singleton births",
  "6" = "Table S6. Detailed education harmonisation mapping across the United States and Brazil",
  "7" = "Table S7. Sensitivity analysis using no prenatal care as the visit-count sensitivity domain",
  "8" = "Table S8. Sensitivity analysis using age plus education risk-score domains only",
  "9" = "Table S9. Distribution of singleton registry risk-marker profiles by country",
  "10" = "Table S10. Cross-national standardisation of risk-profile-distribution-adjusted outcome rates",
  "11" = "Table S11. Additional sensitivity analyses for age-education coupling and temporal heterogeneity",
  "12" = "Table S12. Complete-case derivation for primary outcome models among singleton births",
  "13" = "Table S13. Registry variable algorithms, unknown handling, and grouped-count modelling implementation",
  "14" = "Table S14. Country interaction P values for singleton maternal risk-profile associations",
  "15" = "Table S15. Annual outcome rates among singleton births by country",
  "16" = "Table S16. Association between registry risk-marker score and primary outcomes among singleton births",
  "17" = "Table S17. Association between singleton registry risk-marker profile and primary outcomes",
  "18" = "Table S18. Adjusted absolute risks and risk differences by singleton registry risk-marker profile",
  "19" = "Table S19. Grouped-count Poisson coefficient and reconstructed-HC0 validation against individual-record Poisson models"
)
supp_notes <- c(
  "1" = "Singleton births denotes singleton records in each country-year stratum.",
  "2" = "Ratio of aRRs represents the multiplicative difference in association strength (United States vs Brazil) from pooled modified Poisson models adjusted for birth year, parity or birth order, and newborn sex.",
  "3" = "Harmonisation reflects registry-based comparability and may not represent perfect equivalence of source coding across countries.",
  "4" = "Reference group is 0 risk domains. Models adjusted for birth year, parity or birth order, and newborn sex.",
  "5" = "Age subtype categories were modelled using modified Poisson regression with adjustment for birth year, parity or birth order, and newborn sex.",
  "6" = "Education categories were used as harmonised registry markers. The table provides source-code-level mapping; unknown categories were retained and not recoded as low risk.",
  "7" = "Reference group is 0 risk domains under the no-prenatal-care definition. Models adjusted for birth year, parity or birth order, and newborn sex.",
  "8" = "Prenatal visit count was excluded from this score definition to evaluate robustness to gestational-length-related bias. Models adjusted for birth year, parity or birth order, and newborn sex.",
  "9" = "Percentages were calculated among profile-classifiable singleton births with non-missing maternal age, prenatal visit-count category, and maternal education fields. Denominators therefore differ from the full singleton cohort.",
  "10" = "Descriptive standardisation only. Standardised rate = sum(profile prevalence x profile-specific adjusted risk). This analysis is descriptive and not interpreted causally.",
  "11" = "Age-restricted models used the age-plus-education score among births to mothers aged ≥25 years. Period-specific models used the main 3-domain score in 2017-2019, 2020-2021, and 2022-2024. Models excluding 2024 births used the main 3-domain score after removing births from the most recent registry year. All models adjusted for birth year where applicable, parity or birth order, and newborn sex.",
  "12" = "Steps are sequential within country and outcome. Profile-classifiable records required non-missing maternal age risk, education domain, and prenatal visit-count domain. Model covariates were parity or birth order and newborn sex.",
  "13" = "Rules are provided for reproducibility and RECORD-style reporting. Prenatal visit count is interpreted as a registry risk marker and not as a causal care-quality exposure. Grouped covariate-pattern counts document exact categorical aggregation used for modified Poisson models.",
  "14" = "P values correspond to pooled risk profile by country interaction terms from models adjusted for birth year, parity or birth order, and newborn sex. Main-text interpretation emphasises ratio estimates and 95% confidence intervals.",
  "15" = "Rates are per 1,000 singleton live births. Caesarean delivery is summarised as a contextual service-delivery indicator rather than as a direct analogue of preterm birth or low birth weight.",
  "16" = "Reference group is 0 risk domains. Domains were maternal age risk, low recorded prenatal-visit count (<4 visits), and low maternal education. Models adjusted for birth year, parity or birth order, and newborn sex. Ratio of aRRs compares the United States with Brazil using pooled interaction models. Interaction P values are provided in Table S2.",
  "17" = "Reference group is the low-risk profile. Visit-count profiles refer to low recorded prenatal-visit count (<4 visits), not a validated measure of care quality. Models adjusted for birth year, parity or birth order, and newborn sex. Ratio of aRRs compares the United States with Brazil using pooled interaction terms. Interaction P values are provided in Table S14.",
  "18" = "Adjusted risks and risk differences are reported per 1,000 singleton live births with delta-method 95% confidence intervals. Estimates were standardised to the observed covariate distribution within each country and use outcome-specific complete-case analytic populations.",
  "19" = "Validation used a country-year stratified singleton subsample and compared identical model specifications fitted to individual records and grouped covariate-pattern counts. Matching point estimates and reconstructed individual-record HC0 standard errors support the grouped-count implementation used for full-data models. The naive grouped-pattern HC0 standard error is shown only as a diagnostic and was not used for inference."
)
supp_abbreviations <- c(
  "1" = "Apgar, Appearance, Pulse, Grimace, Activity, and Respiration; n, number.",
  "2" = "aRR, adjusted risk ratio; CI, confidence interval; US, United States.",
  "3" = "GED, General Educational Development; ICD, International Classification of Diseases; LBW, low birth weight; US, United States.",
  "4" = "aRR, adjusted risk ratio; CI, confidence interval; n, number.",
  "5" = "aRR, adjusted risk ratio; CI, confidence interval; n, number.",
  "6" = "GED, General Educational Development; US, United States.",
  "7" = "aRR, adjusted risk ratio; CI, confidence interval; n, number.",
  "8" = "aRR, adjusted risk ratio; CI, confidence interval; n, number.",
  "9" = "n, number.",
  "10" = "US, United States.",
  "11" = "aRR, adjusted risk ratio; CI, confidence interval; n, number.",
  "12" = "n, number.",
  "13" = "NCHS, National Center for Health Statistics; RECORD, REporting of studies Conducted using Observational Routinely-collected health Data; UF, unidade federativa; US, United States.",
  "14" = "aRR, adjusted risk ratio; CI, confidence interval; US, United States.",
  "15" = "n, number.",
  "16" = "aRR, adjusted risk ratio; CI, confidence interval; US, United States.",
  "17" = "aRR, adjusted risk ratio; CI, confidence interval; US, United States.",
  "18" = "CI, confidence interval.",
  "19" = "aRR, adjusted risk ratio; HC0, heteroskedasticity-consistent type 0; n, number; SE, standard error."
)
pretty_names <- function(nm) {
  generic <- nm |>
    str_replace_all("_", " ") |>
    str_squish()
  generic <- tools::toTitleCase(generic)
  generic <- str_replace_all(generic, "\\bUs\\b", "US")
  generic <- str_replace_all(generic, "\\bArr\\b", "aRR")
  generic <- str_replace_all(generic, "\\bCi\\b", "CI")
  generic
}
supp_column_names <- c(
  country = "Country",
  birth_year = "Birth year",
  singleton_births = "Singleton births, n",
  gestational_age_missing_pct = "Gestational age missing, %",
  birth_weight_missing_pct = "Birth weight missing, %",
  education_unknown_pct = "Education unknown, %",
  prenatal_care_unknown_pct = "Prenatal visit count unknown, %",
  prenatal_visit_count_unknown_pct = "Prenatal visit count unknown, %",
  delivery_mode_unknown_pct = "Delivery mode unknown, %",
  apgar5_missing_pct = "Apgar 5 missing, %"
)
standardise_names <- function(x) {
  original_names <- names(x)
  names(x) <- pretty_names(original_names)
  mapped <- original_names %in% names(supp_column_names)
  names(x)[mapped] <- unname(supp_column_names[original_names[mapped]])
  names(x) <- str_replace_all(names(x), "Harmonization", "Harmonisation")
  names(x) <- str_replace_all(names(x), "P Value", "P value")
  names(x) <- str_replace_all(names(x), "P For Interaction", "P for interaction")
  x[] <- lapply(x, function(col) {
    col <- str_replace_all(as.character(col), "Harmonized|harmonized", "harmonised")
    str_replace_all(col, ">=", "≥")
  })
  x
}
supp_files <- list.files(tab_supp, pattern = "^Supplementary_Table_[0-9]+.*[.]csv$", full.names = TRUE)
supp_numbers <- as.integer(str_match(basename(supp_files), "^Supplementary_Table_([0-9]+)")[, 2])
supp_files <- supp_files[order(supp_numbers)]
supp_numbers <- supp_numbers[order(supp_numbers)]

combined_parts <- lapply(seq_along(supp_files), function(i) {
  num <- as.character(supp_numbers[[i]])
  x <- standardise_names(read.csv(supp_files[[i]], stringsAsFactors = FALSE, check.names = FALSE))
  out_base <- paste0("Supplementary_Table_", num, "_publication_ready")
  abbreviations <- if (num %in% names(supp_abbreviations)) supp_abbreviations[[num]] else ""
  table_md <- write_table_md(supp_titles[[num]], x, supp_notes[[num]], abbreviations)
  writeLines(table_md, file.path(supp_ready, paste0(out_base, ".md")), useBytes = TRUE)
  if (nzchar(pandoc)) {
    run_pandoc_docx(
      file.path(supp_ready, paste0(out_base, ".md")),
      file.path(supp_ready, paste0(out_base, ".docx"))
    )
  }
  table_md
})
nav_note <- "This file is organised for reviewer navigation. Tables S1 and S12 document missingness and complete-case derivation; Tables S3, S6, and S13 document harmonisation, unknown handling, and modelling algorithms; Tables S2, S4, S5, S7, S8, S11, S14, S16, and S17 report relative-effect, interaction, and sensitivity models; Tables S9, S10, S15, and S18 report profile prevalence, standardisation, annual rates, and adjusted absolute risks; Table S19 validates grouped-count coefficients and reconstructed individual-record HC0 standard errors against individual-record Poisson models."
combined_supp_md <- paste0("# Publication-ready supporting tables\n\n", nav_note, "\n\n", paste(combined_parts, collapse = "\n\\newpage\n\n"))
writeLines(combined_supp_md, file.path(supp_ready, "supplementary_tables_publication_ready.md"), useBytes = TRUE)
if (nzchar(pandoc)) {
  run_pandoc_docx(
    file.path(supp_ready, "supplementary_tables_publication_ready.md"),
    file.path(supp_ready, "supplementary_tables_publication_ready.docx")
  )
}

figure_index <- c(
  "| Figure S1 | supplementary_figure1_sensitivity_forest | Robustness of largest available risk-score contrasts across alternative exposure definitions and term low birth weight. |",
  "| Figure S2 | supplementary_figure2_risk_profile_prevalence | Distribution of profile-classifiable singleton registry risk-marker profiles by country, with percentage-point differences. |",
  "| Figure S3 | supplementary_figure3_outcome_trends | Annual trends in primary and severe adverse outcomes among singleton births. |"
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
  "Risk-profile country-interaction P values.",
  "Annual singleton outcome rates by country.",
  "Risk-score model estimates supporting main Figure 2.",
  "Risk-profile model estimates supporting profile interpretation.",
  "Adjusted absolute risks and risk differences supporting main Figure 3.",
  "Grouped-count Poisson coefficient and reconstructed-HC0 validation against individual-record Poisson models."
)
index_lines <- c(
  "# Supporting Information Index",
  "",
  nav_note,
  "",
  "## Supporting figures",
  "",
  "| Item | File stem | Purpose |",
  "| --- | --- | --- |",
  figure_index,
  "",
  "## Supporting tables",
  "",
  "| Item | File | Purpose |",
  "| --- | --- | --- |",
  sprintf(
    "| Table S%s | %s | %s |",
    supp_numbers,
    basename(supp_files),
    table_purposes[supp_numbers]
  ),
  "",
  "Editable publication-ready supporting tables are provided in `tables/supplementary/publication_ready/` as one combined DOCX file and as individual DOCX/Markdown files."
)
writeLines(index_lines, file.path(tab_supp, "SUPPLEMENTARY_MATERIAL_INDEX.md"), useBytes = TRUE)
writeLines(index_lines, file.path(supp_ready, "SUPPLEMENTARY_MATERIAL_INDEX.md"), useBytes = TRUE)

message("BJOG display repackage complete.")
