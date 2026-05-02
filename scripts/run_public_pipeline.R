set.seed(20260429)

steps <- c(
  "scripts/00_setup_project.R",
  "scripts/01_verify_data_sources.R",
  "scripts/02_download_us_nchs.R",
  "scripts/03_parse_us_nchs.R",
  "scripts/04_download_clean_br_sinasc.R",
  "scripts/05_harmonize_us_br.R",
  "scripts/16_revised_singleton_analysis.R",
  "scripts/18_revision2_addenda.R",
  "scripts/20_final_submission_enhancements.R",
  "scripts/22_additional_sensitivity.R",
  "scripts/24_s2_reporting_transparency.R",
  "scripts/27_grouped_count_validation.R"
)

for (step in steps) {
  message("\n=== Running ", step, " ===")
  source(step, local = new.env(parent = globalenv()))
}

message("Public reproducibility pipeline complete.")

