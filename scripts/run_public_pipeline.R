set.seed(20260429)

rscript <- file.path(R.home("bin"), "Rscript")

run_r_steps <- function(steps) {
  for (step in steps) {
    message("\n=== Running ", step, " in a fresh R process ===")
    status <- system2(rscript, step, env = "R_MAX_VSIZE=40Gb")
    if (!identical(status, 0L)) {
      stop("Public pipeline failed at ", step, " with status ", status, call. = FALSE)
    }
  }
}

run_python <- function(args, label = args[[1]]) {
  message("\n=== Running ", label, " ===")
  status <- system2("python3", args)
  if (!identical(status, 0L)) {
    stop("Public pipeline failed at ", label, " with status ", status, call. = FALSE)
  }
}

run_r_steps(c(
  "scripts/00_setup_project.R",
  "scripts/01_verify_data_sources.R",
  "scripts/02_download_us_nchs.R",
  "scripts/03_parse_us_nchs.R",
  "scripts/04_download_clean_br_sinasc.R",
  "scripts/05_harmonize_us_br.R"
))

run_python(c(
  "scripts/34_verify_restored_data.py",
  "--manifest", "outputs/logs/raw_file_manifest.csv",
  "--data-root", "data",
  "--report-json", "outputs/logs/restored_data_hash_audit_20260713.json"
), "raw-file integrity audit")
run_python(c(
  "scripts/35_audit_analysis_data.py",
  "--data-root", "data/final",
  "--report-json", "outputs/logs/analysis_data_audit_20260713.json",
  "--country-year-csv", "outputs/logs/final_birth_counts_by_country_year.csv"
), "analysis-data reconciliation")
run_python(c(
  "scripts/36_verify_us_2024_layout.py",
  "--dictionary", "data/raw/us_nchs/2024/natality2024.dct",
  "--user-guide", "data/raw/us_nchs/2024/UserGuide2024.pdf",
  "--report-json", "outputs/logs/us_2024_layout_audit_20260713.json"
), "US 2024 layout audit")

run_r_steps(c(
  "scripts/16_revised_singleton_analysis.R",
  "scripts/18_revision2_addenda.R",
  "scripts/20_final_submission_enhancements.R",
  "scripts/22_additional_sensitivity.R",
  "scripts/24_s2_reporting_transparency.R",
  "scripts/27_grouped_count_validation.R"
))

run_python("scripts/41_restore_submission_table_inputs.py")
run_python("scripts/21_revision7_table_format.py")

run_r_steps(c(
  "scripts/25_upgrade_submission_figures_tables.R",
  "scripts/26_upgrade_supplementary_figures_tables.R",
  "scripts/28_bjog_display_repackage.R"
))

run_python("scripts/33_format_submission_tables.py")
run_python("scripts/43_prepare_submission_provenance.py")

message("Public reproducibility pipeline complete.")
