set.seed(20260429)

required_packages <- c(
  "arrow", "archive", "broom", "data.table", "digest", "dplyr", "fs",
  "ggplot2", "healthbR", "lmtest", "marginaleffects", "purrr", "readr",
  "rmarkdown", "sandwich", "scales", "stringr", "tibble", "tidyr", "yaml"
)

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_packages) > 0) {
  message("Missing packages: ", paste(missing_packages, collapse = ", "))
  message("Install with: install.packages(c(", paste(sprintf("'%s'", missing_packages), collapse = ", "), "))")
}

source("R/utils.R")
ensure_project_dirs()

writeLines(
  c(
    paste0("setup_timestamp_utc: ", timestamp_utc()),
    paste0("r_version: ", R.version.string),
    paste0("missing_packages: ", paste(missing_packages, collapse = ", "))
  ),
  con = root_path("outputs", "logs", "setup_environment.txt")
)

message("Project directories verified.")

