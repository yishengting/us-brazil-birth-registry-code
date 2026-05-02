required_packages <- c(
  "archive",
  "arrow",
  "broom",
  "digest",
  "dplyr",
  "fs",
  "ggplot2",
  "healthbR",
  "lmtest",
  "marginaleffects",
  "patchwork",
  "purrr",
  "readr",
  "rmarkdown",
  "sandwich",
  "scales",
  "stringr",
  "tibble",
  "tidyr",
  "yaml"
)

missing_packages <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

message("Package check complete.")

