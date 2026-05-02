source("R/br_sinasc.R")

cfg <- load_config()
ensure_project_dirs(cfg)

if (!requireNamespace("healthbR", quietly = TRUE)) {
  stop("healthbR is not installed. Install it with install.packages('healthbR') before running Brazil SINASC download.", call. = FALSE)
}

jobs <- tidyr::crossing(
  year = cfg$project$years,
  uf = cfg$brazil_sinasc$states
)

paths <- purrr::pmap_chr(jobs, function(year, uf) {
  message("Downloading and cleaning Brazil SINASC ", year, " ", uf)
  download_clean_br_sinasc_year_state(year, uf)
})

write_csv_safe(
  dplyr::bind_cols(jobs, tibble::tibble(file_path = paths, timestamp_utc = timestamp_utc())),
  root_path("outputs", "logs", "br_clean_files.csv")
)

message("Brazil SINASC yearly-state clean files complete.")

