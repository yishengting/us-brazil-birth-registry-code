source("R/us_nchs.R")

cfg <- load_config()
ensure_project_dirs(cfg)

paths <- purrr::map_chr(cfg$project$years, function(year) {
  message("Parsing and cleaning US NCHS ", year)
  clean_us_nchs_year(year)
})

write_csv_safe(
  tibble::tibble(year = cfg$project$years, file_path = paths, timestamp_utc = timestamp_utc()),
  root_path("outputs", "logs", "us_clean_files.csv")
)

message("US NCHS yearly clean files complete.")

