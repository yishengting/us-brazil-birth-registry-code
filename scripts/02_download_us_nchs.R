source("R/us_nchs.R")

cfg <- load_config()
ensure_project_dirs(cfg)

purrr::walk(cfg$project$years, function(year) {
  message("Downloading/verifying US NCHS files for ", year)
  download_us_user_guide(year, cfg)
  download_us_dictionary(year, cfg)
  download_us_data_zip(year, cfg)
})

message("US NCHS raw downloads complete.")

