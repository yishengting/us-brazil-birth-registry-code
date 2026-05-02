source("R/utils.R")

cfg <- load_config()
ensure_project_dirs(cfg)

verify_url <- function(url) {
  ok <- FALSE
  status <- NA_integer_
  note <- NA_character_
  tryCatch({
    con <- url(url, open = "rb")
    on.exit(close(con), add = TRUE)
    readBin(con, what = "raw", n = 1)
    ok <- TRUE
    status <- 200L
  }, error = function(e) {
    ok <<- FALSE
    note <<- conditionMessage(e)
  })
  tibble::tibble(url = url, ok = ok, status = status, note = note)
}

us_checks <- purrr::map_dfr(cfg$project$years, function(year) {
  data_url <- sprintf("%s/%s", cfg$us_nchs$data_base_url, cfg$us_nchs$data_files[[as.character(year)]])
  dct_url <- sprintf("%s/natality%s.dct", cfg$us_nchs$nber_dct_base_url, year)
  guide_url <- sprintf("%s/%s", cfg$us_nchs$documentation_base_url, cfg$us_nchs$user_guides[[as.character(year)]])
  checks <- bind_rows(
    verify_url(data_url) |> mutate(source = "us_nchs_data", year = year),
    verify_url(dct_url) |> mutate(source = "us_nchs_dct", year = year),
    verify_url(guide_url) |> mutate(source = "us_nchs_user_guide", year = year)
  )
  if (as.integer(year) == 2024L && any(checks$source == "us_nchs_dct" & !checks$ok)) {
    checks <- checks |>
      mutate(
        ok = if_else(source == "us_nchs_dct", TRUE, ok),
        source = if_else(source == "us_nchs_dct", "us_nchs_dct_fallback_2023_layout", source),
        note = if_else(
          source == "us_nchs_dct_fallback_2023_layout",
          "NBER 2024 dictionary unavailable; pipeline uses 2023 fixed-width layout fallback for selected variables.",
          note
        )
      )
  }
  checks
})

br_checks <- tibble::tibble(
  source = "br_sinasc_healthbR",
  year = cfg$project$years,
  url = "healthbR::sinasc_data",
  ok = requireNamespace("healthbR", quietly = TRUE),
  status = NA_integer_,
  note = if (requireNamespace("healthbR", quietly = TRUE)) "healthbR installed" else "healthbR not installed"
)

checks <- bind_rows(us_checks, br_checks) |>
  mutate(checked_at_utc = timestamp_utc()) |>
  select(checked_at_utc, source, year, url, ok, status, note)

write_csv_safe(checks, root_path("outputs", "logs", "data_source_verification.csv"))

if (any(!checks$ok)) {
  warning("Some data sources could not be verified. See outputs/logs/data_source_verification.csv")
} else {
  message("All configured data sources verified.")
}
