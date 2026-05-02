suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tibble)
  library(archive)
  library(arrow)
})

source("R/utils.R")

us_selected_vars <- c(
  "dob_yy", "mager", "meduc", "previs", "previs_rec", "dplural",
  "lbo_rec", "oegest_comb", "combgest", "dbwt", "dmeth_rec", "rdmeth_rec",
  "sex", "apgar5", "ca_anen", "ca_mnsb", "ca_cchd", "ca_cdh", "ca_omph",
  "ca_gast", "ca_limb", "ca_cleft", "ca_clpal", "ca_down", "ca_disor",
  "ca_hypo", "no_congen"
)

parse_nber_dct <- function(dct_path) {
  lines <- readLines(dct_path, warn = FALSE)
  pattern <- "^_column\\((\\d+)\\s*\\)\\s+\\S+\\s+(\\S+)\\s+%([0-9]+)([sfg])"
  matches <- stringr::str_match(lines, pattern)
  out <- tibble::tibble(
    start = suppressWarnings(as.integer(matches[, 2])),
    name = matches[, 3],
    width = suppressWarnings(as.integer(matches[, 4])),
    type = matches[, 5]
  ) |>
    filter(!is.na(start), !is.na(name)) |>
    arrange(start) |>
    distinct(name, .keep_all = TRUE) |>
    mutate(end = start + width - 1L)
  out
}

download_us_dictionary <- function(year, cfg = load_config()) {
  url <- sprintf("%s/natality%s.dct", cfg$us_nchs$nber_dct_base_url, year)
  dest <- root_path("data", "raw", "us_nchs", as.character(year), sprintf("natality%s.dct", year))
  fallback_note <- "NBER natality2024.dct unavailable; copied 2023 fixed-width layout for selected variables and archived 2024 NCHS UserGuide separately."
  fallback <- root_path("data", "raw", "us_nchs", "2023", "natality2023.dct")
  if (as.integer(year) == 2024L && file.exists(dest) && file.exists(fallback) && identical(sha256_file(dest), sha256_file(fallback))) {
    write_manifest_row(
      root_path("outputs", "logs", "raw_file_manifest.csv"),
      "us_nchs_dct_fallback_2023_layout",
      year,
      url,
      dest,
      note = fallback_note
    )
    return(dest)
  }
  tryCatch(
    {
      download_if_needed(url, dest, overwrite = FALSE, timeout_sec = 600)
      write_manifest_row(root_path("outputs", "logs", "raw_file_manifest.csv"), "us_nchs_dct", year, url, dest)
    },
    error = function(e) {
      if (as.integer(year) != 2024L) {
        stop(e)
      }
      if (!file.exists(fallback)) {
        stop("NBER 2024 dictionary unavailable and 2023 fallback dictionary was not found.", call. = FALSE)
      }
      dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
      file.copy(fallback, dest, overwrite = TRUE)
      write_manifest_row(
        root_path("outputs", "logs", "raw_file_manifest.csv"),
        "us_nchs_dct_fallback_2023_layout",
        year,
        url,
        dest,
        note = fallback_note
      )
    }
  )
  dest
}

download_us_user_guide <- function(year, cfg = load_config()) {
  guide <- cfg$us_nchs$user_guides[[as.character(year)]]
  url <- sprintf("%s/%s", cfg$us_nchs$documentation_base_url, guide)
  dest <- root_path("data", "raw", "us_nchs", as.character(year), guide)
  download_if_needed(url, dest, overwrite = FALSE, timeout_sec = 600)
  write_manifest_row(root_path("outputs", "logs", "raw_file_manifest.csv"), "us_nchs_user_guide", year, url, dest)
  dest
}

download_us_data_zip <- function(year, cfg = load_config()) {
  data_file <- cfg$us_nchs$data_files[[as.character(year)]]
  url <- sprintf("%s/%s", cfg$us_nchs$data_base_url, data_file)
  dest <- root_path("data", "raw", "us_nchs", as.character(year), data_file)
  download_if_needed(url, dest, overwrite = FALSE, timeout_sec = 7200)
  write_manifest_row(root_path("outputs", "logs", "raw_file_manifest.csv"), "us_nchs_data", year, url, dest)
  dest
}

extract_us_zip <- function(zip_path, year) {
  out_dir <- root_path("data", "raw", "us_nchs", as.character(year), "extracted")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  existing <- list.files(out_dir, full.names = TRUE, recursive = TRUE, no.. = TRUE)
  existing_info <- file.info(existing)
  existing <- existing[existing_info$isdir == FALSE & existing_info$size > 0]
  if (length(existing) == 0) {
    tryCatch(
      archive::archive_extract(zip_path, dir = out_dir),
      error = function(e) {
        message("archive::archive_extract failed; falling back to system unzip: ", conditionMessage(e))
        status <- system2("unzip", args = c("-o", zip_path, "-d", out_dir), stdout = TRUE, stderr = TRUE)
        if (!identical(attr(status, "status"), NULL) && attr(status, "status") != 0) {
          stop("System unzip failed for ", zip_path, call. = FALSE)
        }
      }
    )
    existing <- list.files(out_dir, full.names = TRUE, recursive = TRUE, no.. = TRUE)
    existing_info <- file.info(existing)
    existing <- existing[existing_info$isdir == FALSE & existing_info$size > 0]
  }
  data_file <- existing[stringr::str_detect(tolower(existing), "\\.(dat|txt)$")]
  if (length(data_file) == 0) {
    data_file <- existing[which.max(file.info(existing)$size)]
  }
  data_file[[1]]
}

read_us_nchs_year <- function(year) {
  dct_path <- download_us_dictionary(year)
  zip_path <- download_us_data_zip(year)
  download_us_user_guide(year)
  data_path <- extract_us_zip(zip_path, year)
  data_path_for_readr <- file.path("data", "raw", "us_nchs", as.character(year), "extracted", basename(data_path))
  layout <- parse_nber_dct(dct_path) |>
    filter(name %in% us_selected_vars)
  missing_vars <- setdiff(us_selected_vars, layout$name)
  if (length(missing_vars) > 0) {
    warning("Missing US variables in DCT for ", year, ": ", paste(missing_vars, collapse = ", "))
  }
  col_positions <- readr::fwf_positions(layout$start, layout$end, col_names = layout$name)
  col_types <- paste0(ifelse(layout$type == "s", "c", "c"), collapse = "")
  raw <- readr::read_fwf(
    file = data_path_for_readr,
    col_positions = col_positions,
    col_types = col_types,
    progress = TRUE,
    trim_ws = TRUE
  )
  raw <- raw |>
    mutate(across(everything(), ~ na_if(.x, "")))
  numeric_vars <- setdiff(names(raw), c("sex", names(raw)[stringr::str_detect(names(raw), "^ca_")]))
  raw |>
    mutate(across(all_of(numeric_vars), safe_numeric))
}

clean_us_nchs_year <- function(year) {
  df <- read_us_nchs_year(year)
  anomaly_vars <- intersect(
    c("ca_anen", "ca_mnsb", "ca_cchd", "ca_cdh", "ca_omph", "ca_gast", "ca_limb", "ca_cleft", "ca_clpal", "ca_down", "ca_disor", "ca_hypo"),
    names(df)
  )
  out <- df |>
    transmute(
      country = "United States",
      birth_year = coalesce(dob_yy, as.numeric(year)),
      maternal_age = mager,
      maternal_education_raw = meduc,
      prenatal_visits = if_else(previs >= 0 & previs <= 98, previs, NA_real_),
      prenatal_visits_raw = previs,
      plurality_raw = dplural,
      parity_or_birth_order = lbo_rec,
      gestational_age = coalesce(oegest_comb, combgest),
      birth_weight = dbwt,
      delivery_mode_raw = coalesce(dmeth_rec, rdmeth_rec),
      newborn_sex_raw = sex,
      apgar5 = if_else(apgar5 >= 0 & apgar5 <= 10, apgar5, NA_real_),
      congenital_anomaly = if (length(anomaly_vars) > 0) {
        rowSums(across(all_of(anomaly_vars), ~ .x %in% c("Y", "C", "P")), na.rm = TRUE) > 0
      } else {
        NA
      }
    )
  out_path <- root_path("data", "interim", "us", sprintf("us_nchs_%s_clean.parquet", year))
  arrow::write_parquet(out, out_path)
  write_row_count("clean_us_year", "United States", year, nrow(out))
  out_path
}
