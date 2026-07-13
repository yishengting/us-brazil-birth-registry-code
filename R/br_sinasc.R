suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(tibble)
  library(arrow)
})

source("R/utils.R")

br_get_column <- function(df, candidates) {
  hit <- intersect(candidates, names(df))
  if (length(hit) == 0) {
    return(rep(NA, nrow(df)))
  }
  df[[hit[[1]]]]
}

br_birth_year <- function(df, fallback_year) {
  date_col <- br_get_column(df, c("DTNASC", "dt_nasc", "data_nascimento"))
  parsed <- suppressWarnings(as.Date(as.character(date_col), format = "%d%m%Y"))
  if (all(is.na(parsed))) {
    parsed <- suppressWarnings(as.Date(as.character(date_col)))
  }
  dplyr::coalesce(as.numeric(format(parsed, "%Y")), rep(as.numeric(fallback_year), nrow(df)))
}

download_clean_br_sinasc_year_state <- function(year, uf) {
  if (!requireNamespace("healthbR", quietly = TRUE)) {
    stop("Package healthbR is required for Brazil SINASC download. Install it with install.packages('healthbR').", call. = FALSE)
  }
  out_path <- file.path("data", "interim", "br", sprintf("br_sinasc_%s_%s_clean.parquet", year, uf))
  if (file.exists(out_path)) {
    message("Skipping existing Brazil SINASC clean file: ", year, " ", uf)
    return(out_path)
  }
  raw_path <- root_path("data", "raw", "br_sinasc", as.character(year), sprintf("sinasc_%s_%s.rds", year, uf))
  dir.create(dirname(raw_path), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(raw_path)) {
    raw <- readRDS(raw_path)
  } else {
    raw <- retry_sinasc_download(year, uf)
    saveRDS(raw, raw_path)
    write_manifest_row(
      root_path("outputs", "logs", "raw_file_manifest.csv"),
      "br_sinasc_healthbR",
      year,
      sprintf("healthbR::sinasc_data(year=%s, uf='%s', parse=TRUE)", year, uf),
      raw_path,
      note = uf
    )
  }
  out <- clean_br_sinasc_dataframe(raw, year, uf)
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(out, out_path)
  write_row_count("clean_br_year_state", "Brazil", year, nrow(out), note = uf)
  out_path
}

retry_sinasc_download <- function(year, uf, attempts = 6) {
  last_error <- NULL
  for (attempt in seq_len(attempts)) {
    raw <- tryCatch(
      {
        old_timeout <- getOption("timeout")
        on.exit(options(timeout = old_timeout), add = TRUE)
        options(timeout = max(old_timeout, 600))
        healthbR::sinasc_data(year = year, uf = uf, parse = TRUE)
      },
      error = function(e) {
        last_error <<- e
        NULL
      }
    )
    if (!is.null(raw)) {
      return(raw)
    }
    wait <- min(120, 10 * attempt)
    message("SINASC download failed for ", year, " ", uf, " attempt ", attempt, "/", attempts, "; retrying in ", wait, "s")
    Sys.sleep(wait)
  }
  stop("SINASC download failed for ", year, " ", uf, ": ", conditionMessage(last_error), call. = FALSE)
}

clean_br_sinasc_dataframe <- function(df, year, uf = NA_character_) {
  gest_weeks <- safe_numeric(br_get_column(df, c("SEMAGESTAC", "semagestac")))
  gest_cat <- safe_numeric(br_get_column(df, c("GESTACAO", "gestacao")))
  gest_from_cat <- dplyr::case_when(
    gest_cat == 1 ~ 21,
    gest_cat == 2 ~ 25,
    gest_cat == 3 ~ 31,
    gest_cat == 4 ~ 34,
    gest_cat == 5 ~ 39,
    gest_cat == 6 ~ 42,
    TRUE ~ NA_real_
  )
  tibble::tibble(
    country = "Brazil",
    birth_year = br_birth_year(df, year),
    source_uf = uf,
    maternal_age = safe_numeric(br_get_column(df, c("IDADEMAE", "idademae"))),
    maternal_education_raw = safe_numeric(br_get_column(df, c("ESCMAE2010", "ESCMAE", "escmae2010", "escmae"))),
    prenatal_visits_raw = safe_numeric(br_get_column(df, c("CONSULTAS", "consultas"))),
    prenatal_visits = dplyr::case_when(
      prenatal_visits_raw == 1 ~ 0,
      prenatal_visits_raw == 2 ~ 2,
      prenatal_visits_raw == 3 ~ 5,
      prenatal_visits_raw == 4 ~ 7,
      prenatal_visits_raw == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    plurality_raw = safe_numeric(br_get_column(df, c("GRAVIDEZ", "gravidez"))),
    parity_or_birth_order = safe_numeric(br_get_column(df, c("QTDFILVIVO", "qtdfilvivo"))),
    gestational_age = dplyr::coalesce(gest_weeks, gest_from_cat),
    birth_weight = safe_numeric(br_get_column(df, c("PESO", "peso"))),
    delivery_mode_raw = safe_numeric(br_get_column(df, c("PARTO", "parto"))),
    newborn_sex_raw = safe_numeric(br_get_column(df, c("SEXO", "sexo"))),
    apgar5 = safe_numeric(br_get_column(df, c("APGAR5", "apgar5"))),
    congenital_anomaly_raw = as.character(br_get_column(df, c("CODANOMAL", "codanomal", "IDANOMAL", "idanomal"))),
    congenital_anomaly = !is.na(congenital_anomaly_raw) &
      congenital_anomaly_raw != "" &
      !congenital_anomaly_raw %in% c("0", "00", "0000", "9999", "NA")
  )
}

