suppressPackageStartupMessages({
  library(digest)
  library(fs)
  library(readr)
  library(yaml)
  library(dplyr)
  library(tidyr)
  library(stringr)
})

project_root <- function() {
  env_root <- Sys.getenv("PROJECT_ROOT", unset = "")
  if (nzchar(env_root)) {
    return(env_root)
  }
  marker <- "us_brazil_birth_registry_study.Rproj"
  current <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  repeat {
    if (file.exists(file.path(current, marker))) {
      return(current)
    }
    parent <- dirname(current)
    if (identical(parent, current)) {
      stop("Project root not found. Run from inside us_brazil_birth_registry_study.", call. = FALSE)
    }
    current <- parent
  }
}

root_path <- function(...) {
  file.path(project_root(), ...)
}

load_config <- function() {
  yaml::read_yaml(root_path("config", "config.yml"))
}

ensure_project_dirs <- function(cfg = load_config()) {
  dirs <- unlist(cfg$paths, use.names = FALSE)
  purrr::walk(dirs, ~ dir.create(root_path(.x), recursive = TRUE, showWarnings = FALSE))
  invisible(TRUE)
}

timestamp_utc <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

sha256_file <- function(path) {
  digest::digest(file = path, algo = "sha256")
}

append_log_csv <- function(path, row) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  row <- tibble::as_tibble(row)
  write_csv_safe(row, path, append = file.exists(path))
}

write_csv_safe <- function(x, path, append = FALSE) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.table(
    x,
    file = path,
    sep = ",",
    row.names = FALSE,
    col.names = !append,
    append = append,
    quote = TRUE,
    qmethod = "double",
    na = ""
  )
  invisible(path)
}

download_if_needed <- function(url, dest, overwrite = FALSE, timeout_sec = 3600) {
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(dest) && !overwrite) {
    return(dest)
  }
  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)
  options(timeout = max(timeout_sec, old_timeout))
  tmp <- paste0(dest, ".part")
  if (file.exists(tmp)) {
    file.remove(tmp)
  }
  status <- utils::download.file(url, tmp, mode = "wb", quiet = FALSE)
  if (!identical(status, 0L)) {
    stop("Download failed: ", url, call. = FALSE)
  }
  if (!file.rename(tmp, dest)) {
    stop("Could not move temporary download into place: ", dest, call. = FALSE)
  }
  dest
}

write_manifest_row <- function(manifest_path, source, year, url, file_path, status = "downloaded", note = NA_character_) {
  info <- if (file.exists(file_path)) file.info(file_path) else NULL
  append_log_csv(
    manifest_path,
    tibble::tibble(
      timestamp_utc = timestamp_utc(),
      source = source,
      year = year,
      url = url,
      file_path = file_path,
      status = status,
      size_bytes = if (!is.null(info)) info$size else NA_real_,
      sha256 = if (file.exists(file_path)) sha256_file(file_path) else NA_character_,
      note = note
    )
  )
}

record_count <- function(data) {
  if (inherits(data, "data.frame")) {
    return(nrow(data))
  }
  if (inherits(data, "ArrowObject") || inherits(data, "FileSystemDataset")) {
    return(dplyr::collect(dplyr::summarise(data, n = dplyr::n()))$n[[1]])
  }
  NA_integer_
}

write_row_count <- function(stage, country, year, rows, note = NA_character_) {
  append_log_csv(
    root_path("outputs", "logs", "row_counts.csv"),
    tibble::tibble(
      timestamp_utc = timestamp_utc(),
      stage = stage,
      country = country,
      year = year,
      rows = rows,
      note = note
    )
  )
}

safe_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

yes_no_to_logical <- function(x) {
  dplyr::case_when(
    x %in% c("Y", "y", "1", 1, TRUE) ~ TRUE,
    x %in% c("N", "n", "0", 0, FALSE) ~ FALSE,
    TRUE ~ NA
  )
}

# Exact individual-record HC0 covariance for a Bernoulli outcome represented by
# grouped event counts. The grouped Poisson score has the same coefficient
# solution as the individual-record modified Poisson model, but applying
# vcovHC() directly to grouped rows treats each covariate pattern as one
# observation. This function reconstructs the sum of squared individual scores
# within each pattern without expanding tens of millions of records.
grouped_binary_hc0 <- function(fit, events, total) {
  x <- stats::model.matrix(fit)
  beta <- stats::coef(fit)
  if (anyNA(beta)) {
    stop("Aliased coefficients are not supported by grouped_binary_hc0().", call. = FALSE)
  }
  if (nrow(x) != length(events) || length(events) != length(total)) {
    stop("events and total must align with the fitted grouped-model rows.", call. = FALSE)
  }
  if (any(events < 0 | total <= 0 | events > total)) {
    stop("Grouped counts must satisfy 0 <= events <= total and total > 0.", call. = FALSE)
  }

  fitted_events <- stats::fitted(fit)
  fitted_risk <- fitted_events / total
  bread_information <- crossprod(x, x * fitted_events)
  score_squares <- events * (1 - fitted_risk)^2 + (total - events) * fitted_risk^2
  meat <- crossprod(x, x * score_squares)
  bread_inverse <- solve(bread_information)
  covariance <- bread_inverse %*% meat %*% bread_inverse
  dimnames(covariance) <- list(names(beta), names(beta))
  covariance
}
