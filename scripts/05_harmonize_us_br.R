source("R/harmonize.R")

cfg <- load_config()
ensure_project_dirs(cfg)
period_label <- paste0(min(cfg$project$years), "-", max(cfg$project$years))
period_suffix <- gsub("-", "_", period_label)

us_files <- sort(list.files(file.path("data", "interim", "us"), pattern = "\\.parquet$", full.names = TRUE))
br_files <- sort(list.files(file.path("data", "interim", "br"), pattern = "\\.parquet$", full.names = TRUE))

if (length(us_files) == 0) {
  stop("No US clean parquet files found. Run scripts/03_parse_us_nchs.R first.", call. = FALSE)
}
if (length(br_files) == 0) {
  stop("No Brazil clean parquet files found. Run scripts/04_download_clean_br_sinasc.R first.", call. = FALSE)
}

chunk_dir <- root_path("data", "final", sprintf("harmonized_chunks_%s", period_suffix))
unlink(chunk_dir, recursive = TRUE, force = TRUE)
dir.create(file.path(chunk_dir, "us"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(chunk_dir, "br"), recursive = TRUE, showWarnings = FALSE)

process_chunks <- function(files, country_key) {
  counts <- list()
  chunk_paths <- character()
  total_rows <- 0
  out_dir <- file.path(chunk_dir, country_key)

  for (i in seq_along(files)) {
    in_path <- files[[i]]
    message("Harmonizing ", country_key, " chunk ", i, "/", length(files), ": ", basename(in_path))
    h <- arrow::read_parquet(in_path) |>
      harmonize_births()

    out_path <- file.path(out_dir, sprintf("%s_%03d.parquet", country_key, i))
    arrow::write_parquet(h, out_path)
    chunk_paths <- c(chunk_paths, out_path)
    total_rows <- total_rows + nrow(h)
    counts[[i]] <- h |>
      dplyr::count(country, birth_year, name = "births")

    rm(h)
    gc()
  }

  list(paths = chunk_paths, rows = total_rows, counts = dplyr::bind_rows(counts))
}

message("Harmonizing US interim datasets by year.")
us_result <- process_chunks(us_files, "us")
message("Harmonizing Brazil interim datasets by year-state.")
br_result <- process_chunks(br_files, "br")

concat_script <- root_path("scripts", "31_concat_parquet_streaming.py")
if (!file.exists(concat_script)) {
  stop("Missing streaming parquet concatenation helper: ", concat_script, call. = FALSE)
}

concat_parquet <- function(output_path, inputs) {
  unlink(output_path, force = TRUE)
  args <- c(concat_script, output_path, inputs)
  status <- system2("python3", args, stdout = TRUE, stderr = TRUE)
  cat(paste(status, collapse = "\n"), "\n")
  exit_status <- attr(status, "status")
  if (!is.null(exit_status) && exit_status != 0) {
    stop("Streaming parquet concatenation failed for ", output_path, call. = FALSE)
  }
  invisible(output_path)
}

us_out <- file.path("data", "final", sprintf("us_harmonized_births_%s.parquet", period_suffix))
br_out <- file.path("data", "final", sprintf("br_harmonized_births_%s.parquet", period_suffix))
pooled_out <- file.path("data", "final", sprintf("pooled_harmonized_births_%s.parquet", period_suffix))

message("Writing final US harmonized parquet.")
concat_parquet(us_out, us_result$paths)
message("Writing final Brazil harmonized parquet.")
concat_parquet(br_out, br_result$paths)
message("Writing final pooled harmonized parquet.")
concat_parquet(pooled_out, c(us_result$paths, br_result$paths))

write_row_count("final_harmonized", "United States", period_label, us_result$rows)
write_row_count("final_harmonized", "Brazil", period_label, br_result$rows)
write_row_count("final_harmonized", "Pooled", period_label, us_result$rows + br_result$rows)

dplyr::bind_rows(us_result$counts, br_result$counts) |>
  dplyr::group_by(country, birth_year) |>
  dplyr::summarise(births = sum(births), .groups = "drop") |>
  dplyr::arrange(country, birth_year) |>
  write_csv_safe(root_path("outputs", "logs", "final_birth_counts_by_country_year.csv"))

message("Final harmonized datasets written.")
