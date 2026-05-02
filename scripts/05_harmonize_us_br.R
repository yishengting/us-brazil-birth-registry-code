source("R/harmonize.R")

cfg <- load_config()
ensure_project_dirs(cfg)
period_label <- paste0(min(cfg$project$years), "-", max(cfg$project$years))
period_suffix <- gsub("-", "_", period_label)

us_files <- list.files(file.path("data", "interim", "us"), pattern = "\\.parquet$", full.names = TRUE)
br_files <- list.files(file.path("data", "interim", "br"), pattern = "\\.parquet$", full.names = TRUE)

if (length(us_files) == 0) {
  stop("No US clean parquet files found. Run scripts/03_parse_us_nchs.R first.", call. = FALSE)
}
if (length(br_files) == 0) {
  stop("No Brazil clean parquet files found. Run scripts/04_download_clean_br_sinasc.R first.", call. = FALSE)
}

message("Opening interim datasets.")
us <- arrow::open_dataset(us_files)
br <- arrow::open_dataset(br_files)

message("Collecting and harmonizing. This can require substantial memory for full ", period_label, " data.")
us_h <- us |> dplyr::collect() |> harmonize_births()
br_h <- br |> dplyr::collect() |> harmonize_births()

arrow::write_parquet(us_h, file.path("data", "final", sprintf("us_harmonized_births_%s.parquet", period_suffix)))
arrow::write_parquet(br_h, file.path("data", "final", sprintf("br_harmonized_births_%s.parquet", period_suffix)))

pooled <- dplyr::bind_rows(us_h, br_h)
arrow::write_parquet(pooled, file.path("data", "final", sprintf("pooled_harmonized_births_%s.parquet", period_suffix)))

write_row_count("final_harmonized", "United States", period_label, nrow(us_h))
write_row_count("final_harmonized", "Brazil", period_label, nrow(br_h))
write_row_count("final_harmonized", "Pooled", period_label, nrow(pooled))

write_csv_safe(
  pooled |>
    dplyr::count(country, birth_year, name = "births"),
  root_path("outputs", "logs", "final_birth_counts_by_country_year.csv")
)

message("Final harmonized datasets written.")
