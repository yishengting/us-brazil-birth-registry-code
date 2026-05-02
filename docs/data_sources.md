# Data Sources

## United States

The United States analysis uses public-use NCHS Natality Birth Data Files for 2017-2024. Files are downloaded from the NCHS/CDC public FTP directory and parsed from fixed-width records using archived machine-readable layouts. Annual NCHS User Guides are downloaded and archived with the raw data for verification.

For 2024, the NCHS data file and User Guide are archived in the raw-data directory. The pipeline records the selected 2024 dictionary fallback for harmonized variables in `outputs/logs/data_source_verification.csv` and `outputs/logs/raw_file_manifest.csv`.

## Brazil

The Brazil analysis uses DATASUS/SINASC live birth microdata for 2017-2024. The preferred downloader is the R package `healthbR`, which accesses DATASUS public microdata and returns parsed live birth records.

## Provenance

The pipeline writes `outputs/logs/raw_file_manifest.csv` with source, year, URL or package call, file path, file size, SHA256, and timestamp.
