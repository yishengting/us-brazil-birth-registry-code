# Data Sources

## United States

The United States analysis uses public-use NCHS Natality Birth Data Files for 2017-2024. Files are downloaded from the NCHS/CDC public FTP directory and parsed from fixed-width records using archived machine-readable layouts. Annual NCHS User Guides are downloaded and archived with the raw data for verification.

On July 13, 2026, all eight NCHS data ZIP URLs and all eight NCHS User Guide URLs were reachable from the recovery machine. Live NBER dictionary URLs for 2017-2023 returned access errors from this network; this is recorded as a live-access limitation rather than a file-integrity failure because the archived dictionaries match the historical SHA256 manifest.

For 2024, the NCHS data file and User Guide are archived in the raw-data directory. The pipeline records the selected 2024 dictionary fallback for harmonized variables in `outputs/logs/data_source_verification.csv` and `outputs/logs/raw_file_manifest.csv`. Recovery audit `outputs/logs/us_2024_layout_audit_20260713.json` independently compares all 27 selected fixed-width fields with the archived 2024 NCHS User Guide; all positions match.

## Brazil

The Brazil analysis uses DATASUS/SINASC live birth microdata for 2017-2024. The preferred downloader is the R package `healthbR`, which accesses DATASUS public microdata and returns parsed live birth records.

## Provenance

The pipeline writes `outputs/logs/raw_file_manifest.csv` with source, year, URL or package call, file path, file size, SHA256, and timestamp. Recovery audit `outputs/logs/restored_data_hash_audit_20260713.json` rebases the historical manifest to the restored project and verifies every unique raw file by size and SHA256.
