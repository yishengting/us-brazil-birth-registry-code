# Data Directory

This repository does not include raw, interim, or harmonised individual-level data.

Run the download and parsing scripts locally to populate this directory:

```r
source("scripts/02_download_us_nchs.R")
source("scripts/03_parse_us_nchs.R")
source("scripts/04_download_clean_br_sinasc.R")
source("scripts/05_harmonize_us_br.R")
```

The data sources are public-use birth registry resources described in `docs/data_sources.md`.

