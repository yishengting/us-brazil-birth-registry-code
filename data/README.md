# Data directory

This repository does not include raw, interim, or harmonised individual-level data.

Run the download and parsing scripts locally to populate this directory:

```r
source("scripts/02_download_us_nchs.R")
source("scripts/03_parse_us_nchs.R")
source("scripts/04_download_clean_br_sinasc.R")
source("scripts/05_harmonize_us_br.R")
```

The public-use registry sources are documented in `docs/data_sources.md`. Users must obtain the records directly from the source agencies and comply with the source terms.
