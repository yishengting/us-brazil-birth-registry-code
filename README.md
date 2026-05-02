# US-Brazil Birth Registry Study Code

This repository contains reproducible analysis code for a cross-national live birth registry study using public-use data from Brazil and the United States.

The repository is intended for journal review and reproducibility disclosure. It contains code, configuration files, variable harmonisation metadata, and documentation only.

## What Is Included

- `R/`: shared R functions for downloading, parsing, harmonising, modelling, and output preparation.
- `scripts/`: numbered analysis scripts for the public-data pipeline and final sensitivity/validation analyses.
- `config/`: project configuration and variable maps.
- `docs/`: public data-source notes, variable harmonisation notes, and statistical analysis plan.
- `data/README.md`: instructions for obtaining public source data.
- `outputs/README.md`: notes on generated local outputs.

## What Is Not Included

This disclosure repository intentionally excludes:

- manuscript files;
- cover letters or response files;
- generated tables and figures;
- submission packages;
- raw, interim, or harmonised individual-level data;
- ZIP, DOCX, RTF, PDF, PNG, and TIFF outputs.

These exclusions are enforced by `.gitignore`.

## Data Sources

The analysis uses public-use registry data:

- United States: National Center for Health Statistics Natality Birth Data Files, 2017-2024.
- Brazil: DATASUS/SINASC live birth microdata, 2017-2024, accessed through public DATASUS resources and the `healthbR` R package.

The scripts download or access these public data sources locally. Large raw and derived data files are not stored in this repository.

## Versioned Release

The journal-disclosure code snapshot is tagged as `v1.0.0`. The tag is intended to support archival DOI minting through Zenodo, OSF, or another persistent repository if required by the journal.

## Requirements

R version used for the submitted analysis:

```text
R 4.4.3
```

Install required R packages:

```r
source("scripts/install_packages.R")
```

Some downloads are large. Run the pipeline on a machine with sufficient disk space and memory for national individual-level birth registry files.

## Reproduce The Analysis Locally

From the repository root:

```r
source("scripts/run_public_pipeline.R")
```

The pipeline writes downloaded source files under `data/`, derived analytic files under `data/final/`, and generated tables/figures/logs under `outputs/`. Those paths are ignored by git.

## Notes For Reviewers

The code is organised to document the analytic workflow used for the manuscript. Generated submission materials are deliberately not included in this repository. The statistical analysis plan is available at `docs/statistical_analysis_plan.md`.
