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

The corrected journal-disclosure snapshot is version `v1.0.4`. It supersedes `v1.0.3` for inferential output because `v1.0.4` reconstructs individual-record HC0 covariance exactly from grouped Bernoulli counts and uses the joint delta method for risk differences. The versioned source is available at <https://github.com/yishengting/us-brazil-birth-registry-code/tree/v1.0.4>.

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

The harmonisation step also uses Python 3 with `pyarrow` for streaming Parquet concatenation. This avoids collecting the full 2017-2024 pooled registry into R memory during final file assembly.

## Reproduce The Analysis Locally

From the repository root:

```r
source("scripts/run_public_pipeline.R")
```

On macOS, the full 51-million-record analysis can exceed R's default vector-memory ceiling even when physical resident memory remains lower. The verified recovery run used `R_MAX_VSIZE=40Gb` on a 16 GB machine. The statistical corrections and recovery checks are summarised in `docs/release_notes_v1.0.4.md`.

The pipeline writes downloaded source files under `data/`, derived analytic files under `data/final/`, and generated tables/figures/logs under `outputs/`. Those paths are ignored by git.

## Notes For Reviewers

The code is organised to document the analytic workflow used for the manuscript. Generated submission materials are deliberately not included in this repository. The statistical analysis plan is available at `docs/statistical_analysis_plan.md`.
