# Release Notes: v1.0.1

This release updates the journal-disclosure code after final end-to-end validation of the 2017-2024 downloaded registry workflow.

## Changes

- Reworked `scripts/05_harmonize_us_br.R` to harmonise input files in chunks instead of collecting the full US and Brazil registries into R memory.
- Added `scripts/31_concat_parquet_streaming.py` to stream-concatenate harmonised Parquet chunks while normalising schemas across countries.
- Updated `scripts/20_final_submission_enhancements.R` so the public pipeline can read current analysis-table locations after the BJOG display-table repackaging.

## Scope

These updates address reproducibility and packaging robustness. They do not change the prespecified exposure definitions, outcome definitions, model formulae, adjustment set, or interpretation of the registry-based risk markers.

The repository still excludes manuscript files, generated submission figures and tables, submission packages, and individual-level registry data.
