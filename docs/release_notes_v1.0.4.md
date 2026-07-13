# Release notes: v1.0.4

_Recovery and inferential-correction release · July 13, 2026_

---

## 📋 Scope

This release restores the complete local 2017–2024 data tree after storage failure, verifies raw files against the historical download manifest, and corrects inferential calculations while preserving all fitted point estimates.

## 📊 Statistical corrections

- Reconstructs individual-record HC0 covariance exactly from grouped Bernoulli event and non-event score contributions
- Validates grouped coefficients and reconstructed standard errors against individual-record models on a country-year stratified sample
- Uses the full joint delta method for standardized risk differences, including covariance between compared predictions
- Regenerates all affected confidence intervals, interaction tests, tables, figures, and manuscript results
- Serialises the Arrow CPU scan used for the validation pool and confirms byte-identical validation outputs across consecutive reruns

## 💾 Recovery verification

- Confirms 240 unique raw files with zero missing, size-mismatched, or SHA256-mismatched files
- Confirms canonical country files, pooled Parquet files, and streaming chunks have identical row-count totals and schemas
- Confirms all 27 selected 2024 NCHS fixed-width fields match the archived 2024 User Guide

## 📦 Submission repair

- Rebuilds publication-ready Word tables with fixed landscape geometry and repeated headers
- Updates the manuscript, redacted provenance logs, reference audit, result-consistency audit, figure package, and submission manifest
- Quotes Pandoc paths containing spaces and fails the pipeline on document-conversion errors
- Synchronises all six journal-upload TIFF copies from the regenerated 600-dpi sources
- Supersedes public source tag `v1.0.3`; the corrected source is published as GitHub tag `v1.0.4`
