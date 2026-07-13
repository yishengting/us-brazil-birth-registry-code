#!/usr/bin/env python3
"""Restore canonical submission-table inputs from fresh analysis outputs.

The recovered project retained the statistical outputs but lost part of the
submission packaging layer.  This script recreates only deterministic copies
whose provenance is explicit; presentation formatting remains the responsibility
of ``21_revision7_table_format.py`` and later packaging scripts.
"""

from __future__ import annotations

import argparse
import csv
import json
import shutil
from pathlib import Path


COPY_MAP = {
    "outputs/revision/tables/supplementary_table_missingness.csv":
        "submission/tables/supplementary/Supplementary_Table_1_missingness.csv",
    "outputs/revision2/tables/table3_country_interaction_ratios.csv":
        "submission/tables/supplementary/Supplementary_Table_2_country_interaction_ratios.csv",
    "submission/tables/supplementary/supplementary_table_full_variable_harmonization.csv":
        "submission/tables/supplementary/Supplementary_Table_3_variable_harmonization.csv",
    "outputs/revision2/tables/supplementary_table_term_low_birth_weight_models.csv":
        "submission/tables/supplementary/Supplementary_Table_4_term_low_birth_weight.csv",
    "outputs/revision2/tables/supplementary_table_age_subtype_models.csv":
        "submission/tables/supplementary/Supplementary_Table_5_age_subtype_models.csv",
    "outputs/revision2/tables/supplementary_table_no_prenatal_care_sensitivity.csv":
        "submission/tables/supplementary/Supplementary_Table_7_no_prenatal_care_sensitivity.csv",
    "outputs/revision2/tables/supplementary_table_age_education_only_sensitivity.csv":
        "submission/tables/supplementary/Supplementary_Table_8_age_education_only_sensitivity.csv",
    "submission/tables/supplementary/supplementary_table_phenotype_prevalence.csv":
        "submission/tables/supplementary/Supplementary_Table_9_risk_profile_prevalence.csv",
    "submission/tables/supplementary/supplementary_table_cross_national_standardization.csv":
        "submission/tables/supplementary/Supplementary_Table_10_cross_national_standardization.csv",
    "outputs/revision/tables/table2_singleton_outcome_rates.csv":
        "submission/tables/main/table2_singleton_outcome_rates.csv",
    "outputs/revision/tables/table3_singleton_risk_score_models.csv":
        "submission/tables/main/table3_singleton_risk_score_models.csv",
    "outputs/revision/tables/table4_singleton_phenotype_models.csv":
        "submission/tables/main/table4_singleton_phenotype_models.csv",
    "outputs/revision2/tables/table5_absolute_risks_with_ci.csv":
        "submission/tables/main/table5_absolute_risks_with_ci.csv",
    "outputs/logs/final_birth_counts_by_country_year.csv":
        "submission/data_provenance/final_birth_counts_by_country_year.csv",
}

MINIMUM_SCHEMAS = {
    "outputs/revision/tables/supplementary_table_missingness.csv":
        {"country", "birth_year", "births", "gestational_age_missing_pct"},
    "outputs/revision2/tables/table3_country_interaction_ratios.csv":
        {"outcome", "risk_score", "ratio_of_aRR_US_vs_Brazil", "conf.low", "conf.high", "p_interaction"},
    "submission/tables/supplementary/supplementary_table_full_variable_harmonization.csv":
        {"harmonized_variable", "us_source_variable", "brazil_source_variable", "final_harmonized_category"},
    "outputs/revision2/tables/supplementary_table_term_low_birth_weight_models.csv":
        {"term", "estimate", "conf.low", "conf.high", "outcome", "n", "country"},
    "outputs/revision2/tables/supplementary_table_age_subtype_models.csv":
        {"term", "estimate", "conf.low", "conf.high", "outcome", "n", "country"},
    "outputs/revision2/tables/supplementary_table_no_prenatal_care_sensitivity.csv":
        {"term", "estimate", "conf.low", "conf.high", "outcome", "n", "country"},
    "outputs/revision2/tables/supplementary_table_age_education_only_sensitivity.csv":
        {"term", "estimate", "conf.low", "conf.high", "outcome", "n", "country"},
    "submission/tables/supplementary/supplementary_table_phenotype_prevalence.csv":
        {"country", "risk_phenotype3", "births", "total_births", "percent"},
    "submission/tables/supplementary/supplementary_table_cross_national_standardization.csv":
        {"country", "outcome", "observed_rate_per_1000"},
    "outputs/revision/tables/table2_singleton_outcome_rates.csv":
        {"country", "birth_year", "births", "preterm_birth_rate_per_1000", "low_birth_weight_rate_per_1000"},
    "outputs/revision/tables/table3_singleton_risk_score_models.csv":
        {"term", "estimate", "conf.low", "conf.high", "outcome", "n", "country"},
    "outputs/revision/tables/table4_singleton_phenotype_models.csv":
        {"term", "estimate", "conf.low", "conf.high", "outcome", "n", "country"},
    "outputs/revision2/tables/table5_absolute_risks_with_ci.csv":
        {"country", "outcome", "exposure", "adjusted_risk_per_1000", "risk_ci_low", "risk_ci_high"},
    "outputs/logs/final_birth_counts_by_country_year.csv":
        {"country", "birth_year", "births"},
}


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--project-root", type=Path, default=Path(__file__).resolve().parents[1])
    return parser.parse_args()


def inspect_csv(path: Path) -> tuple[list[str], int]:
    with path.open(newline="", encoding="utf-8-sig") as handle:
        reader = csv.reader(handle)
        try:
            header = next(reader)
        except StopIteration as exc:
            raise ValueError(f"Empty CSV: {path}") from exc
        rows = sum(1 for _ in reader)
    if rows == 0:
        raise ValueError(f"CSV has no data rows: {path}")
    return header, rows


def main() -> int:
    root = parse_args().project_root.resolve()
    audit_rows = []
    for source_rel, destination_rel in COPY_MAP.items():
        source = root / source_rel
        destination = root / destination_rel
        if not source.is_file():
            raise FileNotFoundError(f"Missing recovery source: {source}")
        header, row_count = inspect_csv(source)
        required = MINIMUM_SCHEMAS[source_rel]
        missing = sorted(required.difference(header))
        if missing:
            raise ValueError(f"Schema mismatch in {source}: missing {missing}")
        destination.parent.mkdir(parents=True, exist_ok=True)
        temporary = destination.with_suffix(destination.suffix + ".tmp")
        shutil.copyfile(source, temporary)
        temporary.replace(destination)
        audit_rows.append(
            {
                "source": source_rel,
                "destination": destination_rel,
                "rows": row_count,
                "columns": len(header),
            }
        )

    report = {
        "purpose": "restore canonical submission table inputs from freshly regenerated analysis outputs",
        "copy_count": len(audit_rows),
        "copies": audit_rows,
        "ok": True,
    }
    report_path = root / "outputs" / "logs" / "submission_table_input_recovery_20260713.json"
    report_path.parent.mkdir(parents=True, exist_ok=True)
    report_path.write_text(json.dumps(report, indent=2) + "\n", encoding="utf-8")
    print(json.dumps({"copy_count": len(audit_rows), "ok": True, "report": str(report_path)}))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
