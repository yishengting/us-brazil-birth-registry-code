#!/usr/bin/env python3
"""Audit restored harmonized Parquet data without loading it all into memory."""

from __future__ import annotations

import argparse
import csv
import json
from collections import defaultdict
from pathlib import Path

import pyarrow as pa
import pyarrow.compute as pc
import pyarrow.dataset as ds
import pyarrow.parquet as pq


COUNTRIES = ("Brazil", "United States")
YEARS = tuple(range(2017, 2025))
OUTCOMES = ("preterm_birth", "low_birth_weight")
AUDIT_COLUMNS = (
    "country",
    "birth_year",
    "plurality_cat",
    "preterm_birth",
    "low_birth_weight",
    "age_risk",
    "inadequate_prenatal_care",
    "low_education",
    "parity_or_birth_order",
    "newborn_sex",
)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--data-root", type=Path, required=True)
    parser.add_argument("--report-json", type=Path, required=True)
    parser.add_argument("--country-year-csv", type=Path)
    return parser.parse_args()


def parquet_rows(path: Path) -> int:
    return pq.ParquetFile(path).metadata.num_rows


def schema_map(path: Path) -> dict[str, str]:
    schema = pq.ParquetFile(path).schema_arrow
    return {field.name: str(field.type) for field in schema}


def count_true(mask: pa.Array) -> int:
    safe = pc.fill_null(mask, False)
    return int(pc.sum(pc.cast(safe, pa.int64())).as_py() or 0)


def scan_pooled(path: Path) -> dict:
    country_year = defaultdict(int)
    singleton = defaultdict(int)
    outcome_counts = defaultdict(lambda: {"events": 0, "nonmissing": 0})
    missing = defaultdict(int)
    singleton_total = defaultdict(int)

    scanner = ds.dataset(path, format="parquet").scanner(
        columns=list(AUDIT_COLUMNS), batch_size=262_144, use_threads=True
    )
    for batch in scanner.to_batches():
        cols = {name: batch.column(name) for name in AUDIT_COLUMNS}
        singleton_mask = pc.equal(cols["plurality_cat"], "singleton")
        for country in COUNTRIES:
            country_mask = pc.equal(cols["country"], country)
            country_singleton = pc.and_kleene(country_mask, singleton_mask)
            n_singleton = count_true(country_singleton)
            singleton[country] += n_singleton
            singleton_total[country] += n_singleton

            for year in YEARS:
                mask = pc.and_kleene(country_mask, pc.equal(cols["birth_year"], year))
                country_year[(country, year)] += count_true(mask)

            for outcome in OUTCOMES:
                valid = pc.and_kleene(country_singleton, pc.is_valid(cols[outcome]))
                event = pc.and_kleene(valid, pc.equal(cols[outcome], True))
                outcome_counts[(country, outcome)]["nonmissing"] += count_true(valid)
                outcome_counts[(country, outcome)]["events"] += count_true(event)

            for variable in AUDIT_COLUMNS[3:]:
                miss = pc.and_kleene(country_singleton, pc.is_null(cols[variable]))
                missing[(country, variable)] += count_true(miss)

    return {
        "country_year_rows": [
            {"country": country, "birth_year": year, "rows": country_year[(country, year)]}
            for country in COUNTRIES
            for year in YEARS
        ],
        "singleton_rows": dict(singleton),
        "primary_outcomes": [
            {
                "country": country,
                "outcome": outcome,
                **outcome_counts[(country, outcome)],
                "rate_per_1000": (
                    1000
                    * outcome_counts[(country, outcome)]["events"]
                    / outcome_counts[(country, outcome)]["nonmissing"]
                ),
            }
            for country in COUNTRIES
            for outcome in OUTCOMES
        ],
        "singleton_missingness": [
            {
                "country": country,
                "variable": variable,
                "missing": missing[(country, variable)],
                "percent": 100 * missing[(country, variable)] / singleton_total[country],
            }
            for country in COUNTRIES
            for variable in AUDIT_COLUMNS[3:]
        ],
    }


def main() -> int:
    args = parse_args()
    root = args.data_root
    canonical = {
        "brazil": root / "br_harmonized_births_2017_2024.parquet",
        "united_states": root / "us_harmonized_births_2017_2024.parquet",
        "pooled": root / "pooled_harmonized_births_2017_2024.parquet",
    }
    missing_files = [str(path) for path in canonical.values() if not path.is_file()]
    if missing_files:
        raise FileNotFoundError("Missing canonical Parquet files: " + ", ".join(missing_files))

    rows = {name: parquet_rows(path) for name, path in canonical.items()}
    schemas = {name: schema_map(path) for name, path in canonical.items()}
    common_fields = set(schemas["brazil"]) & set(schemas["united_states"])
    country_union = set(schemas["brazil"]) | set(schemas["united_states"])
    chunk_rows = {}
    for country_dir in ("br", "us"):
        paths = sorted((root / "harmonized_chunks_2017_2024" / country_dir).glob("*.parquet"))
        chunk_rows[country_dir] = {
            "files": len(paths),
            "rows": sum(parquet_rows(path) for path in paths),
        }

    checks = {
        "country_sum_equals_pooled": rows["brazil"] + rows["united_states"] == rows["pooled"],
        "brazil_chunks_equal_country_file": chunk_rows["br"]["rows"] == rows["brazil"],
        "us_chunks_equal_country_file": chunk_rows["us"]["rows"] == rows["united_states"],
        "common_field_types_equal": all(
            schemas["brazil"][field]
            == schemas["united_states"][field]
            == schemas["pooled"].get(field)
            for field in common_fields
        ),
        "pooled_contains_country_schema_union": all(
            field in schemas["pooled"]
            and schemas["pooled"][field]
            == (schemas["brazil"].get(field) or schemas["united_states"].get(field))
            for field in country_union
        ),
    }
    scan = scan_pooled(canonical["pooled"])
    scanned_country_rows = {
        country: sum(
            item["rows"] for item in scan["country_year_rows"] if item["country"] == country
        )
        for country in COUNTRIES
    }
    checks["scanned_brazil_rows_equal_metadata"] = scanned_country_rows["Brazil"] == rows["brazil"]
    checks["scanned_us_rows_equal_metadata"] = (
        scanned_country_rows["United States"] == rows["united_states"]
    )

    report = {
        "canonical_rows": rows,
        "chunk_rows": chunk_rows,
        "schemas": schemas,
        "country_specific_fields": {
            "brazil_only": sorted(set(schemas["brazil"]) - set(schemas["united_states"])),
            "united_states_only": sorted(set(schemas["united_states"]) - set(schemas["brazil"])),
        },
        "checks": checks,
        "scan": scan,
        "ok": all(checks.values()),
    }
    args.report_json.parent.mkdir(parents=True, exist_ok=True)
    args.report_json.write_text(json.dumps(report, indent=2) + "\n", encoding="utf-8")
    if args.country_year_csv:
        args.country_year_csv.parent.mkdir(parents=True, exist_ok=True)
        with args.country_year_csv.open("w", newline="", encoding="utf-8") as handle:
            writer = csv.DictWriter(handle, fieldnames=("country", "birth_year", "births"))
            writer.writeheader()
            for item in scan["country_year_rows"]:
                writer.writerow(
                    {
                        "country": item["country"],
                        "birth_year": item["birth_year"],
                        "births": item["rows"],
                    }
                )
    print(json.dumps({"ok": report["ok"], "rows": rows, "checks": checks}, sort_keys=True))
    return 0 if report["ok"] else 1


if __name__ == "__main__":
    raise SystemExit(main())
