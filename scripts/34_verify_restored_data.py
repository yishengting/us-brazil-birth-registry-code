#!/usr/bin/env python3
"""Verify restored raw files against the historical download manifest.

Historical absolute paths are rebased at the project-level ``data`` directory.
Duplicate manifest rows are collapsed only when size and SHA-256 agree.
"""

from __future__ import annotations

import argparse
import csv
import hashlib
import json
from pathlib import Path


def sha256(path: Path, chunk_size: int = 8 * 1024 * 1024) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(chunk_size), b""):
            digest.update(chunk)
    return digest.hexdigest()


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--manifest", type=Path, required=True)
    parser.add_argument("--data-root", type=Path, required=True)
    parser.add_argument("--report-json", type=Path)
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    expected: dict[str, tuple[int, str]] = {}
    conflicts: list[str] = []
    manifest_rows = 0

    with args.manifest.open(newline="", encoding="utf-8-sig") as handle:
        for row in csv.DictReader(handle):
            manifest_rows += 1
            old_path = Path(row["file_path"])
            try:
                relative = Path(*old_path.parts[old_path.parts.index("data") + 1 :])
            except ValueError as exc:
                raise ValueError(f"Manifest path has no data component: {old_path}") from exc
            key = relative.as_posix()
            value = (int(row["size_bytes"]), row["sha256"].lower())
            if key in expected and expected[key] != value:
                conflicts.append(key)
            expected[key] = value

    results = []
    for index, (relative, (expected_size, expected_hash)) in enumerate(
        sorted(expected.items()), start=1
    ):
        path = args.data_root / relative
        exists = path.is_file()
        actual_size = path.stat().st_size if exists else None
        actual_hash = sha256(path) if exists and actual_size == expected_size else None
        results.append(
            {
                "path": relative,
                "exists": exists,
                "size_ok": actual_size == expected_size,
                "sha256_ok": actual_hash == expected_hash,
                "expected_size": expected_size,
                "actual_size": actual_size,
            }
        )
        print(f"[{index:03d}/{len(expected):03d}] {relative}", flush=True)

    summary = {
        "manifest_rows": manifest_rows,
        "unique_paths": len(expected),
        "conflicting_duplicate_paths": len(set(conflicts)),
        "missing": sum(not item["exists"] for item in results),
        "size_mismatches": sum(item["exists"] and not item["size_ok"] for item in results),
        "sha256_mismatches": sum(
            item["exists"] and item["size_ok"] and not item["sha256_ok"] for item in results
        ),
    }
    summary["ok"] = (
        summary["conflicting_duplicate_paths"] == 0
        and summary["missing"] == 0
        and summary["size_mismatches"] == 0
        and summary["sha256_mismatches"] == 0
    )

    if args.report_json:
        args.report_json.parent.mkdir(parents=True, exist_ok=True)
        args.report_json.write_text(
            json.dumps({"summary": summary, "files": results}, indent=2) + "\n",
            encoding="utf-8",
        )
    print(json.dumps(summary, sort_keys=True))
    return 0 if summary["ok"] else 1


if __name__ == "__main__":
    raise SystemExit(main())
