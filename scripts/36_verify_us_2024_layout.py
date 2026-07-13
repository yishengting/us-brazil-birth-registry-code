#!/usr/bin/env python3
"""Verify that the archived 2023 NBER layout matches the 2024 NCHS guide.

Only fields selected by the harmonization pipeline are compared. This closes
the audit trail for the documented 2024 dictionary fallback.
"""

from __future__ import annotations

import argparse
import json
import re
import subprocess
import tempfile
from pathlib import Path


SELECTED = (
    "dob_yy", "mager", "meduc", "previs", "previs_rec", "dplural",
    "lbo_rec", "oegest_comb", "combgest", "dbwt", "dmeth_rec", "rdmeth_rec",
    "sex", "apgar5", "ca_anen", "ca_mnsb", "ca_cchd", "ca_cdh", "ca_omph",
    "ca_gast", "ca_limb", "ca_cleft", "ca_clpal", "ca_down", "ca_disor",
    "ca_hypo", "no_congen",
)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--dictionary", type=Path, required=True)
    parser.add_argument("--user-guide", type=Path, required=True)
    parser.add_argument("--report-json", type=Path, required=True)
    return parser.parse_args()


def parse_dictionary(path: Path) -> dict[str, tuple[int, int]]:
    pattern = re.compile(r"^_column\((\d+)\s*\)\s+\S+\s+(\S+)\s+%([0-9]+)[sfg]")
    fields = {}
    for line in path.read_text(encoding="utf-8", errors="replace").splitlines():
        match = pattern.match(line)
        if match:
            start, name, width = match.groups()
            fields[name.lower()] = (int(start), int(start) + int(width) - 1)
    return fields


def parse_user_guide(path: Path) -> dict[str, tuple[int, int]]:
    with tempfile.TemporaryDirectory() as temp_dir:
        text_path = Path(temp_dir) / "guide.txt"
        subprocess.run(
            ["pdftotext", "-layout", str(path), str(text_path)],
            check=True,
            capture_output=True,
            text=True,
        )
        text = text_path.read_text(encoding="utf-8", errors="replace")
    pattern = re.compile(
        r"^\s*(\d+)(?:-(\d+))?\s+\d+\s+([A-Za-z][A-Za-z0-9_]*)\s+",
        re.MULTILINE,
    )
    fields = {}
    for start, end, name in pattern.findall(text):
        fields[name.lower()] = (int(start), int(end or start))
    return fields


def main() -> int:
    args = parse_args()
    dictionary = parse_dictionary(args.dictionary)
    guide = parse_user_guide(args.user_guide)
    rows = []
    for name in SELECTED:
        dictionary_position = dictionary.get(name)
        guide_position = guide.get(name)
        rows.append(
            {
                "field": name,
                "dictionary_start": dictionary_position[0] if dictionary_position else None,
                "dictionary_end": dictionary_position[1] if dictionary_position else None,
                "guide_start": guide_position[0] if guide_position else None,
                "guide_end": guide_position[1] if guide_position else None,
                "match": dictionary_position is not None and dictionary_position == guide_position,
            }
        )
    report = {
        "dictionary": str(args.dictionary),
        "user_guide": str(args.user_guide),
        "selected_fields": len(SELECTED),
        "matched_fields": sum(row["match"] for row in rows),
        "ok": all(row["match"] for row in rows),
        "fields": rows,
    }
    args.report_json.parent.mkdir(parents=True, exist_ok=True)
    args.report_json.write_text(json.dumps(report, indent=2) + "\n", encoding="utf-8")
    print(json.dumps({key: report[key] for key in ("selected_fields", "matched_fields", "ok")}))
    return 0 if report["ok"] else 1


if __name__ == "__main__":
    raise SystemExit(main())
