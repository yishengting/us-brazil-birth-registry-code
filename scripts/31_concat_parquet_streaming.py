#!/usr/bin/env python3
"""Concatenate parquet files with bounded memory.

The R harmonisation step writes per-year or per-state parquet chunks. This
helper streams those chunks into one parquet file while normalising schemas,
including columns that exist only in one country.
"""

from __future__ import annotations

import sys
from collections import OrderedDict

import pyarrow as pa
import pyarrow.parquet as pq


def merge_type(types: list[pa.DataType]) -> pa.DataType:
    non_null = [typ for typ in types if not pa.types.is_null(typ)]
    if not non_null:
        return pa.string()
    if any(pa.types.is_dictionary(typ) for typ in non_null):
        return pa.string()
    if all(typ == non_null[0] for typ in non_null):
        return non_null[0]
    if all(pa.types.is_integer(typ) or pa.types.is_floating(typ) for typ in non_null):
        return pa.float64()
    if all(pa.types.is_boolean(typ) for typ in non_null):
        return pa.bool_()
    return pa.string()


def unified_schema(paths: list[str]) -> pa.Schema:
    fields: OrderedDict[str, list[pa.DataType]] = OrderedDict()
    for path in paths:
        schema = pq.ParquetFile(path).schema_arrow
        for field in schema:
            fields.setdefault(field.name, []).append(field.type)
    return pa.schema([pa.field(name, merge_type(types)) for name, types in fields.items()])


def normalise_table(table: pa.Table, schema: pa.Schema) -> pa.Table:
    arrays = []
    names = set(table.schema.names)
    for field in schema:
        if field.name in names:
            arr = table[field.name]
            if pa.types.is_dictionary(arr.type):
                arr = arr.cast(pa.string())
            if arr.type != field.type:
                arr = arr.cast(field.type, safe=False)
        else:
            arr = pa.nulls(table.num_rows, type=field.type)
        arrays.append(arr)
    return pa.Table.from_arrays(arrays, schema=schema)


def main() -> int:
    if len(sys.argv) < 4:
        print("Usage: 31_concat_parquet_streaming.py OUTPUT.parquet INPUT1.parquet INPUT2.parquet [...]", file=sys.stderr)
        return 2

    output = sys.argv[1]
    inputs = sys.argv[2:]
    schema = unified_schema(inputs)
    rows = 0

    with pq.ParquetWriter(output, schema=schema, compression="snappy") as writer:
        for path in inputs:
            parquet_file = pq.ParquetFile(path)
            for batch in parquet_file.iter_batches(batch_size=250_000):
                table = normalise_table(pa.Table.from_batches([batch]), schema)
                writer.write_table(table)
                rows += table.num_rows

    print(f"Wrote {output} with {rows} rows from {len(inputs)} input files")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
