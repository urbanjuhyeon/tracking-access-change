"""Generate a deterministic, entirely synthetic r5r fixture.

Fixture data: CC0-1.0. Generator code: MIT.
"""

from __future__ import annotations

import argparse
import csv
import hashlib
import io
import json
import struct
import zipfile
from pathlib import Path
from typing import Iterable


FIXED_ZIP_TIME = (1980, 1, 1, 0, 0, 0)


def encode_varint(value: int) -> bytes:
    if value < 0:
        raise ValueError("varint input must be non-negative")
    output = bytearray()
    while True:
        byte = value & 0x7F
        value >>= 7
        output.append(byte | (0x80 if value else 0))
        if not value:
            return bytes(output)


def zigzag(value: int) -> int:
    return (value << 1) ^ (value >> 63)


def field_key(number: int, wire_type: int) -> bytes:
    return encode_varint((number << 3) | wire_type)


def varint_field(number: int, value: int) -> bytes:
    return field_key(number, 0) + encode_varint(value)


def sint_field(number: int, value: int) -> bytes:
    return varint_field(number, zigzag(value))


def bytes_field(number: int, value: bytes) -> bytes:
    return field_key(number, 2) + encode_varint(len(value)) + value


def packed_uint_field(number: int, values: Iterable[int]) -> bytes:
    payload = b"".join(encode_varint(value) for value in values)
    return bytes_field(number, payload)


def packed_sint_field(number: int, values: Iterable[int]) -> bytes:
    payload = b"".join(encode_varint(zigzag(value)) for value in values)
    return bytes_field(number, payload)


def make_blob_frame(blob_type: str, payload: bytes) -> bytes:
    blob = bytes_field(1, payload)  # Blob.raw
    header = bytes_field(1, blob_type.encode("utf-8")) + varint_field(3, len(blob))
    return struct.pack(">I", len(header)) + header + blob


def expand_grid(fixture: dict) -> tuple[list[dict], list[dict]]:
    grid = fixture["grid"]
    rows = int(grid["rows"])
    columns = int(grid["columns"])
    base_lat = float(grid["base_lat"])
    base_lon = float(grid["base_lon"])
    spacing = float(grid["spacing_degrees"])
    common_tags = {key: str(value) for key, value in grid["way_tags"].items()}

    if rows * columns < 40:
        raise ValueError("The connected synthetic grid must contain at least 40 vertices for R5.")

    def node_id(row: int, column: int) -> int:
        return row * columns + column + 1

    nodes = [
        {
            "id": node_id(row, column),
            "lat": base_lat + row * spacing,
            "lon": base_lon + column * spacing,
        }
        for row in range(rows)
        for column in range(columns)
    ]

    ways = []
    for row in range(rows):
        tags = dict(common_tags, name=f"Synthetic Row {row + 1}")
        ways.append(
            {
                "id": 1000 + row,
                "nodes": [node_id(row, column) for column in range(columns)],
                "tags": tags,
            }
        )
    for column in range(columns):
        tags = dict(common_tags, name=f"Synthetic Column {column + 1}")
        ways.append(
            {
                "id": 2000 + column,
                "nodes": [node_id(row, column) for row in range(rows)],
                "tags": tags,
            }
        )

    return nodes, ways


def make_osm_pbf(fixture: dict) -> bytes:
    nodes, ways = expand_grid(fixture)
    node_ids = {node["id"] for node in nodes}
    if len(node_ids) != len(nodes):
        raise ValueError("OSM node IDs must be unique")
    for way in ways:
        missing = set(way["nodes"]) - node_ids
        if missing:
            raise ValueError(f"Way {way['id']} references missing nodes: {missing}")

    strings = [""]
    string_index = {"": 0}
    for way in ways:
        for key, value in way["tags"].items():
            for item in (key, str(value)):
                if item not in string_index:
                    string_index[item] = len(strings)
                    strings.append(item)

    string_table = b"".join(bytes_field(1, item.encode("utf-8")) for item in strings)

    granularity = 100
    dense_ids = []
    dense_lats = []
    dense_lons = []
    previous_id = 0
    previous_lat = 0
    previous_lon = 0
    for node in nodes:
        lat = round(float(node["lat"]) * 1_000_000_000 / granularity)
        lon = round(float(node["lon"]) * 1_000_000_000 / granularity)
        dense_ids.append(int(node["id"]) - previous_id)
        dense_lats.append(lat - previous_lat)
        dense_lons.append(lon - previous_lon)
        previous_id = int(node["id"])
        previous_lat = lat
        previous_lon = lon

    dense_nodes = (
        packed_sint_field(1, dense_ids)
        + packed_sint_field(8, dense_lats)
        + packed_sint_field(9, dense_lons)
        + packed_uint_field(10, [0] * len(dense_ids))
    )
    primitive_group = bytearray(bytes_field(2, dense_nodes))

    for way in ways:
        keys = [string_index[key] for key in way["tags"]]
        vals = [string_index[str(value)] for value in way["tags"].values()]
        references = []
        previous = 0
        for node_id in way["nodes"]:
            references.append(int(node_id) - previous)
            previous = int(node_id)
        way_message = (
            varint_field(1, int(way["id"]))
            + packed_uint_field(2, keys)
            + packed_uint_field(3, vals)
            + packed_sint_field(8, references)
        )
        primitive_group.extend(bytes_field(3, way_message))

    primitive_block = (
        bytes_field(1, string_table)
        + bytes_field(2, bytes(primitive_group))
        + varint_field(17, granularity)
        + varint_field(18, 1000)
    )
    header_block = (
        bytes_field(4, b"OsmSchema-V0.6")
        + bytes_field(4, b"DenseNodes")
        + bytes_field(16, b"unpacking-access-decline synthetic fixture")
        + bytes_field(17, b"synthetic; CC0-1.0")
    )
    return make_blob_frame("OSMHeader", header_block) + make_blob_frame(
        "OSMData", primitive_block
    )


def csv_bytes(rows: list[dict]) -> bytes:
    if not rows:
        raise ValueError("CSV source must contain at least one row")
    buffer = io.StringIO(newline="")
    writer = csv.DictWriter(buffer, fieldnames=list(rows[0].keys()), lineterminator="\n")
    writer.writeheader()
    writer.writerows(rows)
    return buffer.getvalue().encode("utf-8")


def make_gtfs_zip(gtfs: dict) -> bytes:
    entries = {
        "agency.txt": csv_bytes(gtfs["agency"]),
        "stops.txt": csv_bytes(gtfs["stops"]),
        "routes.txt": csv_bytes(gtfs["routes"]),
        "trips.txt": csv_bytes(gtfs["trips"]),
        "stop_times.txt": csv_bytes(gtfs["stop_times"]),
        "calendar.txt": csv_bytes(gtfs["calendar"]),
    }
    output = io.BytesIO()
    with zipfile.ZipFile(output, "w") as archive:
        for name in sorted(entries):
            info = zipfile.ZipInfo(name, date_time=FIXED_ZIP_TIME)
            info.compress_type = zipfile.ZIP_DEFLATED
            info.external_attr = 0o100644 << 16
            archive.writestr(info, entries[name])
    return output.getvalue()


def write_bytes(path: Path, payload: bytes, root: Path) -> dict:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_bytes(payload)
    return {
        "path": path.relative_to(root).as_posix(),
        "bytes": len(payload),
        "sha256": hashlib.sha256(payload).hexdigest(),
    }


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--output",
        type=Path,
        default=None,
        help="Generated-output directory (default: build beside this script)",
    )
    args = parser.parse_args()

    fixture_dir = Path(__file__).resolve().parent
    fixture_path = fixture_dir / "fixture.json"
    output_dir = (args.output or fixture_dir / "build").resolve()
    fixture = json.loads(fixture_path.read_text(encoding="utf-8"))

    files = []
    files.append(
        write_bytes(
            output_dir / "network" / "synthetic.osm.pbf",
            make_osm_pbf(fixture),
            output_dir,
        )
    )
    files.append(
        write_bytes(
            output_dir / "network" / "synthetic_gtfs.zip",
            make_gtfs_zip(fixture["gtfs"]),
            output_dir,
        )
    )
    files.append(
        write_bytes(
            output_dir / "origins.csv", csv_bytes(fixture["origins"]), output_dir
        )
    )
    files.append(
        write_bytes(
            output_dir / "destinations.csv",
            csv_bytes(fixture["destinations"]),
            output_dir,
        )
    )

    manifest = {
        "fixture_id": fixture["fixture_id"],
        "license": fixture["license"],
        "fixture_sha256": hashlib.sha256(fixture_path.read_bytes()).hexdigest(),
        "files": files,
    }
    manifest_path = output_dir / "manifest.json"
    manifest_path.write_text(
        json.dumps(manifest, indent=2, sort_keys=True) + "\n", encoding="utf-8"
    )

    print(f"generated={len(files)}")
    print(f"manifest={manifest_path}")


if __name__ == "__main__":
    main()
