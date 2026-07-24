# Synthetic routing fixture

This fixture exercises a small accessibility workflow without redistributing
the third-party inputs used in the South Korea case study. It creates a
fictional 7 x 7 street grid, a four-stop bus route, three population-weighted
origins, and three service destinations.

The coordinates give the grid a realistic physical scale but do not identify
real streets, stops, census units, banks, or other facilities.

## Contents

- `fixture.json` — human-readable definition of the entire synthetic system
- `generate_fixture.py` — deterministic GTFS ZIP and OSM PBF generator using
  only the Python standard library
- `run_example.R` — r5r smoke test for walking and public transport
- `SCHEMA.md` — source, generated-input, and output schemas
- `LICENSE.md` — CC0 fixture-data declaration and code-license boundary

Generated files are written under `build/` and are intentionally ignored by
Git. The generator writes sizes and SHA-256 hashes to `build/manifest.json`.

## Generate

From the repository root:

```powershell
python samples/tutorial-synthetic/generate_fixture.py
```

Running the command twice with an unchanged `fixture.json` produces identical
GTFS ZIP, OSM PBF, origin, and destination bytes.

## Run the smoke test

```powershell
& 'C:\Program Files\R\R-4.6.0\bin\Rscript.exe' samples/tutorial-synthetic/run_example.R
```

The R script regenerates the fixture, builds an r5r network, calculates walking
and transit travel-time matrices, and writes compact results under
`build/results/`. The test checks that all nine origin-destination pairs are
reachable for each mode, that the street graph retains walkable edges, that the
GTFS feed produces four stops and at least one route, and that travel times are
finite and non-negative.

## Boundary with the empirical study

This fixture demonstrates file preparation, network building, routing, and
basic aggregation. It does not reproduce South Korean transport schedules,
administrative boundaries, branch locations, empirical estimates, or paper
claims. Those remain governed by the source and evidence manifests in
`infrastructure/`.
