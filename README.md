# Tracking Accessibility Over Time

A reproducible workflow for measuring how accessibility changes over time, for both car
and public transit, using [r5r](https://ipeagit.github.io/r5r/), GTFS, and OpenStreetMap.
The companion book documents the workflow step by step; the scripts implement the full
pipeline behind a study of bank-branch accessibility decline in South Korea (2021–2024).

**Read the book:** <https://juhyeonpark.com/tracking-access-change/>

## Repository layout

- `docs/` — Quarto book source. GitHub Actions renders it to the site above on every
  push to `main`.
- `scripts/` — the analysis pipeline, from raw-data preparation through routing,
  scaling, decomposition, and policy scenarios. `scripts/README.md` is the canonical
  execution map.
- `samples/tutorial-synthetic/` — a license-clean synthetic fixture (CC0): a generated
  street grid, bus route, origins, and destinations for running the routing examples
  without any restricted data.
- `NOTICE.md` — third-party data sources, required attribution, and what is not
  redistributed here.

## Quick start

Generate the synthetic fixture and run the routing smoke test (requires R, the `r5r`
package, and Java 21):

```bash
python samples/tutorial-synthetic/generate_fixture.py
Rscript samples/tutorial-synthetic/run_example.R
```

Then follow the book from [Data Preparation](https://juhyeonpark.com/tracking-access-change/2-data.html)
onward to build the same workflow for your own study area.

## Data availability

The original transit (KTDB GTFS), street-network (OpenStreetMap extracts), census
(SGIS), and bank-branch (Korea Federation of Banks) inputs are not redistributed;
`NOTICE.md` lists each source and how to obtain it. Figures and tables in the book are
aggregate results. The question of *why* access changed, decomposing observed change
into service and network components, is developed in a companion paper; `scripts/6-*`
and `scripts/7-*` implement that extension.

## License

Code and original documentation are released under the [MIT License](LICENSE). The
synthetic fixture is dedicated to the public domain (CC0; see
`samples/tutorial-synthetic/LICENSE.md`). Third-party data retain their original terms.
