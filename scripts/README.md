# Analysis pipeline

Run scripts from the repository root. Set `ACCESS_DECLINE_ROOT` only when running from a
different working directory. Large routing and analysis outputs remain under `workflows/`
and are not versioned.

## Canonical stages

| Stage | Script | Main output or role |
|---|---|---|
| 1 | `1-preprocess-raw.R` | Prepared annual GTFS and OSM inputs |
| 2--4 | `2-pilot-prepare.R`, `3-pilot-analyze.R`, `4-scaling-routing.R` | Public workflow examples |
| 5 | `5-1-main-routing.R`, `5-2-main-analyze.R` | Annual observed travel-time matrices and baseline accessibility |
| 6.1 | `6-1-decompose-routing.R` | Hybrid network--branch travel-time matrices for all ordered year pairs |
| 6.2 | `6-2-decompose-analyze.R` | National and district component decomposition |
| 6.2 checks | `6-2-decompose-metrics.R`, `6-2-decompose-pilot-extended.R`, `6-2-decompose-pilot-aging.R` | Metric, threshold, and demographic sensitivity |
| 6.3--6.4 | `6-3-decompose-demographics-pilot.R`, `6-4-decompose-typology-figures.R` | District models and typology |
| 7.0 | `7-0-policy-pilot.R` | Last-branch policy scenario |
| 7.1 | `7-1-manuscript-figures.R` | Current manuscript figures |
| 7.2--7.3 | `7-2-gtfs-verification.R`, `7-3-gtfs-slot-frequencies.R` | Busan and Daegu feed checks |
| 7.4 | `7-4-decompose-order-averaged.R` | Two-factor order-averaged attribution |

`_bank_locations.R` is an active shared helper. `migrations/` preserves the 2026 location-
ID migration. `archives/` contains superseded machine-specific runs and exploratory
diagnostics and must not be treated as the canonical pipeline.

## Machine configuration

The canonical counterfactual router requires Java 21 and reads:

- `JAVA_HOME`
- `ACCESS_DECLINE_BASE_YEARS` (default `2021,2022,2023`)
- `ACCESS_DECLINE_THREADS` (default `4`)
- `ACCESS_DECLINE_JVM_HEAP` (default `-Xmx10G`)

The main observed router additionally accepts `ACCESS_DECLINE_RAM_GB` and
`ACCESS_DECLINE_SYSTEM_BUFFER_GB`. Do not add machine-specific absolute paths to scripts.

## Safety

Routing scripts write many gigabytes. Verify the project root and output paths before a
full run. Do not delete `workflows/results/` or rebuild all networks merely to reproduce a
downstream table; the derived analysis scripts are designed to reuse stored matrices.
