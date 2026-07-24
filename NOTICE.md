# NOTICE

This repository combines original work with references to third-party data that is
**not redistributed here**. It documents a longitudinal multimodal accessibility
workflow and provides a license-clean synthetic example.

## Original work in this repository

- **Source code** — MIT License (see [`LICENSE`](LICENSE)).
- **Written documentation and original figures** — © the project authors. A content
  license (for example CC BY 4.0) may be applied; see the repository for the current
  choice.
- **Synthetic tutorial fixture** (`samples/tutorial-synthetic/`) — CC0 1.0 public
  domain dedication. It is generated and contains no third-party data.

## Third-party components included, with attribution

### OpenStreetMap

Street-network data in the documented workflow is derived from OpenStreetMap.

> © OpenStreetMap contributors, available under the Open Database License (ODbL) 1.0.

- https://www.openstreetmap.org/copyright
- https://opendatacommons.org/licenses/odbl/1-0/

Any OpenStreetMap-derived data distributed from this repository must retain this
attribution and the ODbL notice.

## Third-party sources used in the research but NOT redistributed here

The following inputs were used to produce the reported results but are not included in
this repository, because their terms do not grant redistribution or because
redistribution permission has not been confirmed. To reproduce the reported results,
obtain them from their providers and run the scripts in this repository. The synthetic
example above reproduces the workflow end to end without them.

| Source | Role | Why not redistributed | How to obtain |
|---|---|---|---|
| KTDB national GTFS | Transit networks | Provider terms; redistribution not confirmed | KTDB data request (see `docs/2-data.qmd`) |
| SGIS census boundaries and population | Origins | Not bundled here; the release ships the synthetic example instead. SGIS content marked 공공누리 (KOGL) Type 1 is redistributable with attribution | SGIS portal |
| Korea Federation of Banks branch status | Destinations | Private association; site asserts all rights reserved | KFB 공시·자료실 › 자료실 › 기타자료 |
| Gyeonggi Data Dream restaurants | Validation destinations | Open-license type not yet confirmed | Gyeonggi Data Dream portal |
| Geocoded branch coordinates | Destination coordinates | Derived via commercial geocoders (Naver Cloud Platform, Kakao, Google) whose terms restrict storage and redistribution | Re-geocode the addresses under a redistribution-permitting service |

This notice records redistribution decisions for a research release. It is not legal
advice and does not itself grant permission to redistribute third-party data.
