# Synthetic fixture schemas

All fixture records are synthetic and released under CC0-1.0.

## `fixture.json`

### Grid definition

| Field | Type | Meaning |
|---|---|---|
| `rows`, `columns` | integer | Connected grid dimensions; their product must be at least 40 for R5 island pruning |
| `base_lat`, `base_lon` | number | Synthetic lower-left coordinate in EPSG:4326 |
| `spacing_degrees` | number | Uniform spacing between adjacent grid vertices |
| `way_tags` | object | OSM-compatible tags assigned to generated row and column ways |

### Origins

| Field | Type | Meaning |
|---|---|---|
| `id` | string | Stable synthetic origin identifier |
| `population` | integer | Synthetic aggregation weight |
| `lon`, `lat` | number | EPSG:4326 point coordinate |

### Destinations

| Field | Type | Meaning |
|---|---|---|
| `id` | string | Stable synthetic destination identifier |
| `opportunities` | integer | Synthetic opportunity count |
| `lon`, `lat` | number | EPSG:4326 point coordinate |

### GTFS

The `gtfs` object provides rows for the standard `agency.txt`, `stops.txt`,
`routes.txt`, `trips.txt`, `stop_times.txt`, and `calendar.txt` tables. Column
names are written unchanged from the JSON objects.

## Generated inputs

| Path | Schema |
|---|---|
| `build/origins.csv` | `id`, `population`, `lon`, `lat` |
| `build/destinations.csv` | `id`, `opportunities`, `lon`, `lat` |
| `build/network/synthetic_gtfs.zip` | Six GTFS tables listed above |
| `build/network/synthetic.osm.pbf` | 49 dense OSM nodes and 14 tagged ways forming a connected 7 x 7 grid |
| `build/manifest.json` | fixture ID, data license, fixture hash, and per-file path, byte size, and SHA-256 |

## Generated results

### `build/results/travel_times.csv`

| Field | Type | Meaning |
|---|---|---|
| `from_id` | string | Origin identifier |
| `to_id` | string | Destination identifier |
| `travel_time_p50` | integer | Median travel time in minutes |
| `mode` | string | `walk` or `transit` |

### `build/results/summary.csv`

| Field | Type | Meaning |
|---|---|---|
| `mode` | string | Routing mode |
| `od_pairs` | integer | Number of returned origin-destination pairs |
| `min_minutes`, `median_minutes`, `max_minutes` | number | Travel-time summary in minutes |
