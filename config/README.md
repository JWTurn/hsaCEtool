# Settings (config/)

The pipeline is set up for a study by a short settings file. You do not need to
edit any R code to run it for a new herd or data set.

## Files

| File | What it is | Shared in git? |
|---|---|---|
| `default.yaml` | Every setting, its default value and what it means | yes |
| `<project>.yaml` | One per study (e.g. `NMC.yaml`): only the settings that differ from the defaults | yes |
| `local.yaml` | Settings for your computer only, such as folder paths | no |
| `<project>.R` | Optional, for programmers: changes settings with R code | yes |

They are combined in that order, so later files win. A section such as
`tracks:` is combined setting by setting. A list (for example
`cleaning: steps: [...]`) is replaced whole.

## Starting a new project

1. Copy `NMC.yaml` to `<project>.yaml` and change the locations, years and
   other settings.
2. Add a section for it to `_targets.yaml` saying where its results are stored.
3. In `run.R`, set `Sys.setenv(TAR_PROJECT = '<project>')` and run.

## Inputs: local files or URLs

Each input is a *source*: either a `path` to a file on your computer, or a
`url` to download (a web or Google Drive link). Downloads use
`reproducible::prepInputs()`. They are
saved in `paths: inputs` and are only downloaded again if that copy is missing
or incomplete (`prepInputs()` does not check the web for a newer version). To
get a newer version, add `refresh: true` to that source for one run: it is
downloaded afresh and replaces the kept copy. Remove it afterwards.

```yaml
locations:
  sources:
    - type: file
      url: "https://drive.google.com/file/d/.../view"
      target_file: clean_NMC.RDS     # the file wanted from the download
    - type: file
      path: "BC/BC_NMC_Telemetry.csv"  # relative to paths: data_in
      columns: {id: animal_id, datetime: timestamp, x: longitude, y: latitude}
      input_crs: "EPSG:4326"
    - type: movebank
      study: "Klaza caribou"
      user: jwturner
```

Movebank passwords are never written in the settings. Save yours once with
`move2::movebank_store_credentials("<user>")`, which keeps it in your computer's
keyring.

All times are stored in UTC. A source whose times are local clock time gives
its time zone as `tz` (a name from the time zone database, e.g.
`America/Whitehorse`, which follows Yukon daylight saving until it ended in
November 2020). If the date and the hour are in separate columns, name the hour
column too; the date column is then `datetime`:

```yaml
    - type: file
      url: "..."
      target_file: fixes.shp
      columns: {id: Collar_ID, datetime: Date_Time, hour: Hour}
      tz: America/Whitehorse   # leave out if the times are already UTC
```

Check a source's times before trusting its stated time zone. Two checks
have helped: collar temperature (warmest mid-afternoon, about 22-23 h UTC in
this region) and, for collars on a fixed schedule, the gap between fixes
across a daylight-saving change. In local clock time a 13 h schedule kept by
a UTC clock shows 14 h across the spring change and 12 h in the fall. The
Prairie Creek 2015-18 file shows the reverse (12 h in spring, 14 h in fall),
which fits UTC times from a collar scheduled by local time, so its hours are
read as UTC.

Some older files have dates but no times of day (in NMC, Chisana and
Redstone). Mark such a source `date_only: true`: its fixes are given noon UTC
on their date and a `date_only` column. They are cleaned only with
`cleaning: date_only_steps:` (by default `bounds` and `exclude_points`), help
shape the study area, and are then left out of tracks and steps.

### B.C. telemetry (`type: bc`)

The B.C. caribou telemetry comes as overlapping exports. A `type: bc` source
combines them (`read_bc_locations()`); each part is a source as above:

```yaml
    - type: bc
      name: BC
      new: {url: "...", target_file: Caribou_Telem.shp}               # recent export: the base
      old: {url: "...", target_file: BC_NMC_Telemetry.csv}            # optional
      reference: {url: "...", target_file: nmc_combined_data_cleaned.csv}  # optional
      params: {local_tz: America/Vancouver, match_m: 2, max_shift_h: 48}  # defaults
```

The exports' times are not what they say. Collar temperatures (warmest in the
mid-afternoon, about 22-23 h UTC) and the same collars on Movebank show that:

- **New file:** the clock time in `Date` is UTC; the -0700/-0800 offset some
  rows carry is wrong and ignored.
- **Reference file:** its `timestamp` is UTC (`Acquisition_Date` is shifted).
  Where it has the same collar's fix at the same position exactly 7 or 8 hours
  earlier than the new file's time, its time is used: about 250 fixes in the
  2025 export are 7 hours late.
- **Old file:** its times are UTC plus the Pacific offset (`local_tz`; 7 hours
  in daylight time, 8 in standard time), which is subtracted. Caribou only,
  duplicates removed. Fixes also in the new file
  (same herd, within `match_m` metres, same time) give it their mortality flag.
  Fixes at the same position as a new fix but at another time (within
  `max_shift_h` hours; mostly Lotek collars) give their flag **and their time**:
  the old times were consistent, while the new export often had two positions
  at one time. Fixes only in the old file are added.
- Each fix records `bc_file` (`new` or `old`) and `time_note` (how its time
  was set), and `time_clash` marks fixes that share their animal and time with
  another fix, for the cleaning steps to resolve. The animal ID is `Anml_ID`
  (or `collar_<Cllr_ID>`). The target `locations_source_<n>_log` counts each step.

Where sources overlap (e.g. Little Rancheria collars on Movebank and in the B.C.
export), list the source that should win first: fixes another, earlier-listed
source also has are flagged in `dup_source` (`locations: source_duplicates:`),
and the cleaning step `source_duplicates` removes them.

### Cleaning

Cleaning first removes exact duplicates (same animal, time and position), then
runs the steps in `locations: cleaning: steps:` in order (the available steps
are listed in `default.yaml`). Fixes of one animal at the same time but at
different positions are left for the `duplicates` step, which keeps the fix
with the best DOP (dilution of precision, a GPS accuracy score). If any remain
after the steps, for example because `duplicates` is not listed, the first of
each is kept with a warning, so each animal has one fix per time.

- The target `cleaning_log` counts the fixes each step removed. The cleaned
  locations also carry an attribute `removed`, naming the step that removed
  each fix (by its row in `locations_raw`).
- The target `mortality_check` counts, by source and animal, the fixes flagged
  as mortalities (column `mortality`, e.g. from the old B.C. export) that each
  step removed, and those kept. It is empty if no source has the column.
- Two steps flag possible dead animals or dropped collars without removing
  anything. `clusters` uses amt's rule (`defunct_cluster_`): it flags
  everything after the last stretch of short steps lasting `theta_hours`,
  even if the animal moves on later, so it also flags many live animals.
  `end_clusters` (`end_cluster_`) flags only the end of a track that stays
  within `zeta` metres for at least `theta_hours`; it also catches a collar
  picked up and taken elsewhere (e.g. to an office): two stationary stretches
  joined by up to `transit_fixes` fixes on the move, or a stationary stretch
  followed by up to `transit_fixes` fixes before the data end (picked up and
  switched off). Known offices can also go in `exclude_points`.
- `end_clusters` also adds `mortality_status`: `likely` (a `mortality` flag
  in a stationary end of track), `unknown` (a stationary end with no
  mortality flag), `flag only` (a mortality flag elsewhere; in B.C. several
  such animals moved on for years, so these look like false alarms). The
  target `flag_summary` counts, by source, the fixes each rule flags, both,
  those with a `mortality` flag, and each `mortality_status`.

The comparison behind these rules (counts only) is in
`inputs/bc_compare/BC_telemetry_comparison.md`, kept with the downloads rather
than in git.

## Landscape layers: `layers_<project>.csv`

Each landscape layer is one row of a table named in `landscape: layers:`. To add
a layer, add a row. Each layer's values are extracted at the start and/or end of
every step (`extract: where:`). They become columns `<name>_start` and
`<name>_end` in the final table, `issa_data`.

| Column | Meaning |
|---|---|
| `name` | Column name for the layer, e.g. `prop_water_1000` |
| `family`, `resolution` | For your reference (e.g. `landcover`, `1000`) |
| `first_year`, `last_year` | Years that have a raster. Leave blank for a layer that doesn't change (e.g. elevation) |
| `path` | A local file, relative to `paths: data_in`. For a yearly layer, `{year}` marks where the year goes: `derived/harv1000/harv1000_{year}.tif` |
| `url`, `target_file`, `archive` | Or a download (`{year}` allowed) |
| `time_since` | `TRUE` if the raster holds the year of a disturbance (fire, harvest). Adds `ts_<name>_start/end`, the years since that disturbance |
| `ts_nodata` | Years since, used where there was no disturbance (e.g. 200 for fire) |
| `build` | Instead of `path` or `url`: the pipeline builds the layer from source data (see below). Leave blank for existing rasters |

Each step gets the raster for its year. Steps after `last_year` use the last
raster, and steps before `first_year` use the first.

## Layers the pipeline builds

A row with `build` is made by the pipeline on the **template rasters**, so all
built layers line up cell for cell and are in the project's `crs` (distances in
true metres). The templates come from `landscape: template:`:

- `base_res: 30` is the size of the finest cells (the 30 m Landsat-based inputs).
- `factors: [8, 40, 160]` says how many fine cells along each side make one
  coarser cell: 8 x 30 m = 240 m, 40 x 30 m = 1200 m and 160 x 30 m = 4800 m.
  Five 240 m cells fit exactly along a 1200 m cell, and four 1200 m cells along
  a 4800 m cell.
- `buffer_m: 50000` is how far beyond the study area the layers extend.
- `source:` can give your own finest template raster instead. A programmer can
  instead set `cfg$landscape$template$fun` in `<project>.R` to a function of
  `(study_area, tmpl, crs)` that returns one.

A built layer's `resolution` must be `base_res` times one of the factors (e.g.
240), and it needs `first_year` and `last_year`.

Distances (to harvest and human disturbance) are measured over the template
plus `landscape: distance_margin_m` (default 10 km) on every side, so a road or
cutblock just outside the template still counts.

National rasters are read with `landscape: access:` either `stream` (only the
part covering the study area is read from the web; nothing is stored) or
`download` (each whole file is saved once to `paths: input_cache`, which several
projects can share, like `reproducible.inputPaths`).

### Harvest (`build: harvest_*`)

Built from the Canada Landsat Disturbance maps (CanLaD, 30 m): the 1965-1984
maps (disturbance type 3 = harvest, with the year from the matching year map)
and the annual maps from 1985 (value 2 = harvesting). Settings are under
`landscape: harvest:`. "Recent" harvest means the last `recent_years` years,
that year included (default 2).

| `build` | Each cell holds |
|---|---|
| `harvest_year` | The most recent harvest year up to that year, in any of its 30 m cells (blank = never harvested). Use with `time_since: TRUE` for years since harvest |
| `harvest_pa` | 1 if any of its 30 m cells had recent harvest, otherwise 0 |
| `harvest_dens` | The proportion of its 30 m cells with recent harvest |
| `harvest_dist` | The mean distance (m) from its 30 m cells to the nearest recently harvested 30 m cell (including those in the margin) |

### Land cover (`build: landcover` or `landcover_cell`)

Yearly proportion of each land-cover class, from SCANFI v3 (1985-2025) or NTEMS
VLCE2 (1984-2022), chosen with `landscape: landcover: source:`. The classes are
kept as the source defines them (listed, with their codes, under
`landscape: landcover: scanfi: classes:` and `ntems: classes:`), so they can be
combined later in a biologically meaningful way. Each row names its class in
the `class` column, e.g. `treed_coniferous_lichen`.

| `build` | Each cell holds |
|---|---|
| `landcover_cell` | The proportion of its classified 30 m cells in that class |
| `landcover` | At resolutions with a moving window (`landscape: landcover: window_m:`, default 500 m radius at 240 m), the mean of those proportions within that circle around the cell, as in `make_landforest_prop()`. At other resolutions, the same as `landcover_cell` |

The cell proportions are always saved too (in
`outputs/<project>/landscape/landcover/`), e.g. for masking water or rock.

Reading SCANFI from NRCan is slow (about 1 MB/s per connection; each yearly
file is 3.4 GB). With `access: download`, each file is fetched in 8 pieces at
once.

### Human disturbance (`build: disturb_*`)

Roads, trails, seismic lines, mines, clearings and other human disturbance,
from the sources listed under `landscape: disturbance: sources:` (for NMC:
GeoYukon surface disturbance, and the BC and NWT compilations; see
`NMC.yaml`). Each feature is sorted into a category, named in the row's
`class` column (for NMC: paved, unpaved, lfother, placer, quartz, othermines,
otherpoly). A feature counts from the year it was built; one without a year
counts in every year.

| `build` | Each cell holds, counting every feature built up to that year |
|---|---|
| `disturb_pa` | 1 if any of its 30 m cells is touched by a feature of that category, otherwise 0 |
| `disturb_dens` | The proportion of its 30 m cells touched |
| `disturb_dist` | The mean distance (m) from its 30 m cells to the nearest touched 30 m cell (including those in the margin) |

Each source is a download (`url`: a file, a zip, or a Google Drive folder) and
the `files` wanted from it. Each file has:

- `category`: one category for all its features, or `rules`: a list of
  `{where: {field: value(s)}, category: ...}`, checked in order; the first
  that matches gives the category. A rule without `where` catches the rest.
  Category `drop` leaves features out (e.g. harvest, which comes from CanLaD).
- `year`: fields giving the year built, in order of preference (a year or a
  date). None, or all empty: the feature counts in every year.
- `lookup`: optionally, columns added from a table in `landscape: disturbance:
  lookups:` (e.g. a reclassification spreadsheet), matched on some fields:
  `{table: ..., sheet: ..., by: {feature field: table column}, add: column}`.
  Types missing from the table are listed in a warning and sorted by the
  remaining rules.

Each download is kept in `paths: input_cache` and is not downloaded again
(`prepInputs()` does not check the web for a newer version). To pick up an
update (e.g. a new GeoYukon release), add `refresh: true` to that source or
lookup for one run: it is downloaded afresh, and the layers are only rebuilt
if its features changed. Remove it afterwards. Google Drive
downloads use the saved login of `run: google_email` (set it in `local.yaml`
and run `googledrive::drive_auth(email = "<it>")` once).

Layers are only built for the first year and the years in which features were
added; the other years reuse the layer of the year before.

## Running steps in parallel

Set `run: workers:` (usually in `local.yaml`) above 1 to run independent steps,
such as each year of a layer, at the same time in separate R sessions (needs
the crew package). Each worker uses memory and temporary disk space, so start
with 2-4. `run: memfrac:` is the share of the computer's memory the workers
share for raster work.

## Programmer settings: `<project>.R`

The file must end with a function that takes the settings and returns them.
For example, to add your own cleaning step:

```r
function(cfg) {
  cfg$locations$cleaning$custom$drop_herd <- function(DT, params, crs) {
    DT[herd != params$herd]
  }
  cfg$locations$cleaning$steps <- c(cfg$locations$cleaning$steps, 'drop_herd')
  cfg$locations$cleaning$params$drop_herd <- list(herd = 'Pelly')
  cfg
}
```

---
Drafted with assistance from Claude (Claude Code).
