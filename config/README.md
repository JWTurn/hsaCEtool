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

### Premade or remade

Each stage can start from files made earlier instead of being remade:

- `locations: premade:` a cleaned locations file saved from the
  `locations_clean` target (.rds). The `sources` are then not read or cleaned;
  the file keeps each fix's `source`, the flag and `mortality_status` columns
  and its `cleaning_log`. It is downloaded to `inputs/premade/`, so an older
  file of the same name in `inputs/` is not mistaken for it.
- `locations: study_area: source:` and `study_area: extended: source:` existing
  study area polygons (a zipped shapefile from a url needs `archive:` too).
- `landscape: template: source:` and the layer table's `path`/`url` columns:
  existing template and layer rasters.

Set to `null` (or blank in the layer table, with a `build`), each is remade.
`NMC.yaml` has commented lines to start from the NMC files saved on the
restricted shared drive.

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
- Two steps flag possible dead animals or dropped collars. `clusters` uses
  amt's rule (`defunct_cluster_`) and only flags: it flags
  everything after the last stretch of short steps lasting `theta_hours`,
  even if the animal moves on later, so it also flags many live animals.
  `end_clusters` (`end_cluster_`) flags only the end of a track that stays
  within `zeta` metres for at least `theta_hours`; it also catches a collar
  picked up and taken elsewhere (e.g. to an office): two stationary stretches
  joined by up to `transit_fixes` fixes on the move, or a stationary stretch
  followed by up to `transit_fixes` fixes before the data end (picked up and
  switched off). Known offices can also go in `exclude_points`.
  Its first flagged fix, where the animal stopped, is marked `end_of_track_`.
- The step `drop_end_clusters` (listed after `end_clusters`) removes the
  stationary end of each track except that first fix, which becomes the
  track's last fix. Tracks then hold only fixes from when the animal was
  likely alive. `locations_raw` still has every fix.
- `end_clusters` also adds `cluster_status`, for each fix: `mortality-cluster`
  (a `mortality` flag in a stationary end of track), `unknown-cluster` (a
  stationary end with no mortality flag), `flag-only` (a mortality flag
  elsewhere; in B.C. several such animals moved on for years, so these look
  like false alarms, and they are not used for `mortality_status`).
- `end_clusters` also gives each animal a `mortality_status`, its fate at the
  end of its track, and a `mortality_basis` saying how it was decided. The
  first rule that applies wins (the target `mortality_rules` holds this
  table):

  | Rule | `mortality_status` | Evidence | Kind |
  |---|---|---|---|
  | 1 | `dead` | Movebank `deployment_end_type` is dead | recorded |
  | 2 | `dead` | a `mortality` flag within the stationary end of the track | recorded |
  | 3 | the `deployment_end_type` (e.g. `equipment-failure`, `removal`, `captured`, `unknown`) | Movebank | recorded |
  | 4 | `unknown` | the track ends in a stationary stretch, no recorded outcome | inferred |
  | 5 | `alive` | the track does not end in a stationary stretch, no recorded outcome | inferred |

  Inferred outcomes come from this cluster analysis only: they are our best
  guess about the end of the track, not an observed fate. Movebank's own
  `unknown` (rule 3) and the inferred `unknown` (rule 4) are told apart by
  `mortality_basis`.
- The target `flag_summary` counts, by source, the fixes flagged by amt's
  rule, with a `mortality` flag, or both; the fixes removed by
  `drop_end_clusters` (and those with a mortality flag); and the animals with
  each `mortality_status` and `mortality_basis`.

The comparison behind these rules (counts only) is in
`inputs/bc_compare/BC_telemetry_comparison.md`, kept with the downloads rather
than in git.

### Records kept outside the location data

Three cleaning steps correct animals from records kept apart from the
locations, and say what they did in the target `animal_decisions` (one row per
animal and action, with the number of fixes and the reason; IDs only, no dates
or positions). It is written to `outputs/<project>/decisions/animal_decisions.csv`,
restricted storage that is not in git, to report back to the data owners.

- `captures` uses a capture log (`locations: captures:`; for NMC the B.C. WLRS
  log, matched on collar serial) of which animal each collar was put on, and
  when. It acts only where the log is clear: fixes filed under an animal the
  log shows wearing the collar first, after the collar was put on another
  animal, are moved to that animal (e.g. a collar redeployed after a death but
  still filed under the dead animal). Fixes from before an animal's logged
  capture, and fixes the log gives to another animal, are kept and logged to
  check. It also fills missing `sex` from the log (B.C. `FALSE` is read as
  female, as Excel turns F into FALSE) and adds `deployment_start`, so
  `capture_effect` drops the first days after each capture. Dates written with
  slashes are read in `date_order` (`dmy` or `mdy`).
- `mortality_records` uses recorded deaths (`locations: mortality_records:`;
  for NMC the B.C. mortality table, matched on WLH ID): the animal's
  `mortality_status` becomes `dead` (the first rule in `mortality_rules`) and
  fixes more than `after_days` (default 1) after the death are dropped.
  Records for animals not in the data are skipped with a warning.
- `long_gaps` splits a track at a silence of more than `max_gap_days`
  (default 365) into separate animals (`id`, `id_2`, ...), as it usually joins
  two deployments, and drops pieces of `min_fixes` (default 3) fixes or fewer
  as stray fixes.

The target `animal_lookup` (also in `outputs/<project>/decisions/`) links each
animal across recaptures, new IDs, collars and study periods: captures from
the capture log with the fixes under our IDs, animals with more than one
collar but no logged recapture, and Movebank animals with more than one
deployment.

### After cleaning: columns kept, sex, study and herd

`locations_clean` keeps every column. The target `locations` (the fixes used
for tracks) keeps only `id`, `source`, `t_`, `x_`, `y_` and the columns in
`locations: keep_after_cleaning:` (by default `mortality_status`,
`mortality_basis`, `deployment_end_type`, `sex`, `juris`, `study` and `herd`), so tracks
and steps stay small. Columns listed but not in the data are skipped; `[]`
keeps every column. Before that:

- `sex` is coded `f`, `m` or blank (NA), whatever the source used (`Female`,
  `F`, `f`; `Unknown` becomes blank), and filled from a `Gender` column where
  `sex` is missing.
- `juris` is the jurisdiction the data come from, in lower case (e.g. `bc`,
  `yt`, `nt`), set for each source in `locations: juris: by_source:` (`source
  name: juris`), or taken from `juris: columns:` as for `herd`.
- `study` is taken from the first column in `locations: study: columns:` that
  has a value (as for `herd` below; e.g. B.C. fixes, which have no study, use
  their herd), otherwise from `study: by_source:` (`source name: study`, e.g. to
  join two sources from one study), otherwise it is the source's name.
- `herd` is taken from the first column in `locations: herd: columns:` that has
  a value, otherwise from `herd: by_source:` (`source name: herd`). A column can
  be used for some sources only: `{column: individual_comments, sources:
  [TayRiver]}`. Movebank studies can hold the herd in `group_id`,
  `study_site` or `individual_comments`.
- `study` and `herd` are written in CamelCase ("Pink Mountain" becomes
  `PinkMountain`). `herd: rename:` (`old: new`, both in CamelCase) then gives a
  herd the same name in every source. The target `herd_check` counts animals by
  source, jurisdiction, study and herd, to check the names.

## Landscape layers: `layers_<project>.csv`

Each landscape layer is one row of a table named in `landscape: layers:`. To add
a layer, add a row. Each layer's values are extracted at the start and/or end of
every step (`extract: where:`), as columns `<name>_start` and `<name>_end`.
The table used to fit models will be made from these, with only the layers a
model uses (still to come).

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

Extraction runs one year at a time (target `extracted`, one branch per year):
that year's raster of every layer is read at that year's step starts and ends,
layers on the same grid together. Each year is saved as
`outputs/<project>/extracted/extracted_<year>.parquet` (Parquet stores data by
column, so later steps read only the columns they need). Each file holds `row`,
the step's row number in `steps`, and every layer's values; like the steps, these
files are confidential. Changing any layer re-extracts every year.

To help choose and combine covariates, the target `availability` (also saved as
`outputs/<project>/availability.csv`) summarizes every layer's start and end
values for used and available steps: number of steps, min, mean, median, max,
share above 0 and share missing. Each layer is summarized overall, then split by
the columns in `extract: availability_by:` (by default jurisdiction, then study,
then herd; "all" where not split). It holds summaries only, no positions or
times.

## Layers the pipeline builds

A row with `build` is made by the pipeline on the **template rasters**, so all
built layers line up cell for cell and are in the project's `crs` (distances in
true metres). The templates come from `landscape: template:`:

- `base_res: 30` is the size of the finest cells (the 30 m Landsat-based inputs).
- `factors: [8, 40, 160]` says how many fine cells along each side make one
  coarser cell: 8 x 30 m = 240 m, 40 x 30 m = 1200 m and 160 x 30 m = 4800 m.
  Five 240 m cells fit exactly along a 1200 m cell, and four 1200 m cells along
  a 4800 m cell.
- The templates cover the extended study area (target `study_area_extended`),
  set by `locations: study_area: extended:`: the study area wrapped in a
  convex hull (`convex_hull: true`) and buffered by `buffer_m: 50000` (50 km),
  so it is one continuous area, as in NMC_CEtool's
  `3-prep-land-UPDATED.R`. Layers then include features beyond where the
  animals were and avoid edge effects. `template: buffer_m` (0 by default)
  can add a further margin.
- `source:` can give your own finest template raster instead. A programmer can
  instead set `cfg$landscape$template$fun` in `<project>.R` to a function of
  `(study_area, tmpl, crs)` that returns one.

A built layer's `resolution` must be `base_res` times one of the factors (e.g.
240). Its `first_year` and `last_year` can be left blank: it is then built for
`tracks: min_year` to `max_year`, or, where those are `null`, for the first to
the last year of the steps that come out of track preparation (target
`step_years`; the layers then wait for the tracks). Fill them in for a layer
whose source covers fewer years (e.g. harvest from CanLaD ends in 2024).
Elevation and northness do not change by year: leave both blank.

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
that year included (default 2). A `harvest_pa`, `harvest_dens` or
`harvest_dist` row can give its own number of years in its `class` column (e.g.
`10`), so several windows can be built side by side.

| `build` | Each cell holds |
|---|---|
| `harvest_year` | The most recent harvest year up to that year, in any of its 30 m cells (blank = never harvested). Use with `time_since: TRUE` for years since harvest |
| `harvest_pa` | 1 if any of its 30 m cells had recent harvest, otherwise 0 |
| `harvest_dens` | The proportion of its 30 m cells with recent harvest |
| `harvest_dist` | The mean distance (m) from its 30 m cells to the nearest recently harvested 30 m cell (including those in the margin) |

### Fire (`build: fire_*`)

Built from the National Burned Area Composite (NBAC, all fires 1972-2025 in
one file) and, for earlier fires, the National Fire Database polygons (NFDB
"pre-1972"; `landscape: fire: pre1972: false` leaves them out). As in
NMC_CEtool, an older fire with no valid year takes it from its report date or
its ID. Fires up to the first year built (e.g. 2012) are put onto the 30 m
template together, keeping each cell's most recent fire, then each later year
on its own: every fire layer needs only each cell's most recent fire up to its
year. Settings are under `landscape: fire:`.

| `build` | Each cell holds |
|---|---|
| `fire_year` | The most recent fire year up to that year, in any of its 30 m cells (blank = never burned). Use with `time_since: TRUE` and `ts_nodata` (e.g. 200) for years since fire |
| `fire_pa` | 1 if any of its 30 m cells burned in the past n years, otherwise 0 |
| `fire_dens` | The proportion of its 30 m cells burned in the past n years (that year included) |
| `fire_dist` | The mean distance (m) from its 30 m cells to the nearest 30 m cell burned in the past n years |

n is the row's `class` (e.g. `2` and `60` for burned in the past 2 or 60
years, as in NMC_CEtool), or `landscape: fire: recent_years` if blank. Add a
row for each cutoff wanted.

### Forest age (`build: forest_age`)

The mean age of each cell's forested 30 m cells (not forest = blank), from
`landscape: forest_age: source:` either `scanfi` (SCANFI v2: a map every 5
years, 1985-2025, which already accounts for fire and harvest up to its year)
or `ntems` (NTEMS: 2019 only, as in NMC_CEtool). Each year uses the latest map
at or before it plus the years since; years before the first map use it minus
the years before (stands then younger than 0 are blank). With
`reset_by_disturbance: true` (default), a forested 30 m cell burned (NBAC/NFDB)
or harvested (CanLaD) after the map's year gets the years since that
disturbance instead; going back before a map, a cell disturbed in between is
blank, as its age then is unknown.

### Terrain (`build: elevation` or `northness`)

From a 30 m elevation model, `landscape: terrain: source:` either `mrdem`
(NRCan's MRDEM-30 bare-earth model, read straight from the web) or your own
raster (a source: path or url). Put onto the 30 m template (bilinear), then:

| `build` | Each cell holds |
|---|---|
| `elevation` | The mean elevation (m) of its 30 m cells |
| `northness` | cos(aspect): 1 facing north, -1 facing south, 0 facing east or west, and 0 on flat ground |

Aspect is measured from true north. In a projected coordinate system the
grid's north differs from true north away from the central meridian (across
the NMC study area in UTM zone 7, by up to about 20 degrees), so aspect is
turned by that angle first. With `northness: fine` (default), northness is
worked out for every 30 m cell and then averaged; with `coarse`, it is worked
out from each cell's mean elevation, as in NMC_CEtool. Slopes at the template's
edge use the elevation in the distance margin.

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

## Checking web sources

Layers built from web sources (SCANFI land cover, CanLaD harvest) ask the
server on every run whether the files have changed. When the layers are
already built and are not being remade, set `run: check_web_files: false`
(usually in `local.yaml`) to skip that check, so a slow or unavailable server
does not stop the run. These files have their version in their name, so a
new release is still noticed when its web address is updated in the
settings. Set it back to `true` when remaking the layers.

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
