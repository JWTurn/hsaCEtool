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
      name: NMC2020
      url: "https://drive.google.com/file/d/.../view"
      target_file: clean_NMC.RDS     # the file wanted from the download
    - type: file
      name: BCtelemetry
      path: "BC/BC_NMC_Telemetry.csv"  # relative to paths: data_in
      columns: {id: animal_id, datetime: timestamp, x: longitude, y: latitude}
      input_crs: "EPSG:4326"
    - type: movebank
      name: Klaza
      study: "Klaza caribou"
```

Each source's targets are named after its `name`: `locations_source_<name>`
(characters other than letters, digits and `_` become `_`), so names must be
unique. Sources can then be added, removed or reordered without renaming, and
rerunning, the others. A source without a name is named by its position.

Each source is got (downloaded) in one target and read in another, and only the
settings that say which file it is (`path`, `url`, `target_file`, `archive`,
`fun`, `subfolder`, `refresh`) are part of the download. So editing e.g. its
`columns` or `herd` reads the file again but does not download it again.

### Inputs table: `inputs_<project>.csv`

Input files can be named once in a table, one row per file, and referred to by
name in the settings (`input: <name>`) instead of repeating their url. The
table also records who provided each file and on what terms, for an input
manifest or a report's data sources. The package has its own table
(`inst/extdata/inputs_default.csv`) for the national inputs (CanLaD, SCANFI,
NTEMS, MRDEM, NBAC, NFDB), which `default.yaml` uses. A project's table, named
in `inputs:` (relative to the config folder), adds its own inputs and replaces
the package's row of the same name.

| Column | What it holds |
|---|---|
| `name` | the input's name, used in the settings and as its id in a manifest (required) |
| `title` | a short title, e.g. "SCANFI land cover v3" |
| `theme` | what kind of data it is, e.g. `landcover`, `harvest`, `fire`, `forest_age`, `elevation`, `disturbance`, `boundary`, `study_area` or `caribou` |
| `url` or `path` | where the file is (one of them is required) |
| `target_file`, `archive` | with `url`: the file wanted from the download, and the archive's name |
| `access` | `public` (the url may be published) or `restricted` (shared with named people only: never publish its url) (required) |
| `provider`, `terms` | who provided the data, and on what terms (a licence or data-sharing agreement) |
| `notes` | anything worth knowing about the input |

```yaml
inputs: inputs_NMC.csv
locations:
  captures:
    source: BC
    input: bc_captures      # its url, target_file and archive come from the table
    columns: {...}
landscape:
  fire:
    nbac: {input: nbac}     # in the package's table
  harvest:
    canlad_old:
      type_input: canlad_1965_1984_type   # <what>_input: <name> becomes <what>_url
```

`read_config()` puts each input's `url` (or `path`), `target_file` and
`archive` where the settings name it, so the settings come out as if the urls
were typed there. A url or path typed beside `input:` wins (e.g. a project's own
NBAC). The other columns never reach the pipeline, so editing them reruns
nothing. The combined table is kept as `cfg$inputs_table`, with a column
`used_in` saying where in the settings each input is used (blank if nowhere).
What a file is used for (its years, fields, crosswalk, the file wanted from a
folder) stays in the YAML.

### Location sources table: `locations_<project>.csv`

Instead of typing the sources in the YAML, `locations: sources:` can name a CSV
file in `config/`, one row per source in order of priority:

```yaml
locations:
  sources: locations_NMC.csv
```

`read_config()` turns the table into the same settings as the typed list, so
moving sources between the two does not change or rerun anything. Blank cells
are left out. Columns (only `name` is required; leave out any not used):

| Column | What it is |
|---|---|
| `name` | the source's name (unique; used in target names and the `source` column) |
| `type` | `file` (the default), `movebank` or `bc` |
| `movebank_study` | Movebank study ID (or name), for `type` movebank |
| `part` | `new`, `old` or `reference`, for `type` bc: one row per part, all with the same `name` |
| `url`, `path`, `target_file`, `archive`, `fun` | where the file is, as for any source |
| `id_col`, `datetime_col`, `hour_col`, `x_col`, `y_col` | the source's `columns`: its animal ID, date and time, hour, and coordinate columns |
| `input_crs`, `tz` | the source's coordinate system and time zone, if not the defaults |
| `date_only`, `clock_time`, `id_from_deployment`, `refresh` | `TRUE` or `FALSE` (blank = `FALSE`) |
| `study`, `herd`, `juris` | the source's study, herd and jurisdiction, for fixes without one from a column (`by_source`, see "After cleaning") |
| `study_column`, `herd_column` | a column of the data checked first for each fix's study or herd in this source |
| `provider`, `terms` | who provided the data and on what terms (e.g. a data-sharing agreement), for the record of where the source came from; not used by the pipeline, so editing them reruns nothing |
| `notes` | anything worth knowing about the source; not used by the pipeline, so editing it reruns nothing |

`by_source` entries and `columns` in the YAML still apply: the table's are added
to them (its columns first, limited to their own sources). Rules that cover
several sources, such as `herd: columns:` (`group_id`, `study_site`) and
`herd: rename:`, stay in the YAML.

### Exclusion points table

The cleaning step `exclude_points` drops fixes near known false locations, such
as an office where collars are stored. The points can be typed in the YAML or
listed in a CSV file in `config/`, one row per point:

```yaml
locations:
  cleaning:
    params:
      exclude_points: exclude_points_NMC.csv
```

Columns: `name` (optional), `lon` and `lat` (decimal degrees, WGS 84),
`radius_m` (fixes closer than this, in metres, are dropped) and `notes` (not
used by the pipeline). `read_config()` turns the table into the same settings
as typed points, so moving them between the two reruns nothing.

### Freezing downloads

A download would still run again when `paths:` change (e.g. on another
computer) or the package's download code changes, and a new download of a
Movebank study that is still collecting brings new fixes, which rebuild
everything after it. With `locations: freeze_downloads: true`, a location
source, capture log or mortality file is downloaded again only if its file is
missing or has changed, or the source has `refresh: true`. Updating the data is
then always a choice. Note that it also ignores a new `url` or `study`: set
`refresh: true` for one run when you change which file a source is.

### Premade or remade

Each stage can start from files made earlier instead of being remade:

- `locations: premade:` a cleaned locations file saved from the
  `locations_clean` target (.rds). The `sources` are then not read or cleaned;
  the file keeps each fix's `source`, the flag and `mortality_status` columns
  and its `cleaning_log`. It is downloaded to `inputs/premade/`, so an older
  file of the same name in `inputs/` is not mistaken for it.
- `locations: study_area: source:` and `study_area: extended: source:` existing
  study area polygons (a zipped shapefile from a url needs `archive:` too).
  Or, to build the study area from the fixes and also cover places they do not
  reach, `locations: study_area: add:` names polygons (e.g. herd ranges) that
  are joined to the buffered fixes; the extended study area is then built
  around both. Give a source (path or url), or `{target: <name>}` when a
  target in the project's own `_targets.R` makes the file (it must return the
  file's path, with `format = "file"`), so the study area waits for it.
- `landscape: template: source:` and the layer table's `path`/`url` columns:
  existing template and layer rasters.

Set to `null` (or blank in the layer table, with a `build`), each is remade.
`NMC.yaml` has commented lines to start from the NMC files saved on the
restricted shared drive.

To share or update these files, set `locations: premade_folder:` to a
restricted Google Drive folder, or to file name patterns, each with its folder,
e.g. so the cleaned locations go to a private folder and the study areas to one
shared with other phases:

```yaml
locations:
  premade_folder:
    clean_NMC.zip: "https://drive.google.com/drive/folders/<private folder>"
    "*": "https://drive.google.com/drive/folders/<shared folder>"
```

Patterns are wildcards as for file names (`*` any characters, `?` one
character; quote a pattern that starts with `*`). They are tried in order, and
the first that matches a file's whole name gives its folder, so put `"*"`
(everything else) last.

Then in R:

```r
targets::tar_source("R")
write_premade("NMC")    # zips the current cleaned locations (.rds) and study areas (.gpkg)
upload_premade("NMC")   # puts them in premade_folder, replacing files of the same name
```

Replacing keeps each file's link, so the `premade:` and `source:` links in the
settings stay valid. Study areas are saved as GeoPackages, which keep the
geometry exactly (a shapefile can differ slightly, which would rebuild every
layer). `upload_premade()` stops if the folder for the cleaned locations
(`clean_<project>.zip`) is shared with anyone with the link, as they are caribou
locations; the study areas may go to a folder shared more widely.

Movebank user names and passwords are never written in the settings. Save
yours once with `move2::movebank_store_credentials("<user>")`, which keeps it
in your computer's keyring; downloads read it from there.

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

Some files label local clock times as UTC (e.g. `2020-06-01T08:00:00Z`, which
is read as 08:00 UTC). Add `clock_time: true` to such a source: the label is
ignored and the clock time is read in `tz`. Without it, a time already read as
a date-time keeps its label and `tz` has no effect. A fixed offset with no
daylight saving is written `Etc/GMT+8` (UTC-8; the sign is reversed).

If only some of a file's fixes are local clock time (e.g. a file joining
several studies, all labelled UTC), use the cleaning step `local_times`
instead, listed first. It reads the clock time of each fix whose `column` has
a listed value in that value's time zone, and leaves the rest as they are:

```yaml
  cleaning:
    steps: [local_times, bounds, ...]
    params:
      local_times: {column: DataSource, tz: {CMCRP: Etc/GMT+8}}
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
  (or `collar_<Cllr_ID>`). The target `locations_source_<name>_log` (e.g. `locations_source_BC_log`) counts each step.

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

### Keeping only some herds

To use only the herds a project studies, list them in `locations: herd: keep:`,
with their final names (CamelCase, after `herd: rename:`). Each fix's herd is
found as described above, and the other herds' fixes are dropped as soon as the
sources are combined (target `locations_raw`), before cleaning and before the
study area is made. With premade locations, they are dropped from the premade
file. A listed herd with no fixes stops the run, as its name is probably
misspelled. The target `herd_selection` counts the fixes and animals of every
herd and says which were kept. `[]` (the default) keeps every herd.

### Time between fixes

Tracks are resampled to one fix every `tracks: rate_hours`. To choose that
rate, the target `fix_intervals` summarizes the hours between each animal's
consecutive fixes after cleaning (mean, median, 10th and 90th percentiles, and
the median of the animals' own medians), for each animal and for each value of
each column in `locations: fix_intervals: by:` (default `[source, herd]`).
A column used here must be kept in `keep_after_cleaning`.

## Landscape layers: `layers_<project>.csv`

Each landscape layer is one row of a table named in `landscape: layers:`. To add
a layer, add a row. Each layer's values are extracted at the start and/or end of
every step (`extract: where:`), as columns `<name>_start` and `<name>_end`.
The table each model is fitted to is made from these, with only the layers the
model uses (see [iSSA models](#issa-models-model)).

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
- The extended study area is rebuilt from the cleaned fixes, so any change to
  cleaning moves it slightly, which rebuilds the templates and every layer.
  Once it is built, set `locations: study_area: extended: freeze: true` to keep
  it and the templates as they are (they are then never rerun). The target
  `study_area_check` warns when cleaned fixes come within the extended
  `buffer_m` of its edge, and says to set `freeze: false` to rebuild. A saved
  area can also be given as its `source:`, but one read back from a file may
  differ slightly from the original and so rebuild the layers once.
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
| `harvest_year` | Year of harvest, averaged in steps: each 30 m cell's most recent harvest up to that year; at the finest resolution, the mean of its harvested 30 m cells; at each coarser one, the mean of the next finer resolution's cells (e.g. 1200 m = mean of its 240 m cells). Cells never harvested are left out of the means (blank = never harvested). Use with `time_since: TRUE` for years since harvest |
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
| `fire_year` | Year of fire, averaged in steps as `harvest_year`: each 30 m cell's most recent fire up to that year; at the finest resolution, the mean of its burned 30 m cells; at each coarser one, the mean of the next finer resolution's cells (e.g. 1200 m = mean of its 240 m cells). Unburned cells are left out of the means (blank = never burned). Use with `time_since: TRUE` and `ts_nodata` (e.g. 200) for years since fire |
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
the `files` wanted from it. A source's categories can come from a crosswalk
table (`crosswalk:`, below) instead of each file's `category` or `rules`.
Each file has:

- `category`: one category for all its features, or `rules`: a list of
  `{where: {field: value(s)}, category: ...}`, checked in order; the first
  that matches gives the category. `match: {field: pattern}` matches a
  regular expression instead (e.g. `{match: {ReportClass: '^Paved'}}`); a rule
  can have both. A rule without `where` or `match` catches the rest.
  Category `drop` leaves features out (e.g. harvest, which comes from CanLaD).
- `keep`: fields copied into the features (e.g. Yukon `feature_id`,
  `REF_ID`, `year_rule`, `year_source`), for the combined files below.
- `subtype` (on a rule or the whole file) or `subtype_field` (a
  field holding it): optionally, a finer grouping within the category,
  recorded on each feature and in the summary (for NMC, the kind of unpaved
  road: winter, resource or all-season). The layers are by category only.
- `year`: fields giving the year built, in order of preference (a year or a
  date). None, or all empty: the feature counts in every year.
- `lookup`: optionally, columns added from a table in `landscape: disturbance:
  lookups:` (e.g. a reclassification spreadsheet), matched on some fields:
  `{table: ..., sheet: ..., by: {feature field: table column}, add: column}`.
  Types missing from the table are listed in a warning and sorted by the
  remaining rules.

`outputs/<project>/disturbance_summary.csv` (target `disturb_summary`) lists,
for each source and file, how many features of each type went into each
category (dropped ones included), with their total length (km, lines) or area
(km², polygons), how many have no year, and the first and last year built. A
feature's type is the values of the fields its rules look at (e.g.
`feature_type=placer`). Features are those overlapping the template with its
margin, measured whole.

#### Crosswalk tables (`crosswalk: crosswalk_<source>_<project>.csv`)

A crosswalk table says which category each kind of feature goes into, listing
every value of the fields used, so it can be checked in a spreadsheet by
people who know the data. Name it on the source (`crosswalk:`, a CSV file in
the project's settings folder) and leave `category` and `rules` off its files;
the other file settings (`year`, `keep`, `subtype_field`) stay in the
YAML. Several sources can share one table (e.g. the Yukon lines and polygons).
Columns:

| Column | What it holds |
|---|---|
| `file` | The file of the source the row is for |
| one column per field used (e.g. `ReportClass`, `RoadDescription`, `Industry`) | The field's value for this kind of feature |
| `category` | The category given (e.g. `paved`), or `drop` to leave those features out |
| `subtype` | Optional: a finer grouping within the category (e.g. for unpaved roads: `winter`, `resource`, `all-season`) |
| `notes` | Optional: anything worth knowing about the row; not used by the pipeline |

A file uses the field columns that any of its rows fill in. A blank cell in
a field the file uses means the value is missing in the data. A file whose
rows fill in no field has one row: all its features get that category. For
example:

```
file,ReportClass,Industry,category,subtype,notes
roads.gpkg,Paved 2 Lane,Transportation,paved,,
roads.gpkg,Loose 1 Lane,Forestry,unpaved,resource,
roads.gpkg,,Forestry,unpaved,resource,no surface recorded
trails.gpkg,,,lfother,,
```

The values must match exactly (spaces at either end are ignored); there are
no patterns and no catch-all row. Features whose values the table doesn't
list are left out of the layers, so target `disturb_uncovered` lists them in
`outputs/<project>/disturbance_uncovered.csv` (one column per field, so rows
can be copied into the table) and stops the run until they are added, with
`drop` for those meant to be left out. It does the same for features that
match none of a file's `rules`. Only the features in the area kept (the
template with its margin) are checked, so a new study area or a new data
release can bring new values. Editing a table reruns only the sources that
use it.

#### Merged Yukon disturbance (`yukon_merge:`)

With `yukon_merge: build: true`, the Yukon sources (`from: yukon_merge`) read
files the pipeline builds by merging GeoYukon surface disturbance with the
Government of Yukon's newer work packages (`R/yukon_disturbance.R`, after NMC_CEtool
`scripts/prep-disturb-cleaned.R`):

- Types are grouped with the reclassification sheet (v4 and v5 types), plus
  `config/yukon_reclass_patches.csv` for types missing from it or fixes to it.
- Inside each work package's area, a GeoYukon feature is replaced only where a
  work-package feature lies within `new_feature_buffer_m` (20 m) of it; features
  the work package did not map again are kept. Of a feature found in
  overlapping work packages (same REF_ID, same place), one copy is kept: the
  latest, with the earliest image year. Features Yukon's QA marked for deletion
  are left out.
- Years built (`year_start`, `year_end`, with `year_rule` and `year_source` on
  every feature): reported years first (quartz mine production, matched to
  the mine polygon holding the point or the nearest within `mine_max_m`; and
  `config/yukon_year_fixes.csv`, by REF_ID). Image years are used only for
  features that are new in a work package (more than 20 m from anything on the
  earlier GeoYukon map) in an area GeoYukon had already mapped from older
  imagery: they count from the image year. That is the year they had been
  built by (seen on the image), not the year they were built. Pieces smaller
  than `min_new_size_m` are ignored. Every other feature (on both maps, or
  outside GeoYukon's mapped areas) has no year: it is assumed to have been
  there the whole time and counts in every year. Features keep counting after
  `year_end`.
- Outputs, in `outputs/<project>/derived/`: `YT_linear.gpkg`, `YT_areal.gpkg`,
  `YT_merge_log.csv` (features by source, type and year rule) and
  `YT_cleaning_log.csv` (every feature changed or left out, and why, to report
  back to the data owners).
- `upload_yukon_disturbance()` (run by hand) zips the two GeoPackages with the
  cleaning log and replaces the files of the same name in `upload_folder`, so
  the sources' `url` links keep working for runs with `build: false`.

`border_check:` compares the merged Yukon lines with the B.C. and N.W.T.
features where they meet the border: the gap to the nearest neighbour line and
both sides' categories and years (`YT_border_pairs.csv`, `YT_border_summary.csv`).
Yukon roads that line up with a B.C. road whose year built is known take that
year (`years_from`).

`combined:` writes every source's features (the area the layers use) to one
lines and one polygons GeoPackage, for maps of results (target
`disturb_combined`, in `outputs/<project>/derived/combined/`), with `region`,
`source`, `file`, `category`, `subtype`, `year` and the sources' `keep`
columns. `upload_combined_disturbance()` (run by hand) replaces the files of
the same name in `upload_folder`.

Each download is kept in `paths: input_cache` and is not downloaded again
(`prepInputs()` does not check the web for a newer version). To pick up an
update (e.g. a new GeoYukon release), add `refresh: true` to that source or
lookup for one run: it is downloaded afresh, and the layers are only rebuilt
if its features changed. Remove it afterwards. Google Drive
downloads use the saved login of `run: google_email` (set it in `local.yaml`
and run `googledrive::drive_auth(email = "<it>")` once).

Layers are only built for the first year and the years in which features were
added; the other years reuse the layer of the year before.

## iSSA models (`model:`)

Integrated step selection models, fitted with glmmTMB (the Poisson form, with
each step's intercept variance fixed). Set them in `model:` in
`<project>.yaml`; `default.yaml` lists every setting.

**Each model has a name you choose**: its key under `model: models:`, e.g.
`full`, `full_winter` or `simple_yt`. The name is not the project name. It names
the model's targets (`model_data_<name>`, `model_fit_<name>`,
`model_coefs_<name>`) and files (`outputs/<project>/models/<name>_fixed.csv`,
`_random.csv`, `_by_group.csv`, `_data_log.csv`). Use letters, numbers and `_`,
starting with a letter.

```yaml
model:
  groups:                       # combined covariates
    prop_veg: {sum: [prop_herbaceous, prop_low_shrubs, prop_tall_shrubs]}
    dist_otherpoly: {min: [dist_otherpoly, harv_dist]}
  models:
    full:
      fit: true                 # false: build and check the table only
      n_random: 10              # random steps per used step (of tracks: n_random)
      seasons: all              # or e.g. [winter]
      subset: juris == 'yt'     # optional
      formula: |
        case_ ~ -1 + I(log((sl_ + 1)/1000)) + prop_veg_240_end + ...
          + (1 | indiv_step_id) + (0 + prop_veg_240_end | id)
```

- **Formula**: written out in full, as for glmmTMB. Covariates are extracted
  columns (layer name with `_start` or `_end`, e.g. `dist_paved_240_end`; see
  `outputs/<project>/availability.csv`) or groups with a resolution and start or
  end (e.g. `prop_veg_240_end`). Steps also have `sl_`, `ta_`, `id`, `year`,
  `juris`, `study`, `herd` and `sex`, and `season` and `yearseason` (e.g.
  `2019_winter`) are added. It must include `(1 | indiv_step_id)`; its SD is
  fixed at `step_sd` (1000) without writing `map` or `start`. Positions and
  times (`x1_`, `t1_`, ...) cannot be used.
- **Groups** (`model: groups:`): `name: {rule: [layers]}`, rule `sum`, `min`,
  `max` or `mean`, layers without resolution. Made at every resolution, for
  start and end. A group can reuse a layer's name to replace it.
- **Distance cap** (`model: distance_cap:`): a step that starts more than
  `beyond_km` from a disturbance gets `set_to_km` for the used step and all its
  random steps, so it says nothing about that disturbance. It goes by the step's
  start, which the used and random steps share, so it does not depend on where
  the animal went.
- **Seasons**: by the date a step starts (UTC), from a season table (CSV) named
  in `model: season_table:` (in the project's config folder; `''` uses the
  package's `inst/extdata/seasons_default.csv`). Each row is one season of a **set**, one
  way of dividing the year, so a project can keep several (e.g. simplified and
  biological seasons):

  ```
  set,season,start_month,start_day,end_month,end_day,source
  simplified,winter,11,1,4,15,Yukon (2026)
  simplified,spring,4,16,6,15,Yukon (2026)
  bio,calving,5,15,6,15,Yukon (2026)
  ```

  Each season runs from its start day to its end day, both included, and may
  cross the new year. Months and days are separate numbers so a spreadsheet
  cannot turn them into dates. Within a set, the first season listed that holds
  a date wins; steps in no season are `other`. Other columns (e.g. `source`)
  are kept as notes. Models use `model: season_set:`, or their own
  `season_set:`; a model keeps only its `seasons:` (e.g. `[winter]`). Editing a
  set refits only the models that use it.

  The table could later hold definitions that differ by herd or jurisdiction,
  with an optional column (e.g. `herd`) matched to the steps' column of that
  name, and rows without it as the fallback. This is not built yet.
- **Missing values**: a step whose used step has a missing value is dropped
  whole; a random step with one is dropped on its own. Counts are in
  `<name>_data_log.csv`.
- **Fit settings** (a model's `control:`): passed to glmmTMB
  (`glmmTMBControl()`), on top of `rank_check: adjust`. The usual one is a
  higher iteration limit for a model that stops before it converges (the fit
  log says "iteration limit reached"):
  `control: {optCtrl: {iter.max: 1000, eval.max: 1000}}`. Only that model is
  refitted.
- **Sharing fitted models** (a model's `upload: true`): after a run,
  `upload_models("<project>")` puts each such model in the Drive folder
  `model: upload_folder:` (it may be shared widely: each fit is first checked
  for positions, times and animal IDs that are not codes) as two zips,
  `<project>_model_fit_<name>.zip` (the fit, `.rds`) and `<project>_model_<name>.zip` (the coefficient tables,
  `.csv`). Files of the same name are replaced in place, so links stay the
  same. Models unchanged since their last upload are skipped (recorded in
  `outputs/<project>/upload/uploaded_models.csv`); `force = TRUE` uploads them
  anyway, and `models = "<name>"` uploads named models only. It stops if a
  model is not up to date, or if a fit holds positions, times or animal IDs
  that are not codes. It is never run as part of the pipeline.
- **Factors**: text columns and grouping columns (`id`, `yearseason`, ...)
  become factors only just before fitting, with only the values left in the
  table, so a value dropped earlier is not an empty level.
- **Sharing**: a model's table holds only its formula's columns, with animal
  IDs replaced by codes, so a fitted model can be shared. The table linking
  codes to IDs, `outputs/<project>/models/animal_codes_CONFIDENTIAL.csv`,
  cannot.

## Layer stacks for maps (`model_land:`)

To map a model for a year (e.g. in caribou_SSUD), the pipeline makes one
GeoTIFF per year in `model_land: years:`,
`outputs/<project>/model_land/model_land_<year>.tif` (target `model_land`). It
holds every covariate the models' formulas use (`model_land: models:`, `all` by
default), made from the layers exactly as the model tables are, so a map
uses the same values the model was fitted to:

- **Names**: as in the formulas, without `_start` or `_end`, e.g.
  `prop_forest_240` for `prop_forest_240_end`.
- **Years**: each layer's raster for that year; years past a layer's last year
  use its last raster (before its first, its first), and layers that do not
  change (e.g. elevation) are in every stack.
- **Groups** (`model: groups:`, e.g. `prop_forest`): combined cell by cell by
  the same code as the model tables: a sum counts a missing layer as 0, and
  the group is missing only where all its layers are.
- **Time since disturbance** (e.g. `ts_fire_240`): the year minus the
  disturbance year. No disturbance, or one after that year, is left NA (the
  model tables use the layer's `ts_nodata` there): caribou_SSUD fills these in,
  so that forecasts from the stack's year can age the disturbances.
- **Capped distances**: each distance matching `model: distance_cap: columns`
  is there twice, as it is (`dist_paved_240`) and capped (`dist_paved_240_adj`:
  more than `beyond_km` set to `set_to_km`, as the model tables do).
- **Grid**: the finest cell size used (e.g. 240 m); coarser layers (e.g.
  1200 m) give each fine cell the value of the coarse cell it lies in, as a
  point there would get. Masked to the extended study area's template.

`model_land_layers.csv` in the same folder says what each layer is made from.
After a run, `upload_model_land("<project>")` puts the years in
`model_land: upload_years:` (all years if empty) in the Drive folder
`model_land: upload_folder:`, each as `<project>_model_land_<year>.zip` (the
stack and the layer list). As for models, files of the same name are replaced
in place, unchanged years are skipped (recorded in
`outputs/<project>/upload/uploaded_model_land.csv`), `force = TRUE` uploads
anyway, and it stops if the stacks are not up to date.

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
