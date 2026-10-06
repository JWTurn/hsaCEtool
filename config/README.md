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
or incomplete. To check a URL for a newer version on every run, add
`refresh: true` to that source.

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
| `harvest_dist` | The mean distance (m) from its 30 m cells to the nearest recently harvested 30 m cell |

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
