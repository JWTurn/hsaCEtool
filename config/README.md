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
`reproducible::preProcess()`, the download step of `prepInputs()`. They are
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

Each step gets the raster for its year. Steps after `last_year` use the last
raster, and steps before `first_year` use the first.

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
