# hsaCEtool 0.0.0.9000

- Optional herd selection: `locations: herd: keep:` lists the herds to use
  (final names, after `herd: rename:`); other herds' fixes are dropped right
  after the sources are combined, before cleaning (`keep_herds()`). Target
  `herd_selection` counts fixes and animals by herd and says which were kept.
  Empty (the default), every herd is kept, as before.
- `locations: study_area: add:` (optional) names polygons, such as herd ranges,
  joined to the buffered fixes in `make_study_area()` (new argument `add`), so
  the study area also covers places the fixes do not reach. A source (path or
  url), or `{target: <name>}` for a file made by a target of the project's own
  pipeline. The extended study area is built around both. Not set (the default), the study area is as before.

- `read_config()` has two optional arguments, for studies that run several
  pipelines from one set of settings: `shared`, settings files read between the
  package defaults and the project file (e.g. settings every pipeline shares),
  and `project_file`, a project settings file other than
  `<config_dir>/<project>.yaml` (e.g. a pipeline's `default.yaml`). A
  `default.yaml` in `config_dir` read either way no longer gives a warning.
  Without them, settings are read as before.
- Layer stacks for maps (`model_land:`; targets `model_land_spec`,
  `model_land_layers_file` and `model_land`, in `targets_maps()`): for each year
  in `model_land: years:`, one GeoTIFF,
  `outputs/<project>/model_land/model_land_<year>.tif`, with every covariate the
  models' formulas use, so a model can be mapped for that year from its stack
  alone (e.g. in caribou_SSUD). Replaces NMC_CEtool `scripts/7-SSUDmaps.R`.
  Layers are made as the model tables are: named as in the formulas without
  `_start` or `_end` (e.g. `prop_forest_240`); groups (`model: groups:`)
  combined by the same rules; each distance as it is and capped
  (`<name>_adj`, `model: distance_cap:`); layers that do not change in every
  stack, and years past a layer's last year using its last raster. Time since
  disturbance (`ts_*`) is left NA where there was none (the model tables use
  `ts_nodata`), for caribou_SSUD to fill in for its forecasts.
  `model_land_layers.csv` says what each layer is made from
  (`make_model_land_spec()`, `make_model_land()`, `write_model_land_spec()`).
- `upload_model_land()`, run by hand, zips each year in
  `model_land: upload_years:` with the layer list and puts it in the Drive
  folder `model_land: upload_folder:`, skipping years unchanged since their
  last upload.
- `upload_models()` and `upload_model_land()` no longer need `TAR_PROJECT` set
  before they are called: they set it to their project while checking that
  the results are up to date, then put it back.

- Input files can be named once in an inputs table (`inputs:
  inputs_<project>.csv`; `read_inputs_table()`) and referred to by name in the
  settings (`input: <name>`, or `<what>_input:` for `<what>_url`). Each row has
  the file's url or path and its title, theme (kind of data), access (`public`
  or `restricted`), provider, terms and notes, for an input manifest. The
  package's own table (`inst/extdata/inputs_default.csv`) has the national
  inputs, which `default.yaml` now names instead of giving their urls; a
  project's table adds to it and replaces its rows by name. `read_config()`
  puts the urls back where they were, so the settings, and the targets, are
  unchanged; the combined table is kept as `cfg$inputs_table`, with where each
  input is used (`used_in`).

- A disturbance source can take its categories from a crosswalk table
  (`crosswalk: crosswalk_<source>_<project>.csv`) listing every value of the
  fields used, instead of `category` or `rules` in the YAML
  (`read_disturbance_crosswalk()`, `apply_disturbance_crosswalk()`; see
  `config/README.md`, "Crosswalk tables"). `read_config()` turns each row
  into an exact-value rule, so the rest of the pipeline is unchanged.
- New target `disturb_uncovered` (`check_disturbance_coverage()`) lists the
  kinds of disturbance feature that no rule or crosswalk row covers in
  `outputs/<project>/disturbance_uncovered.csv`, and stops the run if there
  are any; the disturbance layers wait for it. Before, such features were
  left out with only a warning. The `disturb_history_*` commands change, so
  they rerun once; the layers after them rerun only if the features changed.
- In a rule's `where`, `""` now matches a missing value.
- The disturbance settings `unpaved_type` and `unpaved_type_field` are now
  `subtype` and `subtype_field`, a finer grouping within any category, not
  only unpaved roads. The `unpaved_type` column of the features, the
  disturbance summary and the combined files is now `subtype`. The merged
  Yukon files keep their `unpaved_type` column (name it with
  `subtype_field: unpaved_type`).

- `nanoparquet` is now a required package (Imports), not an optional one
  (Suggests). Every run with extraction reads and writes parquet files with it,
  and renv leaves optional packages out of a project's lock file, so a library
  restored from that lock lacked it.
- `download_movebank()`, `read_movebank()`, `read_yukon_reclass()` and the
  readers of xlsx tables now say which package to install when `move2` or
  `readxl` is missing.

- The helpers run by hand after a pipeline run (`write_premade()`,
  `upload_premade()`, `upload_models()`, `upload_combined_disturbance()`,
  `upload_yukon_disturbance()`, `run_pipeline()`, `push_store()`,
  `pull_store()`) find the settings and the targets store the same way in a
  repository with one targets project and its settings in `config/` (as
  before) and in one with several, each with its settings beside its
  `_targets.R` (e.g. YT_caribou's `pipelines/dataprep-caribou/config/NMC.yaml`).
  New argument `config_dir`; `project` may be the settings name or the
  targets project, and defaults to `TAR_PROJECT`. They no longer change
  `TAR_PROJECT` (except `run_pipeline()`, which runs that project).
  `push_store()` and `pull_store()` name the shared copy after the targets
  project.
- A disturbance source with no lookup tables no longer names a missing
  `disturb_lookup_` in its target's command (it worked only because the
  lookups were never read). The command changes, so those targets rerun.
- Downloads from a `url` work again with reproducible 3.2.1, which fails
  when `prepInputs()` is given the reading function itself. The reader (`fun`,
  or the default for the file type) is now passed as a call such as
  `sf::st_read(targetFile)`.
- New `upload_models()` puts fitted models marked `upload: true` (or named)
  in the Drive folder `model: upload_folder:`, as a zipped fit
  (`.rds`) and zipped coefficient tables (`.csv`), skipping models unchanged
  since their last upload. Run by hand after a pipeline run.
- `locations: sources:` can name a CSV table in `config/` (e.g.
  `locations_NMC.csv`), one row per source, read by the new
  `read_location_sources()`. It gives the same settings as sources typed in the
  YAML, including each source's study, herd and jurisdiction (`by_source`) and
  `study_column`/`herd_column`, plus `provider`, `terms` and `notes` columns
  the pipeline ignores (for the record of where each source came from).
- `locations: premade_folder:` can give file name patterns, each with its
  Drive folder (first match wins), e.g. the cleaned locations to a private
  folder and the study areas to a shared one; `upload_premade()` also takes
  them as `folders`. The matching (`upload_destinations()`) is not specific
  to premade files, so other uploads can use it.
- `locations: cleaning: params: exclude_points:` can name a CSV table in
  `config/` (`name`, `lon`, `lat`, `radius_m`, `notes`), read by the new
  `read_exclude_points()`; it gives the same settings as points typed in the YAML.
- Location source targets are named after each source's `name`
  (`locations_source_<name>`), not its position, so adding or reordering
  sources does not rerun the others. Names must be unique. This renames the
  targets of existing projects, so their sources are read (and downloaded)
  again once.
- A location source given by `url` is now downloaded and read in two targets
  (`locations_source_<name>_file` and `locations_source_<name>`), as a `path`
  already was. Download targets include only the settings that say which
  file a source is, so editing e.g. `columns` does not download it again.
- New setting `locations: freeze_downloads:` (default `false`): `true`
  downloads a location source, capture log or mortality file again only if its
  file is missing or has changed, or with `refresh: true`.
- Movebank sources no longer need `user:`; credentials come from the keyring.
- Seasons now come from a season table (CSV) with named sets of seasons
  (`model: season_table:` and `season_set:`; a model can use its own
  `season_set:`), read by the new `read_seasons()` and `season_dates()`.
  Season dates typed in `model: seasons:` are no longer read.
- Package created from the `caribou_CEtool_dataPrep` pipeline code, keeping the
  history of its `R/` and `tests/` folders.
- The default settings are now part of the package (`inst/config/default.yaml`);
  `read_config()` reads them from there, then the project's `config/` files. A
  project `config/default.yaml` is no longer read (with a warning).
- New `ce_session_setup()` sets the options and environment variables the
  pipeline needs; call it near the top of a project's `_targets.R`.
- Functions that target commands or projects call are exported; the rest are
  internal.
