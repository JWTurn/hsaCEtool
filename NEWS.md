# hsaCEtool 0.0.0.9000

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
