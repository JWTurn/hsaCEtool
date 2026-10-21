# hsaCEtool 0.0.0.9000

- Package created from the `caribou_CEtool_dataPrep` pipeline code, keeping the
  history of its `R/` and `tests/` folders.
- The default settings are now part of the package (`inst/config/default.yaml`);
  `read_config()` reads them from there, then the project's `config/` files. A
  project `config/default.yaml` is no longer read (with a warning).
- New `ce_session_setup()` sets the options and environment variables the
  pipeline needs; call it near the top of a project's `_targets.R`.
- Functions that target commands or projects call are exported; the rest are
  internal.
