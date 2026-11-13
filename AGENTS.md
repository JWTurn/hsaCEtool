# hsaCEtool — agent guide

Project memory for AI-assisted work in this repository. Adapted from the
PredictiveEcology `AI` repo template (`PredictiveEcology/AI`, `AGENTS.md`); team
guidelines live in that repo's `guidelines/` folder (start at `00-user-setup.md`).

## What this package is

An R package of steps for a **`targets` pipeline** that prepares caribou locations
and landscape layers for habitat selection analysis (iSSA) behind the caribou
cumulative effects (CE) tool. `targets` runs a workflow as named steps ("targets"),
keeps each result, and reruns only steps whose code or inputs changed.

- Repo: `JWTurn/hsaCEtool`, default branch `main`. Private for now; will become public.
- Code came from `JWTurn/caribou_CEtool_dataPrep` (history of `R/` and `tests/` kept).
- The package holds **code only**. Projects (study areas) keep their own
  `_targets.R`, `config/<project>.yaml` and data store. The main user is the
  FOR-CAST `YT_caribou` project.
- Dependencies: `FOR-CAST/cariboutools` will import this package (e.g. its study
  area), so this package must **never** import `cariboutools`. It may use
  `ytdata` (GeoYukon downloads) and `workflowtools`.

### Layout

- `R/` — package functions. `targets_*()` functions return lists of targets that a
  project's `_targets.R` combines; the other functions do the work.
- `inst/config/` — `default.yaml` (every setting and its default) and its README.
- No restricted inputs: settings tables that may not be public (e.g. the Yukon
  reclass patches and year fixes) stay in each project's `config/`, not here.
- `tests/testthat/` — unit tests with small, made-up inputs; `fixtures/config/` is a
  made-up project's settings (`test_config()` in `helper-source.R`).
- Projects call `ce_session_setup()` near the top of `_targets.R`.

### Package rules

- Functions named directly in a target's command must be **exported**: targets
  runs commands in the global environment, where internal functions are not found.
  Helpers stay internal (`@keywords internal` + `@noRd`).
- Projects should set `tar_option_set(packages = "hsaCEtool", imports = "hsaCEtool")`
  so changes to package code rerun the targets that use it. Calls written as
  `hsaCEtool::fun()` are not tracked.
- Settings files are found with `system.file(..., package = "hsaCEtool")`; project
  files are relative to the project folder.
- Don't change the user's session on load (no options or environment variables in
  `.onLoad()`); session settings go in an exported function the project calls.

### targets, LandR and reproducible

- **One cache only.** `targets` stores every result, so don't wrap steps in
  `Cache()` (`options(reproducible.useCache = FALSE)`). `prepInputs()` is fine for
  downloading and cropping. Only the user can decide to use `Cache()` for a step.
- **Spatial results are files.** Never return a `SpatRaster`/`SpatVector` from a
  plain `tar_target()`; use `geotargets::tar_terra_*()` or `format = "file"`
  returning paths.
- **Track input files** with `format = "file"` targets; for a folder, add a manifest
  target (path, size, modification time).
- `getOption()` or `Sys.getenv()` inside a target is invisible to `targets`: turn the
  value into its own target.
- **prepInputs() pitfalls:** an empty `cropTo`/`maskTo` silently skips cropping;
  crop before reprojecting large rasters; a restricted Google Drive download can
  save a sign-in page instead of the data (check the file type).
- **GeoPackages:** `OGR_SQLITE_ALLOW_ANY_EXTENSION=YES` and a pinned
  `OGR_CURRENT_DATE`.

### Caribou location data are confidential

This repo will become public. Caribou GPS locations, raw or cleaned, must never
become public: not through git, GitHub (issues, PRs, comments, releases, Actions
logs), published artifacts or web pages, or anything shared outside the projects'
restricted storage. Data-sharing agreements depend on this.

- **Restricted:** raw collar locations (fixes), and anything that still holds
  them: e.g. the used (observed) steps in an iSSA table, used points in an RSF
  table, fitted models whose data still hold them, and maps or plots showing
  individual fixes or tracks. Mortality sites are restricted too.
- **Not restricted on their own:** derived features such as available points,
  iSSA random steps (even though they start at observed fixes) and home ranges.
  A table that combines them with the used steps or points is restricted.
- **This repo holds none of it.** No data files, no printed rows (`head()` output
  with coordinates or IDs plus times) in code, comments, docs, commit messages,
  issues or PRs. Summaries without coordinates are fine.
- **Tests and examples** use made-up locations only (see `tests/testthat/helper-source.R`).
- **Guard:** `.githooks/pre-commit` blocks data file types and tables with
  coordinate-like columns. Turn it on once per clone with
  `git config core.hooksPath .githooks`. Never bypass it with `--no-verify`; if it
  blocks something that is not location data, ask the user.
- **Before the repo goes public:** check the whole git history, not just current files.

---

## Shared conventions

- Use `data.table` and the base pipe `|>` (not magrittr `%>%`). Match the conventions
  already in the file you are editing.
- **Memory-efficient R:** avoid `Map()`/`do.call()`/the `apply` family (prefer `purrr`);
  avoid `as.formula()`; don't define closures inside large environments.
- **Document R functions with `#'` roxygen blocks.** Internal helpers get
  `@keywords internal` + `@noRd`; use `@inheritParams`; preserve `@author`; never
  reorder function arguments.
- **Don't reformat or touch code unless asked.** If cleanup beyond the request seems
  needed, flag it, justify it, and get approval first.
- **Edit `.R` files with R, not the editor's edit/write tools** (e.g. `writeLines()`),
  re-reading the file first so the user's own edits are not overwritten.
- **Plan, then implement.** Agree the approach with the user before non-trivial edits.
- **After any file change, verify the diff** (`git diff <file>`, `git diff --stat`).
- Check with `devtools::document()`, `devtools::test()` and `devtools::check()`.

## Guardrails

Outside this folder is read-only and needs approval each time; never `git commit`
unless asked; stage files by name (never `git add -A`/`.`); dry-run destructive
commands first.

## Writing and output rules

- **Disclose AI assistance on all outputs** (commits, issues, PRs, docs, code
  documentation, reports) in one line naming the tool used, e.g. "Drafted with
  assistance from Claude (Claude Code)." The README statement covers the repo's
  documents.
- Keep punctuation **outside** formatting unless it is part of a name.
- Write for ecologists and other scientists: minimize jargon and define technical terms.

---

Drafted with assistance from Claude (Claude Code).
