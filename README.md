# hsaCEtool

Steps for a [`targets`](https://docs.ropensci.org/targets/) pipeline that
prepares caribou GPS locations and landscape layers for **habitat selection
analysis** (integrated step selection analysis, iSSA) behind the caribou
**cumulative effects (CE) tool**.

**Status:** early development. The code is moving here from the
`caribou_CEtool_dataPrep` pipeline; functions and settings may still change.

## What it does

- reads and cleans caribou GPS locations (files, Movebank, B.C. telemetry)
- builds tracks, regular fix rates, and random steps for iSSA
- builds landscape layers on a common grid: disturbance (including a merged
  Yukon layer), fire, harvest, land cover, terrain and forest age
- extracts the layers along steps and fits the iSSA models

A study is set up with short YAML settings files rather than by editing code;
the defaults and what each setting means are in `inst/config/`.

## Install

```r
pak::pak("JWTurn/hsaCEtool")
```

## Using it in a project

A project (for example a study area) keeps its own `_targets.R` and
`config/<project>.yaml`, and calls the pipeline stages from this package
(`targets_locations()`, `targets_tracks()`, ...). List the package in
`tar_option_set(packages = "hsaCEtool", imports = "hsaCEtool")` so that a
change to its code reruns the affected steps.

## Caribou locations are confidential

This package holds code and made-up test data only. Raw caribou locations, and
anything that still holds them (such as the used steps of an iSSA table, or
mortality sites), must stay in each project's restricted storage. Derived
features on their own, such as iSSA random steps or home ranges, are not
restricted. See `AGENTS.md`.

---

Drafted with assistance from Claude (Claude Code).
