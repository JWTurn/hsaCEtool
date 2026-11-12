# === Targets: layer stacks and selection maps -----------------------------------
# Comparable to caribou_SSUD. The layer stacks (model land) are made here; the
# selection maps are still to come.

#' @title Targets: layer stacks for maps (and, to come, selection maps)
#' @description With years in `model_land: years:` (and layers and models set):
#'   - `cfg_model_land`: the formulas whose covariates go in the stacks
#'     (`model_land: models:`), with `model: groups:` and `distance_cap:`;
#'   - `model_land_spec`: what each stack layer is ([make_model_land_spec()]);
#'   - `model_land_layers_file`: that list as
#'     outputs/<project>/model_land/model_land_layers.csv;
#'   - `model_land`: one stack per year, outputs/<project>/model_land/
#'     model_land_<year>.tif ([make_model_land()]), one branch per year.
#'   [upload_model_land()], run by hand, puts some years on Drive.
#' @inheritParams targets_config
#' @return list of targets (empty if no years are set).
#' @author Julie W. Turner
#' @export
targets_maps <- function(cfg) {
  ml <- cfg$model_land
  if (!length(unlist(ml$years)) || is.null(cfg$layers) || nrow(cfg$layers) == 0 || !length(cfg$model$models)) {
    return(list())
  }
  # built layers have templates, which mask the stacks
  tmpl <- if (any(!is.na(cfg$layers$build))) quote(templates) else NULL
  list(
    tar_target(cfg_model_land, list(
      formulas = model_land_formulas(cfg$model$models, cfg$model_land$models),
      groups = cfg$model$groups,
      distance_cap = cfg$model$distance_cap
    )),
    tar_target(model_land_spec, make_model_land_spec(cfg_model_land$formulas, cfg_model_land$groups,
                                                     cfg_model_land$distance_cap, layer_index)),
    tar_target(model_land_years, as.integer(unlist(cfg$model_land$years))),
    tar_target(model_land_dir, file.path(cfg_paths$outputs, cfg_project$name, 'model_land')),
    tar_target(model_land_layers_file,
               write_model_land_spec(model_land_spec, cfg_model_land$distance_cap, model_land_dir),
               format = 'file'),
    tar_target_raw('model_land', substitute(
      make_model_land(model_land_years, layer_index, model_land_spec, cfg_model_land$distance_cap,
                      templates = TMPL, out_dir = model_land_dir),
      list(TMPL = tmpl)),
      pattern = quote(map(model_land_years)), format = 'file')
  )
}
