# === Which settings and store a helper works on ----------------------------------
# The helpers run by hand after a pipeline run (write_premade(), upload_premade(),
# upload_models(), the disturbance uploads, run_pipeline(), push_store() and
# pull_store()) find a project's settings and targets store the same way, so they
# work in a repository with one targets project and its settings in config/
# (e.g. TAR_PROJECT = 'NMC', config/NMC.yaml) and in one with several, each with
# its settings beside its script (e.g. YT_caribou: TAR_PROJECT = 'dataprep-caribou',
# pipelines/dataprep-caribou/_targets.R and pipelines/dataprep-caribou/config/NMC.yaml).

#' @title Find a project's settings and targets store
#' @description Used by the helpers run by hand after a pipeline run, e.g.
#'   [write_premade()] and [upload_models()]. Found in this order:
#'   \describe{
#'     \item{targets project (the store)}{`project`, if `_targets.yaml` lists
#'       it; otherwise `TAR_PROJECT`.}
#'     \item{settings folder}{`config_dir`; otherwise `config/` beside the
#'       targets project's script (`_targets.R`).}
#'     \item{settings name}{`project`, if `<config_dir>/<project>.yaml`
#'       exists; otherwise the targets project, if its `.yaml` is there;
#'       otherwise the only `.yaml` in the settings folder (apart from
#'       `local.yaml`).}
#'   }
#'   Nothing in the session is changed (e.g. `TAR_PROJECT` is left as it is).
#' @param project character or `NULL`. Settings name (e.g. "NMC") or targets
#'   project (e.g. "dataprep-caribou"); `NULL` = from `TAR_PROJECT`.
#' @param config_dir character or `NULL`. Settings folder; `NULL` = `config/`
#'   beside the targets project's script.
#' @return named list: `project` (settings name), `config_dir`, `tar_project`,
#'   `script`, `store`, and `cfg` (the settings, from [read_config()]).
#' @author Julie W. Turner
#' @keywords internal
#' @noRd
pipeline_context <- function(project = NULL, config_dir = NULL) {
  if (!is.null(project) && !nzchar(project)) project <- NULL
  if (is.null(project) && !nzchar(Sys.getenv('TAR_PROJECT'))) {
    stop('No project chosen. Give project = (e.g. "NMC") or set one first, e.g. Sys.setenv(TAR_PROJECT = "NMC")',
         call. = FALSE)
  }
  listed <- if (file.exists('_targets.yaml')) names(yaml::read_yaml('_targets.yaml')) else character()
  tar_project <- if (!is.null(project) && project %in% listed) project else Sys.getenv('TAR_PROJECT', 'main')
  script <- targets::tar_config_get('script', project = tar_project)
  store <- targets::tar_config_get('store', project = tar_project)
  if (is.null(config_dir)) {
    dir <- dirname(script)
    config_dir <- if (dir %in% c('.', '')) 'config' else file.path(dir, 'config')
  }
  has_yaml <- function(nm) file.exists(file.path(config_dir, paste0(nm, '.yaml')))
  name <- project
  if (is.null(name) || (!has_yaml(name) && name %in% listed)) {
    yamls <- setdiff(tools::file_path_sans_ext(list.files(config_dir, pattern = '[.]yaml$')), c('local', 'default'))
    name <- if (has_yaml(tar_project)) {
      tar_project
    } else if (length(yamls) == 1) {
      yamls
    } else {
      stop('Which settings? Give project = , one of: ', paste(yamls, collapse = ', '), ' (in ', config_dir, ')',
           call. = FALSE)
    }
  }
  list(project = name, config_dir = config_dir, tar_project = tar_project, script = script, store = store,
       cfg = read_config(name, config_dir = config_dir))
}

#' @title Run code with `TAR_PROJECT` set to a targets project
#' @description Checking a store (e.g. [targets::tar_outdated()]) runs the
#'   project's `_targets.R`, which reads its settings from `TAR_PROJECT`. The
#'   helpers run by hand set it to their targets project only for that
#'   check, and leave it afterwards as it was (or unset).
#' @param tar_project character. Targets project (`pipeline_context()$tar_project`).
#' @param code Code to run.
#' @return The value of `code`.
#' @keywords internal
#' @noRd
with_tar_project <- function(tar_project, code) {
  old <- Sys.getenv('TAR_PROJECT', unset = NA)
  on.exit(if (is.na(old)) Sys.unsetenv('TAR_PROJECT') else Sys.setenv(TAR_PROJECT = old), add = TRUE)
  Sys.setenv(TAR_PROJECT = tar_project)
  code
}
