# === Sharing results between computers ------------------------------------------
# The targets store is kept on this computer (fast, and safe from cloud-drive
# syncing part way through a write). After each run, push_store() copies new and
# changed files to the shared folder (`paths: shared`, e.g. the WSC_Yukon Google
# shared drive). On another computer, pull_store() copies them back, so that
# computer can carry on from those results instead of rerunning everything.
#
#   run_pipeline('NMC')   # tar_make(), then push_store()
#   pull_store('NMC')     # on the other computer, before running

#' @title Run the pipeline and share the results
#' @description Runs `targets::tar_make()` for a project, then copies the
#'   results to the shared folder with [push_store()], even if the run stopped
#'   with an error (finished targets are still worth sharing).
#' @param project character. Project name, matching `config/<project>.yaml`.
#' @param share logical. Copy results to the shared folder afterwards?
#' @param ... passed to `targets::tar_make()`, e.g. `names = steps`.
#' @return NULL, invisibly.
#' @author Julie W. Turner
#' @export
run_pipeline <- function(project = Sys.getenv('TAR_PROJECT'), share = TRUE, ...) {
  Sys.setenv(TAR_PROJECT = project)
  result <- tryCatch(targets::tar_make(...), error = function(e) e)
  if (share) push_store(project)
  if (inherits(result, 'error')) stop(result)
  invisible()
}

#' @title Copy this computer's results to the shared folder
#' @description Copies new and changed files from the local targets store and
#'   the project's outputs folder to `<paths: shared>/<project>/`. Results
#'   (`objects/`) are copied before the record of them (`meta/`), so the shared
#'   copy never lists results that are not there yet. Stops if the shared copy
#'   was updated from another computer since this computer last synced, so one
#'   computer's results don't silently replace another's.
#' @inheritParams run_pipeline
#' @param prune logical. Also delete shared files no longer in the local copy
#'   (e.g. results of targets that were removed)? Run with `dry_run = TRUE` first.
#' @param force logical. Push even if the shared copy is newer.
#' @param dry_run logical. Only list what would be copied or deleted.
#' @return data.table of files copied (and deleted), invisibly.
#' @author Julie W. Turner
#' @export
push_store <- function(project = Sys.getenv('TAR_PROJECT'), prune = FALSE, force = FALSE,
                       dry_run = FALSE) {
  loc <- store_locations(project)
  remote_info <- read_sync_info(file.path(loc$remote, 'sync_info.yaml'))
  local_info <- read_sync_info(loc$local_info)
  if (!force && !is.null(remote_info) &&
      !identical(remote_info$pushed_by, Sys.info()[['nodename']]) &&
      (is.null(local_info) || remote_info$pushed_at > local_info$synced_at)) {
    stop('The shared results for ', project, ' were updated from ', remote_info$pushed_by,
         ' at ', remote_info$pushed_at, ', after this computer last synced. Run pull_store("',
         project, '") first, or push_store("', project, '", force = TRUE) to replace them.',
         call. = FALSE)
  }

  res <- rbind(
    sync_folder(loc$store, file.path(loc$remote, '_targets'), prune = prune, dry_run = dry_run),
    sync_folder(loc$outputs, file.path(loc$remote, 'outputs'), prune = prune, dry_run = dry_run)
  )
  if (!dry_run) {
    now <- format(Sys.time(), '%Y-%m-%d %H:%M:%S', tz = 'UTC')
    yaml::write_yaml(list(pushed_by = Sys.info()[['nodename']], pushed_at = now),
                     file.path(loc$remote, 'sync_info.yaml'))
    write_sync_info(loc$local_info, now)
  }
  message(sprintf('[push_store] %s: %s files copied%s to %s', project, sum(res$action == 'copy'),
                  if (prune) sprintf(', %s deleted', sum(res$action == 'delete')) else '', loc$remote))
  invisible(res)
}

#' @title Copy the shared results to this computer
#' @description The reverse of [push_store()]: copies new and changed files
#'   from `<paths: shared>/<project>/` into the local store and outputs folder.
#'   Local results of targets that were rerun here since the last sync are
#'   replaced by the shared ones.
#' @inheritParams push_store
#' @return data.table of files copied (and deleted), invisibly.
#' @author Julie W. Turner
#' @export
pull_store <- function(project = Sys.getenv('TAR_PROJECT'), prune = FALSE, dry_run = FALSE) {
  loc <- store_locations(project)
  if (!dir.exists(file.path(loc$remote, '_targets'))) {
    stop('No shared results for ', project, ' in ', loc$remote, call. = FALSE)
  }
  res <- rbind(
    sync_folder(file.path(loc$remote, '_targets'), loc$store, prune = prune, dry_run = dry_run),
    sync_folder(file.path(loc$remote, 'outputs'), loc$outputs, prune = prune, dry_run = dry_run)
  )
  if (!dry_run) write_sync_info(loc$local_info, format(Sys.time(), '%Y-%m-%d %H:%M:%S', tz = 'UTC'))
  message(sprintf('[pull_store] %s: %s files copied from %s', project, sum(res$action == 'copy'), loc$remote))
  invisible(res)
}

#' @title Where a project's local and shared results are
#' @inheritParams run_pipeline
#' @return named list: `store`, `outputs`, `remote`, `local_info`.
#' @keywords internal
#' @noRd
store_locations <- function(project) {
  cfg <- read_config(project)
  if (!nzchar(cfg$paths$shared %||% '')) {
    stop('No shared folder set. Add `paths: shared: <folder>` to config/local.yaml.', call. = FALSE)
  }
  if (!dir.exists(cfg$paths$shared)) {
    stop('Shared folder not found: ', cfg$paths$shared, ' (is Google Drive running?)', call. = FALSE)
  }
  list(
    store = targets::tar_config_get('store', project = project),
    outputs = file.path(cfg$paths$outputs, project),
    remote = file.path(cfg$paths$shared, project),
    local_info = file.path('.sync', paste0(project, '.yaml'))
  )
}

#' @keywords internal
#' @noRd
read_sync_info <- function(file) {
  if (file.exists(file)) yaml::read_yaml(file) else NULL
}

#' @keywords internal
#' @noRd
write_sync_info <- function(file, synced_at) {
  dir.create(dirname(file), showWarnings = FALSE)
  yaml::write_yaml(list(synced_at = synced_at), file)
}

#' @title Copy new and changed files from one folder to another
#' @description A file is copied when it is missing from `to`, or its size or
#'   modification time differs. Modification times are kept, so unchanged files
#'   are skipped next time. Files under `meta/` are copied last.
#' @param from,to character. Folders.
#' @param prune logical. Delete files in `to` that are not in `from`?
#' @param dry_run logical. Only list what would be done.
#' @return data.table with columns `file` and `action` ("copy" or "delete").
#' @author Julie W. Turner
#' @keywords internal
#' @noRd
sync_folder <- function(from, to, prune = FALSE, dry_run = FALSE) {
  out <- data.table::data.table(file = character(), action = character())
  if (!dir.exists(from)) return(out)
  src <- list.files(from, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  dst <- if (dir.exists(to)) list.files(to, recursive = TRUE, all.files = TRUE, no.. = TRUE) else character()

  si <- file.info(file.path(from, src))
  di <- file.info(file.path(to, src))
  # cloud drives can round modification times, so allow 2 seconds
  changed <- is.na(di$size) | si$size != di$size |
    abs(as.numeric(si$mtime) - as.numeric(di$mtime)) > 2
  copy <- src[changed]
  copy <- c(copy[!startsWith(copy, 'meta/')], copy[startsWith(copy, 'meta/')])
  remove <- if (prune) setdiff(dst, src) else character()

  if (!dry_run) {
    for (f in copy) {
      dir.create(dirname(file.path(to, f)), recursive = TRUE, showWarnings = FALSE)
      ok <- file.copy(file.path(from, f), file.path(to, f), overwrite = TRUE, copy.date = TRUE)
      if (!ok) stop('Could not copy ', file.path(from, f), ' to ', file.path(to, f), call. = FALSE)
    }
    if (length(remove)) unlink(file.path(to, remove))
  }
  rbind(out,
        data.table::data.table(file = copy, action = rep('copy', length(copy))),
        data.table::data.table(file = remove, action = rep('delete', length(remove))))
}
