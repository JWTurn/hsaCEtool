# === Running steps in parallel ------------------------------------------------------

#' @title Controller for running steps in parallel
#' @description With `run: workers:` above 1, independent steps (e.g. one year
#'   of a layer) run at the same time in separate R sessions ("workers", from
#'   the crew package). Each worker starts with the session settings from
#'   _targets.R (reproducible's cache off) and lets terra use `run: memfrac`
#'   of the computer's memory divided among the workers, so they do not
#'   compete for it, and uses the Google account in `run: google_email` for
#'   Google Drive downloads. Environment variables set in _targets.R are passed on.
#' @param run named list. The `run:` settings: `workers`, `memfrac`, `google_email`.
#' @return A crew controller, or NULL to run one step at a time.
#' @author Julie W. Turner
parallel_controller <- function(run) {
  workers <- as.integer(run$workers %||% 1)
  if (workers <= 1) return(NULL)
  if (!requireNamespace('crew', quietly = TRUE)) {
    stop('run: workers is ', workers, ' but the crew package is not installed', call. = FALSE)
  }
  setup <- sprintf('options(reproducible.useCache = FALSE); terra::terraOptions(memfrac = %s)',
                   signif((run$memfrac %||% 0.6) / workers, 3))
  if (!is.null(run$google_email)) {
    setup <- paste0(setup, sprintf("; options(gargle_oauth_email = '%s')", run$google_email))
  }
  crew::crew_controller_local(
    workers = workers,
    seconds_idle = 60,
    r_arguments = c('--no-save', '--no-restore', '-e', setup)
  )
}
