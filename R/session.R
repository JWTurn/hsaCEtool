# === Session settings ------------------------------------------------------
# Settings the pipeline needs in the R session that builds the targets. A
# project calls ce_session_setup() near the top of its _targets.R.

#' @title Set up the R session for the pipeline
#' @description Sets the options and environment variables the pipeline relies on.
#'   Call it near the top of a project's `_targets.R`, as `tar_make()` runs that
#'   script in the process that builds the targets.
#'   * `reproducible.useCache = FALSE`: `targets` keeps every result, so the
#'     reproducible package's own cache is turned off (one cache only).
#'   * `timeout` of at least `timeout_s`: large downloads (e.g. the 1.3 GB NBAC
#'     fire file) take longer than R's default of 60 seconds.
#'   * `OGR_SQLITE_ALLOW_ANY_EXTENSION` and a fixed `OGR_CURRENT_DATE`: needed by
#'     geotargets for GeoPackages, and so rewriting an unchanged GeoPackage does not
#'     look like a change.
#'   * `GDAL_HTTP_MAX_RETRY` and `GDAL_HTTP_RETRY_DELAY`: rasters read straight from
#'     the web (e.g. MRDEM, SCANFI) retry a failed read instead of stopping the step.
#' @param timeout_s numeric. Shortest download time limit, in seconds.
#' @return The previous option values, invisibly.
#' @export
#' @author Julie W. Turner
ce_session_setup <- function(timeout_s = 3600) {
  old <- options(reproducible.useCache = FALSE,
                 timeout = max(timeout_s, getOption('timeout')))
  Sys.setenv(OGR_SQLITE_ALLOW_ANY_EXTENSION = 'YES',
             OGR_CURRENT_DATE = '2000-01-01T00:00:00Z',
             GDAL_HTTP_MAX_RETRY = '10', GDAL_HTTP_RETRY_DELAY = '5')
  invisible(old)
}
