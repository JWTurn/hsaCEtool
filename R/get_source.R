#' @title Get an input file from a local path or a URL
#' @description Every input in the settings is a "source": either a `path` to a
#'   file on this computer, or a `url` (web or cloud drive, e.g. Google Drive) that
#'   is downloaded with [reproducible::prepInputs()]. Downloads are kept in
#'   `paths$inputs` and are not downloaded again while the copy there is complete.
#' @param src named list. The source: `path`, or `url` with optional
#'   `target_file` (required with a url), `archive` and `fun` (the function
#'   prepInputs uses to read the file; by default chosen from `target_file`'s
#'   type: `terra::rast` for rasters, `sf::st_read` for vector files, `readRDS`
#'   for .rds).
#' @param paths named list. The `paths:` settings (`data_in`, `inputs`).
#' @return character. Path(s) to the file; for a shapefile, the `.shp` first and
#'   then its companion files, so that `format = "file"` targets notice changes to any of them.
#' @author Julie W. Turner
get_source <- function(src, paths) {
  if (!is.null(src$url) && nzchar(src$url)) {
    dir.create(paths$inputs, recursive = TRUE, showWarnings = FALSE)
    if (is.null(src$target_file)) stop('Set target_file for ', src$url, call. = FALSE)
    fun <- src$fun %||% switch(tolower(tools::file_ext(src$target_file)),
                               tif = , tiff = , grd = 'terra::rast',
                               shp = , gpkg = , geojson = 'sf::st_read',
                               rds = 'readRDS',
                               stop('Set fun for ', src$target_file, call. = FALSE))
    reproducible::prepInputs(
      url = src$url,
      targetFile = src$target_file,
      archive = src$archive,
      destinationPath = paths$inputs,
      fun = fun
    )
    path <- file.path(paths$inputs, src$target_file)
  } else if (!is.null(src$path) && nzchar(src$path)) {
    path <- src$path
    if (!file.exists(path) && nzchar(paths$data_in %||% '')) {
      path <- file.path(paths$data_in, path)
    }
  } else {
    stop('An input has neither a path nor a url')
  }

  if (length(path) != 1 || !file.exists(path)) {
    stop('Input not found: ', paste(c(src$path, src$url, src$target_file), collapse = ' '),
         '. With a url, set target_file to the file wanted from the download.')
  }
  path <- normalizePath(path)
  # inside the project, keep paths relative so stored results stay valid on
  # other computers (see push_store())
  wd <- normalizePath(getwd())
  inside <- startsWith(path, paste0(wd, '/'))
  path[inside] <- substring(path[inside], nchar(wd) + 2)

  # keep shapefile companions together
  if (tolower(tools::file_ext(path)) == 'shp') {
    base <- tools::file_path_sans_ext(path)
    sidecars <- paste0(base, c('.shx', '.dbf', '.prj', '.cpg'))
    path <- c(path, sidecars[file.exists(sidecars)])
  }
  path
}
