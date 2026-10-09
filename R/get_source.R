#' @title Get an input file from a local path or a URL
#' @description Every input in the settings is a "source": either a `path` to a
#'   file on this computer, or a `url` (web or cloud drive, e.g. Google Drive) that
#'   is downloaded with [reproducible::prepInputs()]. Downloads are kept in
#'   `paths$inputs` and are not downloaded again while the copy there is complete.
#' @param src named list. The source: `path`, or `url` with optional
#'   `target_file` (required with a url), `archive` and `fun` (the function
#'   prepInputs uses to read the file; by default chosen from `target_file`'s
#'   type: `terra::rast` for rasters, `sf::st_read` for vector files, `readRDS`
#'   for .rds, `data.table::fread` for .csv), and `refresh` (TRUE to download
#'   afresh, see [prep_inputs_fresh()]), and `subfolder` (a folder inside
#'   `paths$inputs` to download to, e.g. to keep a file apart from an older
#'   download with the same name).
#' @param paths named list. The `paths:` settings (`data_in`, `inputs`).
#' @return character. Path(s) to the file; for a shapefile, the `.shp` first and
#'   then its companion files, so that `format = "file"` targets notice changes to any of them.
#' @author Julie W. Turner
get_source <- function(src, paths) {
  if (!is.null(src$url) && nzchar(src$url)) {
    dest <- if (nzchar(src$subfolder %||% '')) file.path(paths$inputs, src$subfolder) else paths$inputs
    reproducible::checkPath(dest, create = TRUE)
    if (is.null(src$target_file)) stop('Set target_file for ', src$url, call. = FALSE)
    fun <- as_reader(src$fun %||% switch(tolower(tools::file_ext(src$target_file)),
                               tif = , tiff = , grd = 'terra::rast',
                               shp = , gpkg = , geojson = 'sf::st_read',
                               rds = 'readRDS',
                               csv = 'data.table::fread',
                               stop('Set fun for ', src$target_file, call. = FALSE)))
    get <- if (isTRUE(src$refresh)) prep_inputs_fresh else reproducible::prepInputs
    get(
      url = src$url,
      targetFile = src$target_file,
      archive = src$archive,
      destinationPath = dest,
      fun = fun
    )
    path <- file.path(dest, src$target_file)
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

#' @title Turn a reader given as text into the function itself
#' @description [reproducible::prepInputs()] reads text such as
#'   `"sf::st_read"` as code only when it is typed in the call itself; passed in
#'   a variable, it looks for an object with that name and fails. So readers
#'   from the settings (or the defaults chosen by file type) are converted
#'   first and passed as a variable holding the function: `fun = fun`, not
#'   `fun = as_reader(fun)`, as prepInputs re-runs that expression elsewhere.
#' @param fun character (e.g. `"sf::st_read"`), a function, or `NA` (download
#'   without reading).
#' @return The function, or `fun` unchanged if it is not text.
#' @keywords internal
#' @noRd
as_reader <- function(fun) {
  if (is.character(fun)) eval(str2lang(fun)) else fun
}

#' @title Download afresh with prepInputs, replacing the kept copy
#' @description [reproducible::prepInputs()] does not download a file it
#'   already has, and does not check the web for a newer version. This
#'   downloads into a new temporary folder (ignoring `reproducible.inputPaths`,
#'   so no older copy kept elsewhere is reused), then replaces the copies in
#'   `destinationPath` and their entries in its CHECKSUMS.txt, so later runs
#'   use the new copy without downloading it again. Used for sources with
#'   `refresh: true`.
#' @param destinationPath character. Folder the download is kept in.
#' @param ... Passed to [reproducible::prepInputs()].
#' @return What prepInputs returns (with `fun = NA`, a path in the temporary
#'   folder: use the copies in `destinationPath` instead).
#' @author Julie W. Turner
prep_inputs_fresh <- function(destinationPath, ...) {
  reproducible::checkPath(destinationPath, create = TRUE)
  tmp <- reproducible::checkPath(tempfile('download_'), create = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  out <- withr::with_options(list(reproducible.inputPaths = NULL),
                             reproducible::prepInputs(..., destinationPath = tmp))
  got <- setdiff(list.files(tmp), 'CHECKSUMS.txt')
  file.copy(file.path(tmp, got), destinationPath, overwrite = TRUE)
  # keep the other files' checksums; replace those of the new copies
  new_ck <- file.path(tmp, 'CHECKSUMS.txt')
  old_ck <- file.path(destinationPath, 'CHECKSUMS.txt')
  if (file.exists(new_ck)) {
    ck <- data.table::fread(new_ck, colClasses = 'character')
    if (file.exists(old_ck)) {
      old <- data.table::fread(old_ck, colClasses = 'character')
      ck <- rbind(old[!file %in% ck$file], ck, fill = TRUE)
    }
    data.table::fwrite(ck, old_ck, sep = ' ', quote = TRUE)
  }
  out
}
