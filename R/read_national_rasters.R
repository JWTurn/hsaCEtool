# === Reading national rasters -----------------------------------------------------------
# Reading Canada-wide 30 m rasters (CanLaD, SCANFI, NTEMS) onto the template
# grid, either streamed from the web or downloaded once to a shared cache.
# Used by the harvest (canlad.R) and land-cover (landcover_layers.R) builders.

#' @title Where to read a national raster from
#' @description A local file is used as it is. For a web address, with
#'   `access = "stream"` the raster is read straight from the web, fetching only
#'   the parts that cover the template (nothing is stored). With
#'   `access = "download"` the whole file is downloaded once into `cache` (like
#'   `reproducible.inputPaths`, one folder can serve several projects) and read
#'   from there.
#' @param sourceFile character. Web address (URL) or local path of the raster.
#' @param access character. "stream" or "download".
#' @param cache character. Folder for downloads.
#' @param connections integer. For downloads, how many pieces of the file are
#'   fetched at once (some servers limit the speed of each connection).
#' @return character. Something GDAL can open.
#' @author Julie W. Turner
raster_source <- function(sourceFile, access = 'stream', cache = 'inputs', connections = 8) {
  if (file.exists(sourceFile)) return(sourceFile)
  if (identical(access, 'stream')) return(paste0('/vsicurl/', sourceFile))
  if (!identical(access, 'download')) stop('access must be "stream" or "download"', call. = FALSE)
  reproducible::checkPath(cache, create = TRUE)
  dest <- file.path(cache, basename(sourceFile))
  if (!file.exists(dest)) download_parallel(sourceFile, dest, connections)
  dest
}

#' @title Download a large file in several pieces at once
#' @description Some servers (e.g. NRCan's, about 1 MB/s per connection) limit
#'   the speed of each connection, so the file is fetched as `connections` byte
#'   ranges at the same time and joined. The file only appears at `dest` once
#'   complete, so an interrupted download is never mistaken for a finished one.
#' @param url character. Web address.
#' @param dest character. File to write.
#' @param connections integer. Pieces downloaded at once.
#' @return `dest`, invisibly.
#' @keywords internal
#' @noRd
download_parallel <- function(url, dest, connections = 8) {
  head <- curl::curl_fetch_memory(url, handle = curl::new_handle(nobody = TRUE))
  hdr <- curl::parse_headers_list(head$headers)
  size <- suppressWarnings(as.numeric(hdr[['content-length']]))
  part <- paste0(dest, '.part')
  if (is.na(size) || !identical(tolower(hdr[['accept-ranges']] %||% ''), 'bytes') || connections <= 1) {
    curl::curl_download(url, part, quiet = TRUE)
  } else {
    br <- floor(seq(0, size, length.out = connections + 1))
    pieces <- paste0(part, seq_len(connections))
    cons <- purrr::map(pieces, file, open = 'wb')
    pool <- curl::new_pool()
    for (i in seq_len(connections)) {
      curl::curl_fetch_multi(url, pool = pool, data = cons[[i]],
                             fail = function(e) stop('Download failed: ', url, ': ', e, call. = FALSE),
                             handle = curl::new_handle(range = sprintf('%.0f-%.0f', br[i], br[i + 1] - 1)))
    }
    curl::multi_run(pool = pool)
    purrr::walk(cons, close)
    if (sum(file.size(pieces)) != size) stop('Incomplete download: ', url, call. = FALSE)
    file.copy(pieces[1], part, overwrite = TRUE)
    file.append(part, pieces[-1])
    unlink(pieces)
  }
  file.rename(part, dest)
  invisible(dest)
}

#' @title Put a raster onto the template grid
#' @description One gdalwarp call crops, reprojects and resamples (nearest
#'   neighbour, so codes and years are kept as they are). A raster read from the
#'   web is first cut to the template's area in its own projection, which reads
#'   it in larger, faster pieces.
#' @param src character. Raster to read (see [raster_source()]).
#' @param template character. Template raster file.
#' @param dst character. File to write.
#' @return `dst`.
#' @keywords internal
#' @noRd
warp_to_template <- function(src, template, dst = tempfile(fileext = '.tif')) {
  tr <- terra::rast(template)
  e <- as.vector(terra::ext(tr))
  if (grepl('/vsicurl/', src, fixed = TRUE)) {
    src <- clip_remote(src, tr)
    on.exit(unlink(src), add = TRUE)
  }
  sf::gdal_utils('warp', src, dst, options = c(
    '-t_srs', terra::crs(tr), '-te', e[1], e[3], e[2], e[4], '-tr', terra::res(tr),
    '-r', 'near', '-co', 'COMPRESS=DEFLATE', '-co', 'TILED=YES', '-overwrite',
    '-multi', '-wo', 'NUM_THREADS=ALL_CPUS'))
  dst
}

#' @title Cut a web raster to the template's area, in the raster's own projection
#' @param src character. A `/vsicurl/` (or `/vsizip//vsicurl/`) raster.
#' @param tr SpatRaster. The template.
#' @return character. A local file.
#' @keywords internal
#' @noRd
clip_remote <- function(src, tr) {
  crs_src <- terra::crs(terra::rast(src))
  area <- terra::project(terra::as.polygons(terra::ext(tr), crs = terra::crs(tr)), crs_src)
  b <- as.vector(terra::ext(terra::buffer(area, 1000)))
  out <- tempfile(fileext = '.tif')
  sf::gdal_utils('translate', src, out,
                 options = c('-projwin', b[1], b[4], b[2], b[3], '-co', 'COMPRESS=DEFLATE', '-co', 'TILED=YES'),
                 config_options = c(GDAL_HTTP_MERGE_CONSECUTIVE_RANGES = 'YES', GDAL_HTTP_MULTIPLEX = 'YES',
                                    GDAL_NUM_THREADS = 'ALL_CPUS', CPL_VSIL_CURL_CHUNK_SIZE = '4194304',
                                    GDAL_DISABLE_READDIR_ON_OPEN = 'EMPTY_DIR'))
  out
}

#' @title A raster inside a zip file, for GDAL
#' @param src character. From [raster_source()].
#' @param archiveFile character. The raster inside the zip, or NULL.
#' @keywords internal
#' @noRd
in_archive <- function(src, archiveFile) {
  if (is.null(archiveFile)) src else paste0('/vsizip/', src, '/', archiveFile)
}

#' @title Fill `{year}` in a file name or web address
#' @param pattern character. With `{year}` where the year goes.
#' @param years integer.
#' @return character, one per year.
#' @keywords internal
#' @noRd
fill_year <- function(pattern, years) {
  purrr::map_chr(years, function(y) gsub('{year}', y, pattern, fixed = TRUE))
}
