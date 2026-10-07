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
raster_source <- function(sourceFile, access = 'stream', cache = 'inputs', connections = 4) {
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
#'   ranges at the same time and joined. A piece that stalls or is refused is
#'   resumed from where it stopped, up to `attempts` times with a growing pause
#'   (the server can also make requests wait when it is busy, e.g. when several
#'   workers download at once). Pieces are kept between runs, so an interrupted
#'   download continues next time. The file only appears at `dest` once complete.
#' @param url character. Web address.
#' @param dest character. File to write.
#' @param connections integer. Pieces downloaded at once.
#' @param attempts integer. Tries per piece.
#' @return `dest`, invisibly.
#' @keywords internal
#' @noRd
download_parallel <- function(url, dest, connections = 4, attempts = 5) {
  opts <- list(connecttimeout = 60, low_speed_limit = 1000, low_speed_time = 120)
  handle <- function(...) curl::handle_setopt(curl::new_handle(...), .list = opts)
  head <- curl::curl_fetch_memory(url, handle = handle(nobody = TRUE))
  hdr <- curl::parse_headers_list(head$headers)
  size <- suppressWarnings(as.numeric(hdr[['content-length']]))
  part <- paste0(dest, '.part')
  if (is.na(size) || !identical(tolower(hdr[['accept-ranges']] %||% ''), 'bytes') || connections <= 1) {
    curl::curl_download(url, part, quiet = TRUE, handle = handle())
    file.rename(part, dest)
    return(invisible(dest))
  }

  br <- floor(seq(0, size, length.out = connections + 1))
  from <- br[-length(br)]
  to <- br[-1] - 1
  piece <- sprintf('%s.%.0f-%.0f', part, from, to)
  # bits of each piece, one file per try: <piece>.1, <piece>.2, ...
  bits <- function(i) {
    f <- list.files(dirname(dest), full.names = TRUE,
                    pattern = paste0('^', gsub('([.+-])', '\\\\\\1', basename(piece[i])), '\\.[0-9]+$'))
    f[order(as.integer(sub('.*\\.', '', f)))]
  }
  # pieces from an earlier download split differently cannot be resumed
  old <- list.files(dirname(dest), full.names = TRUE,
                    pattern = paste0('^', gsub('([.+-])', '\\\\\\1', basename(part))))
  unlink(setdiff(old, unlist(purrr::map(seq_along(piece), bits))))

  for (attempt in seq_len(attempts)) {
    have <- purrr::map_dbl(seq_along(piece), function(i) sum(file.size(bits(i))))
    todo <- which(have < to - from + 1)
    if (length(todo) == 0) break
    if (attempt > 1) Sys.sleep(30 * (attempt - 1))
    pool <- curl::new_pool()
    files <- character(length(todo))
    cons <- list()
    status <- rep(NA_integer_, length(todo))
    for (j in seq_along(todo)) {
      i <- todo[j]
      files[j] <- paste0(piece[i], '.', length(bits(i)) + 1)
      cons[[j]] <- file(files[j], open = 'wb')
      local({
        jj <- j
        curl::curl_fetch_multi(
          url, pool = pool, data = cons[[jj]],
          done = function(res) status[jj] <<- res$status_code,
          fail = function(e) NULL,
          handle = handle(range = sprintf('%.0f-%.0f', from[i] + have[i], to[i])))
      })
    }
    curl::multi_run(pool = pool)
    purrr::walk(cons, close)
    # keep what arrived from a partial-content reply or a stalled one; drop error pages
    unlink(files[!is.na(status) & status != 206])
  }

  sizes <- purrr::map_dbl(seq_along(piece), function(i) sum(file.size(bits(i))))
  if (any(sizes != to - from + 1)) {
    stop('Download incomplete after ', attempts, ' tries (the pieces are kept and resume next run): ',
         url, call. = FALSE)
  }
  all_bits <- unlist(purrr::map(seq_along(piece), bits))
  file.rename(all_bits[1], part)
  file.append(part, all_bits[-1])
  unlink(all_bits)
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
