# === Human disturbance layers ---------------------------------------------------------
# Linear (roads, trails, seismic lines...) and polygon (mines, clearings...)
# human disturbance from several sources (e.g. GeoYukon surface disturbance and
# the BC and NWT compilations), sorted into categories by rules in the settings
# (landscape: disturbance:) and put onto the 30 m template. Each category's
# layers (distance, presence, density) are then built like the harvest layers
# (harvest_layers.R), counting every feature built up to that year. Replaces the
# human disturbance section of NMC_CEtool scripts/3-prep-land-UPDATED.R.

#' @title Download one disturbance source
#' @description Uses [reproducible::prepInputs()]: a zip is downloaded and
#'   unpacked, a Google Drive folder is downloaded file by file, and a single
#'   file (e.g. a spreadsheet) is downloaded as it is. Files already in `dir`
#'   are not downloaded again (prepInputs does not check the web for a newer
#'   version). With `refresh = TRUE`, the source is downloaded afresh and
#'   replaces the copies in `dir` (see [prep_inputs_fresh()]).
#' @param url character. Web address of a file or zip, or a Google Drive folder.
#' @param files character. The files wanted from the download.
#' @param dir character. Folder to download into.
#' @param refresh logical. Download again even if the files are in `dir`?
#' @return character. The files, with their shapefile companions.
#' @author Julie W. Turner
download_disturbance <- function(url, files, dir, refresh = FALSE) {
  reproducible::checkPath(dir, create = TRUE)
  folder <- grepl('drive.google.com/drive/folders', url, fixed = TRUE)
  get <- if (isTRUE(refresh)) prep_inputs_fresh else reproducible::prepInputs
  get(url = url, destinationPath = dir, fun = NA, targetFile = if (folder) NULL else files[1])
  paths <- file.path(dir, files)
  missing <- !file.exists(paths)
  if (any(missing)) {
    stop('Not in the download from ', url, ': ', paste(files[missing], collapse = ', '), call. = FALSE)
  }
  shp <- tolower(tools::file_ext(paths)) == 'shp'
  sidecars <- as.character(unlist(purrr::map(tools::file_path_sans_ext(paths[shp]),
                                             function(b) paste0(b, c('.shx', '.dbf', '.prj', '.cpg')))))
  c(paths, sidecars[file.exists(sidecars)])
}

#' @title Sort one source's features into categories, with the year each was built
#' @description For each file of the source: reads the features overlapping
#'   `template` (the area distances are measured over), adds columns from a
#'   lookup table if the file has one (e.g. a reclassification spreadsheet),
#'   gives each feature the category of the first rule it matches, and its year
#'   from the first of its year fields that has one (none = there in every
#'   year). Features in category "drop" are left out. All are written, in
#'   `crs`, to one GeoPackage, so later steps only rerun when the features
#'   change, not each time an identical copy is downloaded.
#' @param files character. From [download_disturbance()].
#' @param src named list. The source's settings (`landscape: disturbance:
#'   sources: <name>`): `url` and `files`, each with `file`, `category` or
#'   `rules`, and optionally `year`, `lookup` and `crs`.
#' @param lookups named list. Lookup table files by name, from [download_disturbance()].
#' @param template character. Raster whose extent sets the area kept.
#' @param crs character. The project's coordinate system.
#' @param out character. GeoPackage to write.
#' @return `out`.
#' @author Julie W. Turner
prep_disturbance_features <- function(files, src, lookups, template, crs, out) {
  reproducible::checkPath(dirname(out), create = TRUE)
  e <- unname(as.vector(terra::ext(terra::rast(template))))
  box <- sf::st_as_sfc(sf::st_bbox(c(xmin = e[1], ymin = e[3], xmax = e[2], ymax = e[4]), crs = sf::st_crs(crs)))
  parts <- purrr::map(src$files, function(fs) {
    path <- files[basename(files) == fs$file]
    if (length(path) != 1) stop(fs$file, ' was not downloaded', call. = FALSE)
    lyr <- sf::st_layers(path)
    geom <- tolower(lyr$geomtype[[1]][1])
    # read curved geometries (e.g. MULTISURFACE) as ordinary lines or polygons
    type <- if (grepl('polygon|surface', geom)) 6 else if (grepl('line|curve', geom)) 5 else 0
    src_crs <- fs$crs %||% src$crs %||% lyr$crs[[1]]
    area <- sf::st_transform(sf::st_segmentize(box, 1000), src_crs)
    x <- sf::st_read(path, layer = lyr$name[1], wkt_filter = sf::st_as_text(area), type = type, quiet = TRUE)
    if (is.na(sf::st_crs(x))) sf::st_crs(x) <- src_crs
    d <- data.table::as.data.table(sf::st_drop_geometry(x))
    if (!is.null(fs$lookup)) d <- add_lookup(d, fs$lookup, lookups, fs$file)
    category <- disturbance_category(d, fs)
    year <- feature_year(d, unlist(fs$year), fs$file)
    keep <- !is.na(category) & category != 'drop' & !sf::st_is_empty(x)
    sf::st_sf(category = category[keep], year = year[keep], file = rep(fs$file, sum(keep)),
              geometry = sf::st_transform(sf::st_geometry(x)[keep], crs))
  })
  feats <- purrr::reduce(parts, rbind)
  unlink(out)
  sf::st_write(feats, out, layer = 'disturbance', quiet = TRUE)
  out
}

#' @title Add columns from a lookup table, matching on some of the features' fields
#' @param d data.table. The features' attributes.
#' @param lookup named list. `table` (name in `lookups`), `sheet`, `by` (feature
#'   field: table column) and `add` (table columns to add).
#' @param lookups named list. Lookup table files (.xlsx or .csv) by name.
#' @param file character. The features' file, for messages.
#' @return `d` with the `add` columns (NA where the types are not in the table).
#' @keywords internal
#' @noRd
add_lookup <- function(d, lookup, lookups, file) {
  tf <- lookups[[lookup$table]]
  if (is.null(tf)) stop(file, ': no lookup table "', lookup$table, '"', call. = FALSE)
  tab <- if (grepl('[.]xlsx?$', tf, ignore.case = TRUE)) {
    data.table::as.data.table(readxl::read_excel(tf, sheet = lookup$sheet %||% 1))
  } else {
    data.table::fread(tf)
  }
  by <- unlist(lookup$by)
  add <- unlist(lookup$add)
  miss <- setdiff(c(by, add), names(tab))
  if (length(miss)) stop('Lookup ', lookup$table, ' has no column(s) ', paste(miss, collapse = ', '), call. = FALSE)
  miss <- setdiff(names(by), names(d))
  if (length(miss)) stop(file, ' has no field(s) ', paste(miss, collapse = ', '), call. = FALSE)
  tab <- unique(tab[, c(by, add), with = FALSE])
  data.table::setnames(tab, by, names(by))
  for (k in names(by)) {
    tab[, (k) := trimws(as.character(get(k)))]
    d[, (k) := trimws(as.character(get(k)))]
  }
  dup <- tab[, .N, by = c(names(by))][N > 1]
  if (nrow(dup)) {
    stop('Lookup ', lookup$table, ' gives more than one row for: ',
         paste(purrr::pmap_chr(dup[, names(by), with = FALSE], paste, sep = ' / '), collapse = '; '), call. = FALSE)
  }
  d <- tab[d, on = names(by)]
  unmatched <- d[is.na(get(add[1])), .N, by = c(names(by))]
  if (nrow(unmatched)) {
    warning(file, ': types not in lookup ', lookup$table, ' (sorted by the other rules): ',
            paste0(purrr::pmap_chr(unmatched[, names(by), with = FALSE], paste, sep = ' / '),
                   ' (', unmatched$N, ')', collapse = '; '), call. = FALSE)
  }
  d
}

#' @title Category of each feature, from the first rule it matches
#' @description A file's settings give either one `category` for all its
#'   features, or `rules`: each rule has `where` (field: value or values) and a
#'   `category`; a rule without `where` matches every feature left.
#' @param d data.table. The features' attributes.
#' @param fs named list. The file's settings.
#' @return character, one per feature (NA = no rule matched).
#' @keywords internal
#' @noRd
disturbance_category <- function(d, fs) {
  if (!is.null(fs$category)) return(rep(fs$category, nrow(d)))
  out <- rep(NA_character_, nrow(d))
  for (rule in fs$rules) {
    hit <- is.na(out)
    for (f in names(rule$where)) {
      if (!f %in% names(d)) stop(fs$file, ': no field "', f, '" for the rule giving ', rule$category, call. = FALSE)
      hit <- hit & trimws(as.character(d[[f]])) %in% as.character(unlist(rule$where[[f]]))
    }
    out[hit] <- rule$category
  }
  if (anyNA(out)) warning(fs$file, ': ', sum(is.na(out)), ' features match no rule and are left out', call. = FALSE)
  out
}

#' @title Year each feature was built, from the first of its year fields that has one
#' @description Fields can hold a year (2003), a date ("1993-03-01") or a
#'   date and time; the year is the first four digits.
#' @param d data.table. The features' attributes.
#' @param fields character. Year fields, in order of preference.
#' @param file character. The features' file, for messages.
#' @return integer, one per feature (NA = no year: there in every year).
#' @keywords internal
#' @noRd
feature_year <- function(d, fields, file = '') {
  yr <- rep(NA_integer_, nrow(d))
  for (f in fields) {
    if (!f %in% names(d)) stop(file, ': no year field "', f, '"', call. = FALSE)
    v <- d[[f]]
    s <- trimws(if (inherits(v, c('Date', 'POSIXt'))) format(v, '%Y') else as.character(v))
    y <- suppressWarnings(as.integer(ifelse(grepl('^[0-9]{4}', s), substr(s, 1, 4), NA)))
    y[!is.na(y) & (y < 1800 | y > 2100)] <- NA
    yr[is.na(yr)] <- y[is.na(yr)]
  }
  yr
}

#' @title The 30 m cells each feature of one category touches, and from when
#' @description Puts every feature of the category, from all sources, onto
#'   `template`'s grid: a cell is disturbed if any feature touches it (as
#'   `terra::rasterize(touches = TRUE)` in 3-prep-land-UPDATED.R), from the
#'   earliest year of those features.
#' @param features character. GeoPackages from [prep_disturbance_features()].
#' @param category character. The category, e.g. "paved".
#' @param template character. Raster giving the grid (the template with a
#'   margin, see [make_margin_template()]).
#' @return data.table with columns `cell` (cell number in `template`) and
#'   `year` (0 = no year known: there in every year).
#' @author Julie W. Turner
disturbance_cells <- function(features, category, template) {
  empty <- data.table::data.table(cell = numeric(), year = integer())
  q <- sprintf("SELECT * FROM disturbance WHERE category = '%s'", gsub("'", "''", category))
  x <- purrr::reduce(purrr::map(features, function(f) sf::st_read(f, query = q, quiet = TRUE)), rbind)
  if (nrow(x) == 0) return(empty)
  # burn the year (1 = no year), earliest last so that it is the one kept
  x$burn <- ifelse(is.na(x$year), 1L, x$year)
  x <- x[order(-x$burn), 'burn']
  vec <- tempfile(fileext = '.gpkg')
  ras <- tempfile(fileext = '.tif')
  on.exit(unlink(c(vec, ras)), add = TRUE)
  sf::st_write(x, vec, layer = 'f', quiet = TRUE)
  tr <- terra::rast(template)
  e <- as.vector(terra::ext(tr))
  # gdal_rasterize streams to disk, so very large grids are fine
  sf::gdal_utils('rasterize', vec, ras, options = c(
    '-l', 'f', '-a', 'burn', '-at', '-te', e[1], e[3], e[2], e[4], '-tr', terra::res(tr),
    '-ot', 'UInt16', '-init', '0', '-a_nodata', '0', '-co', 'COMPRESS=DEFLATE', '-co', 'TILED=YES'))
  r <- terra::rast(ras)
  cells <- terra::cells(r)
  if (length(cells) == 0) return(empty)
  burn <- terra::extract(r, cells)[[1]]
  data.table::data.table(cell = as.numeric(cells), year = as.integer(ifelse(burn == 1, 0L, burn)))
}

#' @title Years in which a category's features change
#' @description Layers are built only for `first_year` and each later year
#'   (up to `last_year`) in which a feature was built; other years use the
#'   layer of the year before (see [pick_layer_files()]).
#' @param history data.table. From [disturbance_cells()].
#' @param first_year,last_year integer. Years wanted.
#' @return integer.
#' @keywords internal
#' @noRd
disturbance_years <- function(history, first_year, last_year) {
  y <- history$year
  as.integer(sort(unique(c(first_year, y[y > first_year & y <= last_year]))))
}

#' @title Build a disturbance category's layers for some years
#' @description As [build_harvest_layers()], counting every feature built up to
#'   each year (not only recent ones):
#'   \describe{
#'     \item{pa}{1 if any of the cell's 30 m cells is disturbed, otherwise 0.}
#'     \item{dens}{proportion of the cell's 30 m cells that are disturbed.}
#'     \item{dist}{mean, over the cell's 30 m cells, of the distance (m) to the
#'       nearest disturbed 30 m cell, counting those in the margin around the
#'       template.}
#'   }
#' @param history data.table. From [disturbance_cells()].
#' @param metric character. "pa", "dens" or "dist".
#' @param prefix character. File name start, e.g. "disturb_paved".
#' @param margin character. The template with a margin, which `history`'s cell
#'   numbers refer to.
#' @inheritParams build_harvest_layers
#' @return character. The files written, named `<prefix>_<metric>_<res>m_<year>.tif`.
#' @author Julie W. Turner
build_disturbance_layers <- function(history, metric, resolutions, years, templates, out_dir, prefix, margin) {
  metric <- match.arg(metric, c('pa', 'dens', 'dist'))
  build_harvest_layers(history, metric, resolutions, years, Inf, templates, out_dir, prefix, margin)
}

