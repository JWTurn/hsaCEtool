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
#'   replaces the copies in `dir` (see `prep_inputs_fresh()`).
#' @param url character. Web address of a file or zip, or a Google Drive folder.
#' @param files character. The files wanted from the download.
#' @param dir character. Folder to download into.
#' @param refresh logical. Download again even if the files are in `dir`?
#' @return character. The files, with their shapefile companions.
#' @author Julie W. Turner
#' @export
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
#'   change, not each time an identical copy is downloaded. The GeoPackage
#'   also holds a table (layer "summary") of how many features of each type
#'   went into each category, dropped ones included (see
#'   `summarize_feature_types()`).
#' @param files character. From [download_disturbance()].
#' @param src named list. The source's settings (`landscape: disturbance:
#'   sources: <name>`): `url` and `files`, each with `file`, `category` or
#'   `rules`, and optionally `year`, `lookup`, `crs`, `subtype` or
#'   `subtype_field` (see `disturbance_subtype()`), and `keep`
#'   (fields copied into the features, e.g. for maps).
#' @param lookups named list. Lookup table files by name, from [download_disturbance()].
#' @param template character. Raster whose extent sets the area kept.
#' @param crs character. The project's coordinate system.
#' @param out character. GeoPackage to write.
#' @return `out`, with layers "disturbance" (the features: category,
#'   subtype, year and file) and "summary".
#' @author Julie W. Turner
#' @export
prep_disturbance_features <- function(files, src, lookups, template, crs, out) {
  reproducible::checkPath(dirname(out), create = TRUE)
  e <- unname(as.vector(terra::ext(terra::rast(template))))
  box <- sf::st_as_sfc(sf::st_bbox(c(xmin = e[1], ymin = e[3], xmax = e[2], ymax = e[4]), crs = sf::st_crs(crs)))
  # extra columns kept from the source (files' keep:), blank for files without them
  extra <- unique(unlist(purrr::map(src$files, function(f) f$keep)))
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
    subtype <- disturbance_subtype(d, fs, attr(category, 'rule'))
    year <- feature_year(d, unlist(fs$year), fs$file)
    geom <- sf::st_transform(sf::st_geometry(x), crs)
    full <- !sf::st_is_empty(geom)
    keep <- !is.na(category) & category != 'drop' & full
    feats <- sf::st_sf(category = category[keep], subtype = subtype[keep], year = year[keep],
                       file = rep(fs$file, sum(keep)), geometry = geom[keep])
    for (k in extra) feats[[k]] <- if (k %in% names(d)) as.character(d[[k]])[keep] else rep(NA_character_, sum(keep))
    list(features = feats,
         summary = summarize_feature_types(d[full], fs, category[full], year[full], geom[full], subtype[full]))
  })
  feats <- purrr::reduce(purrr::map(parts, 'features'), rbind)
  unlink(out)
  sf::st_write(feats, out, layer = 'disturbance', quiet = TRUE)
  sf::st_write(data.table::rbindlist(purrr::map(parts, 'summary')), out, layer = 'summary', quiet = TRUE)
  out
}

#' @title How many features of each type went into each category
#' @description The type of a feature is the values of the fields its file's
#'   rules look at (e.g. "feature_type=placer", or "Industry=Transportation"),
#'   or blank for a file whose features all get one category. Features are
#'   those overlapping the area kept (the template with its margin), measured
#'   whole, not cut at its edge.
#' @param d data.table. The features' attributes.
#' @param fs named list. The file's settings.
#' @param category character. Each feature's category (NA = matched no rule).
#' @param year integer. Each feature's year built (NA = none).
#' @param geom sfc. The features, in the project's coordinate system (metres).
#' @param subtype character. Each feature's subtype (NA = none), from
#'   `disturbance_subtype()`.
#' @return data.table, one row per file, type, category and subtype: `features`
#'   (count), `length_km` (lines), `area_km2` (polygons), `no_year` (features
#'   with no year, counted in every year), and the `first_year` and
#'   `last_year` built.
#' @keywords internal
#' @noRd
summarize_feature_types <- function(d, fs, category, year, geom, subtype = rep(NA_character_, nrow(d))) {
  fields <- unique(unlist(purrr::map(fs$rules, function(r) c(names(r$where), names(r$match)))))
  type <- if (length(fields)) {
    purrr::reduce(purrr::map(fields, function(f) paste0(f, '=', trimws(as.character(d[[f]])))),
                  paste, sep = '; ')
  } else rep(NA_character_, nrow(d))
  dt <- data.table::data.table(
    file = rep(fs$file, nrow(d)), type = type,
    category = data.table::fifelse(is.na(category), 'none (left out)', category), subtype = subtype,
    year = year, dim = sf::st_dimension(geom),
    len = as.numeric(sf::st_length(geom)), area = as.numeric(sf::st_area(geom)))
  dt[, .(features = .N,
         length_km = if (any(dim == 1)) round(sum(len[dim == 1]) / 1000, 2) else NA_real_,
         area_km2 = if (any(dim == 2)) round(sum(area[dim == 2]) / 1e6, 2) else NA_real_,
         no_year = sum(is.na(year)),
         first_year = if (all(is.na(year))) NA_integer_ else min(year, na.rm = TRUE),
         last_year = if (all(is.na(year))) NA_integer_ else max(year, na.rm = TRUE)),
     by = .(file, type, category, subtype)][order(file, category, subtype, type)]
}

#' @title Table of the features of each type that went into each category, all sources
#' @description Combines the "summary" layers written by
#'   [prep_disturbance_features()], for reports.
#' @param features character. GeoPackages from [prep_disturbance_features()],
#'   named features_<source>.gpkg.
#' @return data.table as from `summarize_feature_types()`, with `source` first.
#' @author Julie W. Turner
#' @export
summarize_disturbance <- function(features) {
  data.table::rbindlist(purrr::map(features, function(f) {
    s <- data.table::as.data.table(suppressWarnings(sf::st_read(f, layer = 'summary', quiet = TRUE)))
    s[, source := sub('^features_(.*)\\.gpkg$', '\\1', basename(f))]
    data.table::setcolorder(s, 'source')
  }))
}

#' @title Combine every source's features into one lines and one polygons file
#' @description For maps: the features of all sources (as used by the
#'   layers: the template and its margin), with `region` (from `regions`),
#'   `source`, `file`, `category`, `subtype`, `year` (NA = every year) and
#'   any columns the sources keep (e.g. Yukon feature_id, REF_ID, year_rule,
#'   year_source; blank for the others).
#' @param features named character. GeoPackages from [prep_disturbance_features()],
#'   named by source.
#' @param cb named list. `landscape: disturbance: combined:` settings:
#'   `regions` (source: region code) and `files` (`lines` and `areal` file names).
#' @param out_dir character. Folder to write to.
#' @return character. The lines and polygons GeoPackages.
#' @author Julie W. Turner
#' @export
combine_disturbance_features <- function(features, cb, out_dir) {
  reproducible::checkPath(out_dir, create = TRUE)
  x <- purrr::imap(features, function(f, nm) {
    s <- sf::st_read(f, layer = 'disturbance', quiet = TRUE)
    s$source <- nm
    s$region <- unlist(cb$regions)[nm] %||% NA_character_
    s
  })
  cols <- unique(unlist(purrr::map(x, function(s) setdiff(names(s), attr(s, 'sf_column')))))
  x <- purrr::map(x, function(s) {
    for (k in setdiff(cols, names(s))) s[[k]] <- NA_character_
    s[, cols]
  })
  x <- purrr::reduce(x, rbind)
  first <- c('region', 'source', 'file', 'category', 'subtype', 'year')
  x <- x[, c(first, setdiff(cols, first))]
  dim <- sf::st_dimension(x)
  files <- file.path(out_dir, c(cb$files$lines %||% 'combined_linear.gpkg', cb$files$areal %||% 'combined_areal.gpkg'))
  unlink(files)
  sf::st_write(x[dim %in% 1, ], files[1], layer = 'linear', quiet = TRUE)
  sf::st_write(x[dim %in% 2, ], files[2], layer = 'areal', quiet = TRUE)
  files
}

#' @title Upload the combined disturbance files to the restricted Drive folder
#' @description Zips each of the two files from [combine_disturbance_features()]
#'   (target `disturb_combined`; e.g. NMC_linear_combined.gpkg as
#'   NMC_linear_combined.zip) and puts the zips in `landscape: disturbance:
#'   combined: upload_folder:`, replacing files of the same name in place, so
#'   their links stay the same.
#'   The folder must not be shared with anyone with the link (the Yukon work
#'   packages are not public). Run by hand, not by the pipeline.
#' @inheritParams write_premade
#' @details The settings and the targets store are found as in [write_premade()].
#' @return data.table: `name`, `action` (updated or uploaded), `id`, `url`.
#' @author Julie W. Turner
#' @export
upload_combined_disturbance <- function(project = NULL, config_dir = NULL) {
  ctx <- pipeline_context(project, config_dir)
  project <- ctx$project
  cfg <- ctx$cfg
  cb <- cfg$landscape$disturbance$combined
  if (is.null(cb$upload_folder)) {
    stop('Set landscape: disturbance: combined: upload_folder: in ', file.path(ctx$config_dir, project), '.yaml',
         call. = FALSE)
  }
  if (!is.null(cfg$run$google_email)) options(gargle_oauth_email = cfg$run$google_email)
  files <- file.path(cfg$paths$outputs, project, 'derived', 'combined', c(cb$files$lines, cb$files$areal))
  if (!all(file.exists(files))) stop('Not found (run the pipeline first): ', paste(files[!file.exists(files)], collapse = ', '),
                                     call. = FALSE)
  up <- reproducible::checkPath(file.path(cfg$paths$outputs, project, 'derived', 'combined', 'upload'), create = TRUE)
  zips <- file.path(up, sub('[.]gpkg$', '.zip', basename(files)))
  for (i in seq_along(files)) {
    if (file.exists(zips[i])) file.remove(zips[i])
    utils::zip(zips[i], files[i], flags = '-jq')
  }
  upload_to_drive(zips, cb$upload_folder,
                  'The combined disturbance folder is shared with anyone with the link; the Yukon work packages are not public. Restrict it first.')
}

#' @title Write the disturbance summary table
#' @param summary data.table. From [summarize_disturbance()].
#' @param out_dir character. Folder for disturbance_summary.csv.
#' @return character. The CSV file.
#' @author Julie W. Turner
#' @export
write_disturbance_summary <- function(summary, out_dir) {
  reproducible::checkPath(out_dir, create = TRUE)
  file <- file.path(out_dir, 'disturbance_summary.csv')
  data.table::fwrite(summary, file)
  file
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
  if (grepl('[.]xlsx?$', tf, ignore.case = TRUE)) rlang::check_installed('readxl', reason = 'to read xlsx lookup tables.')
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
#'   features, or `rules`: each rule has `where` (field: value or values),
#'   `match` (field: pattern, a regular expression), or both (all must hold),
#'   and a `category`; a rule without either matches every feature left. In
#'   `where`, the value `""` matches a missing value (from a blank cell in a
#'   crosswalk table, see [apply_disturbance_crosswalk()]).
#' @param d data.table. The features' attributes.
#' @param fs named list. The file's settings.
#' @return character, one per feature (NA = no rule matched), with attribute
#'   `rule`: the number of the rule each feature matched.
#' @keywords internal
#' @noRd
disturbance_category <- function(d, fs) {
  if (!is.null(fs$category)) return(structure(rep(fs$category, nrow(d)), rule = rep(NA_integer_, nrow(d))))
  out <- rep(NA_character_, nrow(d))
  which_rule <- rep(NA_integer_, nrow(d))
  for (k in seq_along(fs$rules)) {
    rule <- fs$rules[[k]]
    hit <- is.na(out)
    for (f in c(names(rule$where), names(rule$match))) {
      if (!f %in% names(d)) stop(fs$file, ': no field "', f, '" for the rule giving ', rule$category, call. = FALSE)
    }
    for (f in names(rule$where)) {
      v <- trimws(as.character(d[[f]]))
      v[is.na(v)] <- ''
      hit <- hit & v %in% as.character(unlist(rule$where[[f]]))
    }
    for (f in names(rule$match)) {
      hit <- hit & grepl(rule$match[[f]], as.character(d[[f]]))
    }
    out[hit] <- rule$category
    which_rule[hit] <- k
  }
  if (anyNA(out)) warning(fs$file, ': ', sum(is.na(out)), ' features match no rule and are left out', call. = FALSE)
  structure(out, rule = which_rule)
}

#' @title Subtype of each feature, within its category
#' @description An optional finer grouping within a category (e.g. for
#'   unpaved roads: winter, resource or all-season). From the `subtype` of the rule each feature matched, else
#'   the file's `subtype`, else the value of the file's `subtype_field`.
#'   Recorded on each feature and in the summary; the layers are by category.
#' @param d data.table. The features' attributes.
#' @param fs named list. The file's settings.
#' @param rule integer. The rule each feature matched (attribute `rule` of
#'   `disturbance_category()`).
#' @return character, one per feature (NA = none).
#' @keywords internal
#' @noRd
disturbance_subtype <- function(d, fs, rule) {
  out <- rep(NA_character_, nrow(d))
  # [[ ]], not $: $ would also match subtype_field when looking for subtype
  field <- fs[['subtype_field']]
  if (!is.null(field)) {
    if (!field %in% names(d)) stop(fs$file, ': no subtype_field "', field, '"', call. = FALSE)
    out <- as.character(d[[field]])
  }
  if (!is.null(fs[['subtype']])) out[] <- fs[['subtype']]
  if (length(fs$rules) && length(rule)) {
    rt <- purrr::map_chr(fs$rules, function(r) r[['subtype']] %||% NA_character_)
    from_rule <- !is.na(rule) & !is.na(rt[rule])
    out[from_rule] <- rt[rule[from_rule]]
  }
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
#' @export
disturbance_cells <- function(features, category, template) {
  empty <- data.table::data.table(cell = numeric(), year = integer())
  q <- sprintf("SELECT * FROM disturbance WHERE category = '%s'", gsub("'", "''", category))
  # only the year is needed (sources can keep different extra columns)
  x <- purrr::reduce(purrr::map(features, function(f) sf::st_read(f, query = q, quiet = TRUE)[, 'year']), rbind)
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
#' @export
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
#' @export
build_disturbance_layers <- function(history, metric, resolutions, years, templates, out_dir, prefix, margin) {
  metric <- match.arg(metric, c('pa', 'dens', 'dist'))
  build_harvest_layers(history, metric, resolutions, years, Inf, templates, out_dir, prefix, margin)
}

