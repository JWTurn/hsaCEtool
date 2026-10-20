# === Merged Yukon surface disturbance --------------------------------------------------
# GeoYukon surface disturbance merged with the Government of Yukon's newer work
# packages (WPs), as in NMC_CEtool scripts/prep-disturb-cleaned.R (P. Baker,
# October 2026), keeping the year each feature was built where it is known:
# - types are reclassified with the v4/v5 lookup sheet (plus config/yukon_reclass_patches.csv);
# - inside each WP's area of interest (AOI), the WP's features replace GeoYukon's
#   where the WP mapped them again (within 20 m); GeoYukon features the WP did
#   not map are kept; one copy is kept of features in overlapping WPs;
# - reported years (quartz mine production, config/yukon_year_fixes.csv) are added;
# - parts of WP features that were not on the earlier GeoYukon map of an area,
#   seen on later imagery, count from the WP image year: the year they were
#   seen by, not the year they were built (see yukon_new_parts());
# - all other features have no year and count in every year.
# Settings: landscape: disturbance: yukon_merge: (config/NMC.yaml).

#' @title Normalise type text for matching
#' @description Lower case, single spaces, en and em dashes as hyphens.
#' @param x character.
#' @return character.
#' @keywords internal
#' @noRd
yukon_norm <- function(x) {
  x <- gsub('[–—]', '-', x)
  tolower(gsub('\\s+', ' ', trimws(x)))
}

#' @title Rows of the cleaning log
#' @description One row per feature (or group) and action, for reporting the
#'   cleaning back to the data owners.
#' @param step character. Cleaning step.
#' @param kind character. "lines" or "polys".
#' @param source,ref_id,type,detail character. Work package (or GeoYukon), feature ID, type and what was done.
#' @param size numeric. Length (km) or area (km2) affected.
#' @return data.table.
#' @keywords internal
#' @noRd
clean_rows <- function(step, kind, source = NA_character_, ref_id = NA_character_, type = NA_character_,
                       detail = NA_character_, size = NA_real_) {
  if (any(lengths(list(source, ref_id, type, detail, size)) == 0)) {
    return(data.table::data.table(step = character(), kind = character(), source = character(), REF_ID = character(),
                                  type = character(), detail = character(), size = numeric()))
  }
  data.table::data.table(step = step, kind = kind, source = as.character(source), REF_ID = as.character(ref_id),
                         type = as.character(type), detail = as.character(detail), size = round(as.numeric(size), 4))
}

#' @title Size of features: km for lines, km2 for polygons
#' @param g sfc or sf.
#' @inheritParams yukon_cast
#' @return numeric.
#' @keywords internal
#' @noRd
yukon_size <- function(g, kind) {
  as.numeric(if (kind == 'lines') sf::st_length(g) / 1000 else sf::st_area(g) / 1e6)
}

#' @title Read the Yukon reclassification sheet (v4 and v5 types)
#' @description Reads the "Linear" and "Areal" sheets, each with a set of v4
#'   and v5 columns (e.g. "Industry type v4", "Land use type v5"), as in
#'   prep-disturb-cleaned.R. On the Areal sheet, the "Linear feature type"
#'   columns hold the disturbance reclassification. Rows in `patches` add types
#'   missing from the sheet or replace its rows.
#' @param xlsx character. The reclassification workbook.
#' @param patches character or NULL. CSV with columns kind (lines or polys),
#'   schema (v4 or v5), industry, disturbance, industry_rc, disturbance_rc,
#'   lf_type and note.
#' @return data.table: kind, schema, industry, disturbance, industry_rc,
#'   disturbance_rc, lf_type, and the matching keys k_ind and k_dist.
#' @author Julie W. Turner
read_yukon_reclass <- function(xlsx, patches = NULL) {
  sheets <- c(lines = 'Linear', polys = 'Areal')
  lut <- data.table::rbindlist(purrr::imap(sheets, function(sheet, kind) {
    d <- readxl::read_excel(xlsx, sheet = sheet, .name_repair = 'minimal')
    nm <- names(d)
    data.table::rbindlist(purrr::map(c('v4', 'v5'), function(v) {
      col <- function(pat) {
        i <- grep(paste0(pat, ' ', v, '$'), nm, ignore.case = TRUE)
        if (length(i) != 1) {
          stop(basename(xlsx), ' (', sheet, '): expected one column matching "', pat, ' ', v, '"', call. = FALSE)
        }
        as.character(d[[i]])
      }
      out <- data.table::data.table(
        kind = kind, schema = v,
        industry = col('^(Industry|Land use) type'), disturbance = col('^Disturbance type'),
        industry_rc = col('^(Industry|Land use) reclassification'),
        disturbance_rc = col(if (kind == 'lines') '^Disturbance reclassification' else '^Linear feature type'),
        lf_type = if (kind == 'lines') col('^Linear feature type') else NA_character_)
      out[!is.na(industry) & !is.na(disturbance)]
    }))
  }))
  lut[, `:=`(k_ind = yukon_norm(industry), k_dist = yukon_norm(disturbance))]
  if (!is.null(patches)) {
    p <- data.table::fread(patches, na.strings = '', colClasses = 'character')
    p[, `:=`(k_ind = yukon_norm(industry), k_dist = yukon_norm(disturbance))]
    lut <- rbind(lut[!p, on = c('kind', 'schema', 'k_ind', 'k_dist')], p[, names(lut), with = FALSE])
  }
  dup <- lut[duplicated(lut, by = c('kind', 'schema', 'k_ind', 'k_dist'))]
  if (nrow(dup)) {
    stop('Reclassification has more than one row for: ',
         paste(unique(paste(dup$kind, dup$schema, dup$industry, dup$disturbance)), collapse = '; '), call. = FALSE)
  }
  lut[]
}

#' @title Standardise Yukon disturbance features and add the reclassification
#' @description Renames the type, visibility and image date fields of the
#'   GeoYukon download and the three WP schemas (v5 `TYPE_LANDUSE`; v4
#'   `TYPE_INDUSTRY`; v4 with shapefile names `TYPE_INDUS`, `TYPE_DISTU`),
#'   joins the reclassification on the normalised types (missing types are
#'   "Unknown"), and adds the image year.
#' @param x sf. Features as read.
#' @param kind character. "lines" or "polys".
#' @param source character. "GeoYukon" or the WP name.
#' @param lut data.table. From [read_yukon_reclass()].
#' @param crs character. The project's coordinate system.
#' @param max_year integer. Image years after this (or before 1950) are taken as missing.
#' @return sf with columns REF_ID, DATABASE, FEATURE_VISIBILITY, TYPE_INDUSTRY,
#'   TYPE_DISTURBANCE, industry_rc, disturbance_rc, lf_type, image_year,
#'   schema and source.
#' @keywords internal
#' @noRd
standardise_yukon <- function(x, kind, source, lut, crs, max_year) {
  ren <- c(TYPE_LANDUSE = 'TYPE_INDUSTRY', TYPE_INDUS = 'TYPE_INDUSTRY', INDUSTRY = 'TYPE_INDUSTRY',
           TYPE_DISTU = 'TYPE_DISTURBANCE', DISTURB = 'TYPE_DISTURBANCE',
           FEATURE_VI = 'FEATURE_VISIBILITY', IMG_DATE = 'IMAGE_DATE')
  d <- data.table::as.data.table(sf::st_drop_geometry(x))
  schema <- if ('TYPE_LANDUSE' %in% names(d)) 'v5' else 'v4'
  have <- intersect(names(ren), names(d))
  data.table::setnames(d, have, unname(ren[have]))
  for (cl in c('REF_ID', 'DATABASE', 'FEATURE_VISIBILITY', 'TYPE_INDUSTRY', 'TYPE_DISTURBANCE')) {
    d[, (cl) := if (cl %in% names(d)) as.character(get(cl)) else NA_character_]
  }
  d[, `:=`(TYPE_INDUSTRY = data.table::fcoalesce(TYPE_INDUSTRY, 'Unknown'),
           TYPE_DISTURBANCE = data.table::fcoalesce(TYPE_DISTURBANCE, 'Unknown'))]
  d[, `:=`(kind = kind, schema = schema, k_ind = yukon_norm(TYPE_INDUSTRY), k_dist = yukon_norm(TYPE_DISTURBANCE))]
  d[, `:=`(industry_rc = NA_character_, disturbance_rc = NA_character_, lf_type = NA_character_)]
  d[lut, on = c('kind', 'schema', 'k_ind', 'k_dist'),
    `:=`(industry_rc = i.industry_rc, disturbance_rc = i.disturbance_rc, lf_type = i.lf_type)]
  img <- if ('IMAGE_DATE' %in% names(d)) d$IMAGE_DATE else rep(NA, nrow(d))
  y <- suppressWarnings(as.integer(substr(as.character(img), 1, 4)))
  bad <- !is.na(y) & (y < 1950 | y > max_year)
  bad_dates <- clean_rows('image date not possible: read as missing', kind, source, d$REF_ID[bad],
                          d$TYPE_DISTURBANCE[bad], paste('image date', as.character(img[bad])))
  y[bad] <- NA_integer_
  d[, `:=`(image_year = y, source = source)]
  keep <- c('REF_ID', 'DATABASE', 'FEATURE_VISIBILITY', 'TYPE_INDUSTRY', 'TYPE_DISTURBANCE',
            'industry_rc', 'disturbance_rc', 'lf_type', 'image_year', 'schema', 'source')
  g <- sf::st_transform(sf::st_zm(sf::st_geometry(x)), crs)
  if (kind == 'polys') g <- sf::st_make_valid(g)
  out <- sf::st_sf(d[, keep, with = FALSE], geometry = g)
  out <- out[!sf::st_is_empty(out), ]
  attr(out, 'cleaning') <- bad_dates
  out
}

#' @title Keep only lines or only polygons, as multi-part geometries
#' @param g sfc.
#' @param kind character. "lines" or "polys".
#' @return sfc of MULTILINESTRING or MULTIPOLYGON (empty where nothing is left).
#' @keywords internal
#' @noRd
yukon_cast <- function(g, kind) {
  type <- if (kind == 'lines') 'LINESTRING' else 'POLYGON'
  empty <- if (kind == 'lines') sf::st_multilinestring() else sf::st_multipolygon()
  out <- purrr::map(seq_along(g), function(i) {
    gi <- g[i]
    if (sf::st_is_empty(gi)) return(empty)
    if (grepl('COLLECTION', sf::st_geometry_type(gi))) gi <- sf::st_collection_extract(gi, type, warn = FALSE)
    gi <- gi[!sf::st_is_empty(gi)]
    if (!length(gi)) return(empty)
    u <- sf::st_union(gi)
    if (grepl('COLLECTION', sf::st_geometry_type(u))) u <- sf::st_union(sf::st_collection_extract(u, type, warn = FALSE))
    sf::st_cast(u, if (kind == 'lines') 'MULTILINESTRING' else 'MULTIPOLYGON')[[1]]
  })
  sf::st_sfc(out, crs = sf::st_crs(g))
}

#' @title Overlay one feature with one area, always giving one geometry
#' @description sf drops empty results; this keeps one (possibly empty) result.
#' @param op function. sf::st_difference or sf::st_intersection.
#' @param a,b sfc of length 1.
#' @inheritParams yukon_cast
#' @return sfc of length 1, of lines or polygons.
#' @keywords internal
#' @noRd
yukon_op <- function(op, a, b, kind) {
  r <- op(a, b)
  if (!length(r)) r <- sf::st_sfc(sf::st_geometrycollection(), crs = sf::st_crs(a))
  yukon_cast(r[1], kind)
}

#' @title Read the work packages, dropping features flagged by Yukon's QA
#' @description Reads every lines, polys and aoi layer of the work-package
#'   geodatabase (the QA layer excepted). Features of `qa$applies_to` that the
#'   QA layer marks with one of `qa$drop` (matched on REF_ID and length) are left out.
#' @param gdb character. The geodatabase.
#' @param qa named list or NULL. `layer` (QA copy of a layer, e.g.
#'   test_metadata), `applies_to` (the layer it copies), `field` (e.g.
#'   yg_qaqc) and `drop` (values to drop, e.g. NEEDS_TO_BE_DELETED).
#' @inheritParams standardise_yukon
#' @return list: `lines` and `polys` (sf, from [standardise_yukon()]), `aoi`
#'   (sf: wp, kind, geometry; one row per WP and kind it covers) and `qa_dropped` (integer).
#' @keywords internal
#' @noRd
read_yukon_wp <- function(gdb, qa, lut, crs, max_year) {
  layers <- setdiff(sf::st_layers(gdb)$name, qa$layer)
  tab <- data.table::data.table(layer = layers, type = sub('.*_', '', layers),
                                wp = sub('(_sd)?_(lines|polys|aoi)$', '', layers))
  tab <- tab[type %in% c('lines', 'polys', 'aoi')]
  dropped <- 0L
  qa_log <- list()
  date_log <- list()
  read_kind <- function(kind) {
    parts <- purrr::map(tab[type == kind]$layer, function(l) {
      x <- sf::st_read(gdb, l, type = if (kind == 'polys') 6 else 5, quiet = TRUE)
      if (!is.null(qa$layer) && identical(l, qa$applies_to)) {
        q <- data.table::as.data.table(sf::st_drop_geometry(sf::st_read(gdb, qa$layer, quiet = TRUE)))
        flag <- q[get(qa$field) %in% unlist(qa$drop)]
        key_x <- paste(x$REF_ID, round(x$Shape_Length, 3))
        hit <- key_x %in% paste(flag$REF_ID, round(flag$Shape_Length, 3))
        dropped <<- dropped + sum(hit)
        if (any(hit)) {
          tcol <- intersect(c('TYPE_DISTURBANCE', 'TYPE_DISTU'), names(x))[1]
          qa_log[[length(qa_log) + 1]] <<- clean_rows(
            paste0('dropped: marked ', paste(unlist(qa$drop), collapse = '/'), ' in ', qa$layer), kind,
            tab[layer == l]$wp, x$REF_ID[hit], x[[tcol]][hit], paste(qa$field, 'in', qa$layer), x$Shape_Length[hit] / 1000)
        }
        x <- x[!hit, ]
      }
      s <- standardise_yukon(x, kind, tab[layer == l]$wp, lut, crs, max_year)
      date_log[[length(date_log) + 1]] <<- attr(s, 'cleaning')
      s
    })
    purrr::reduce(parts, rbind)
  }
  lines <- read_kind('lines')
  polys <- read_kind('polys')
  aoi <- purrr::map(seq_len(nrow(tab[type == 'aoi'])), function(i) {
    r <- tab[type == 'aoi'][i]
    a <- sf::st_union(sf::st_make_valid(sf::st_transform(sf::st_zm(sf::st_read(gdb, r$layer, type = 6, quiet = TRUE)), crs)))
    kinds <- intersect(c('lines', 'polys'), tab[wp == r$wp]$type)
    sf::st_sf(wp = rep(r$wp, length(kinds)), kind = kinds, geometry = rep(a, length(kinds)))
  })
  list(lines = lines, polys = polys, aoi = purrr::reduce(aoi, rbind), qa_dropped = dropped,
       cleaning = data.table::rbindlist(c(qa_log, date_log)))
}

#' @title Other features near each feature: a quick search
#' @description Features of `others` overlapping each feature's bounding box
#'   widened by `buffer_m`. This may include a few farther ones; their
#'   `buffer_m` corridors do not reach the feature, so results are the same.
#' @param g sfc. Features.
#' @param others sfc or sf.
#' @param buffer_m numeric.
#' @return list of integer indices into `others`, one per feature.
#' @keywords internal
#' @noRd
yukon_nearby <- function(g, others, buffer_m) {
  boxes <- sf::st_sfc(purrr::map(g, function(gi) {
    sf::st_as_sfc(sf::st_bbox(gi) + c(-buffer_m, -buffer_m, buffer_m, buffer_m))[[1]]
  }), crs = sf::st_crs(g))
  sf::st_intersects(boxes, others)
}

#' @title The part of a feature farther than buffer_m from other features
#' @description Cuts out a `buffer_m` corridor around `others`, then drops
#'   pieces smaller than one grid cell: lines shorter than `min_m`, and polygon
#'   pieces nowhere `min_m` wide (nothing left after shrinking by `min_m / 2`).
#' @param gi sfc of length 1.
#' @param others sfc. Nearby features (from [yukon_nearby()]).
#' @inheritParams yukon_cast
#' @param buffer_m,min_m numeric.
#' @return sfc of length 1 (empty if nothing is left).
#' @keywords internal
#' @noRd
yukon_away_from <- function(gi, others, kind, buffer_m, min_m) {
  out <- yukon_cast(gi, kind)
  if (length(others)) out <- yukon_op(sf::st_difference, out, sf::st_union(sf::st_buffer(others, buffer_m)), kind)
  if (sf::st_is_empty(out)) return(out)
  if (kind == 'lines') {
    parts <- sf::st_cast(out, 'LINESTRING')
    parts <- parts[as.numeric(sf::st_length(parts)) >= min_m]
    if (!length(parts)) return(yukon_cast(sf::st_sfc(sf::st_geometrycollection(), crs = sf::st_crs(gi)), kind))
    return(yukon_cast(sf::st_union(parts), kind))
  }
  # keep whole the polygon pieces that are at least min_m wide somewhere
  parts <- sf::st_cast(out, 'POLYGON')
  parts <- parts[!sf::st_is_empty(sf::st_buffer(parts, -min_m / 2))]
  if (!length(parts)) return(yukon_cast(sf::st_sfc(sf::st_geometrycollection(), crs = sf::st_crs(gi)), kind))
  yukon_cast(sf::st_union(parts), kind)
}

#' @title Keep GeoYukon features where the work packages did not map them
#' @description Inside the WPs' areas of interest, the parts of GeoYukon
#'   features within `buffer_m` of a WP feature of the same kind were mapped
#'   again by the WP and are left out; the rest is kept (`part` = "not in
#'   WP" if cut). Some WP areas are much larger than what was mapped in them,
#'   so dropping all GeoYukon features inside them (as prep-disturb-cleaned.R
#'   did) loses roads, cutlines and mines the WP did not map.
#' @param x sf. GeoYukon features of one kind.
#' @param wp sf. WP features of the same kind.
#' @param aoi sfc. Union of the AOIs of WPs with that kind.
#' @inheritParams yukon_away_from
#' @return list: `x` with `part`, and `n` (features cut or left out).
#' @keywords internal
#' @noRd
yukon_replace_mapped <- function(x, wp, aoi, kind, buffer_m, min_m) {
  x$part <- 'whole'
  x0 <- x
  inside <- which(lengths(sf::st_intersects(x, aoi)) > 0)
  if (!length(inside)) return(list(x = x, n = 0L, log = NULL))
  g <- sf::st_geometry(x)
  wg <- sf::st_geometry(wp)
  near <- yukon_nearby(g[inside], wg, buffer_m)
  has <- which(lengths(near) > 0)
  t0 <- Sys.time()
  kept <- purrr::map(seq_along(has), function(k) {
    if (k %% 5000 == 0) message(sprintf('  %s, GeoYukon kept where not re-mapped: %d of %d, %.1f min', kind, k,
                                        length(has), as.numeric(difftime(Sys.time(), t0, units = 'mins'))))
    yukon_away_from(g[inside[has[k]]], wg[near[[has[k]]]], kind, buffer_m, min_m)[[1]]
  })
  idx <- inside[has]
  changed <- purrr::map_lgl(seq_along(idx), function(k) !isTRUE(all.equal(kept[[k]], g[[idx[k]]])))
  g[idx] <- sf::st_sfc(kept, crs = sf::st_crs(x))
  sf::st_geometry(x) <- g
  x$part[idx[changed]] <- 'not in WP'
  gone <- idx[changed][sf::st_is_empty(g[idx[changed]])]
  cut <- setdiff(idx[changed], gone)
  untouched <- setdiff(inside, idx[changed])
  log <- rbind(
    clean_rows('GeoYukon feature left out: mapped again by a work package (within buffer)', kind, 'GeoYukon',
               x0$REF_ID[gone], x0$TYPE_DISTURBANCE[gone], 'replaced by the work-package feature(s) within 20 m',
               yukon_size(sf::st_geometry(x0)[gone], kind)),
    clean_rows('GeoYukon feature cut: part mapped again by a work package', kind, 'GeoYukon',
               x0$REF_ID[cut], x0$TYPE_DISTURBANCE[cut], 'part not mapped again kept; size = part kept',
               yukon_size(g[cut], kind)),
    clean_rows('GeoYukon feature kept: inside a work-package AOI but not mapped again', kind, 'GeoYukon',
               x0$REF_ID[untouched], x0$TYPE_DISTURBANCE[untouched], 'no work-package feature within 20 m',
               yukon_size(g[untouched], kind)))
  list(x = x[!sf::st_is_empty(x), ], n = sum(changed), log = log)
}

#' @title One copy of each work-package feature mapped by overlapping WPs
#' @description Some WP areas overlap, so the same feature (REF_ID) can be in
#'   two or three WPs. Within one WP, a REF_ID can also have several rows: the
#'   pieces of a feature that was split (all are kept). For a REF_ID in several
#'   WPs, all rows of the WP with the latest image year are kept (ties: the
#'   first WP in name order); a row of another WP is left out if it lies within
#'   `buffer_m` of a kept row (the same feature), and kept otherwise (the REF_ID
#'   was reused for a different feature). Kept rows get `image_year_first` = the
#'   earliest image year of all rows with their REF_ID, which the new-feature
#'   rule uses. Features with no REF_ID are all kept.
#' @param x sf. WP features of one kind.
#' @inheritParams yukon_cast
#' @param buffer_m numeric. Distance within which two rows are the same feature.
#' @return list: `x` with `image_year_first`, `n` (copies left out) and `log`.
#' @keywords internal
#' @noRd
yukon_dedupe <- function(x, kind, buffer_m) {
  d <- data.table::data.table(i = seq_len(nrow(x)), id = x$REF_ID, img = x$image_year, src = x$source)
  d[, first := if (all(is.na(img))) NA_integer_ else min(img, na.rm = TRUE), by = id]
  d[is.na(id), first := img]
  multi <- d[!is.na(id), .(n_src = data.table::uniqueN(src)), by = id][n_src > 1]$id
  # the WP kept for each REF_ID found in several WPs
  pick <- d[id %in% multi][order(id, -img, src, na.last = TRUE)][, .(kept_src = src[1]), by = id]
  d[pick, on = 'id', kept_src := i.kept_src]
  other <- d[!is.na(kept_src) & src != kept_src]
  g <- sf::st_geometry(x)
  same <- purrr::map_lgl(seq_len(nrow(other)), function(k) {
    kept <- d[id == other$id[k] & src == other$kept_src[k]]$i
    any(as.numeric(sf::st_distance(g[other$i[k]], g[kept])) <= buffer_m)
  })
  gone <- other[same]
  reused <- other[!same]
  log <- rbind(
    clean_rows('work-package copy left out: same REF_ID in overlapping work packages', kind, gone$src, gone$id,
               x$TYPE_DISTURBANCE[gone$i], sprintf('image %s; copy kept from %s', gone$img, gone$kept_src),
               yukon_size(g[gone$i], kind)),
    clean_rows('REF_ID reused: same ID in another work package for a different feature (kept)', kind, reused$src,
               reused$id, x$TYPE_DISTURBANCE[reused$i],
               sprintf('more than %s m from the feature with this ID in %s', buffer_m, reused$kept_src),
               yukon_size(g[reused$i], kind)))
  keep <- setdiff(d$i, gone$i)
  x$image_year_first <- d$first
  x <- x[keep, ]
  list(x = x, n = nrow(gone), log = log)
}

#' @title Copies among work-package features with no REF_ID
#' @description Features with no REF_ID cannot be matched by ID, so a feature
#'   with no REF_ID that lies entirely within `buffer_m` of features of another
#'   (overlapping) WP is a copy and is left out (pieces smaller than `min_m`
#'   outside them are ignored, see [yukon_away_from()]). Older copies are
#'   checked first, and features already left out do not count, so of two
#'   copies of each other the one with the later image year is kept. The kept
#'   features it lies along get its image year as `image_year_first` if earlier.
#' @param x sf. WP features of one kind, from [yukon_dedupe()].
#' @inheritParams yukon_away_from
#' @return list: `x`, `n` (copies left out) and `log`.
#' @keywords internal
#' @noRd
yukon_dedupe_noid <- function(x, kind, buffer_m, min_m) {
  noid <- which(is.na(x$REF_ID))
  if (!length(noid)) return(list(x = x, n = 0L, log = NULL))
  noid <- noid[order(x$image_year[noid], na.last = FALSE)]
  g <- sf::st_geometry(x)
  near <- yukon_nearby(g[noid], g, buffer_m)
  gone <- rep(FALSE, nrow(x))
  first <- x$image_year_first
  along <- character(nrow(x))
  for (k in seq_along(noid)) {
    i <- noid[k]
    other <- near[[k]][x$source[near[[k]]] != x$source[i] & !gone[near[[k]]]]
    if (!length(other)) next
    if (!sf::st_is_empty(yukon_away_from(g[i], g[other], kind, buffer_m, min_m))) next
    gone[i] <- TRUE
    along[i] <- paste(sort(unique(x$source[other])), collapse = ' + ')
    if (!is.na(first[i])) first[other] <- pmin(first[other], first[i], na.rm = TRUE)
  }
  x$image_year_first <- first
  log <- clean_rows('work-package copy left out: no REF_ID, along features of an overlapping work package', kind,
                    x$source[gone], NA_character_, x$TYPE_DISTURBANCE[gone],
                    sprintf('image %s; within %s m of features of %s', x$image_year[gone], buffer_m, along[gone]),
                    yukon_size(g[gone], kind))
  list(x = x[!gone, ], n = sum(gone), log = log)
}

#' @title Reported years: quartz mine production and fixes by feature ID
#' @description A mine's production years go to the mine polygon (feature
#'   type in `mine_types`) containing its point, or else the nearest one
#'   within `mine_max_m` (an end year of 0 means still producing: no end
#'   year). Features listed in the fixes table (by kind and REF_ID) get its
#'   years. Fixes are applied last, so they win.
#' @param x sf. Features of one kind, with `feature_type` and year columns.
#' @param kind character. "lines" or "polys".
#' @param mines sf or NULL. Mine points with `start`, `end` and `name` columns.
#' @param fixes data.table or NULL. From config/yukon_year_fixes.csv.
#' @param mine_source character. Year source text for mine years.
#' @param mine_types character. Feature types that can be a mine.
#' @param mine_max_m numeric. Farthest a mine polygon can be from the mine point.
#' @return list: `x` with year_start, year_end, year_rule and year_source
#'   filled where reported, and `log` (data.table: what was matched).
#' @keywords internal
#' @noRd
yukon_reported_years <- function(x, kind, mines, fixes, mine_source, mine_types = c('quartz', 'othermines'),
                                 mine_max_m = 200) {
  log <- list()
  if (kind == 'polys' && !is.null(mines) && nrow(mines)) {
    cand <- which(x$feature_type %in% mine_types)
    inside <- sf::st_intersects(mines, x[cand, ])
    for (i in seq_len(nrow(mines))) {
      j <- cand[inside[[i]]]
      how <- 'inside'
      if (!length(j)) {
        d <- as.numeric(sf::st_distance(mines[i, ], x[cand, ]))
        if (length(d) && min(d) <= mine_max_m) { j <- cand[which.min(d)]; how <- sprintf('nearest, %.0f m', min(d)) }
      }
      log[[length(log) + 1]] <- data.table::data.table(what = 'mine', name = paste0(mines$name[i], if (length(j)) paste0(' (', how, ')')),
                                                       features = length(j), ref_id = paste(x$REF_ID[j], collapse = ' '))
      if (!length(j)) next
      x$year_start[j] <- as.integer(mines$start[i])
      x$year_end[j] <- if (isTRUE(mines$end[i] > 0)) as.integer(mines$end[i]) else NA_integer_
      x$year_rule[j] <- 'reported'
      x$year_source[j] <- sprintf('%s: %s production %s-%s', mine_source, mines$name[i], mines$start[i],
                                  if (isTRUE(mines$end[i] > 0)) mines$end[i] else 'ongoing')
    }
  }
  if (!is.null(fixes) && nrow(fixes)) {
    for (k in which(fixes$kind == kind)) {
      j <- which(x$REF_ID == fixes$ref_id[k])
      log[[length(log) + 1]] <- data.table::data.table(what = 'fix', name = fixes$note[k], features = length(j),
                                                       ref_id = fixes$ref_id[k])
      if (!length(j)) next
      x$year_start[j] <- as.integer(fixes$year_start[k])
      x$year_end[j] <- as.integer(fixes$year_end[k])
      x$year_rule[j] <- 'reported'
      x$year_source[j] <- paste0(fixes$year_source[k], ': ', fixes$note[k])
    }
  }
  list(x = x, log = data.table::rbindlist(log))
}

#' @title Date the parts of work-package features that are new to the map
#' @description A WP feature with no reported year whose REF_ID was not in
#'   GeoYukon, lying in an area GeoYukon had mapped (`extent`) from imagery
#'   older than the feature's earliest image year (`image_year_first`), may
#'   have been built since. Its parts inside such areas and more than
#'   `buffer_m` from every earlier GeoYukon feature of the same kind (see
#'   [yukon_away_from()]) get `year_start` = that image year: the year it had
#'   been built by (seen on the image), not the year it was built, which may be
#'   any time after the earlier imagery.
#'   The rest of the feature keeps no year. The data cannot tell a feature
#'   built since from one the coarser earlier mapping missed.
#' @param x sf. WP features of one kind, with year columns and `image_year_first`.
#' @param old sf. Earlier GeoYukon features of the same kind (before the replacement).
#' @param extent sf. GeoYukon mapping extent with `project`, `img_years` and
#'   `last_img_year`.
#' @inheritParams yukon_away_from
#' @return list: `x` with new parts as separate rows (`part` = "new", the rest
#'   "rest"), and `n` (features split).
#' @author Julie W. Turner
yukon_new_parts <- function(x, old, extent, kind, buffer_m, min_m) {
  if (is.null(x$part)) x$part <- 'whole'
  yr <- x$image_year_first
  cand <- which(!(x$REF_ID %in% old$REF_ID) & is.na(x$year_rule) & !is.na(yr))
  if (!length(cand)) return(list(x = x, n = 0L))
  g <- sf::st_geometry(x)
  in_ext <- sf::st_intersects(g[cand], extent)
  og <- sf::st_geometry(old)
  near_old <- yukon_nearby(g[cand], og, buffer_m)
  eg <- sf::st_geometry(extent)
  areas <- new.env()   # union of each set of mapped areas, made once
  area_of <- function(e) {
    key <- paste(e, collapse = '_')
    if (is.null(areas[[key]])) areas[[key]] <- sf::st_union(eg[e])
    areas[[key]]
  }
  t0 <- Sys.time()
  pieces <- purrr::map(seq_along(cand), function(k) {
    if (k %% 5000 == 0) message(sprintf('  %s, new parts: %d of %d candidates, %.1f min', kind, k, length(cand),
                                        as.numeric(difftime(Sys.time(), t0, units = 'mins'))))
    i <- cand[k]
    e <- in_ext[[k]][extent$last_img_year[in_ext[[k]]] < yr[i]]
    if (!length(e)) return(NULL)
    gi <- g[i]
    a <- area_of(e)
    inside <- if (lengths(sf::st_within(gi, a)) > 0) yukon_cast(gi, kind) else yukon_op(sf::st_intersection, gi, a, kind)
    if (sf::st_is_empty(inside)) return(NULL)
    new <- yukon_away_from(inside, og[near_old[[k]]], kind, buffer_m, min_m)
    if (sf::st_is_empty(new)) return(NULL)
    # lines: take away a 1 cm corridor around the new part, not the line itself,
    # whose cut ends are not exactly on gi (the rest would keep the new part too)
    cut <- if (kind == 'lines') sf::st_buffer(new, 0.01) else new
    list(i = i, new = new, rest = yukon_op(sf::st_difference, gi, cut, kind),
         source = sprintf('Seen by %d on imagery (not the year built); not on the earlier GeoYukon map (%s)', yr[i],
                          paste(sprintf('%s, imagery %s', extent$project[e], extent$img_years[e]), collapse = '; ')))
  })
  pieces <- purrr::compact(pieces)
  if (!length(pieces)) return(list(x = x, n = 0L))
  idx <- purrr::map_int(pieces, 'i')
  new_rows <- x[idx, ]
  sf::st_geometry(new_rows) <- sf::st_sfc(purrr::map(pieces, function(p) p$new[[1]]), crs = sf::st_crs(x))
  new_rows$part <- 'new'
  new_rows$year_start <- yr[idx]
  new_rows$year_rule <- 'image_new'
  new_rows$year_source <- purrr::map_chr(pieces, 'source')
  g[idx] <- sf::st_sfc(purrr::map(pieces, function(p) p$rest[[1]]), crs = sf::st_crs(x))
  sf::st_geometry(x) <- g
  x$part[idx] <- 'rest'
  log <- clean_rows('year seen by (image date, not year built): part not on the earlier GeoYukon map', kind, new_rows$source,
                    new_rows$REF_ID, new_rows$TYPE_DISTURBANCE, new_rows$year_source,
                    yukon_size(sf::st_geometry(new_rows), kind))
  x <- rbind(x, new_rows)
  list(x = x[!sf::st_is_empty(x), ], n = length(idx), log = log)
}

#' @title Disturbance type of merged Yukon features
#' @description The rules of prep-disturb-cleaned.R. Lines: paved (Highway,
#'   Railway, Paved Road), unpaved (linear feature type Unpaved Road), otherwise
#'   otherlf. Polygons: othermines (v4 Mining/Mining; v5 Mining/Unknown),
#'   placer, quartz, NA for harvest (left out; it comes from CanLaD), otherwise otherpoly.
#' @param x sf or data.frame. Merged features.
#' @param kind character. "lines" or "polys".
#' @return character.
#' @keywords internal
#' @noRd
yukon_feature_type <- function(x, kind) {
  drc <- yukon_norm(x$disturbance_rc)
  if (kind == 'lines') {
    return(data.table::fcase(drc %in% c('highway', 'railway', 'paved road'), 'paved',
                             yukon_norm(x$lf_type) %in% 'unpaved road', 'unpaved',
                             default = 'otherlf'))
  }
  ind <- yukon_norm(x$TYPE_INDUSTRY); dist <- yukon_norm(x$TYPE_DISTURBANCE)
  data.table::fcase(x$schema == 'v4' & dist == 'mining', 'othermines',
                    x$schema == 'v5' & ind == 'mining' & dist == 'unknown', 'othermines',
                    drc %in% 'placer mining', 'placer',
                    drc %in% 'quartz mining', 'quartz',
                    drc %in% 'harvest', NA_character_,
                    default = 'otherpoly')
}

#' @title Unpaved type of merged Yukon unpaved roads: winter, resource or all-season
#' @description Winter first (winter or ice roads; none in the data so far),
#'   then roads built for resource industries (industry or its reclassification
#'   Forestry, Mining, Oil and Gas, Placer, Quartz; or types such as Resource
#'   road and Major or Minor mining road), and other unpaved roads all-season.
#'   NA for features that are not unpaved roads.
#' @param x sf or data.frame. Merged lines with `feature_type`.
#' @return character.
#' @keywords internal
#' @noRd
yukon_unpaved_type <- function(x) {
  dist <- yukon_norm(x$TYPE_DISTURBANCE)
  ind <- yukon_norm(c(x$TYPE_INDUSTRY))
  ind_rc <- yukon_norm(x$industry_rc)
  resource <- c('forestry', 'mining', 'oil and gas', 'placer', 'quartz', 'quartz mining', 'placer mining')
  out <- data.table::fcase(grepl('winter|ice road', dist), 'winter',
                           ind %in% resource | ind_rc %in% resource | grepl('resource|mining road', dist), 'resource',
                           default = 'all-season')
  out[x$feature_type != 'unpaved'] <- NA_character_
  out
}

#' @title Merge GeoYukon and the work packages into Yukon disturbance files with years
#' @description Runs the steps listed at the top of this file and writes
#'   YT_linear.gpkg and YT_areal.gpkg (layers "linear" and "areal") with all
#'   columns, and YT_merge_log.csv: features by source, type and year rule,
#'   types not in the lookup, copies and QA-flagged features left out, and
#'   what the reported years matched.
#' @param inputs named list. Downloaded files: geoyukon_lines, geoyukon_polys,
#'   extent (shapefiles), work_packages (geodatabase), mines (shapefile),
#'   reclass (workbook).
#' @param ym named list. `landscape: disturbance: yukon_merge:` settings.
#' @param patches,fixes character. The CSV files (config/yukon_reclass_patches.csv,
#'   config/yukon_year_fixes.csv).
#' @param crs character. The project's coordinate system.
#' @param out_dir character. Folder to write to.
#' @return character. The two GeoPackages, the summary log and the cleaning
#'   log (YT_cleaning_log.csv: one row per feature or type and action, with
#'   the work package, REF_ID, type, what was done and the km or km2 affected,
#'   for reporting back to the Government of Yukon).
#' @author Julie W. Turner
merge_yukon_disturbance <- function(inputs, ym, patches, fixes, crs, out_dir) {
  reproducible::checkPath(out_dir, create = TRUE)
  max_year <- as.integer(ym$max_image_year %||% format(Sys.Date(), '%Y'))
  buffer_m <- ym$new_feature_buffer_m %||% 20
  min_m <- ym$min_new_size_m %||% 30
  lut <- read_yukon_reclass(inputs$reclass, patches)
  pick <- function(f, ext) f[tolower(tools::file_ext(f)) == ext][1]
  gy <- list(
    lines = standardise_yukon(sf::st_read(pick(inputs$geoyukon_lines, 'shp'), type = 5, quiet = TRUE),
                              'lines', 'GeoYukon', lut, crs, max_year),
    polys = standardise_yukon(sf::st_read(pick(inputs$geoyukon_polys, 'shp'), type = 6, quiet = TRUE),
                              'polys', 'GeoYukon', lut, crs, max_year))
  wp <- read_yukon_wp(inputs$work_packages, ym$qa, lut, crs, max_year)
  ext <- sf::st_make_valid(sf::st_transform(sf::st_read(pick(inputs$extent, 'shp'), quiet = TRUE), crs))
  ext <- sf::st_sf(project = ext[[ym$extent_fields$project %||% 'PROJECT']],
                   img_years = as.character(ext[[ym$extent_fields$img_years %||% 'IMG_YEARS']]),
                   geometry = sf::st_geometry(ext))
  ext$last_img_year <- as.integer(sub('.*-', '', ext$img_years))
  mines <- NULL
  if (!is.null(inputs$mines)) {
    m <- sf::st_transform(sf::st_read(pick(inputs$mines, 'shp'), quiet = TRUE), crs)
    mf <- ym$mine_fields
    mines <- sf::st_sf(name = m[[mf$name]], start = m[[mf$start]], end = m[[mf$end]], geometry = sf::st_geometry(m))
  }
  fx <- if (!is.null(fixes)) data.table::fread(fixes, na.strings = '', colClasses = 'character') else NULL
  message('Yukon merge: inputs read')
  logs <- list()
  out <- list()
  cleaning <- list(wp$cleaning, attr(gy$lines, 'cleaning'), attr(gy$polys, 'cleaning'))
  for (kind in c('lines', 'polys')) {
    message('Yukon merge: ', kind)
    dd <- yukon_dedupe(wp[[kind]], kind, buffer_m)
    dn <- yukon_dedupe_noid(dd$x, kind, buffer_m, min_m)
    w <- dn$x
    aoi <- sf::st_union(sf::st_geometry(wp$aoi)[wp$aoi$kind == kind])
    rm <- yukon_replace_mapped(gy[[kind]], w, aoi, kind, buffer_m, min_m)
    g <- rm$x
    g$image_year_first <- g$image_year
    w$part <- 'whole'
    x <- rbind(g, w[, names(g)])
    x$feature_type <- yukon_feature_type(x, kind)
    unmatched <- data.table::as.data.table(sf::st_drop_geometry(x))[is.na(disturbance_rc) | is.na(industry_rc),
                                                                     .N, by = .(schema, TYPE_INDUSTRY, TYPE_DISTURBANCE)]
    xd <- data.table::as.data.table(sf::st_drop_geometry(x))
    xd[, `:=`(kind = kind, k_ind = yukon_norm(TYPE_INDUSTRY), k_dist = yukon_norm(TYPE_DISTURBANCE),
              size = yukon_size(x, kind))]
    pt <- data.table::fread(patches, na.strings = '', colClasses = 'character')[
      , `:=`(k_ind = yukon_norm(industry), k_dist = yukon_norm(disturbance))]
    patched <- xd[pt, on = c('kind', 'schema', 'k_ind', 'k_dist'), nomatch = NULL]
    harvest <- which(is.na(x$feature_type))
    cleaning[[length(cleaning) + 1]] <- rbind(
      dd$log, dn$log, rm$log,
      if (nrow(patched)) patched[, .(n = .N, size = sum(size)), by = .(source, schema, TYPE_INDUSTRY, TYPE_DISTURBANCE, note)][
        , clean_rows('type not in the reclassification sheet: patch used', kind, source, NA_character_,
                     paste(schema, TYPE_INDUSTRY, TYPE_DISTURBANCE, sep = ' | '), paste0(n, ' features; ', note), size)],
      if (nrow(unmatched)) clean_rows('type not in the reclassification sheet: no patch (sorted by the other rules)', kind,
                                      NA_character_, NA_character_,
                                      paste(unmatched$schema, unmatched$TYPE_INDUSTRY, unmatched$TYPE_DISTURBANCE, sep = ' | '),
                                      paste(unmatched$N, 'features')),
      clean_rows('left out: harvest (comes from CanLaD)', kind, x$source[harvest], x$REF_ID[harvest],
                 x$TYPE_DISTURBANCE[harvest], NA_character_, yukon_size(sf::st_geometry(x)[harvest], kind)))
    x <- x[!is.na(x$feature_type), ]
    x$year_start <- NA_integer_; x$year_end <- NA_integer_
    x$year_rule <- NA_character_; x$year_source <- NA_character_
    reported <- yukon_reported_years(x, kind, mines, fx, ym$mine_year_source %||% 'Reported',
                                     unlist(ym$mine_types %||% list('quartz', 'othermines')), ym$mine_max_m %||% 200)
    x <- reported$x
    is_wp <- x$source != 'GeoYukon'
    np <- yukon_new_parts(x[is_wp, ], gy[[kind]], ext, kind, buffer_m, min_m)
    x <- rbind(x[!is_wp, ], np$x[, names(x)])
    if (nrow(reported$log)) {
      cleaning[[length(cleaning) + 1]] <- clean_rows(paste('year reported:', reported$log$what), kind, NA_character_,
                                                     reported$log$ref_id, NA_character_,
                                                     paste0(reported$log$name, '; ', reported$log$features, ' feature(s) matched'))
    }
    cleaning[[length(cleaning) + 1]] <- np$log
    x$year_rule[is.na(x$year_rule)] <- 'none'
    x$unpaved_type <- if (kind == 'lines') yukon_unpaved_type(x) else NA_character_
    x$region <- 'YT'
    x$feature_id <- paste0(if (kind == 'lines') 'YTL' else 'YTA', seq_len(nrow(x)))
    cols <- c('feature_id', 'region', 'source', 'REF_ID', 'DATABASE', 'FEATURE_VISIBILITY', 'TYPE_INDUSTRY', 'TYPE_DISTURBANCE',
              'industry_rc', 'disturbance_rc', 'lf_type', 'schema', 'feature_type', 'unpaved_type', 'image_year', 'image_year_first',
              'year_start', 'year_end', 'year_rule', 'year_source', 'part')
    out[[kind]] <- x[, cols]
    d <- data.table::as.data.table(sf::st_drop_geometry(x))
    d[, size := as.numeric(if (kind == 'lines') sf::st_length(x) / 1000 else sf::st_area(x) / 1e6)]
    logs[[kind]] <- rbind(
      d[, .(features = .N, size = round(sum(size), 2)), keyby = .(source, feature_type, year_rule, part)][
        , `:=`(table = 'features', kind = kind)],
      unmatched[, .(table = 'not in lookup', kind = kind, source = paste(schema, TYPE_INDUSTRY, TYPE_DISTURBANCE, sep = ' | '),
                    features = N)],
      data.table::data.table(table = c('WP copies left out', 'WP copies with no REF_ID left out',
                                       'GeoYukon cut or left out where re-mapped', 'split new parts'),
                             kind = kind, features = c(dd$n, dn$n, rm$n, np$n)),
      if (nrow(reported$log)) reported$log[, .(table = paste('reported', what), kind = kind, source = name, features, ref_id)],
      fill = TRUE)
  }
  logs$qa <- data.table::data.table(table = 'dropped by QA', kind = 'lines', features = wp$qa_dropped)
  files <- file.path(out_dir, c('YT_linear.gpkg', 'YT_areal.gpkg', 'YT_merge_log.csv', 'YT_cleaning_log.csv'))
  unlink(files[1:2])
  sf::st_write(out$lines, files[1], layer = 'linear', quiet = TRUE)
  sf::st_write(out$polys, files[2], layer = 'areal', quiet = TRUE)
  data.table::fwrite(data.table::rbindlist(logs, fill = TRUE), files[3])
  data.table::fwrite(data.table::rbindlist(cleaning, fill = TRUE), files[4])
  files
}

#' @title Give Yukon roads that meet a neighbour's road at the border its year
#' @description A Yukon road (category in `categories`) that lines up with a
#'   neighbour's road (within the first `match_m`, both in `categories`) whose
#'   year built is known takes that year (the neighbour's is likely more
#'   precise): `year_rule` "neighbour", with the road, the gap and the Yukon
#'   year it replaces in `year_source`. If it meets several, the closest
#'   wins. Writes the final YT_linear.gpkg, YT_areal.gpkg, YT_merge_log.csv and
#'   YT_cleaning_log.csv (with a row per road changed) to `out_dir`.
#' @param merged character. Files from [merge_yukon_disturbance()].
#' @param check list or NULL. From [check_yukon_borders()]; NULL copies the files.
#' @param bc named list. `border_check:` settings: `years_from` (neighbour
#'   sources whose years are used, e.g. bc), `match_m`, and `year_categories`
#'   (road categories, e.g. paved and unpaved).
#' @param out_dir character. Folder to write to.
#' @return character. The four files.
#' @author Julie W. Turner
apply_border_years <- function(merged, check, bc, out_dir) {
  reproducible::checkPath(out_dir, create = TRUE)
  pick <- function(n) merged[basename(merged) == n]
  lines <- sf::st_read(pick('YT_linear.gpkg'), quiet = TRUE)
  clean <- data.table::fread(pick('YT_cleaning_log.csv'), colClasses = list(character = 'REF_ID'))
  if (!is.null(check) && nrow(check$pairs)) {
    cats <- unlist(bc$year_categories %||% list('paved', 'unpaved'))
    m <- min(unlist(bc$match_m %||% list(50, 500)))
    codes <- unlist(bc$sources)[unlist(bc$years_from %||% list('bc'))]
    p <- check$pairs[from == 'YK' & to %in% codes & gap_m <= m & yk_category %in% cats & nb_category %in% cats & !is.na(nb_year)]
    p <- p[order(gap_m)][!duplicated(yk_feature_id)]
    i <- match(p$yk_feature_id, lines$feature_id)
    was <- ifelse(is.na(lines$year_start[i]), 'none (counted in every year)',
                  paste0(lines$year_start[i], ' (', lines$year_rule[i], ')'))
    src <- sprintf('%s road built %d (%s), %.0f m away at the border; Yukon year was %s', p$to, p$nb_year,
                   p$nb_file, p$gap_m, was)
    lines$year_start[i] <- p$nb_year
    lines$year_rule[i] <- 'neighbour'
    lines$year_source[i] <- src
    clean <- rbind(clean, clean_rows(paste0('year from the neighbour road at the border (', paste(codes, collapse = ', '), ')'),
                                     'lines', lines$source[i], lines$REF_ID[i], lines$TYPE_DISTURBANCE[i], src,
                                     yukon_size(lines[i, ], 'lines')), fill = TRUE)
  }
  files <- file.path(out_dir, c('YT_linear.gpkg', 'YT_areal.gpkg', 'YT_merge_log.csv', 'YT_cleaning_log.csv'))
  unlink(files[1])
  sf::st_write(lines, files[1], layer = 'linear', quiet = TRUE)
  file.copy(pick('YT_areal.gpkg'), files[2], overwrite = TRUE)
  file.copy(pick('YT_merge_log.csv'), files[3], overwrite = TRUE)
  data.table::fwrite(clean, files[4])
  files
}

#' @title Upload the merged Yukon files to the restricted Drive folder
#' @description Zips YT_linear.gpkg and YT_areal.gpkg (from target
#'   `yukon_merged`, in outputs/<project>/derived/) as YT_linear.zip and
#'   YT_areal.zip, each with the cleaning log, and puts them in
#'   `yukon_merge: upload_folder:`, replacing files of the same name in place
#'   (so links in the settings stay the same). The folder must not be shared
#'   with anyone with the link. Uses the Google login of `run: google_email`.
#'   Run by hand, not by the pipeline.
#' @param project character. Project name, e.g. "NMC".
#' @param names character. Which files to upload.
#' @return data.table: `name`, `action` (updated or uploaded), `id`, `url`.
#' @author Julie W. Turner
upload_yukon_disturbance <- function(project = Sys.getenv('TAR_PROJECT'), names = c('YT_linear', 'YT_areal')) {
  cfg <- read_config(project)
  folder <- cfg$landscape$disturbance$yukon_merge$upload_folder
  if (is.null(folder)) stop('Set landscape: disturbance: yukon_merge: upload_folder: in config/', project, '.yaml', call. = FALSE)
  if (!is.null(cfg$run$google_email)) options(gargle_oauth_email = cfg$run$google_email)
  dir <- file.path(cfg$paths$outputs, project, 'derived')
  up <- reproducible::checkPath(file.path(dir, 'upload'), create = TRUE)
  zips <- file.path(up, paste0(names, '.zip'))
  for (i in seq_along(names)) {
    files <- file.path(dir, c(paste0(names[i], '.gpkg'), 'YT_cleaning_log.csv'))
    if (!all(file.exists(files))) stop('Not found (run the pipeline first): ', paste(files[!file.exists(files)], collapse = ', '),
                                       call. = FALSE)
    if (file.exists(zips[i])) file.remove(zips[i])
    utils::zip(zips[i], files, flags = '-jq')
  }
  upload_to_drive(zips, folder, 'The Yukon upload folder is shared with anyone with the link; the work packages are not public. Restrict it first.')
}
