# === Yukon disturbance at the B.C. and N.W.T. borders ----------------------------------
# A check of the merged Yukon lines where they meet a neighbour: does each
# Yukon road or trail that reaches the border line up with a line in the
# neighbour's data, and does the neighbour know the year it was built? The
# table helps refine Yukon years; it does not change them. Settings:
# landscape: disturbance: border_check: (config/NMC.yaml).

#' @title Where lines meet a border: crossings, and ends near it
#' @param x sf. Lines.
#' @param border sfc. The border line.
#' @param near_m numeric. Line ends within this distance of the border count.
#' @return sf of points with `row` (the row of `x`) and `kind` (crossing or end).
#' @keywords internal
#' @noRd
border_points <- function(x, border, near_m) {
  empty <- sf::st_sf(row = integer(), kind = character(), geometry = sf::st_sfc(crs = sf::st_crs(x)))
  if (!nrow(x)) return(empty)
  g <- sf::st_sf(row = seq_len(nrow(x)), geometry = sf::st_geometry(x))
  parts <- suppressWarnings(sf::st_cast(sf::st_cast(g, 'MULTILINESTRING'), 'LINESTRING'))
  xy <- data.table::as.data.table(sf::st_coordinates(parts))
  ends <- xy[, .SD[c(1, .N)], by = L1]
  ends <- sf::st_as_sf(ends, coords = c('X', 'Y'), crs = sf::st_crs(x))
  ends <- sf::st_sf(row = parts$row[ends$L1], kind = 'end', geometry = sf::st_geometry(ends))
  ends <- ends[lengths(sf::st_is_within_distance(ends, border, near_m)) > 0, ]
  cr <- suppressWarnings(sf::st_intersection(parts, sf::st_sf(geometry = border)))
  cr <- if (nrow(cr)) {
    cr <- suppressWarnings(sf::st_cast(sf::st_collection_extract(cr, 'POINT'), 'POINT'))
    sf::st_sf(row = cr$row, kind = rep('crossing', nrow(cr)), geometry = sf::st_geometry(cr))
  } else empty
  out <- rbind(if (nrow(ends)) ends else empty, cr)
  # one point per line and kind is enough (a line can touch the border several times)
  out[!duplicated(data.table::data.table(out$row, out$kind, round(sf::st_coordinates(out), -2))), ]
}

#' @title Compare merged Yukon lines with B.C. and N.W.T. lines at the borders
#' @description For each Yukon line that crosses the border with a neighbour,
#'   or ends within `near_m` of it, finds the nearest line in that neighbour's
#'   features, and the reverse (neighbour lines reaching the border, nearest
#'   Yukon line). Records the gap and both sides' categories and years.
#' @param yukon character. YT_linear.gpkg from [merge_yukon_disturbance()].
#' @param features named character. GeoPackages from [prep_disturbance_features()],
#'   named by source (e.g. bc, nwt).
#' @param borders character. Border lines with fields JURISDICT1 and JURISDICT2.
#' @param bc named list. `landscape: disturbance: border_check:` settings:
#'   `sources` (source name: jurisdiction code, e.g. bc: BC), `near_m`,
#'   `match_m` (gaps for "lines up" and "possible match"), and `yukon_types`
#'   (Yukon feature_type to neighbour category, e.g. otherlf: lfother).
#' @param crs character. The project's coordinate system.
#' @return list of data.tables: `pairs` (one row per border point: border,
#'   side, kind, gap, match class, Yukon feature_id, REF_ID, feature_type, year_start,
#'   year_rule, year_source, the neighbour's category and year, and longitude
#'   and latitude) and `summary` (by border, side and match class: points, and
#'   how many have a neighbour year, a Yukon year, or both).
#' @author Julie W. Turner
check_yukon_borders <- function(yukon, features, borders, bc, crs) {
  near_m <- bc$near_m %||% 500
  match_m <- sort(unlist(bc$match_m %||% list(50, 500)))
  src <- unlist(bc$sources)
  b <- sf::st_transform(sf::st_read(borders, quiet = TRUE), crs)
  b <- b[b$JURISDICT1 == 'YK' | b$JURISDICT2 == 'YK', ]
  b$other <- ifelse(b$JURISDICT1 == 'YK', b$JURISDICT2, b$JURISDICT1)
  out <- list()
  for (nm in names(src)) {
    border <- sf::st_union(sf::st_geometry(b[b$other == src[[nm]], ]))
    if (sf::st_is_empty(border)) next
    band <- sf::st_buffer(border, near_m * 4)
    yk <- sf::st_read(yukon, wkt_filter = sf::st_as_text(band), quiet = TRUE)
    nb <- sf::st_read(features[[nm]], query = "SELECT * FROM disturbance", wkt_filter = sf::st_as_text(band), quiet = TRUE)
    nb <- nb[grepl('LINE', sf::st_geometry_type(nb)), ]
    yk_cat <- unlist(bc$yukon_types)[yk$feature_type]
    yk_cat[is.na(yk_cat)] <- yk$feature_type[is.na(yk_cat)]
    side <- function(from, to, pts, to_x) {
      if (!nrow(pts)) return(NULL)
      nn <- if (nrow(to_x)) sf::st_nearest_feature(pts, to_x) else rep(NA_integer_, nrow(pts))
      gap <- if (nrow(to_x)) as.numeric(sf::st_distance(pts, to_x[nn, ], by_element = TRUE)) else rep(Inf, nrow(pts))
      ll <- sf::st_coordinates(sf::st_transform(pts, 4326))
      data.table::data.table(border = paste0('YK-', src[[nm]]), from = from, to = to, kind = pts$kind,
                             gap_m = round(gap, 1), point = pts$row, nearest = nn,
                             lon = round(ll[, 1], 5), lat = round(ll[, 2], 5))
    }
    p1 <- side('YK', src[[nm]], border_points(yk, border, near_m), nb)
    p2 <- side(src[[nm]], 'YK', border_points(nb, border, near_m), yk)
    # Yukon row and neighbour row of each pair
    if (!is.null(p1)) p1[, `:=`(yk_row = point, nb_row = nearest)]
    if (!is.null(p2)) p2[, `:=`(yk_row = nearest, nb_row = point)]
    p <- data.table::rbindlist(list(p1, p2))
    if (!nrow(p)) next
    yd <- data.table::as.data.table(sf::st_drop_geometry(yk))
    yk_id <- if ('feature_id' %in% names(yd)) yd$feature_id else rep(NA_character_, nrow(yd))
    p[, `:=`(yk_feature_id = yk_id[yk_row], yk_ref_id = yd$REF_ID[yk_row], yk_type = yd$feature_type[yk_row], yk_category = yk_cat[yk_row],
             yk_year = yd$year_start[yk_row], yk_year_rule = yd$year_rule[yk_row],
             yk_year_source = yd$year_source[yk_row],
             nb_category = nb$category[nb_row], nb_year = nb$year[nb_row], nb_file = nb$file[nb_row])]
    out[[nm]] <- p
  }
  pairs <- data.table::rbindlist(out)
  if (!nrow(pairs)) return(list(pairs = pairs, summary = data.table::data.table()))
  pairs[, match := data.table::fcase(gap_m <= match_m[1], paste0('lines up (<= ', match_m[1], ' m)'),
                                     gap_m <= match_m[2], paste0('possible (<= ', match_m[2], ' m)'),
                                     default = 'no match')]
  pairs[, c('point', 'nearest', 'yk_row', 'nb_row') := NULL]
  summary <- pairs[, .(points = .N, same_category = sum(yk_category == nb_category, na.rm = TRUE),
                       neighbour_year = sum(!is.na(nb_year)), yukon_year = sum(!is.na(yk_year)),
                       both = sum(!is.na(nb_year) & !is.na(yk_year))),
                   keyby = .(border, from, match)]
  list(pairs = pairs, summary = summary)
}

#' @title Write the border check tables
#' @param check list. From [check_yukon_borders()].
#' @param out_dir character. Folder for the CSV files.
#' @return character. The CSV files.
#' @author Julie W. Turner
write_border_check <- function(check, out_dir) {
  reproducible::checkPath(out_dir, create = TRUE)
  files <- file.path(out_dir, paste0('YT_border_', names(check), '.csv'))
  purrr::walk2(check, files, function(x, f) data.table::fwrite(x, f))
  files
}
