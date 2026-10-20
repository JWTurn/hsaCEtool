# Merged Yukon disturbance and the border check, with small made-up features (EPSG:3154)

crs <- 'EPSG:3154'
ln <- function(...) sf::st_linestring(rbind(...))
sq <- function(x0, y0, w, h = w) sf::st_polygon(list(rbind(c(x0, y0), c(x0 + w, y0), c(x0 + w, y0 + h), c(x0, y0 + h), c(x0, y0))))
yk_feats <- function(geoms, ref_id, source = 'wp1', image_year = 2022L, type = 'Trail') {
  n <- length(ref_id)
  sf::st_sf(REF_ID = ref_id, source = rep(source, length.out = n), image_year = rep(as.integer(image_year), length.out = n),
            TYPE_DISTURBANCE = rep(type, length.out = n), year_start = NA_integer_, year_end = NA_integer_,
            year_rule = NA_character_, year_source = NA_character_, geometry = sf::st_sfc(geoms, crs = crs))
}
mapped <- function(last_year = 2012L) {
  sf::st_sf(project = 'Made-up area', img_years = paste0(last_year - 2, '-', last_year), last_img_year = last_year,
            geometry = sf::st_sfc(sq(0, 0, 10000), crs = crs))
}

test_that('yukon_norm matches types written differently', {
  expect_equal(yukon_norm(c(' Developed – dense ', 'Survey /  Cutline', 'UNKNOWN')),
               c('developed - dense', 'survey / cutline', 'unknown'))
})

test_that('standardise_yukon reads the three schemas and joins the reclassification', {
  lut <- data.table(kind = 'lines', schema = c('v4', 'v5'), industry = c('Transportation', 'Forestry'),
                    disturbance = c('Unpaved Road', 'Resource road'), industry_rc = c('Transportation', 'Forestry'),
                    disturbance_rc = 'Unpaved Road', lf_type = 'Unpaved Road')
  lut[, `:=`(k_ind = yukon_norm(industry), k_dist = yukon_norm(disturbance))]
  g <- sf::st_sfc(ln(c(0, 0), c(100, 0)), crs = crs)
  v5 <- sf::st_sf(REF_ID = 'a', TYPE_LANDUSE = 'Forestry', TYPE_DISTURBANCE = 'resource  road',
                  IMAGE_DATE = as.POSIXct('2023-07-08', tz = 'UTC'), geometry = g)
  v4 <- sf::st_sf(REF_ID = 'b', TYPE_INDUSTRY = 'Transportation', TYPE_DISTURBANCE = 'Unpaved Road',
                  IMAGE_DATE = as.Date('2041-01-01'), geometry = g)
  shp <- sf::st_sf(REF_ID = 'c', TYPE_INDUS = 'Transportation', TYPE_DISTU = 'Unpaved Road', FEATURE_VI = 'VISIBLE',
                   IMAGE_DATE = as.Date('2019-08-01'), geometry = g)
  a <- standardise_yukon(v5, 'lines', 'wp1', lut, crs, 2026L)
  b <- standardise_yukon(v4, 'lines', 'wp1', lut, crs, 2026L)
  c3 <- standardise_yukon(shp, 'lines', 'wp2', lut, crs, 2026L)
  expect_equal(c(a$schema, b$schema, c3$schema), c('v5', 'v4', 'v4'))
  expect_equal(c(a$lf_type, b$lf_type, c3$lf_type), rep('Unpaved Road', 3))
  expect_equal(c(a$image_year, b$image_year, c3$image_year), c(2023L, NA, 2019L))   # 2041 is not possible
  expect_equal(nrow(attr(b, 'cleaning')), 1)
  expect_equal(c3$FEATURE_VISIBILITY, 'VISIBLE')
})

test_that('yukon_feature_type follows the merge rules', {
  l <- data.frame(disturbance_rc = c('Highway', 'Railway', 'Unpaved Road', 'Trail'),
                  lf_type = c('Highway', 'Other LF', 'Unpaved Road', 'Other LF'))
  expect_equal(yukon_feature_type(l, 'lines'), c('paved', 'paved', 'unpaved', 'otherlf'))
  p <- data.frame(schema = c('v4', 'v5', 'v4', 'v5', 'v4', 'v4'),
                  TYPE_INDUSTRY = c('Mining', 'Mining', 'Mining', 'Mining', 'Forestry', 'Urban'),
                  TYPE_DISTURBANCE = c('Mining', 'Unknown', 'Placer Mining - Minor', 'Quartz mine', 'Clearing', 'Building'),
                  disturbance_rc = c('Clearing', 'Clearing', 'Placer mining', 'Quartz Mining', 'Harvest', 'Building'))
  expect_equal(yukon_feature_type(p, 'polys'), c('othermines', 'othermines', 'placer', 'quartz', NA, 'otherpoly'))
})

test_that('new parts of work-package lines count from the image year, only in older mapped areas', {
  old <- yk_feats(list(ln(c(1000, 1000), c(3000, 1000))), 'L1', 'GeoYukon', 2011L)
  # w1: runs 5 m beside the old line for 2 km, then 1 km away from it; w2: same ID as the old line;
  # w3: outside the mapped area; w4: entirely along the old line
  x <- yk_feats(list(ln(c(1000, 1005), c(3000, 1005), c(3000, 2005)), ln(c(1000, 5000), c(3000, 5000)),
                     ln(c(20000, 0), c(21000, 0)), ln(c(1200, 1010), c(2800, 1010))),
                c('GL1', 'L1', 'GL3', 'GL4'))
  x$image_year_first <- x$image_year
  r <- yukon_new_parts(x, old, mapped(2012L), 'lines', 20, 30)
  expect_equal(r$n, 1L)
  new <- r$x[r$x$part == 'new', ]
  expect_equal(new$REF_ID, 'GL1')
  expect_equal(new$year_start, 2022L)
  expect_equal(new$year_rule, 'image_new')
  # the new part starts 20 m past the old line's end (within the corridor around it is not new)
  expect_equal(as.numeric(sf::st_length(new)), 1000 - 20 + 5, tolerance = 1)
  expect_true(all(is.na(r$x$year_start[r$x$part != 'new'])))
  # imagery of the mapped area as recent as the work package: nothing is new
  expect_equal(yukon_new_parts(x, old, mapped(2022L), 'lines', 20, 30)$n, 0L)
})

test_that('the new part and the rest of a split line do not overlap', {
  old <- yk_feats(list(ln(c(1000, 1000), c(3000, 1000))), 'L1', 'GeoYukon', 2011L)
  # a diagonal line leaving the old one: the cut point is not exactly on it
  x <- yk_feats(list(ln(c(1000, 1003.7), c(2000, 1011.3), c(3170.3, 2213.9))), 'GL1')
  x$image_year_first <- x$image_year
  r <- yukon_new_parts(x, old, mapped(2012L), 'lines', 20, 30)
  len <- as.numeric(sf::st_length(r$x))
  expect_equal(sort(r$x$part), c('new', 'rest'))
  expect_equal(sum(len), as.numeric(sf::st_length(x)), tolerance = 0.1)
})

test_that('new polygon parts narrower than min_m are ignored', {
  old <- yk_feats(list(sq(1000, 1000, 100)), 'P1', 'GeoYukon', 2011L)
  # the same clearing drawn 10 m wider (a sliver) and a large new extension
  x <- yk_feats(list(sq(1000, 1000, 110, 100), sq(5000, 5000, 200)), c('GP1', 'GP2'))
  x$image_year_first <- x$image_year
  r <- yukon_new_parts(x, old, mapped(2012L), 'polys', 20, 30)
  expect_equal(r$x$REF_ID[r$x$part == 'new'], 'GP2')
})

test_that('GeoYukon features are only replaced where a work package mapped them again', {
  aoi <- sf::st_sfc(sq(0, 0, 10000), crs = crs)
  gy <- yk_feats(list(ln(c(1000, 1000), c(3000, 1000)), ln(c(1000, 3000), c(3000, 3000)), ln(c(20000, 0), c(21000, 0))),
                 c('L1', 'L2', 'L3'), 'GeoYukon', 2011L)
  # the work package maps L1 again (8 m away) and half of L2
  wp <- yk_feats(list(ln(c(1000, 1008), c(3000, 1008)), ln(c(1000, 3005), c(2000, 3005))), c('L1', 'GL2'))
  r <- yukon_replace_mapped(gy, wp, aoi, 'lines', 20, 30)
  expect_false('L1' %in% r$x$REF_ID)
  expect_equal(as.numeric(sf::st_length(r$x[r$x$REF_ID == 'L2', ])), 1000 - 20, tolerance = 1)
  expect_equal(r$x$part[r$x$REF_ID == 'L2'], 'not in WP')
  expect_equal(r$x$part[r$x$REF_ID == 'L3'], 'whole')   # outside the AOI
  expect_setequal(unique(r$log$step), c('GeoYukon feature left out: mapped again by a work package (within buffer)',
                                        'GeoYukon feature cut: part mapped again by a work package'))
})

test_that('copies in overlapping work packages are dropped, split pieces and reused IDs kept', {
  x <- yk_feats(list(ln(c(0, 0), c(100, 0)), ln(c(0, 2), c(100, 2)),          # L1 in wp1 (2019) and wp2 (2022)
                     ln(c(0, 500), c(100, 500)), ln(c(100, 500), c(200, 500)), # L2: two pieces in wp1
                     ln(c(0, 900), c(100, 900)), ln(c(5000, 900), c(5100, 900))),  # L3: wp1 and, elsewhere, wp2
                c('L1', 'L1', 'L2', 'L2', 'L3', 'L3'), c('wp1', 'wp2', 'wp1', 'wp1', 'wp1', 'wp2'),
                c(2019, 2022, 2019, 2019, 2022, 2019))
  r <- yukon_dedupe(x, 'lines', 20)
  expect_equal(r$n, 1L)
  expect_equal(r$x$source[r$x$REF_ID == 'L1'], 'wp2')               # the latest copy...
  expect_equal(r$x$image_year_first[r$x$REF_ID == 'L1'], 2019L)     # ...with the earliest year
  expect_equal(sum(r$x$REF_ID == 'L2'), 2L)
  expect_equal(sum(r$x$REF_ID == 'L3'), 2L)
  expect_true(any(grepl('REF_ID reused', r$log$step)))
})

test_that('reported years: mine inside or near a mine polygon, and fixes by REF_ID', {
  x <- yk_feats(list(sq(0, 0, 100), sq(1000, 0, 100), sq(5000, 0, 100)), c('P1', 'P2', 'P3'), 'GeoYukon', 2011L)
  x$feature_type <- c('quartz', 'othermines', 'otherpoly')
  mines <- sf::st_sf(name = c('In', 'Near', 'Far'), start = c(1990, 2005, 2010), end = c(2000, 0, 2015),
                     geometry = sf::st_sfc(sf::st_point(c(50, 50)), sf::st_point(c(1250, 50)), sf::st_point(c(3000, 50)), crs = crs))
  fixes <- data.table(kind = 'polys', ref_id = 'P3', year_start = '2007', year_end = '2019', year_source = 'Someone', note = 'Made up')
  r <- yukon_reported_years(x, 'polys', mines, fixes, 'Reported', c('quartz', 'othermines'), 200)
  expect_equal(r$x$year_start, c(1990L, 2005L, 2007L))
  expect_equal(r$x$year_end, c(2000L, NA, 2019L))      # 0 = still producing
  expect_equal(r$x$year_rule, rep('reported', 3))
  expect_equal(r$log[what == 'mine']$features, c(1L, 1L, 0L))   # Far: 1.9 km from any mine polygon
})

test_that('the border check pairs Yukon lines with the neighbour\'s and gives both years', {
  dir <- withr::local_tempdir()
  # border along y = 0 (Yukon to the north), as in the GeoYukon border file
  b <- sf::st_sf(JURISDICT1 = 'BC', JURISDICT2 = 'YK', geometry = sf::st_sfc(ln(c(-10000, 0), c(10000, 0)), crs = crs))
  bf <- file.path(dir, 'borders.gpkg'); sf::st_write(b, bf, quiet = TRUE)
  yk <- sf::st_sf(feature_id = c('YTL1', 'YTL2'), REF_ID = c('R1', 'R2'), feature_type = c('paved', 'otherlf'), year_start = c(NA, 2020L),
                  year_rule = c('none', 'image_new'), year_source = c(NA, 'Image 2020'),
                  geometry = sf::st_sfc(ln(c(0, 5000), c(0, 0)), ln(c(3000, 5000), c(3000, 100)), crs = crs))
  yf <- file.path(dir, 'YT_linear.gpkg'); sf::st_write(yk, yf, layer = 'linear', quiet = TRUE)
  nb <- sf::st_sf(category = c('paved', 'lfother'), year = c(1998L, 2016L), file = 'roads.gpkg',
                  geometry = sf::st_sfc(ln(c(20, 0), c(20, -5000)), ln(c(3300, 0), c(3300, -2000)), crs = crs))
  nf <- file.path(dir, 'features_bc.gpkg'); sf::st_write(nb, nf, layer = 'disturbance', quiet = TRUE)
  r <- check_yukon_borders(yf, c(bc = nf), bf, list(sources = list(bc = 'BC'), near_m = 500, match_m = list(50, 500),
                                                    yukon_types = list(otherlf = 'lfother')), crs)
  p <- r$pairs[from == 'YK']
  expect_equal(unique(p[yk_ref_id == 'R1']$match), 'lines up (<= 50 m)')
  expect_equal(unique(p[yk_ref_id == 'R1']$nb_year), 1998L)
  expect_equal(unique(p[yk_ref_id == 'R2']$match), 'possible (<= 500 m)')   # ends 100 m from the border, 316 m from the BC line
  expect_equal(unique(p[yk_ref_id == 'R2']$yk_year), 2020L)
  expect_true(all(c('points', 'neighbour_year', 'yukon_year') %in% names(r$summary)))
})

test_that('yukon_unpaved_type sorts unpaved roads into winter, resource and all-season', {
  x <- data.frame(feature_type = c('unpaved', 'unpaved', 'unpaved', 'unpaved', 'paved'),
                  TYPE_INDUSTRY = c('Mining', 'Transportation', 'Forestry', 'Transportation', 'Transportation'),
                  TYPE_DISTURBANCE = c('Access Road', 'Local Road', 'Resource road', 'Winter Road', 'Highway'),
                  industry_rc = c('Mining', 'Transportation', 'Forestry', 'Transportation', 'Transportation'))
  expect_equal(yukon_unpaved_type(x), c('resource', 'all-season', 'resource', 'winter', NA))
})

test_that('copies with no REF_ID in overlapping work packages are dropped, keeping one', {
  # A (wp1, 2019, no ID) is along B (wp2, 2022, with ID); C (wp1, no ID) and D (wp2, no ID) are copies of each other;
  # E (wp1, no ID) is on its own
  x <- yk_feats(list(ln(c(0, 0), c(100, 0)), ln(c(0, 5), c(100, 5)), ln(c(0, 500), c(100, 500)), ln(c(0, 505), c(100, 505)),
                     ln(c(0, 900), c(100, 900))),
                c(NA, 'B', NA, NA, NA), c('wp1', 'wp2', 'wp1', 'wp2', 'wp1'), c(2019, 2022, 2018, 2023, 2019))
  x$image_year_first <- x$image_year
  r <- yukon_dedupe_noid(x, 'lines', 20, 30)
  expect_equal(r$n, 2L)
  expect_equal(r$x$REF_ID, c('B', NA, NA))
  expect_equal(r$x$image_year, c(2022L, 2023L, 2019L))          # of C and D, the later (D) is kept
  expect_equal(r$x$image_year_first, c(2019L, 2018L, 2019L))    # with the earliest year of its copies
})

test_that('Yukon roads meeting a B.C. road with a year take its year', {
  dir <- withr::local_tempdir()
  yk <- sf::st_sf(feature_id = c('YTL1', 'YTL2', 'YTL3'), REF_ID = c('R1', 'R2', 'R3'), source = 'wp1',
                  TYPE_DISTURBANCE = 'Access Road', feature_type = c('unpaved', 'unpaved', 'otherlf'),
                  year_start = c(NA, 2022L, NA), year_rule = c('none', 'image_new', 'none'), year_source = NA_character_,
                  geometry = sf::st_sfc(ln(c(0, 0), c(0, 100)), ln(c(500, 0), c(500, 100)), ln(c(900, 0), c(900, 100)), crs = crs))
  m <- file.path(dir, 'merge'); reproducible::checkPath(m, create = TRUE)
  sf::st_write(yk, file.path(m, 'YT_linear.gpkg'), layer = 'linear', quiet = TRUE)
  sf::st_write(yk[0, ], file.path(m, 'YT_areal.gpkg'), layer = 'areal', quiet = TRUE)
  data.table::fwrite(data.table(table = 'features'), file.path(m, 'YT_merge_log.csv'))
  data.table::fwrite(clean_rows('x', 'lines', 'wp1', 'R9', 'Trail', 'made up', 1), file.path(m, 'YT_cleaning_log.csv'))
  merged <- file.path(m, c('YT_linear.gpkg', 'YT_areal.gpkg', 'YT_merge_log.csv', 'YT_cleaning_log.csv'))
  pairs <- data.table(from = 'YK', to = 'BC', gap_m = c(10, 30, 400, 5), yk_feature_id = c('YTL1', 'YTL2', 'YTL2', 'YTL3'),
                      yk_category = c('unpaved', 'unpaved', 'unpaved', 'lfother'), nb_category = c('unpaved', 'paved', 'unpaved', 'unpaved'),
                      nb_year = c(1995L, 2004L, 1980L, 1990L), nb_file = 'roads.gpkg')
  f <- apply_border_years(merged, list(pairs = pairs), list(sources = list(bc = 'BC'), years_from = list('bc'),
                                                            match_m = list(50, 500)), file.path(dir, 'final'))
  out <- sf::st_read(f[1], quiet = TRUE)
  expect_equal(out$year_start, c(1995L, 2004L, NA))   # R2: the 400 m match is too far; R3 is not a road
  expect_equal(out$year_rule, c('neighbour', 'neighbour', 'none'))
  expect_match(out$year_source[2], 'Yukon year was 2022 [(]image_new[)]')
  expect_equal(nrow(data.table::fread(f[4])), 3)
})
