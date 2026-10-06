# Land-cover proportions and parallel settings, with small made-up rasters

# Fine grid 16 x 16 cells of 30 m (480 m square); templates at 120 and 240 m
lc_grid <- function(dir) {
  fine <- terra::rast(xmin = 0, xmax = 480, ymin = 0, ymax = 480, res = 30, crs = 'EPSG:3154', vals = 1)
  f <- file.path(dir, 'fine.tif'); terra::writeRaster(fine, f)
  make_templates(NULL, list(source = list(path = f), base_res = 30, factors = list(4, 8)),
                 'EPSG:3154', list(), file.path(dir, 'templates'))
}
# A made-up classification: left half code 1, right half code 11, top-left
# 120 m block code 5, one cell with no data and one with an unlisted code
lc_map <- function(dir) {
  r <- terra::rast(xmin = 0, xmax = 480, ymin = 0, ymax = 480, res = 30, crs = 'EPSG:3154')
  v <- matrix(rep(c(1L, 11L), each = 8), 16, 16, byrow = TRUE)
  v[1:4, 1:4] <- 5L
  v[16, 16] <- NA
  v[16, 15] <- 99L
  terra::values(r) <- as.vector(t(v))
  f <- file.path(dir, 'scanfi_2020.tif'); terra::writeRaster(r, f, datatype = 'INT1U', NAflag = 255)
  f
}
classes <- list('1' = 'water', '5' = 'lichen', '11' = 'treed_coniferous')

test_that('count_classes gives proportions of classified cells per coarse cell', {
  dir <- withr::local_tempdir()
  tf <- lc_grid(dir)
  p <- count_classes(lc_map(dir), tf, 120, c(1L, 5L, 11L))
  expect_equal(terra::nlyr(p), 4)
  v <- unname(terra::values(p))
  expect_equal(v[1, ], c(0, 1, 0, 16))                     # top-left block is all lichen
  expect_equal(v[2, ], c(1, 0, 0, 16))                     # water
  expect_equal(v[4, ], c(0, 0, 1, 16))                     # conifer
  expect_equal(v[16, ], c(0, 0, 1, 14))                    # NA and unlisted code not counted
  expect_equal(unname(rowSums(v[, 1:3])), rep(1, 16))
})

test_that('coarser_props matches counting at the coarser resolution', {
  dir <- withr::local_tempdir()
  tf <- lc_grid(dir)
  m <- lc_map(dir)
  a <- coarser_props(count_classes(m, tf, 120, c(1L, 5L, 11L)), 2)
  b <- count_classes(m, tf, 240, c(1L, 5L, 11L))
  expect_equal(as.vector(terra::values(a)), as.vector(terra::values(b)))
})

test_that('build_landcover_year writes cell and moving-window proportions', {
  dir <- withr::local_tempdir()
  tf <- lc_grid(dir)
  files <- build_landcover_year(lc_map(dir), 2020, classes, c(120, 240), list('120' = 200),
                                tf, file.path(dir, 'out'))
  b <- basename(files)
  expect_true(all(c('landcover_cell_water_120m_2020.tif', 'landcover_cell_lichen_240m_2020.tif',
                    'landcover_water_120m_2020.tif') %in% b))
  expect_false(any(grepl('^landcover_water_240m', b)))     # no window at 240 m
  expect_length(files, 3 * 3)                              # cell at 2 resolutions + window at 1
  cell <- terra::rast(files[b == 'landcover_cell_lichen_240m_2020.tif'])
  expect_equal(as.vector(terra::values(cell))[1], 0.25)
  # window: circular weights summing to 1, as in make_landforest_prop()
  p <- terra::rast(files[b == 'landcover_cell_water_120m_2020.tif'])
  w <- terra::focalMat(p, d = 200, type = 'circle')
  expected <- as.vector(terra::values(terra::focal(p, w, na.rm = TRUE, pad = TRUE, padValue = 0)))
  expect_equal(as.vector(terra::values(terra::rast(files[b == 'landcover_water_120m_2020.tif']))), expected,
               tolerance = 1e-6)
})

test_that('build_landcover_year ignores classes set to NULL', {
  dir <- withr::local_tempdir()
  tf <- lc_grid(dir)
  files <- build_landcover_year(lc_map(dir), 2020, c(classes, list('20' = NULL)), 120, list(),
                                tf, file.path(dir, 'out'))
  expect_length(files, 3)
})

test_that('in_archive points GDAL inside a zip', {
  expect_equal(in_archive('/vsicurl/https://x.org/a.zip', 'a.tif'), '/vsizip//vsicurl/https://x.org/a.zip/a.tif')
  expect_equal(in_archive('a.tif', NULL), 'a.tif')
})

test_that('parallel_controller runs one step at a time unless workers > 1', {
  expect_null(parallel_controller(list(workers = 1)))
  expect_null(parallel_controller(list()))
  skip_if_not_installed('crew')
  ctrl <- parallel_controller(list(workers = 2, memfrac = 0.6))
  expect_s3_class(ctrl, 'crew_class_controller')
})
