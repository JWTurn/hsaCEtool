# Tree species sources (SCANFI by species, VRI species composition), with small made-up files

# Made-up SCANFI-like species rasters: 10 x 10 cells of 30 m (300 m square), crown closure
# 0-100 with 255 for no data, one file per species for 2025
scanfi_files <- function(dir) {
  make <- function(sp, v) {
    r <- terra::rast(xmin = 0, xmax = 300, ymin = 0, ymax = 300, res = 30, crs = 'EPSG:3154')
    terra::values(r) <- v
    terra::writeRaster(
      r,
      file.path(dir, sprintf('cc_%s_2025.tif', sp)),
      datatype = 'INT1U',
      NAflag = 255
    )
  }
  make('ABIE_LAS', rep(c(40L, NA), each = 50)) # 40 % in the top half, no data below
  make('PICE_ENG', rep(10L, 100))
  file.path(dir, 'cc_{species}_{year}.tif')
}
# The left half of the rasters
left_half <- function() {
  terra::vect('POLYGON ((0 0, 150 0, 150 300, 0 300, 0 0))', crs = 'EPSG:3154')
}

test_that('read_scanfi_species cuts each species to the area, on the source grid', {
  dir <- withr::local_tempdir()
  pattern <- scanfi_files(dir)
  out <- read_scanfi_species(
    pattern,
    c('ABIE_LAS', 'PICE_ENG'),
    2025,
    left_half(),
    file.path(dir, 'out')
  )
  expect_named(out, c('ABIE_LAS', 'PICE_ENG'))
  expect_true(all(file.exists(out)))
  expect_match(basename(out), '^scanfi_(ABIE_LAS|PICE_ENG)_2025\\.tif$')
  bl <- terra::rast(out[['ABIE_LAS']])
  expect_equal(terra::res(bl), c(30, 30))
  expect_equal(as.vector(terra::ext(bl)), c(xmin = 0, xmax = 150, ymin = 0, ymax = 300))
  # 5 columns x 5 rows of 40 %, the rest no data
  expect_equal(sum(terra::values(bl), na.rm = TRUE), 25 * 40)
  expect_equal(sum(is.na(terra::values(bl))), 25)
  expect_equal(sum(terra::values(terra::rast(out[['PICE_ENG']]))), 50 * 10)
})

test_that('read_scanfi_species masks cells outside a polygon that is not a rectangle', {
  dir <- withr::local_tempdir()
  pattern <- scanfi_files(dir)
  tri <- terra::vect('POLYGON ((0 0, 290 0, 0 290, 0 0))', crs = 'EPSG:3154')
  out <- read_scanfi_species(pattern, 'PICE_ENG', 2025, tri, file.path(dir, 'out'))
  v <- terra::values(terra::rast(out))
  # cells whose centres are inside the triangle (none on its edge): 9 + 8 + ... + 1
  expect_equal(sum(!is.na(v)), 45)
})

test_that('read_scanfi_species reads one year at a time', {
  expect_error(
    read_scanfi_species('x_{species}_{year}.tif', 'A', c(2020, 2025), left_half(), tempdir()),
    'one year'
  )
})

# Made-up VRI-like polygons: three 100 m squares in a row, with species columns
vri_file <- function(dir, zip = FALSE) {
  sq <- function(x0) {
    sprintf('POLYGON ((%1$d 0, %2$d 0, %2$d 100, %1$d 100, %1$d 0))', x0, x0 + 100L)
  }
  v <- sf::st_sf(
    FEATURE_ID = 1:3,
    BCLCS_LEVEL_2 = c('T', 'T', 'N'),
    SPECIES_CD_1 = c('BL', 'SX', NA),
    SPECIES_PCT_1 = c(70, 100, NA),
    SPECIES_CD_2 = c('SE', NA, NA),
    SPECIES_PCT_2 = c(30, NA, NA),
    OTHER = c('a', 'b', 'c'),
    geometry = sf::st_as_sfc(c(sq(0L), sq(100L), sq(200L)), crs = 3154)
  )
  f <- file.path(dir, 'vri.gpkg')
  sf::st_write(v, f, layer = 'veg', quiet = TRUE)
  if (!zip) {
    return(f)
  }
  z <- file.path(dir, 'vri.gpkg.zip')
  withr::with_dir(dir, utils::zip(basename(z), basename(f), flags = '-q'))
  z
}
vri_cols <- c('FEATURE_ID', 'BCLCS_LEVEL_2', 'SPECIES_CD_1', 'SPECIES_PCT_1', 'SPECIES_CD_2')

test_that('read_vri_species keeps whole polygons overlapping the area, and the columns asked', {
  dir <- withr::local_tempdir()
  area <- terra::vect('POLYGON ((50 10, 150 10, 150 90, 50 90, 50 10))', crs = 'EPSG:3154')
  out <- read_vri_species(vri_file(dir), area, file.path(dir, 'out', 'v.gpkg'), columns = vri_cols)
  v <- sf::st_read(out, quiet = TRUE)
  expect_equal(sort(v$FEATURE_ID), 1:2)
  expect_setequal(setdiff(names(v), attr(v, 'sf_column')), vri_cols)
  expect_equal(sum(as.numeric(sf::st_area(v))), 2 * 100^2) # not cut at the area's edge
})

test_that('read_vri_species reads a zipped source and an area in another projection', {
  dir <- withr::local_tempdir()
  area <- terra::project(
    terra::vect('POLYGON ((210 10, 290 10, 290 90, 210 90, 210 10))', crs = 'EPSG:3154'),
    'EPSG:4326'
  )
  out <- read_vri_species(
    vri_file(dir, zip = TRUE),
    area,
    file.path(dir, 'v.gpkg'),
    target_file = 'vri.gpkg',
    columns = vri_cols
  )
  expect_equal(sf::st_read(out, quiet = TRUE)$FEATURE_ID, 3L)
})

test_that('read_vri_species asks for a layer when the source has several', {
  dir <- withr::local_tempdir()
  f <- vri_file(dir)
  sf::st_write(sf::st_read(f, quiet = TRUE), f, layer = 'veg2', append = FALSE, quiet = TRUE)
  expect_error(
    read_vri_species(f, left_half(), file.path(dir, 'v.gpkg'), columns = vri_cols),
    'name one with layer'
  )
})
