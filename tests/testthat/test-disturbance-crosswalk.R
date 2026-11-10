# Disturbance crosswalk tables, with made-up road classes and files

# A roads file sorted by two fields (one road with no Surface), and a
# trails file whose features all get one category
cw_write <- function(dir, lines = NULL) {
  f <- file.path(dir, 'crosswalk_TEST.csv')
  writeLines(lines %||% c(
    'file,Surface,Use,category,subtype,notes',
    'roads.gpkg,Paved,Public,paved,,',
    'roads.gpkg,Gravel,Public,unpaved,all-season,',
    'roads.gpkg,Gravel,"Forestry, Oil",unpaved,resource,a value with a comma',
    'roads.gpkg,,Forestry,unpaved,resource,no Surface recorded',
    'roads.gpkg,Ferry,Public,drop,,on water',
    'trails.gpkg,,,lfother,,'), f)
  f
}
cw_src <- list(url = 'made-up', crosswalk = 'crosswalk_TEST.csv',
               files = list(list(file = 'roads.gpkg', year = list('yr')), list(file = 'trails.gpkg')))

test_that('read_disturbance_crosswalk reads a table and reports problems', {
  dir <- withr::local_tempdir()
  tab <- read_disturbance_crosswalk(cw_write(dir))
  expect_equal(nrow(tab), 6)
  expect_equal(tab$Use[3], 'Forestry, Oil')
  expect_true(is.na(tab$Surface[4]))
  expect_error(read_disturbance_crosswalk(file.path(dir, 'none.csv')), 'not found')
  expect_error(read_disturbance_crosswalk(cw_write(dir, c('file,Surface', 'roads.gpkg,Paved'))), 'needs column.*category')
  bad <- cw_write(dir, c('file,Surface,category', 'roads.gpkg,Paved,paved', 'roads.gpkg,Paved,unpaved',
                         'trails.gpkg,,lfother', 'trails.gpkg,,otherlf', ',Gravel,'))
  expect_error(read_disturbance_crosswalk(bad), 'no file: rows 6')
  expect_error(read_disturbance_crosswalk(bad), 'no category: rows 6')
  expect_error(read_disturbance_crosswalk(bad), 'roads.gpkg: the same values listed twice: rows 3')
  expect_error(read_disturbance_crosswalk(bad), 'trails.gpkg fills in no field, so it needs exactly one row')
})

test_that('a crosswalk becomes one exact rule per row, or one category per file', {
  dir <- withr::local_tempdir()
  src <- apply_disturbance_crosswalk(cw_src, read_disturbance_crosswalk(cw_write(dir)), 'test')
  expect_null(src$crosswalk)
  roads <- src$files[[1]]
  expect_equal(roads$year, list('yr'))
  expect_length(roads$rules, 5)
  expect_equal(roads$rules[[1]], list(where = list(Surface = 'Paved', Use = 'Public'), category = 'paved'))
  expect_equal(roads$rules[[4]], list(where = list(Surface = '', Use = 'Forestry'), category = 'unpaved',
                                      subtype = 'resource'))
  expect_equal(src$files[[2]], list(file = 'trails.gpkg', category = 'lfother'))
  # sorting: blank matches a missing value; values not listed match no rule
  d <- data.table(Surface = c('Paved', 'Gravel', NA, 'Ferry', 'Gravel', 'Paved'),
                  Use = c('Public', 'Forestry, Oil', 'Forestry', 'Public', 'Mining', 'Forestry'))
  cat <- suppressWarnings(disturbance_category(d, roads))
  expect_equal(as.vector(cat), c('paved', 'unpaved', 'unpaved', 'drop', NA, NA))
  expect_equal(disturbance_subtype(d, roads, attr(cat, 'rule')), c(NA, 'resource', 'resource', NA, NA, NA))
  expect_warning(disturbance_category(d, roads), '2 features match no rule')
})

test_that('a crosswalk must match the files listed for the source', {
  dir <- withr::local_tempdir()
  tab <- read_disturbance_crosswalk(cw_write(dir))
  # rows for other files are skipped here, but must be for a file of a source naming the table
  one <- cw_src
  one$files <- one$files[1]
  expect_length(apply_disturbance_crosswalk(one, tab, 'test')$files, 1)
  two <- list(sources = list(a = one, b = one))
  expect_error(apply_disturbance_crosswalks(two, dir), 'lists file\\(s\\) not in the files of a or b: trails.gpkg')
  two$sources$b$files <- cw_src$files[2]
  two <- apply_disturbance_crosswalks(two, dir)
  expect_equal(two$sources$b$files[[1]]$category, 'lfother')
  expect_length(two$sources$a$files[[1]]$rules, 5)
  more <- cw_src
  more$files <- c(more$files, list(list(file = 'mines.gpkg')))
  expect_error(apply_disturbance_crosswalk(more, tab, 'test'), 'mines.gpkg has no rows in crosswalk')
  both <- cw_src
  both$files[[2]]$category <- 'lfother'
  expect_error(apply_disturbance_crosswalk(both, tab, 'test'), 'trails.gpkg has a category or rules and a crosswalk')
})

test_that('read_config applies a crosswalk named in the settings', {
  dir <- withr::local_tempdir()
  file.copy(list.files(test_path('fixtures', 'config'), full.names = TRUE), dir)
  y <- readLines(file.path(dir, 'TEST.yaml'))
  i <- grep('^ *- file: roads.shp', y)
  stopifnot(length(i) == 1, grepl('^ *rules:', y[i + 1]))
  # replace the roads rules with a crosswalk table
  y <- c(y[1:(i - 2)], '        crosswalk: crosswalk_TEST.csv', y[i - 1], '          - file: roads.shp', y[-(1:(i + 3))])
  writeLines(y, file.path(dir, 'TEST.yaml'))
  cw_write(dir, c('file,Surface,category', 'roads.shp,Paved,paved', 'roads.shp,Gravel,unpaved'))
  cfg <- read_config('TEST', config_dir = dir)
  expect_equal(cfg$landscape$disturbance$sources$roads$files[[1]]$rules[[2]],
               list(where = list(Surface = 'Gravel'), category = 'unpaved'))
})

test_that('check_disturbance_coverage lists the kinds of feature left out, then stops', {
  dir <- withr::local_tempdir()
  s <- data.table(
    source = c('bc', 'bc', 'bc', 'nwt'), file = c('roads.gpkg', 'roads.gpkg', 'roads.gpkg', 'trails.gpkg'),
    type = c('Surface=Paved; Use=Public', 'Surface=NA; Use=Forestry, Oil', 'Surface=Gravel; Use=Mining', NA),
    category = c('paved', 'none (left out)', 'none (left out)', 'lfother'), subtype = NA_character_,
    features = c(10L, 2L, 3L, 4L), length_km = c(5, 1, 2, 3), area_km2 = NA_real_,
    no_year = 0L, first_year = 2000L, last_year = 2010L)
  expect_error(check_disturbance_coverage(s, dir), '2 kind\\(s\\) of disturbance feature \\(5 features, in source\\(s\\) bc\\)')
  out <- data.table::fread(file.path(dir, 'disturbance_uncovered.csv'), na.strings = '')
  expect_equal(names(out), c('source', 'file', 'Surface', 'Use', 'features', 'length_km', 'area_km2', 'first_year', 'last_year'))
  expect_equal(out$Surface, c(NA, 'Gravel'))
  expect_equal(out$Use, c('Forestry, Oil', 'Mining'))
  # all covered: the file has only its header
  f <- check_disturbance_coverage(s[c(1, 4)], dir)
  expect_equal(nrow(data.table::fread(f)), 0)
})
