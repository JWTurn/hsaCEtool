# Exclusion points given as a table (CSV) rather than typed in the YAML.
# Made-up points only.

test_that('an exclusion points table gives the same settings as points typed in the YAML', {
  dir <- withr::local_tempdir()
  writeLines(c('name,lon,lat,radius_m,notes',
               'Made-up office,-120.5,60.25,10000,a note',
               ',-121,61,500,'), file.path(dir, 'points_TEST.csv'))
  typed <- list(list(name = 'Made-up office', lon = -120.5, lat = 60.25, radius_m = 10000L),
                list(lon = -121L, lat = 61L, radius_m = 500L))
  yaml_file <- file.path(dir, 'typed.yaml')
  writeLines(c('exclude_points:',
               '  - {name: Made-up office, lon: -120.5, lat: 60.25, radius_m: 10000}',
               '  - {lon: -121, lat: 61, radius_m: 500}'), yaml_file)
  expect_identical(read_settings_file(yaml_file)$exclude_points, typed)
  yaml::write_yaml(list(locations = list(sources = list(list(type = 'movebank', name = 'A', study = 1L)),
                                         cleaning = list(params = list(exclude_points = 'points_TEST.csv')))),
                   file.path(dir, 'TEST.yaml'))
  cfg <- read_config('TEST', config_dir = dir)
  expect_identical(cfg$locations$cleaning$params$exclude_points, typed)
})

test_that('an exclusion points table with problems is reported row by row', {
  dir <- withr::local_tempdir()
  f <- file.path(dir, 'points_TEST.csv')
  writeLines(c('lon,lat,radius_m', '-200,60,100', '-120,x,100', '-120,60,0'), f)
  err <- tryCatch(read_exclude_points(f), error = conditionMessage)
  expect_match(err, 'lon must be a longitude (-180 to 180): rows 2', fixed = TRUE)
  expect_match(err, 'lat must be a latitude (-90 to 90): rows 3', fixed = TRUE)
  expect_match(err, 'radius_m must be a distance above 0: rows 4')
  writeLines(c('lon,lat,radius', '-120,60,100'), f)
  expect_error(read_exclude_points(f), 'unknown columns: radius')
})
