test_that('merge_config combines sections and replaces lists whole', {
  x <- list(a = list(b = 1, c = 2), steps = list('x', 'y'))
  y <- list(a = list(c = 3), steps = list('z'))
  out <- merge_config(x, y)
  expect_equal(out$a, list(b = 1, c = 3))
  expect_equal(out$steps, list('z'))
})

test_that('merge_config can set a setting to NULL', {
  out <- merge_config(list(a = 1, b = 2), list(a = NULL))
  expect_true('a' %in% names(out))
  expect_null(out$a)
})

test_that('read_config combines default, project and override files', {
  dir <- withr::local_tempdir()
  yaml::write_yaml(list(locations = list(sources = list(list(type = 'file', path = 'x.rds'))),
                        tracks = list(n_random = 5)),
                   file.path(dir, 'TEST.yaml'))
  writeLines('function(cfg) { cfg$tracks$min_steps <- 7; cfg }', file.path(dir, 'TEST.R'))

  cfg <- read_config('TEST', config_dir = dir)
  expect_equal(cfg$tracks$n_random, 5)
  expect_equal(cfg$tracks$min_steps, 7)
  expect_equal(cfg$tracks$rate_hours, 11)   # from default.yaml
  expect_equal(cfg$project$name, 'TEST')
})

test_that('check_config reports problems in plain language', {
  dir <- withr::local_tempdir()
  yaml::write_yaml(list(project = list(crs = 'EPSG:4326'),
                        locations = list(cleaning = list(steps = list('no_such_step')))),
                   file.path(dir, 'BAD.yaml'))
  err <- tryCatch(read_config('BAD', config_dir = dir), error = conditionMessage)
  expect_match(err, 'units in metres')
  expect_match(err, 'sources is empty')
  expect_match(err, 'unknown step "no_such_step"')
})

test_that('read_config needs a project name', {
  expect_error(read_config(''), 'No project chosen')
})

test_that('a complete made-up project has valid settings', {
  cfg <- test_config()
  expect_equal(cfg$project$name, 'TEST')
  expect_equal(cfg$layers$name, 'dist_paved_240')
})

test_that('a default.yaml in the project folder is not read, with a warning', {
  dir <- withr::local_tempdir()
  yaml::write_yaml(list(tracks = list(n_random = 99)), file.path(dir, 'default.yaml'))
  yaml::write_yaml(list(locations = list(sources = list(list(type = 'file', path = 'x.rds')))), file.path(dir, 'TEST.yaml'))
  expect_warning(cfg <- read_config('TEST', config_dir = dir), 'is not read')
  expect_equal(cfg$tracks$n_random, 30)
})

test_that('a key named y stays a name, not TRUE', {
  file <- withr::local_tempfile(fileext = '.yaml')
  writeLines(c('columns: {x: lon, y: lat}', 'keep: yes'), file)
  out <- read_settings_file(file)
  expect_equal(out$columns, list(x = 'lon', y = 'lat'))
  expect_true(out$keep)
})

test_that('premade locations can replace the sources', {
  dir <- withr::local_tempdir()
  yaml::write_yaml(list(locations = list(premade = list(path = 'clean.rds'))), file.path(dir, 'PRE.yaml'))
  expect_equal(read_config('PRE', config_dir = dir)$locations$premade$path, 'clean.rds')
  yaml::write_yaml(list(locations = list(premade = list(target_file = 'clean.rds'))), file.path(dir, 'BAD.yaml'))
  expect_error(read_config('BAD', config_dir = dir), 'premade: give a path or a url')
})
