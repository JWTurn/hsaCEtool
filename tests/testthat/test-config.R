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
  # seasons: the package's table unless the project names its own
  expect_equal(names(season_dates(cfg$seasons, cfg$model$season_set)), c('winter', 'spring', 'summer', 'fall'))
  writeLines(c('set,season,start_month,start_day,end_month,end_day', 'mine,warm,5,1,9,30'),
             file.path(dir, 'seasons_TEST.csv'))
  yaml::write_yaml(list(locations = list(sources = list(list(type = 'file', path = 'x.rds'))),
                        model = list(season_table = 'seasons_TEST.csv', season_set = 'mine')),
                   file.path(dir, 'TEST.yaml'))
  cfg <- read_config('TEST', config_dir = dir)
  expect_equal(season_dates(cfg$seasons, 'mine'), list(warm = list('05-01', '09-30')))
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

test_that('shared settings files come between the defaults and the project file', {
  dir <- withr::local_tempdir()
  shared1 <- file.path(dir, 'shared1.yaml')
  shared2 <- file.path(dir, 'shared2.yaml')
  yaml::write_yaml(list(project = list(crs = 'EPSG:3005'), tracks = list(n_random = 8, min_steps = 9)), shared1)
  yaml::write_yaml(list(tracks = list(n_random = 6)), shared2)
  yaml::write_yaml(list(locations = list(sources = list(list(type = 'file', path = 'x.rds'))),
                        tracks = list(min_steps = 4)),
                   file.path(dir, 'TEST.yaml'))
  cfg <- read_config('TEST', config_dir = dir, shared = c(shared1, shared2))
  expect_equal(cfg$project$crs, 'EPSG:3005')   # shared over the defaults
  expect_equal(cfg$tracks$n_random, 6)         # later shared file wins
  expect_equal(cfg$tracks$min_steps, 4)        # project file wins over shared
  expect_equal(cfg$tracks$rate_hours, 11)      # still from the defaults
  expect_error(read_config('TEST', config_dir = dir, shared = file.path(dir, 'none.yaml')), 'not found')
})

test_that('a default.yaml read as the project file or as shared settings gives no warning', {
  dir <- withr::local_tempdir()
  yaml::write_yaml(list(locations = list(sources = list(list(type = 'file', path = 'x.rds'))),
                        tracks = list(n_random = 12, min_steps = 5)),
                   file.path(dir, 'default.yaml'))
  yaml::write_yaml(list(tracks = list(min_steps = 3)), file.path(dir, 'local.yaml'))
  expect_no_warning(cfg <- read_config('TEST', config_dir = dir, project_file = file.path(dir, 'default.yaml')))
  expect_equal(cfg$tracks$n_random, 12)
  expect_equal(cfg$tracks$min_steps, 3)        # local.yaml still wins
  expect_equal(cfg$project$name, 'TEST')
  yaml::write_yaml(list(locations = list(sources = list(list(type = 'file', path = 'x.rds')))), file.path(dir, 'TEST.yaml'))
  expect_no_warning(cfg <- read_config('TEST', config_dir = dir, shared = file.path(dir, 'default.yaml')))
  expect_equal(cfg$tracks$n_random, 12)
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
