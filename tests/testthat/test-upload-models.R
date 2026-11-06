# upload_models() helpers, with made-up fits (no Drive access)

test_that('models to upload come from upload: true or the names given', {
  mods <- list(a = list(upload = TRUE), b = list(), c = list(upload = TRUE, fit = TRUE), d = list(fit = FALSE))
  expect_equal(models_to_upload(mods), c('a', 'c'))
  expect_equal(models_to_upload(mods, 'b'), 'b')
  expect_error(models_to_upload(mods, c('a', 'z')), 'Not models in the settings: z')
  expect_error(models_to_upload(mods, 'd'), 'fit: false have no fit to upload: d')
  expect_error(models_to_upload(list(b = list())), 'No models to upload')
})

test_that('fits with positions, times or real animal IDs are not shared', {
  ok <- list(frame = data.frame(case_ = c(TRUE, FALSE), id = c('A0001', 'A0002'), year = 2020))
  expect_true(check_fit_shareable(ok, 'm'))
  expect_error(check_fit_shareable(list(frame = cbind(ok$frame, t1_ = Sys.time())), 'm'), 'm: the fit holds positions or times \\(t1_\\)')
  real <- ok; real$frame$id <- c('A0001', 'caribou7')
  expect_error(check_fit_shareable(real, 'm'), 'animal IDs that are not codes')
})

test_that('a model is zipped as its fit and its coefficient tables', {
  dir <- withr::local_tempdir()
  csv <- file.path(dir, paste0('m_', c('fixed', 'random'), '.csv'))
  for (f in csv) writeLines('term,estimate', f)
  zips <- write_model_zips(list(frame = data.frame(id = 'A0001')), csv, 'm', 'TEST', file.path(dir, 'up'))
  expect_equal(basename(zips), c('TEST_model_fit_m.zip', 'TEST_model_m.zip'))
  expect_equal(utils::unzip(zips[1], list = TRUE)$Name, 'TEST_model_fit_m.rds')
  expect_setequal(utils::unzip(zips[2], list = TRUE)$Name, basename(csv))
})

test_that('models unchanged since their last upload are skipped', {
  dir <- withr::local_tempdir()
  file <- file.path(dir, 'uploaded_models.csv')
  rec <- read_upload_record(file)
  expect_equal(nrow(rec), 0)
  expect_true(needs_upload(rec, 'm', c(fit = 'f1', coefs = 'c1')))
  data.table::fwrite(data.table(model = 'm', fit_hash = 'f1', coefs_hash = 'c1', fit_id = 'x', coefs_id = 'y',
                                uploaded = '2026-11-06'), file)
  rec <- read_upload_record(file)
  expect_false(needs_upload(rec, 'm', c(fit = 'f1', coefs = 'c1')))
  expect_true(needs_upload(rec, 'm', c(fit = 'f2', coefs = 'c1')))
  expect_true(needs_upload(rec, 'm', c(fit = 'f1', coefs = 'c2')))
  expect_true(needs_upload(rec, 'other', c(fit = 'f1', coefs = 'c1')))
})
