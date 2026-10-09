test_that('readers given as text reach prepInputs as functions', {
  expect_identical(as_reader('sf::st_read'), sf::st_read)
  expect_identical(as_reader('readRDS'), readRDS)
  expect_identical(as_reader(data.table::fread), data.table::fread)
  expect_true(is.na(as_reader(NA)))
  # prepInputs fails on text with a package prefix held in a variable, but reads with the function
  tmp <- withr::local_tempdir()
  data.table::fwrite(data.table(a = 1:3), file.path(tmp, 'made_up.csv'))
  read <- function(fun) {
    withr::with_options(list(reproducible.useCache = FALSE),
      suppressMessages(reproducible::prepInputs(targetFile = 'made_up.csv', destinationPath = tmp, fun = fun)))
  }
  expect_error(read('data.table::fread'), 'not found')
  expect_equal(nrow(read(as_reader('data.table::fread'))), 3)
})
