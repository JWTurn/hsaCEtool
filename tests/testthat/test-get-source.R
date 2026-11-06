test_that('readers given as text become functions, or calls for prepInputs', {
  expect_identical(as_reader('sf::st_read'), sf::st_read)
  expect_identical(as_reader('readRDS'), readRDS)
  expect_identical(as_reader(data.table::fread), data.table::fread)
  expect_true(is.na(as_reader(NA)))
  expect_identical(prep_reader('sf::st_read'), quote(sf::st_read(targetFile)))
  expect_true(is.na(prep_reader(NA)))
  # prepInputs fails when given the function, but reads with the call
  tmp <- withr::local_tempdir()
  data.table::fwrite(data.table(a = 1:3), file.path(tmp, 'made_up.csv'))
  read <- function(fun) {
    withr::with_options(list(reproducible.useCache = FALSE),
      suppressMessages(reproducible::prepInputs(targetFile = 'made_up.csv', destinationPath = tmp, fun = fun)))
  }
  expect_error(read(data.table::fread))
  expect_equal(nrow(read(prep_reader('data.table::fread'))), 3)
})
