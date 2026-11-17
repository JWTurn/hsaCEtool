# Which Drive folder each file to upload goes to (nothing is uploaded).

files <- c('clean_TEST.zip', 'clean_TEST_20kmBuff.zip', 'TEST_studyArea_extended.zip')

test_that('one folder takes every file', {
  expect_equal(upload_destinations(files, 'folderA'), rep('folderA', 3))
})

test_that('folders by file name pattern: the first match wins', {
  folders <- list(clean_TEST.zip = 'private', '*' = 'shared')
  expect_equal(upload_destinations(files, folders), c('private', 'shared', 'shared'))
  # e.g. layer stacks every 5 years
  stacks <- c('TEST_layers_2015.zip', 'TEST_layers_2020.zip', 'notes.txt')
  expect_equal(
    upload_destinations(stacks, c('TEST_layers_*.zip' = 'layers', '*' = 'other')),
    c('layers', 'layers', 'other')
  )
  # a pattern only matches the whole name
  expect_error(
    upload_destinations(files, list(clean_TEST.zip = 'private')),
    'No Drive folder for: clean_TEST_20kmBuff.zip, TEST_studyArea_extended.zip'
  )
  expect_error(upload_destinations(files, list('private', 'shared')), 'named by file name pattern')
})

test_that('only the folder of the cleaned locations must be restricted', {
  calls <- list()
  local_mocked_bindings(
    read_config = function(project, ...) {
      list(locations = list(premade_folder = list(clean_TEST.zip = 'private', '*' = 'shared')))
    },
    upload_to_drive = function(files, folder, public_msg) {
      calls[[folder]] <<- list(files = basename(files), checked = !is.null(public_msg))
      data.table::data.table(name = basename(files))
    }
  )
  upload_premade('TEST', zips = file.path('somewhere', files))
  expect_equal(calls$private, list(files = 'clean_TEST.zip', checked = TRUE))
  expect_equal(calls$shared, list(files = files[2:3], checked = FALSE))
})

test_that('a folder open to anyone with the link is refused only when checked', {
  local_mocked_bindings(
    drive_get = function(id) id,
    drive_reveal = function(x, what) {
      list(permissions_resource = list(list(permissions = list(list(type = 'anyone')))))
    },
    drive_ls = function(x) data.frame(name = character(), id = character()),
    drive_upload = function(media, path) data.frame(id = 'new'),
    drive_link = function(f) 'link',
    .package = 'googledrive'
  )
  expect_error(upload_to_drive('a.zip', 'open', 'must be restricted'), 'must be restricted')
  expect_equal(upload_to_drive('a.zip', 'open', NULL)$action, 'uploaded')
})

test_that('write_premade_files zips the cleaned locations and both study areas', {
  skip_if_not_installed('terra')
  dir <- withr::local_tempdir()
  # made-up locations and squares, nothing real
  locs <- data.table::data.table(id = 'a', x_ = 1, y_ = 2)
  sq <- terra::vect('POLYGON ((0 0, 10 0, 10 10, 0 10, 0 0))', crs = 'EPSG:3005')
  zips <- write_premade_files(locs, sq, sq, 'TEST', 20000, file.path(dir, 'premade'))
  expect_equal(basename(zips), files)
  expect_true(all(file.exists(zips)))
  inside <- utils::unzip(zips[1], list = TRUE)$Name
  expect_equal(inside, 'clean_TEST.rds')
})

test_that('upload_premade_files checks only the folder of the cleaned locations', {
  calls <- list()
  local_mocked_bindings(
    upload_to_drive = function(files, folder, public_msg) {
      calls[[folder]] <<- !is.null(public_msg)
      data.table::data.table(name = basename(files))
    }
  )
  upload_premade_files(files, list(clean_TEST.zip = 'private', '*' = 'shared'), 'TEST')
  expect_equal(calls, list(private = TRUE, shared = FALSE))
  expect_error(upload_premade_files(files, NULL, 'TEST'), 'premade_folder')
})

test_that('drive_file_version gives the checksum, and a clear error offline', {
  local_mocked_bindings(
    drive_get = function(id) list(drive_resource = list(list(md5Checksum = 'abc'))),
    .package = 'googledrive'
  )
  expect_equal(drive_file_version('https://drive.google.com/file/d/xyz/view'), 'abc')
  local_mocked_bindings(
    drive_get = function(id) stop('no connection'),
    .package = 'googledrive'
  )
  expect_error(
    drive_file_version('https://drive.google.com/file/d/xyz/view'),
    'premade_check: false'
  )
  expect_true(is_drive_url('https://drive.google.com/file/d/xyz/view'))
  expect_false(is_drive_url('https://example.org/a.zip'))
  expect_false(is_drive_url(NA_character_))
})
