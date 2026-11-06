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
  expect_equal(upload_destinations(stacks, c('TEST_layers_*.zip' = 'layers', '*' = 'other')),
               c('layers', 'layers', 'other'))
  # a pattern only matches the whole name
  expect_error(upload_destinations(files, list(clean_TEST.zip = 'private')),
               'No Drive folder for: clean_TEST_20kmBuff.zip, TEST_studyArea_extended.zip')
  expect_error(upload_destinations(files, list('private', 'shared')), 'named by file name pattern')
})

test_that('only the folder of the cleaned locations must be restricted', {
  calls <- list()
  local_mocked_bindings(
    read_config = function(project) list(locations = list(premade_folder = list(clean_TEST.zip = 'private', '*' = 'shared'))),
    upload_to_drive = function(files, folder, public_msg) {
      calls[[folder]] <<- list(files = basename(files), checked = !is.null(public_msg))
      data.table::data.table(name = basename(files))
    })
  upload_premade('TEST', zips = file.path('somewhere', files))
  expect_equal(calls$private, list(files = 'clean_TEST.zip', checked = TRUE))
  expect_equal(calls$shared, list(files = files[2:3], checked = FALSE))
})

test_that('a folder open to anyone with the link is refused only when checked', {
  local_mocked_bindings(
    drive_get = function(id) id,
    drive_reveal = function(x, what) list(permissions_resource = list(list(permissions = list(list(type = 'anyone'))))),
    drive_ls = function(x) data.frame(name = character(), id = character()),
    drive_upload = function(media, path) data.frame(id = 'new'),
    drive_link = function(f) 'link',
    .package = 'googledrive')
  expect_error(upload_to_drive('a.zip', 'open', 'must be restricted'), 'must be restricted')
  expect_equal(upload_to_drive('a.zip', 'open', NULL)$action, 'uploaded')
})
