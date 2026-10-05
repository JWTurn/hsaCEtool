test_that('sync_folder copies only new and changed files, meta last', {
  from <- withr::local_tempdir()
  to <- withr::local_tempdir()
  dir.create(file.path(from, 'objects'))
  dir.create(file.path(from, 'meta'))
  writeLines('a', file.path(from, 'objects', 'a'))
  writeLines('m', file.path(from, 'meta', 'meta'))
  writeLines('b', file.path(from, 'objects', 'b'))

  dry <- sync_folder(from, to, dry_run = TRUE)
  expect_equal(nrow(dry), 3)
  expect_equal(dry$file[3], 'meta/meta')
  expect_length(list.files(to, recursive = TRUE), 0)

  expect_equal(nrow(sync_folder(from, to)), 3)
  expect_equal(nrow(sync_folder(from, to)), 0)       # nothing changed

  writeLines('a changed', file.path(from, 'objects', 'a'))
  expect_equal(sync_folder(from, to)$file, 'objects/a')
  expect_equal(readLines(file.path(to, 'objects', 'a')), 'a changed')

  unlink(file.path(from, 'objects', 'b'))
  expect_equal(nrow(sync_folder(from, to)), 0)       # not deleted without prune
  expect_equal(sync_folder(from, to, prune = TRUE)$action, 'delete')
  expect_false(file.exists(file.path(to, 'objects', 'b')))
})

test_that('sync_folder skips a missing source folder', {
  expect_equal(nrow(sync_folder(file.path(tempdir(), 'nope'), tempdir())), 0)
})
