# Location source targets: names, what a download depends on, and freezing.
# Made-up sources only; nothing is downloaded (the target list is only built).

source_targets <- function(sources, ...) {
  dir <- withr::local_tempdir()
  yaml::write_yaml(list(locations = c(list(sources = sources), list(...))), file.path(dir, 'TEST.yaml'))
  tl <- targets_locations(read_config('TEST', config_dir = dir))
  stats::setNames(tl, purrr::map_chr(tl, function(t) t$settings$name))
}
cmd_text <- function(t) paste(deparse(t$command$expr), collapse = ' ')

made_up <- list(
  list(type = 'file', name = 'Herd A', url = 'https://example.org/a.zip', target_file = 'a.shp',
       columns = list(id = 'ID', datetime = 'Date')),
  list(type = 'movebank', name = 'Klaza', study = 123, user = 'someone'),
  list(type = 'file', path = 'x.rds')
)

test_that('source targets are named after the source, or its position without a name', {
  tl <- source_targets(made_up)
  expect_true(all(c('locations_source_Herd_A_file', 'locations_source_Herd_A',
                    'locations_source_Klaza_download', 'locations_source_Klaza',
                    'locations_source_3_file', 'locations_source_3') %in% names(tl)))
  # reordering does not rename a named source
  tl2 <- source_targets(made_up[c(2, 1, 3)])
  expect_true(all(c('locations_source_Herd_A_file', 'locations_source_Klaza_download') %in% names(tl2)))
})

test_that('a download depends only on the settings that say which file it is', {
  tl <- source_targets(made_up)
  dl <- cmd_text(tl$locations_source_Herd_A_file)
  expect_match(dl, 'example.org', fixed = TRUE)
  expect_no_match(dl, 'columns|Date')
  expect_match(cmd_text(tl$locations_source_Herd_A), 'columns', fixed = TRUE)
  mb <- cmd_text(tl$locations_source_Klaza_download)
  expect_match(mb, '123', fixed = TRUE)
  expect_no_match(mb, 'someone', fixed = TRUE)
})

test_that('freeze_downloads ignores settings and code changes; refresh still downloads', {
  tl <- source_targets(made_up)
  expect_true(tl$locations_source_Herd_A_file$cue$command)
  frozen <- made_up
  frozen[[3]]$refresh <- TRUE
  tl <- source_targets(frozen, freeze_downloads = TRUE)
  for (nm in c('locations_source_Herd_A_file', 'locations_source_Klaza_download')) {
    expect_false(tl[[nm]]$cue$command)
    expect_false(tl[[nm]]$cue$depend)
    expect_true(tl[[nm]]$cue$file)
  }
  expect_equal(tl$locations_source_3_file$cue$mode, 'always')
})

test_that('source names must be unique', {
  dir <- withr::local_tempdir()
  yaml::write_yaml(list(locations = list(sources = list(
    list(type = 'file', name = 'A b', path = 'x.rds'),
    list(type = 'file', name = 'A_b', path = 'y.rds')))), file.path(dir, 'DUP.yaml'))
  expect_error(read_config('DUP', config_dir = dir), 'same name.*A_b')
})
