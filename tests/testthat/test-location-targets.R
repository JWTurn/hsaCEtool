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

test_that('herd keep and study area add change the targets only when set', {
  tl <- source_targets(made_up)
  expect_no_match(cmd_text(tl$locations_raw), 'keep_herds')
  expect_false(any(c('herd_selection', 'study_area_add_file') %in% names(tl)))
  expect_no_match(cmd_text(tl$study_area), 'add')
  tl <- source_targets(made_up, herd = list(keep = list('HerdA')),
                       study_area = list(add = list(path = 'ranges.gpkg')))
  expect_match(cmd_text(tl$locations_raw), 'keep_herds(combine_locations(', fixed = TRUE)
  expect_true('herd_selection' %in% names(tl))
  expect_match(cmd_text(tl$study_area_add_file), 'ranges.gpkg', fixed = TRUE)
  expect_match(cmd_text(tl$study_area), 'add = study_area_add_file', fixed = TRUE)
  # or the file another target makes (e.g. herd ranges built in the project's pipeline)
  tl <- source_targets(made_up, study_area = list(add = list(target = 'herd_ranges_file')))
  expect_match(cmd_text(tl$study_area_add_file), "herd_ranges_file", fixed = TRUE)
  expect_no_match(cmd_text(tl$study_area_add_file), "get_source", fixed = TRUE)
  expect_equal(tl$study_area_add_file$settings$format, 'file')
  # premade locations are filtered too
  dir <- withr::local_tempdir()
  yaml::write_yaml(list(locations = list(premade = list(path = 'clean.rds'), herd = list(keep = list('HerdA')))),
                   file.path(dir, 'PRE.yaml'))
  tl <- targets_locations(read_config('PRE', config_dir = dir))
  tl <- stats::setNames(tl, purrr::map_chr(tl, function(t) t$settings$name))
  expect_match(cmd_text(tl$locations_clean), 'keep_herds(read_premade_locations(', fixed = TRUE)
})
