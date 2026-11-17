# Location source targets: names, what a download depends on, and freezing.
# Made-up sources only; nothing is downloaded (the target list is only built).

source_targets <- function(sources, ...) {
  dir <- withr::local_tempdir()
  yaml::write_yaml(
    list(locations = c(list(sources = sources), list(...))),
    file.path(dir, 'TEST.yaml')
  )
  tl <- targets_locations(read_config('TEST', config_dir = dir))
  stats::setNames(tl, purrr::map_chr(tl, function(t) t$settings$name))
}
cmd_text <- function(t) paste(deparse(t$command$expr), collapse = ' ')

made_up <- list(
  list(
    type = 'file',
    name = 'Herd A',
    url = 'https://example.org/a.zip',
    target_file = 'a.shp',
    columns = list(id = 'ID', datetime = 'Date')
  ),
  list(type = 'movebank', name = 'Klaza', study = 123, user = 'someone'),
  list(type = 'file', path = 'x.rds')
)

test_that('source targets are named after the source, or its position without a name', {
  tl <- source_targets(made_up)
  expect_true(all(
    c(
      'locations_source_Herd_A_file',
      'locations_source_Herd_A',
      'locations_source_Klaza_download',
      'locations_source_Klaza',
      'locations_source_3_file',
      'locations_source_3'
    ) %in%
      names(tl)
  ))
  # reordering does not rename a named source
  tl2 <- source_targets(made_up[c(2, 1, 3)])
  expect_true(all(
    c('locations_source_Herd_A_file', 'locations_source_Klaza_download') %in% names(tl2)
  ))
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
  yaml::write_yaml(
    list(
      locations = list(
        sources = list(
          list(type = 'file', name = 'A b', path = 'x.rds'),
          list(type = 'file', name = 'A_b', path = 'y.rds')
        )
      )
    ),
    file.path(dir, 'DUP.yaml')
  )
  expect_error(read_config('DUP', config_dir = dir), 'same name.*A_b')
})

test_that('herd keep and study area add change the targets only when set', {
  tl <- source_targets(made_up)
  expect_no_match(cmd_text(tl$locations_raw), 'keep_herds')
  expect_false(any(c('herd_selection', 'study_area_add_file') %in% names(tl)))
  expect_no_match(cmd_text(tl$study_area), 'add')
  tl <- source_targets(
    made_up,
    herd = list(keep = list('HerdA')),
    study_area = list(add = list(path = 'ranges.gpkg'))
  )
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
  yaml::write_yaml(
    list(locations = list(premade = list(path = 'clean.rds'), herd = list(keep = list('HerdA')))),
    file.path(dir, 'PRE.yaml')
  )
  tl <- targets_locations(read_config('PRE', config_dir = dir))
  tl <- stats::setNames(tl, purrr::map_chr(tl, function(t) t$settings$name))
  expect_match(cmd_text(tl$locations_clean), 'keep_herds(read_premade_locations(', fixed = TRUE)
})

test_that('premade_write and premade_upload add the hand-off targets', {
  tl <- source_targets(made_up)
  expect_false(any(c('premade_files', 'premade_upload') %in% names(tl)))
  tl <- source_targets(
    made_up,
    premade_write = TRUE,
    premade_upload = TRUE,
    premade_folder = list(clean_TEST.zip = 'private', '*' = 'shared')
  )
  expect_equal(tl$premade_files$settings$format, 'file')
  expect_match(cmd_text(tl$premade_files), 'write_premade_files(locations_clean', fixed = TRUE)
  expect_match(cmd_text(tl$premade_upload), 'upload_premade_files(premade_files', fixed = TRUE)
  expect_match(cmd_text(tl$premade_upload), 'private', fixed = TRUE)
  # upload needs the write and a folder
  expect_error(source_targets(made_up, premade_upload = TRUE), 'premade_write')
  expect_error(
    source_targets(made_up, premade_write = TRUE, premade_upload = TRUE),
    'premade_folder'
  )
})

test_that('premade_check checks Drive links every run, and only those', {
  pre_targets <- function(...) {
    dir <- withr::local_tempdir()
    yaml::write_yaml(list(locations = list(...)), file.path(dir, 'PRE.yaml'))
    tl <- targets_locations(read_config('PRE', config_dir = dir))
    stats::setNames(tl, purrr::map_chr(tl, function(t) t$settings$name))
  }
  drive <- list(url = 'https://drive.google.com/file/d/xyz/view', target_file = 'clean.rds')
  area <- list(url = 'https://drive.google.com/file/d/abc/view', target_file = 'a.gpkg')
  tl <- pre_targets(premade = drive, premade_check = TRUE, study_area = list(source = area))
  expect_equal(tl$locations_premade_file_drive$cue$mode, 'always')
  expect_match(cmd_text(tl$locations_premade_file_drive), 'drive_file_version(', fixed = TRUE)
  expect_match(cmd_text(tl$locations_premade_file), 'locations_premade_file_drive', fixed = TRUE)
  expect_match(cmd_text(tl$locations_premade_file), 'refresh = TRUE', fixed = TRUE)
  expect_true('study_area_file_drive' %in% names(tl))
  # off by default, and never for a local path
  tl <- pre_targets(premade = drive)
  expect_false(any(grepl('_drive$', names(tl))))
  tl <- pre_targets(premade = list(path = 'clean.rds'), premade_check = TRUE)
  expect_false(any(grepl('_drive$', names(tl))))
  # a frozen extended study area is not checked
  tl <- pre_targets(
    premade = drive,
    premade_check = TRUE,
    study_area = list(source = area, extended = list(source = area, freeze = TRUE))
  )
  expect_false('study_area_extended_file_drive' %in% names(tl))
  # writing is for the phase that cleans the locations
  expect_error(pre_targets(premade = drive, premade_write = TRUE), 'premade_write')
})
