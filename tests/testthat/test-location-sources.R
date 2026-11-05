# Location sources given as a table (CSV) rather than typed in the YAML.
# Made-up sources only; nothing is downloaded.

write_sources <- function(dir, lines) writeLines(lines, file.path(dir, 'sources_TEST.csv'))

made_up_csv <- c(
  'name,type,movebank_study,part,url,target_file,id_col,datetime_col,hour_col,date_only,id_from_deployment,study,study_column,herd,herd_column,juris,notes',
  'HerdA,movebank,2460570864,,,,,,,,TRUE,,,,,yt,a note',
  'HerdB,movebank,123,,,,,,,,,JoinedStudy,,,,yt,',
  'Files,file,,,https://example.org/a.zip,a.shp,ID,Date,Hour,,,JoinedStudy,,,Herd,yt,',
  'Prov,bc,,new,https://example.org/new.zip,new.shp,,,,,,,herd,,herd,bc,',
  'Prov,bc,,old,https://example.org/old.zip,old.csv,,,,,,,herd,,herd,bc,'
)

same_as_yaml <- list(
  list(type = 'movebank', name = 'HerdA', study = 2460570864, id_from_deployment = TRUE),
  list(type = 'movebank', name = 'HerdB', study = 123L),
  list(type = 'file', name = 'Files', url = 'https://example.org/a.zip', target_file = 'a.shp',
       columns = list(id = 'ID', datetime = 'Date', hour = 'Hour')),
  list(type = 'bc', name = 'Prov', new = list(url = 'https://example.org/new.zip', target_file = 'new.shp'),
       old = list(url = 'https://example.org/old.zip', target_file = 'old.csv'))
)

test_that('a sources table gives the same settings as the sources typed in the YAML', {
  dir <- withr::local_tempdir()
  write_sources(dir, made_up_csv)
  yaml::write_yaml(list(locations = list(sources = 'sources_TEST.csv',
                                         herd = list(columns = list('group_id'), by_source = list(Other = 'X')))),
                   file.path(dir, 'TEST.yaml'))
  cfg <- read_config('TEST', config_dir = dir)
  expect_identical(cfg$locations$sources, same_as_yaml)
  expect_equal(cfg$locations$study$by_source, list(HerdB = 'JoinedStudy', Files = 'JoinedStudy'))
  expect_equal(cfg$locations$juris$by_source, list(HerdA = 'yt', HerdB = 'yt', Files = 'yt', Prov = 'bc'))
  # by_source in the YAML is kept; columns from the table come first, limited to their sources
  expect_equal(cfg$locations$herd$by_source, list(Other = 'X'))
  expect_equal(cfg$locations$herd$columns,
               list(list(column = 'Herd', sources = 'Files'), list(column = 'herd', sources = 'Prov'), 'group_id'))
  expect_equal(cfg$locations$study$columns[[1]], list(column = 'herd', sources = 'Prov'))
  # notes stay in the table, out of the source settings (so editing one reruns nothing)
  expect_equal(cfg$location_sources$notes[1], 'a note')
  expect_false(any(purrr::map_lgl(cfg$locations$sources, function(s) 'notes' %in% names(s))))
})

test_that('a sources table with problems is reported row by row', {
  dir <- withr::local_tempdir()
  write_sources(dir, c('name,type,movebank_study,part,date_only,juris',
                       ',file,,,,yt',
                       'A,movebank,,,,yt',
                       'B,file,,new,maybe,yt',
                       'C,bc,,new,,yt',
                       'C,bc,,new,,bc'))
  err <- tryCatch(read_location_sources(file.path(dir, 'sources_TEST.csv')), error = conditionMessage)
  expect_match(err, 'no name: rows 2')
  expect_match(err, 'needs movebank_study: rows 3')
  expect_match(err, 'part is only for type bc: rows 4')
  expect_match(err, 'date_only must be TRUE or FALSE: rows 4')
  expect_match(err, 'bc part listed twice: rows 6')
  expect_match(err, 'rows of bc source C differ in: juris')
  write_sources(dir, c('name,typo', 'A,file'))
  expect_error(read_location_sources(file.path(dir, 'sources_TEST.csv')), 'unknown columns: typo')
})
