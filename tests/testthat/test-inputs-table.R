# Inputs named once in a table (CSV) and referred to by name in the YAML.
# Made-up inputs only; nothing is downloaded.

write_inputs <- function(dir, lines) writeLines(lines, file.path(dir, 'inputs_TEST.csv'))

# a project's settings, with a made-up location source so they pass check_config()
write_settings <- function(dir, x) {
  x$locations$sources <- x$locations$sources %||% list(list(type = 'movebank', name = 'HerdA', study = 1L))
  yaml::write_yaml(x, file.path(dir, 'TEST.yaml'))
}

made_up_inputs <- c(
  'name,title,theme,url,path,target_file,archive,access,provider,terms,notes',
  'roads,Made-up roads,disturbance,https://example.org/roads.zip,,roads.shp,,public,Made-up agency,Open licence,a note',
  'records,Made-up records,caribou,https://example.org/rec/view,,rec.xlsx,rec.zip,restricted,Made-up ministry,No sharing,',
  'old_type,Made-up type map,harvest,https://example.org/type.tif,,,,public,,,',
  'nbac,My own fires,fire,,fires/my_fires.shp,,,restricted,,,'
)

test_that('input: <name> gives the same settings as the url typed in the YAML', {
  dir <- withr::local_tempdir()
  write_inputs(dir, made_up_inputs)
  write_settings(dir, list(
    inputs = 'inputs_TEST.csv',
    locations = list(captures = list(source = 'BC', input = 'records', fun = 'readxl::read_excel')),
    landscape = list(
      harvest = list(canlad_old = list(type_input = 'old_type', year_url = 'https://example.org/year.tif')),
      disturbance = list(sources = list(roads = list(input = 'roads', files = list(list(file = 'a.shp'))))))
  ))
  cfg <- read_config('TEST', config_dir = dir)
  expect_identical(cfg$locations$captures,
                   list(source = 'BC', url = 'https://example.org/rec/view', target_file = 'rec.xlsx',
                        archive = 'rec.zip', fun = 'readxl::read_excel'))
  expect_identical(cfg$landscape$harvest$canlad_old[c('type_url', 'year_url')],
                   list(type_url = 'https://example.org/type.tif', year_url = 'https://example.org/year.tif'))
  expect_identical(cfg$landscape$disturbance$sources$roads,
                   list(url = 'https://example.org/roads.zip', target_file = 'roads.shp',
                        files = list(list(file = 'a.shp'))))
  # the project's row replaces the package's of the same name
  expect_identical(cfg$landscape$fire$nbac, list(path = 'fires/my_fires.shp'))
  # provider, terms, access and notes stay in the table, which records where each input is used
  tab <- cfg$inputs_table
  expect_equal(tab[name == 'roads', notes], 'a note')
  expect_equal(tab[name == 'records', used_in], 'locations/captures')
  expect_equal(tab[name == 'old_type', used_in], 'landscape/harvest/canlad_old/type_url')
  expect_equal(tab[name == 'nbac', provider], NA_character_)
  expect_true(all(c('canlad_annual', 'scanfi_landcover') %in% tab$name))
})

test_that('the package table names the national inputs, giving the urls the defaults always had', {
  dir <- withr::local_tempdir()
  write_settings(dir, list(project = list(seed = 1L)))
  cfg <- read_config('TEST', config_dir = dir)
  expect_identical(cfg$landscape$fire$nfdb,
                   list(url = 'https://cwfis.cfs.nrcan.gc.ca/downloads/nfdb/fire_poly/current_version/NFDB_poly_pre1972.zip',
                        target_file = 'NFDB_poly_pre1972_20250630.shp'))
  expect_match(cfg$landscape$harvest$canlad_old$type_url, 'disturbanceType.tif$')
  expect_true(all(cfg$inputs_table$access == 'public'))
  expect_false(any(is.na(cfg$inputs_table$used_in)))
})

test_that('a url typed beside input: wins', {
  dir <- withr::local_tempdir()
  write_settings(dir, list(landscape = list(fire = list(nbac = list(url = 'https://example.org/nbac.zip',
                                                                  target_file = 'n.shp')))))
  cfg <- read_config('TEST', config_dir = dir)
  expect_identical(cfg$landscape$fire$nbac, list(url = 'https://example.org/nbac.zip', target_file = 'n.shp'))
  expect_true(is.na(cfg$inputs_table[name == 'nbac', used_in]))
})

test_that('an input not in the table is an error', {
  dir <- withr::local_tempdir()
  write_settings(dir, list(tracks = list(within = list(input = 'nowhere'))))
  expect_error(read_config('TEST', config_dir = dir), 'nowhere \\(tracks/within\\)')
})

test_that('an inputs table with problems is reported row by row', {
  dir <- withr::local_tempdir()
  write_inputs(dir, c('name,url,path,access',
                      ',https://example.org/a,,public',
                      'b,,,public',
                      'c,https://example.org/c,c.shp,public',
                      'd,https://example.org/d,,maybe',
                      'd,https://example.org/d,,public'))
  err <- expect_error(read_inputs_table(file.path(dir, 'inputs_TEST.csv')))
  expect_match(err$message, 'no name: rows 2', fixed = TRUE)
  expect_match(err$message, 'give a url or a path: rows 3', fixed = TRUE)
  expect_match(err$message, 'not both: rows 4', fixed = TRUE)
  expect_match(err$message, 'public or restricted: rows 5', fixed = TRUE)
  expect_match(err$message, 'listed twice: rows 6', fixed = TRUE)
  write_inputs(dir, c('name,url,access,licence', 'a,https://example.org/a,public,x'))
  expect_error(read_inputs_table(file.path(dir, 'inputs_TEST.csv')), 'unknown columns: licence')
})
