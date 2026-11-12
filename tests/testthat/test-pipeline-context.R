# Which settings and store the write and upload helpers use (pipeline_context()),
# in made-up repositories: one with a single targets project and its settings in
# config/, and one laid out like YT_caribou (several projects, each with its own
# config/ beside its script, sharing one settings name).

# a repository in a temporary folder: _targets.yaml lines, and settings folders
# each holding copies of the test settings (fixtures/config/TEST.yaml) named
# as given
make_repo <- function(targets_yaml, settings, env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  writeLines(targets_yaml, file.path(dir, '_targets.yaml'))
  fx <- testthat::test_path('fixtures', 'config')
  for (cd in names(settings)) {
    reproducible::checkPath(file.path(dir, cd), create = TRUE)
    file.copy(file.path(fx, 'layers_TEST.csv'), file.path(dir, cd))
    for (nm in settings[[cd]]) file.copy(file.path(fx, 'TEST.yaml'), file.path(dir, cd, paste0(nm, '.yaml')))
  }
  dir
}

test_that('one targets project, settings in config/', {
  dir <- make_repo(c('main:', '  store: _targets/main', 'TEST:', '  store: _targets/TEST'),
                   list(config = 'TEST'))
  withr::local_dir(dir)
  withr::local_envvar(TAR_PROJECT = 'TEST')
  ctx <- pipeline_context()
  expect_equal(ctx[c('project', 'config_dir', 'tar_project', 'store')],
               list(project = 'TEST', config_dir = 'config', tar_project = 'TEST', store = '_targets/TEST'))
  expect_equal(ctx$cfg$project$name, 'TEST')
  # named, with TAR_PROJECT unset or set to another project: the named one
  withr::local_envvar(TAR_PROJECT = NA)
  expect_equal(pipeline_context('TEST')$store, '_targets/TEST')
  withr::local_envvar(TAR_PROJECT = 'main')
  expect_equal(pipeline_context('TEST')$store, '_targets/TEST')
  expect_equal(Sys.getenv('TAR_PROJECT'), 'main')   # not changed
})

test_that('several targets projects, each with its settings beside its script (as in YT_caribou)', {
  dir <- make_repo(c('phase-a:', '  script: pipelines/phase-a/_targets.R', '  store: _targets/phase-a',
                     'phase-b:', '  script: pipelines/phase-b/_targets.R', '  store: _targets/phase-b'),
                   list('pipelines/phase-a/config' = 'TEST', 'pipelines/phase-b/config' = 'TEST'))
  withr::local_dir(dir)
  withr::local_envvar(TAR_PROJECT = 'phase-b')
  want <- list(project = 'TEST', config_dir = 'pipelines/phase-b/config', tar_project = 'phase-b',
               store = '_targets/phase-b')
  # from TAR_PROJECT, by settings name, or by targets project
  expect_equal(pipeline_context()[names(want)], want)
  expect_equal(pipeline_context('TEST')[names(want)], want)
  withr::local_envvar(TAR_PROJECT = NA)
  expect_equal(pipeline_context('phase-b')[names(want)], want)
  # the other phase
  withr::local_envvar(TAR_PROJECT = 'phase-a')
  expect_equal(pipeline_context('TEST')$config_dir, 'pipelines/phase-a/config')
  expect_equal(pipeline_context('TEST')$store, '_targets/phase-a')
  # settings folder given
  expect_equal(pipeline_context('TEST', config_dir = 'pipelines/phase-b/config')$config_dir,
               'pipelines/phase-b/config')
})

test_that('unclear or missing settings are errors', {
  dir <- make_repo(c('main:', '  store: _targets/main', 'other:', '  store: _targets/other'),
                   list(config = c('TEST', 'TEST2')))
  withr::local_dir(dir)
  withr::local_envvar(TAR_PROJECT = NA)
  expect_error(pipeline_context(), 'No project chosen')
  withr::local_envvar(TAR_PROJECT = 'other')
  expect_error(pipeline_context(), 'Which settings\\? .*TEST, TEST2')
  expect_equal(pipeline_context('TEST2')$project, 'TEST2')
  expect_error(pipeline_context('NOPE'), 'No settings file for project "NOPE"')
})

test_that('a disturbance source without lookup tables gets an empty list of them', {
  used <- NULL
  call <- as.call(c(quote(list), stats::setNames(purrr::map(paste0('disturb_lookup_', used, recycle0 = TRUE),
                                                            as.symbol), used)))
  expect_identical(call, quote(list()))
})

test_that('TAR_PROJECT is set only while checking the store, then left as it was', {
  withr::local_envvar(TAR_PROJECT = NA)
  expect_equal(with_tar_project('NMC', Sys.getenv('TAR_PROJECT')), 'NMC')
  expect_equal(Sys.getenv('TAR_PROJECT', unset = 'none'), 'none')
  Sys.setenv(TAR_PROJECT = 'other')
  expect_error(with_tar_project('NMC', stop('failed')), 'failed')
  expect_equal(Sys.getenv('TAR_PROJECT'), 'other')
})
