# Made-up steps and extracted values only (no real locations)

# steps: n_strata per animal, each a used step and n_rand random steps, years 2020-2021
toy_model_steps <- function(ids = c('a', 'b', 'c'), n_strata = 40, n_rand = 5) {
  set.seed(5)
  st <- CJ(id = ids, k = seq_len(n_strata))
  st[, indiv_step_id := paste(id, k, sep = '_')]
  st[, t1_ := as.POSIXct('2020-01-01', tz = 'UTC') + (k - 1) * 20 * 86400]
  d <- st[, .(case_ = c(TRUE, rep(FALSE, n_rand))), by = .(id, indiv_step_id, t1_)]
  d[, `:=`(year = as.integer(format(t1_, '%Y')), sl_ = rgamma(.N, 2, 0.002), ta_ = runif(.N, -pi, pi),
           x1_ = 5e5, y1_ = 6.7e6, juris = ifelse(id == 'c', 'bc', 'yt'))]
  d[]
}

# extracted values for those steps, written as two Parquet files (by year) as extraction does
toy_extracted <- function(steps, dir) {
  set.seed(6)
  n <- nrow(steps)
  v <- data.table(row = seq_len(n),
                  prop_a_240_end = runif(n, 0, 0.5), prop_b_240_end = runif(n, 0, 0.5),
                  dist_x_240_start = rep(runif(n / 6, 0, 40000), each = 6),
                  dist_x_240_end = runif(n, 0, 40000), harv_dist_240_end = runif(n, 0, 40000),
                  harv_dist_240_start = rep(runif(n / 6, 0, 40000), each = 6))
  v[, dist_x_240_end := dist_x_240_end + 0]
  dir.create(dir, showWarnings = FALSE)
  purrr::map_chr(split(v, steps$year), function(x) {
    f <- file.path(dir, paste0('extracted_', steps$year[x$row[1]], '.parquet'))
    nanoparquet::write_parquet(x, f)
    f
  })
}

common <- list(seasons = list(winter = list('11-01', '04-15'), summer = list('04-16', '10-31')),
               groups = list(prop_ab = list(sum = list('prop_a', 'prop_b')),
                             dist_x = list(min = list('dist_x', 'harv_dist'))),
               distance_cap = list(columns = '^dist_', beyond_km = 20, set_to_km = 21))
form <- 'case_ ~ -1 + prop_ab_240_end + I(log((dist_x_240_end + 1)/1000)) + (1 | indiv_step_id) + (0 + prop_ab_240_end | id)'

test_that('seasons follow the start date and can cross the new year', {
  t <- as.POSIXct(c('2020-12-31', '2021-04-15', '2021-04-16', '2021-10-31', '2021-11-01'), tz = 'UTC')
  expect_equal(season_of(t, common$seasons), c('winter', 'winter', 'summer', 'summer', 'winter'))
  expect_equal(season_of(t, list(summer = list('07-01', '09-30'))), rep('other', 5))
})

test_that('model settings are checked', {
  ok <- list(seasons = common$seasons, groups = common$groups, distance_cap = common$distance_cap,
             models = list(full = list(formula = form, n_random = 5)))
  expect_length(check_model_settings(ok, 30), 0)
  bad <- ok
  bad$models <- list(`1st model` = list(formula = 'case_ ~ x + (0 + x | id)', n_random = 50, seasons = list('spring')),
                     m2 = list(formula = 'case_ ~ x1_ + (1 | indiv_step_id)', n_random = 5))
  p <- check_model_settings(bad, 30)
  expect_true(any(grepl('not a usable name', p)))
  expect_true(any(grepl('needs \\(1 \\| indiv_step_id\\)', p)))
  expect_true(any(grepl('n_random must be', p)))
  expect_true(any(grepl('seasons must be', p)))
  expect_true(any(grepl('positions and times', p)))
})

test_that('a model formula keeps no environment of its own', {
  f <- model_formula(form)
  expect_identical(environment(f), baseenv())
  expect_error(model_formula('prop_ab_240_end + 1'), 'response')
})

test_that('the model table: groups, distance cap by step start, random steps, codes', {
  steps <- toy_model_steps()
  files <- toy_extracted(steps, file.path(tempdir(), 'toy_extracted'))
  codes <- make_animal_codes(steps$id)
  m <- list(formula = form, n_random = 3)
  r <- build_model_data(steps, files, codes, common, m)
  d <- r$data
  # used step + first 3 random steps of each
  expect_equal(d[, .N, by = indiv_step_id][, unique(N)], 4L)
  expect_equal(nrow(d), 3 * 40 * 4)
  # only formula columns, coded IDs, no positions or times
  expect_setequal(names(d), c('case_', 'indiv_step_id', 'id', 'prop_ab_240_end', 'dist_x_240_end'))
  expect_setequal(unique(d$id), codes$code)
  # groups: sum, and min (harvest folded in)
  v <- data.table::rbindlist(purrr::map(files, nanoparquet::read_parquet))
  keep_rows <- steps[, .(row = .I, k = cumsum(!case_)), by = indiv_step_id][k <= 3, row]
  v <- v[row %in% keep_rows][order(row)]
  expect_equal(d$prop_ab_240_end, v$prop_a_240_end + v$prop_b_240_end)
  # cap: a step starting > 20 km from x (nearer of dist_x and harv_dist at the start) gets 21 km
  # for every row; a step starting nearer keeps its real distances, even beyond 20 km
  start <- pmin(v$dist_x_240_start, v$harv_dist_240_start)
  far <- start > 20000
  expect_true(all(d$dist_x_240_end[far] == 21000))
  expect_equal(d$dist_x_240_end[!far], pmin(v$dist_x_240_end, v$harv_dist_240_end)[!far])
  expect_true(any(d$dist_x_240_end[!far] > 20000))
  expect_equal(r$log[grepl('set to 21 km', what), n], data.table::uniqueN(d$indiv_step_id[far]))
})

test_that('subsets, seasons and missing values in the model table', {
  steps <- toy_model_steps()
  files <- toy_extracted(steps, file.path(tempdir(), 'toy_extracted2'))
  codes <- make_animal_codes(steps$id)
  m <- list(formula = sub('-1 +', '-1 + yearseason +', form, fixed = TRUE), n_random = 5,
            subset = "juris == 'yt'", seasons = list('winter'))
  r <- build_model_data(steps, files, codes, common, m)
  expect_setequal(unique(r$data$id), codes[id %in% c('a', 'b'), code])
  expect_true(all(grepl('_winter$', r$data$yearseason)))
  # a missing value at a used step drops the whole step; at a random step, only that row
  v <- nanoparquet::read_parquet(files[1])
  v$prop_a_240_end[c(1, 8)] <- NA        # row 1 = used step of a_1; row 8 = random step of a_2
  v$prop_b_240_end[c(1, 8)] <- NA        # (a group is missing only where all its layers are)
  nanoparquet::write_parquet(v, files[1])
  expect_warning(r <- build_model_data(steps, files, codes, common, list(formula = form, n_random = 5)),
                 'prop_ab_240_end \\(1 used, 1 random\\)')
  expect_equal(r$log[grepl('used step', what), n], 1L)
  expect_equal(r$log[grepl('^random steps dropped', what), n], 1L)
  expect_false('a_1' %in% r$data$indiv_step_id)
  expect_equal(r$data[indiv_step_id == 'a_2', .N], 5L)
})

test_that('text and grouping columns become factors only at the end, with the levels present', {
  d <- data.table(case_ = c(TRUE, FALSE), indiv_step_id = c('s1', 's1'), id = c('A0002', 'A0002'),
                  herd = factor(c('Hess', 'Hess'), levels = c('Hess', 'Klaza')), x = 1:2)
  f <- model_formula('case_ ~ x + herd + (1 | indiv_step_id) + (0 + x | id)')
  out <- as_model_factors(d, f)
  expect_true(is.factor(out$indiv_step_id) && is.factor(out$id))
  expect_equal(levels(out$herd), 'Hess')   # the empty level is gone
  expect_identical(out$case_, c(1L, 0L))
  expect_true(is.character(d$id))         # the table itself is unchanged
})

test_that('model tables refuse positions, times and real IDs', {
  codes <- make_animal_codes(c('a', 'b'))
  expect_error(check_model_data(data.table(id = 'A0001', x1_ = 1), codes), 'positions or times')
  expect_error(check_model_data(data.table(id = 'A0001', when = Sys.time()), codes), 'positions or times')
  expect_error(check_model_data(data.table(id = 'a'), codes), 'animal IDs')
})

test_that('the iSSA fit fixes the step intercept SD and gives coefficient tables', {
  steps <- toy_model_steps(n_strata = 30)
  files <- toy_extracted(steps, file.path(tempdir(), 'toy_extracted3'))
  codes <- make_animal_codes(steps$id)
  r <- build_model_data(steps, files, codes, common, list(formula = form, n_random = 5))
  fit <- suppressWarnings(fit_issa(r$data, form))
  sds <- attr(glmmTMB::VarCorr(fit)$cond$indiv_step_id, 'stddev')
  expect_equal(unname(sds), 1000)
  cf <- suppressWarnings(model_coefficients(fit))
  expect_setequal(cf$fixed$term, c('prop_ab_240_end', 'I(log((dist_x_240_end + 1)/1000))'))
  expect_equal(cf$by_group[, unique(group)], 'id')
  expect_setequal(cf$by_group$level, codes$code)
  files_out <- write_model_coefficients(cf, r$log, 'toy', file.path(tempdir(), 'toy_models'))
  expect_true(all(file.exists(files_out)))
})
