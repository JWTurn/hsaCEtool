# === iSSA models: model tables, fits and coefficients -----------------------------------
# Comparable to the caribouiSSA SpaDES module and NMC_CEtool scripts/6-iSSA.R.
# Models are set in `model:` (config/<project>.yaml). Each model is named by its
# key under `model: models:` (e.g. full, full_winter, simple_yt); the name is
# chosen by the user, is not the project name, and is used in the model's target
# names (model_data_<name>, model_fit_<name>, model_coefs_<name>) and output files
# (outputs/<project>/models/<name>_*.csv).
#
# A model's table holds only what its formula uses, with animal IDs replaced by
# codes and no positions, dates or times, so the fitted model can be shared (see
# AGENTS.md). The table linking codes to animal IDs (animal_codes) is confidential.

# columns that place an animal in space or time; never allowed in a model table
model_private_columns <- c('x1_', 'y1_', 'x2_', 'y2_', 't1_', 't2_', 'x_', 'y_', 't_')

#' @title Read a model formula written as text
#' @description The formula is made in a bare environment, so a fitted model does
#'   not keep a copy of the data or other objects around it.
#' @param text character. A glmmTMB formula, e.g. "case_ ~ -1 + x + (1 | indiv_step_id)".
#' @return formula.
#' @author Julie W. Turner
model_formula <- function(text) {
  e <- tryCatch(str2lang(text), error = function(err) {
    stop('model formula could not be read: ', conditionMessage(err), call. = FALSE)
  })
  if (!is.call(e) || !identical(e[[1]], as.name('~')) || length(e) != 3) {
    stop('model formula must have a response, e.g. case_ ~ ...', call. = FALSE)
  }
  eval(e, envir = baseenv())
}

#' @title Random-effect terms of a formula
#' @description Each `(lhs | group)` term: the grouping variable(s) and whether
#'   the term is an intercept only (`1 | group`).
#' @param f formula.
#' @return data.table: `term` (text), `group` and `intercept_only`.
#' @keywords internal
#' @noRd
model_bars <- function(f) {
  bars <- list()
  walk <- function(e) {
    if (!is.call(e)) return(invisible())
    if (identical(e[[1]], as.name('|')) || identical(e[[1]], as.name('||'))) {
      bars[[length(bars) + 1]] <<- e
      return(invisible())
    }
    for (a in as.list(e)[-1]) walk(a)
  }
  walk(f[[length(f)]])
  data.table::data.table(
    term = purrr::map_chr(bars, function(b) paste(deparse(b), collapse = '')),
    group = purrr::map_chr(bars, function(b) paste(all.vars(b[[3]]), collapse = ':')),
    intercept_only = purrr::map_lgl(bars, function(b) identical(b[[2]], 1) || identical(b[[2]], 1L)))
}

#' @title Check the model settings
#' @description Called by [read_config()], so mistakes show before a run starts.
#' @param mcfg named list. The `model:` settings.
#' @param n_made integer. Random steps made per used step (`tracks: n_random`).
#' @return character. Problems found (empty if none).
#' @author Julie W. Turner
check_model_settings <- function(mcfg, n_made) {
  out <- character()
  add <- function(...) out <<- c(out, paste0('model: ', ...))
  seasons <- mcfg$seasons %||% list()
  for (nm in names(seasons)) {
    r <- unlist(seasons[[nm]])
    if (length(r) != 2 || !all(grepl('^[0-1][0-9]-[0-3][0-9]$', r))) {
      add('seasons: ', nm, ' must be two month-day dates, e.g. [\'11-01\', \'04-15\']')
    }
  }
  rules <- c('sum', 'min', 'max', 'mean')
  for (nm in names(mcfg$groups)) {
    g <- mcfg$groups[[nm]]
    if (!is.list(g) || length(g) != 1 || !names(g) %in% rules || length(unlist(g)) < 1) {
      add('groups: ', nm, ' must be {rule: [layers]}, with rule one of ', paste(rules, collapse = ', '))
    }
  }
  cap <- mcfg$distance_cap
  if (length(cap) && (!is.numeric(cap$beyond_km) || !is.numeric(cap$set_to_km))) {
    add('distance_cap needs numbers for beyond_km and set_to_km (or distance_cap: null)')
  }
  for (nm in names(mcfg$models)) {
    m <- mcfg$models[[nm]]
    if (!grepl('^[A-Za-z][A-Za-z0-9_]*$', nm)) {
      add('models: "', nm, '" is not a usable name: use letters, numbers and _ only, starting with a letter')
    }
    f <- tryCatch(model_formula(m$formula %||% ''), error = function(e) conditionMessage(e))
    if (is.character(f)) {
      add('models: ', nm, ': ', f)
    } else {
      bars <- model_bars(f)
      if (!any(bars$group == 'indiv_step_id' & bars$intercept_only)) {
        add('models: ', nm, ': the formula needs (1 | indiv_step_id), the intercept of each step')
      }
      bad <- intersect(all.vars(f), model_private_columns)
      if (length(bad)) {
        add('models: ', nm, ': positions and times cannot be in a model (', paste(bad, collapse = ', '), ')')
      }
    }
    n <- m$n_random
    if (!is.numeric(n) || length(n) != 1 || n < 1 || n > n_made) {
      add('models: ', nm, ': n_random must be a number from 1 to ', n_made, ' (tracks: n_random)')
    }
    s <- unlist(m$seasons %||% 'all')
    if (!identical(s, 'all') && length(setdiff(s, c(names(seasons), 'other')))) {
      add('models: ', nm, ': seasons must be all or names from model: seasons (',
          paste(c(names(seasons), 'other'), collapse = ', '), ')')
    }
    if (!is.null(m$fit) && !isTRUE(m$fit) && !isFALSE(m$fit)) add('models: ', nm, ': fit must be true or false')
    if (nzchar(m$subset %||% '')) {
      ok <- tryCatch({ str2lang(m$subset); TRUE }, error = function(e) FALSE)
      if (!ok) add('models: ', nm, ': subset could not be read')
    }
  }
  out
}

#' @title Season of each step
#' @description By the date the step starts, as month-day (UTC). A season may
#'   cross the new year (e.g. winter 11-01 to 04-15). The first season listed
#'   that holds a date wins; dates in no season are "other".
#' @param t POSIXct. Step start times.
#' @param seasons named list. Each a pair of month-day dates (start, end), inclusive.
#' @return character.
#' @author Julie W. Turner
season_of <- function(t, seasons) {
  md <- format(t, '%m-%d', tz = 'UTC')
  out <- rep(NA_character_, length(md))
  for (nm in names(seasons)) {
    r <- unlist(seasons[[nm]])
    inside <- if (r[1] <= r[2]) md >= r[1] & md <= r[2] else md >= r[1] | md <= r[2]
    out[is.na(out) & inside] <- nm
  }
  out[is.na(out)] <- 'other'
  out
}

#' @title Codes for animal IDs
#' @description One code per animal (A0001, A0002, ...), in ID order. Models use
#'   the codes, so they can be shared; the table linking codes to IDs cannot.
#' @param ids character. Animal IDs (e.g. `steps$id`).
#' @return data.table: `id` and `code`.
#' @author Julie W. Turner
make_animal_codes <- function(ids) {
  u <- sort(unique(as.character(ids)))
  data.table::data.table(id = u, code = sprintf(paste0('A%0', max(4, nchar(length(u))), 'd'), seq_along(u)))
}

#' @title Save the animal codes (confidential)
#' @param codes data.table from [make_animal_codes()].
#' @param out_dir character. Folder to write to (in outputs/, never shared publicly).
#' @return character. The file written.
#' @author Julie W. Turner
write_animal_codes <- function(codes, out_dir) {
  reproducible::checkPath(out_dir, create = TRUE)
  file <- file.path(out_dir, 'animal_codes_CONFIDENTIAL.csv')
  data.table::fwrite(codes, file)
  file
}

#' @title Steps used by one model
#' @description Keeps the steps in `subset` and the model's seasons, and each
#'   used step with its first `n_random` random steps. Adds `season` and
#'   `yearseason` (e.g. 2019_winter).
#' @param steps data.table from [prep_steps()].
#' @param model named list. One model's settings.
#' @param seasons named list. `model: seasons:`.
#' @param cols character. Step columns to keep.
#' @return data.table with `row` (row number in `steps`).
#' @author Julie W. Turner
select_model_steps <- function(steps, model, seasons, cols) {
  rows <- seq_len(nrow(steps))
  if (nzchar(model$subset %||% '')) {
    keep <- eval(str2lang(model$subset), steps, baseenv())
    if (!is.logical(keep) || length(keep) != nrow(steps)) {
      stop('model subset must give TRUE or FALSE for each step: ', model$subset, call. = FALSE)
    }
    rows <- which(keep)
  }
  s <- steps[rows, unique(c('case_', 'indiv_step_id', 'year', 't1_', cols)), with = FALSE]
  data.table::set(s, j = 'row', value = rows)
  s[, season := season_of(t1_, seasons)]
  s[, yearseason := paste(year, season, sep = '_')]
  wanted <- unlist(model$seasons %||% 'all')
  if (!identical(wanted, 'all')) s <- s[season %in% wanted]
  s[, .k := cumsum(!case_), by = indiv_step_id]
  s <- s[case_ | .k <= model$n_random]
  s[, .k := NULL]
  s <- s[indiv_step_id %in% s[case_ == TRUE, indiv_step_id]]
  s[, t1_ := NULL]
  s[]
}

#' @title Extracted columns each covariate is made from
#' @description A covariate is either an extracted column (e.g.
#'   `dist_paved_240_end`) or a group from `model: groups:` with a resolution and
#'   start or end (e.g. `prop_forest_240_end`, made from each of the group's
#'   layers with `_240_end`).
#' @param vars character. Covariate names.
#' @param groups named list. `model: groups:`.
#' @return named list: for each covariate, its extracted columns and `rule`
#'   (NULL for a plain column).
#' @keywords internal
#' @noRd
covariate_sources <- function(vars, groups) {
  stats::setNames(purrr::map(vars, function(v) {
    g <- names(groups)[startsWith(v, paste0(names(groups), '_'))]
    if (!length(g)) return(list(cols = v, rule = NULL))
    g <- g[which.max(nchar(g))]
    list(cols = paste0(unlist(groups[[g]]), substring(v, nchar(g) + 1)), rule = names(groups[[g]]))
  }), vars)
}

#' @title Combine columns by a rule, row by row
#' @param DT data.table.
#' @param cols character. Columns to combine.
#' @param rule character. sum, min, max or mean (missing values ignored; NA if all are).
#' @return numeric.
#' @keywords internal
#' @noRd
combine_columns <- function(DT, cols, rule) {
  x <- purrr::map(cols, function(cl) as.numeric(DT[[cl]]))
  n <- Reduce(`+`, purrr::map(x, function(v) !is.na(v)))
  out <- switch(rule,
                sum = Reduce(`+`, purrr::map(x, function(v) data.table::fcoalesce(v, 0))),
                mean = Reduce(`+`, purrr::map(x, function(v) data.table::fcoalesce(v, 0))) / pmax(n, 1),
                min = Reduce(function(a, b) pmin(a, b, na.rm = TRUE), x),
                max = Reduce(function(a, b) pmax(a, b, na.rm = TRUE), x))
  out[n == 0] <- NA_real_
  out
}

#' @title Read some columns of the extracted values, for some steps
#' @param files character. Parquet files from [write_extracted_year()].
#' @param cols character. Columns wanted.
#' @param rows integer. Row numbers in `steps` wanted.
#' @return data.table with `row` and `cols`.
#' @keywords internal
#' @noRd
read_extracted <- function(files, cols, rows) {
  data.table::rbindlist(purrr::map(files, function(f) {
    x <- data.table::as.data.table(nanoparquet::read_parquet(f, col_select = c('row', cols)))
    x[, row := as.integer(row)]
    x[row %in% rows]
  }))
}

#' @title Make one model's table
#' @description Builds the table a model is fitted to, with only what its
#'   formula uses:
#'   1. steps in the model's `subset` and `seasons`, each used step with its
#'      first `n_random` random steps ([select_model_steps()]);
#'   2. the covariates in the formula, read from the extracted values; groups
#'      from `model: groups:` are made here (e.g. `prop_forest_240_end`);
#'   3. distances far from a disturbance (`model: distance_cap:`): for each
#'      covariate matching `columns`, a step whose START is more than
#'      `beyond_km` from that disturbance (its `_start` value) gets `set_to_km`
#'      for the used step and all its random steps. The start is the same for
#'      all of them, so this does not depend on where the animal went;
#'   4. rows with missing values are dropped, with a warning naming the columns:
#'      the whole step where the used step has one, otherwise only the random step;
#'   5. animal IDs are replaced by codes, and the table is checked for
#'      positions and times.
#'   Text columns stay text here; [fit_issa()] makes them factors just before
#'   fitting.
#' @param steps data.table from [prep_steps()].
#' @param files character. Parquet files from [write_extracted_year()].
#' @param codes data.table from [make_animal_codes()].
#' @param common named list. `model:` settings `seasons`, `groups` and `distance_cap`.
#' @param model named list. This model's settings (formula, n_random, seasons, subset).
#' @return list: `data` (data.table) and `log` (data.table: what was kept,
#'   adjusted and dropped).
#' @author Julie W. Turner
build_model_data <- function(steps, files, codes, common, model) {
  f <- model_formula(model$formula)
  vars <- all.vars(f)
  step_vars <- intersect(vars, names(steps))
  derived <- c('season', 'yearseason')
  cov <- setdiff(vars, c(step_vars, derived))
  s <- select_model_steps(steps, model, common$seasons %||% list(), unique(c('id', step_vars)))
  log <- list(data.table::data.table(what = 'steps kept (used)', n = s[case_ == TRUE, .N]),
              data.table::data.table(what = 'rows kept (used + random)', n = nrow(s)))

  # covariates: extracted columns, groups made from them, and the start values the cap uses
  cap <- common$distance_cap
  capped <- if (length(cap)) cov[grepl(cap$columns %||% '^dist_', cov)] else character()
  starts <- sub('_end$', '_start', capped)
  made <- unique(c(cov, starts))
  src <- covariate_sources(made, common$groups %||% list())
  need <- unique(unlist(purrr::map(src, 'cols')))
  have <- nanoparquet::read_parquet_schema(files[1])$name
  miss <- setdiff(need, have)
  if (length(miss)) {
    stop('model formula uses columns that were not extracted: ', paste(miss, collapse = ', '),
         '. Use layer names with resolution and _start or _end (see outputs/<project>/availability.csv),',
         ' or groups from model: groups:', call. = FALSE)
  }
  s <- read_extracted(files, need, s$row)[s, on = 'row']
  grouped <- purrr::keep(src, function(x) !is.null(x$rule))
  values <- purrr::map(grouped, function(x) combine_columns(s, x$cols, x$rule))
  for (v in names(values)) data.table::set(s, j = v, value = values[[v]])
  rm(values)

  # distance cap, by where each step starts
  for (i in seq_along(capped)) {
    v <- capped[i]
    st <- s[[starts[i]]]
    far <- unique(s$indiv_step_id[s$case_ & !is.na(st) & st > cap$beyond_km * 1000])
    data.table::set(s, j = v, value = as.numeric(s[[v]]))
    data.table::set(s, i = which(s$indiv_step_id %in% far), j = v, value = cap$set_to_km * 1000)
    log[[length(log) + 1]] <- data.table::data.table(
      what = paste0(v, ': steps starting > ', cap$beyond_km, ' km away, set to ', cap$set_to_km, ' km'), n = length(far))
  }

  # missing values
  keep_cols <- unique(c('case_', 'indiv_step_id', 'id', setdiff(vars, 'id')))
  na_row <- !stats::complete.cases(s[, intersect(setdiff(keep_cols, 'id'), names(s)), with = FALSE])
  if (any(na_row)) {
    cols <- intersect(setdiff(keep_cols, 'id'), names(s))
    nas <- purrr::map(cols, function(cl) c(used = sum(is.na(s[[cl]]) & s$case_), random = sum(is.na(s[[cl]]) & !s$case_)))
    nas <- stats::setNames(nas, cols)[purrr::map_lgl(nas, function(n) sum(n) > 0)]
    warning('model table has missing values (dropped; see the data log): ',
            paste(sprintf('%s (%d used, %d random)', names(nas), purrr::map_int(nas, 1), purrr::map_int(nas, 2)), collapse = '; '),
            call. = FALSE)
  }
  bad <- unique(s$indiv_step_id[s$case_ & na_row])
  log[[length(log) + 1]] <- data.table::data.table(
    what = c('steps dropped: missing value at the used step', 'random steps dropped: missing value'),
    n = c(length(bad), sum(na_row & !s$case_ & !s$indiv_step_id %in% bad)))
  s <- s[!na_row & !indiv_step_id %in% bad, intersect(keep_cols, names(s)), with = FALSE]

  # animal codes in place of IDs
  s[codes, on = 'id', id := i.code]
  check_model_data(s, codes)
  log[[length(log) + 1]] <- data.table::data.table(
    what = c('animals', 'steps in the model (used)', 'rows in the model'),
    n = c(data.table::uniqueN(s$id), s[case_ == TRUE, .N], nrow(s)))
  list(data = s[], log = data.table::rbindlist(log))
}

#' @title Check a model table holds no positions, times or animal IDs
#' @param DT data.table. A model table.
#' @param codes data.table from [make_animal_codes()].
#' @return DT, invisibly; stops if a check fails.
#' @author Julie W. Turner
check_model_data <- function(DT, codes) {
  bad <- intersect(names(DT), model_private_columns)
  timed <- names(DT)[purrr::map_lgl(DT, function(x) inherits(x, c('POSIXt', 'Date')))]
  if (length(c(bad, timed))) {
    stop('model table holds positions or times: ', paste(unique(c(bad, timed)), collapse = ', '), call. = FALSE)
  }
  if ('id' %in% names(DT) && (anyNA(DT$id) || any(DT$id %in% codes$id) || !all(DT$id %in% codes$code))) {
    stop('model table holds animal IDs, not codes', call. = FALSE)
  }
  invisible(DT)
}

#' @title Text and grouping columns as factors, just before fitting
#' @description Text columns, and the grouping columns of random terms (e.g.
#'   `indiv_step_id`), become factors with only the values present in the
#'   table. Done at this last stage so a value dropped earlier (e.g. a herd or
#'   season with no steps left) is not a factor level with no data. The
#'   response (`case_`) becomes 0/1.
#' @param DT data.table. A model table.
#' @param f formula.
#' @return data.table (a copy).
#' @author Julie W. Turner
as_model_factors <- function(DT, f) {
  d <- data.table::copy(DT)
  groups <- unlist(strsplit(model_bars(f)$group, ':', fixed = TRUE))
  cols <- union(names(d)[purrr::map_lgl(d, function(x) is.character(x) || is.factor(x))], intersect(groups, names(d)))
  for (cl in cols) data.table::set(d, j = cl, value = factor(as.character(d[[cl]])))
  if ('case_' %in% names(d)) data.table::set(d, j = 'case_', value = as.integer(d$case_))
  d
}

#' @title Fit an integrated step selection model (iSSA) with glmmTMB
#' @description The Poisson form of the conditional logistic model (Muff et
#'   al. 2020): the intercept of each step, `(1 | indiv_step_id)`, has its
#'   variance fixed at `step_sd`^2 (1e6 by default); every other random term
#'   is estimated. The terms are found from the formula, so `map` and `start`
#'   need not be written by hand. Text columns become factors first
#'   ([as_model_factors()]).
#' @param data data.table. `data` from [build_model_data()].
#' @param formula character. The model formula.
#' @param step_sd numeric. Fixed SD of the step intercepts.
#' @param control named list. Passed to [glmmTMB::glmmTMBControl()]; by
#'   default `rank_check = 'adjust'`, as in NMC_CEtool.
#' @return glmmTMB model.
#' @author Julie W. Turner
fit_issa <- function(data, formula, step_sd = 1000, control = list(rank_check = 'adjust')) {
  f <- model_formula(formula)
  d <- as_model_factors(data, f)
  # random-term structure, from a small spread of steps (each term's number of variances)
  st <- levels(d$indiv_step_id)
  probe <- droplevels(d[indiv_step_id %in% st[unique(round(seq(1, length(st), length.out = min(length(st), 2000))))]])
  p <- suppressWarnings(glmmTMB::glmmTMB(f, family = stats::poisson(), data = probe, doFit = FALSE))
  rm(probe)
  n_theta <- purrr::map_int(p$condReStruc, function(r) as.integer(r$blockNumTheta))
  bars <- model_bars(f)
  is_step <- bars$group == 'indiv_step_id' & bars$intercept_only
  if (sum(is_step) != 1 || length(n_theta) != nrow(bars)) {
    stop('the formula needs exactly one (1 | indiv_step_id) term', call. = FALSE)
  }
  which_term <- rep(seq_along(n_theta), n_theta)
  fixed <- is_step[which_term]
  theta_map <- ifelse(fixed, NA_integer_, cumsum(!fixed))
  theta_start <- ifelse(fixed, log(step_sd), 0)
  glmmTMB::glmmTMB(f, family = stats::poisson(), data = d,
                   map = list(theta = factor(theta_map)), start = list(theta = theta_start),
                   control = do.call(glmmTMB::glmmTMBControl, control))
}

#' @title Coefficient tables of a fitted iSSA
#' @param fit glmmTMB model from [fit_issa()].
#' @param step_group character. The step grouping, left out of the per-group table.
#' @return list of data.tables: `fixed` (population-level estimates: term,
#'   estimate, se, z, p), `random` (SD of each random term by group) and
#'   `by_group` (each animal code's, or year-season's, coefficient: the
#'   population estimate plus its deviation).
#' @author Julie W. Turner
model_coefficients <- function(fit, step_group = 'indiv_step_id') {
  cf <- summary(fit)$coefficients$cond
  fixed <- data.table::data.table(term = rownames(cf), estimate = cf[, 1], se = cf[, 2], z = cf[, 3], p = cf[, 4])
  vc <- glmmTMB::VarCorr(fit)$cond
  base <- function(g) sub('[.][0-9]+$', '', g)   # glmmTMB names repeated groups id, id.1, id.2, ...
  random <- data.table::rbindlist(purrr::imap(vc, function(v, g) {
    sdv <- attr(v, 'stddev')
    data.table::data.table(group = base(g), term = names(sdv), sd = unname(sdv))
  }))
  re <- glmmTMB::ranef(fit)$cond
  re <- re[base(names(re)) != step_group]
  by_group <- data.table::rbindlist(purrr::imap(re, function(r, g) {
    d <- data.table::as.data.table(r, keep.rownames = 'level')
    d <- data.table::melt(d, id.vars = 'level', variable.name = 'term', value.name = 'deviation', variable.factor = FALSE)
    d[, group := base(g)]
  }))
  by_group[fixed, on = 'term', coefficient := estimate + deviation]
  data.table::setcolorder(by_group, c('group', 'level', 'term', 'coefficient', 'deviation'))
  list(fixed = fixed, random = random, by_group = by_group)
}

#' @title Save a model's coefficient tables and table log as CSV
#' @param coefs list from [model_coefficients()].
#' @param log data.table. `log` from [build_model_data()].
#' @param name character. The model's name (its key under `model: models:`).
#' @param out_dir character. Folder to write to.
#' @return character. The files written: <name>_fixed.csv, <name>_random.csv,
#'   <name>_by_group.csv and <name>_data_log.csv.
#' @author Julie W. Turner
write_model_coefficients <- function(coefs, log, name, out_dir) {
  reproducible::checkPath(out_dir, create = TRUE)
  tabs <- c(coefs, list(data_log = log))
  files <- file.path(out_dir, paste0(name, '_', names(tabs), '.csv'))
  purrr::walk2(tabs, files, function(x, f) data.table::fwrite(x, f))
  files
}
