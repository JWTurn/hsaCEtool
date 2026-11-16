# === Capture logs, mortality records and long gaps ------------------------------
# Corrections from records kept outside the location data: which animal wore
# each collar when (capture logs), recorded deaths, and silences inside a track
# that join two deployments. Each step writes what it did to a decisions log
# (attribute `decisions`, target animal_decisions), so the corrections can be
# reported back to the data owners. The log holds animal IDs, actions and
# counts only, no dates or positions.

#' @title Read a capture log
#' @description One row per capture: the animal's main ID (`id`, e.g. B.C.
#'   `Primary.WLH_ID`), the ID given at that capture, the capture date, the
#'   collar put on, sex, whether it was a recapture, and herd. Dates may be
#'   Excel day numbers, yyyy-mm-dd, numbers with slashes in `date_order`, or
#'   written out (e.g. 18 Feb 2019); those that cannot be read are NA (and
#'   logged by the captures step). Sex `FALSE` is taken as female: Excel
#'   turns a typed F into FALSE.
#' @param file character. The spreadsheet (xlsx) or CSV.
#' @param cols named list. Column names in the file for `id`, `capture_id`,
#'   `date`, `collar`, and optionally `sex`, `recapture`, `herd`.
#' @param source character. The location source the log belongs to (e.g. BC).
#' @param date_order character. Order of day, month and year in dates written
#'   with slashes: "dmy" (e.g. 25/3/2019) or "mdy" (3/25/2019).
#' @return data.table with `source`, `id`, `capture_id`, `capture_date`
#'   (POSIXct, UTC midnight), `collar`, `sex`, `sex_raw`, `recapture`, `herd`.
#' @author Julie W. Turner
#' @export
read_capture_log <- function(file, cols, source, date_order = 'dmy') {
  x <- read_table_file(file)
  get <- function(k) if (!is.null(cols[[k]]) && cols[[k]] %in% names(x)) trimws(as.character(x[[cols[[k]]]])) else NA_character_
  out <- data.table::data.table(source = source, id = tidy_id(get('id')), capture_id = tidy_id(get('capture_id')),
                                capture_date = parse_record_dates(get('date'), date_order), collar = tidy_id(get('collar')),
                                sex_raw = get('sex'), recapture = get('recapture'), herd = get('herd'))
  out[, sex := data.table::fcase(toupper(sex_raw) %in% c('F', 'FALSE', 'FEMALE'), 'f',
                                 toupper(sex_raw) %in% c('M', 'MALE'), 'm', default = NA_character_)]
  out[!is.na(id) & nzchar(id)]
}

#' @title Read mortality records
#' @param file character. The spreadsheet (xlsx) or CSV.
#' @param cols named list. Column names in the file for `id`, `date` and
#'   optionally `cause`, `outcome` (e.g. Mortality, Censored) and `herd`.
#' @param source character. The location source the records belong to.
#' @inheritParams read_capture_log
#' @return data.table with `source`, `id`, `mortality_date` (POSIXct, UTC
#'   midnight), `mortality_cause`, `mortality_outcome` and `herd`.
#' @param sheet character or integer. Sheet of a spreadsheet; NULL for the first.
#' @author Julie W. Turner
#' @export
read_mortality_records <- function(file, cols, source, date_order = 'dmy', sheet = NULL) {
  x <- read_table_file(file, sheet)
  get <- function(k) if (!is.null(cols[[k]]) && cols[[k]] %in% names(x)) trimws(as.character(x[[cols[[k]]]])) else NA_character_
  out <- data.table::data.table(source = source, id = tidy_id(get('id')), mortality_date = parse_record_dates(get('date'), date_order),
                                mortality_cause = get('cause'), mortality_outcome = get('outcome'), herd = get('herd'))
  out[!is.na(id) & nzchar(id)]
}

#' @title Trim spaces, even from text that is not valid UTF-8
#' @description Some exports (e.g. the old B.C. telemetry file) hold bytes
#'   that are not valid text; [trimws()] fails on those.
#' @param x vector.
#' @return character.
#' @keywords internal
#' @noRd
trim_bytes <- function(x) gsub('^[[:space:]]+|[[:space:]]+$', '', as.character(x), useBytes = TRUE)

#' @title IDs and collars as in the tidied location data
#' @description As `tidy_text()` does for location columns: invalid text read
#'   as Latin-1 and every space removed, so record tables match the locations.
#' @param x vector.
#' @return character.
#' @keywords internal
#' @noRd
tidy_id <- function(x) {
  x <- as.character(x)
  bad <- !is.na(x) & !validUTF8(x)
  if (any(bad)) x[bad] <- iconv(x[bad], 'latin1', 'UTF-8')
  x <- gsub('[[:space:]\u00a0]+', '', x)
  x[!is.na(x) & !nzchar(x)] <- NA_character_
  x
}

#' @title Read a sheet of a spreadsheet (the first, by default), or a CSV
#' @param file character. One or more files; the first xlsx, xls or csv is read.
#' @param sheet character or integer. Sheet of a spreadsheet; NULL for the first.
#' @return data.table, all columns as read.
#' @keywords internal
#' @noRd
read_table_file <- function(file, sheet = NULL) {
  f <- file[grepl('[.](xlsx|xls|csv)$', file, ignore.case = TRUE)][1]
  if (is.na(f)) stop('No xlsx, xls or csv file in: ', paste(basename(file), collapse = ', '), call. = FALSE)
  if (grepl('csv$', f, ignore.case = TRUE)) return(data.table::fread(f, colClasses = 'character'))
  rlang::check_installed('readxl', reason = 'to read xlsx tables.')
  data.table::as.data.table(readxl::read_excel(f, sheet = sheet %||% 1, col_types = 'text'))
}

#' @title Dates written in different ways
#' @param x character. Excel day numbers, yyyy-mm-dd, dates with slashes or written-out dates.
#' @param date_order character. "dmy" or "mdy", for dates with slashes.
#' @return POSIXct at UTC midnight; NA where not readable.
#' @keywords internal
#' @noRd
parse_record_dates <- function(x, date_order = 'dmy') {
  out <- as.Date(rep(NA_character_, length(x)))
  serial <- grepl('^[0-9]{5}([.][0-9]+)?$', x)
  out[serial] <- as.Date(floor(as.numeric(x[serial])), origin = '1899-12-30')
  iso <- grepl('^[0-9]{4}-[0-9]{2}-[0-9]{2}', x)
  out[iso] <- as.Date(substr(x[iso], 1, 10))
  slash <- grepl('^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}$', x)
  out[slash] <- as.Date(x[slash], switch(match.arg(date_order, c('dmy', 'mdy')), dmy = '%d/%m/%Y', mdy = '%m/%d/%Y'))
  rest <- is.na(out) & !is.na(x) & nzchar(x)
  if (any(rest)) out[rest] <- as.Date(lubridate::parse_date_time(x[rest], c('dbY', 'dBY', 'bdY', 'BdY'), quiet = TRUE))
  as.POSIXct(format(out), tz = 'UTC')
}

#' @title Cleaning step: correct fixes with a capture log
#' @description For one source (e.g. B.C.), compares the fixes with a capture
#'   log ([read_capture_log()]) of which animal each collar was put on, and
#'   when. It acts only where the log is clear, and records everything else
#'   for the data owners to check (attribute `decisions`):
#'   \itemize{
#'     \item Moved: fixes filed under an animal that the log shows wearing the
#'       collar before it was put on another animal, from that later capture
#'       on (a collar redeployed, e.g. after a death, but still filed under
#'       the first animal).
#'     \item Kept, logged: fixes from before the filed animal's own logged
#'       capture of that collar, and fixes the log gives to another animal
#'       without showing the filed animal with that collar first (e.g. a
#'       later capture missing from the log, or an ID typo).
#'   }
#'   Adds `deployment_start` (the capture date) for fixes after a logged
#'   capture, so `capture_effect` drops the first days after each capture,
#'   and fills `sex` from the log where it is missing.
#' @inheritParams clean_bounds
#' @param params named list: `table` (from [read_capture_log()]), `source`,
#'   `collar_column` (default `collar_id`).
#' @return data.table, with attribute `decisions`.
#' @author Julie W. Turner
#' @keywords internal
#' @noRd
clean_captures <- function(DT, params, crs) {
  cap <- params$table
  if (is.null(cap) || nrow(cap) == 0) return(DT)
  col <- params$collar_column %||% 'collar_id'
  src <- params$source
  dec <- list()
  note <- function(id, action, fixes, detail) {
    dec[[length(dec) + 1]] <<- data.table::data.table(source = src, id = id, action = action, fixes = as.integer(fixes), detail = detail)
  }

  if (any(toupper(cap$sex_raw) %in% 'FALSE')) {
    note(NA_character_, 'capture log: sex FALSE read as female', sum(toupper(cap$sex_raw) %in% 'FALSE'),
         'Sex was FALSE in these captures (Excel turns a typed F into FALSE); read as female')
  }
  bad_date <- cap[is.na(capture_date)]
  if (nrow(bad_date)) {
    note(NA_character_, 'capture log: date not readable', nrow(bad_date),
         paste('captures left out:', paste(unique(bad_date$id), collapse = ', ')))
  }

  log <- cap[!is.na(capture_date) & !is.na(collar) & nzchar(collar), .(collar, capture_date, cap_id = id, cap_date = capture_date)]
  data.table::setkey(log, collar, capture_date)
  if (!col %in% names(DT)) DT[, (col) := NA_character_]
  if (!'deployment_start' %in% names(DT)) DT[, deployment_start := as.POSIXct(NA, tz = 'UTC')]
  DT[, .day := as.POSIXct(format(t_, '%Y-%m-%d', tz = 'UTC'), tz = 'UTC')]
  DT[, .collar := trim_bytes(get(col))]
  mine <- which(DT$source %in% src & DT$.collar %in% log$collar)

  # the latest logged capture of each fix's collar at or before the fix's day
  q <- DT[mine, .(.r = mine, collar = .collar, capture_date = .day, id_now = id)]
  m <- log[q, on = .(collar, capture_date), roll = TRUE]
  # the filed animal's own first logged capture of this collar
  own <- log[, .(own_date = min(cap_date)), by = .(collar, id_now = cap_id)]
  m <- own[m, on = .(collar, id_now)]

  # same animal: mark the deployment
  same <- m[!is.na(cap_id) & cap_id == id_now]
  # moved: the filed animal wore the collar first, then the log puts it on another
  move <- m[!is.na(cap_id) & cap_id != id_now & !is.na(own_date) & own_date < cap_date]
  if (nrow(move)) {
    move[, note(id_now[1], 'fixes moved to another animal', .N,
                paste0('collar ', collar[1], ': logged on this animal first, then put on ', cap_id[1],
                       '; fixes after that capture moved to ', cap_id[1])), by = .(id_now, cap_id, collar)]
  }
  # kept: before the filed animal's own capture of this collar
  early <- m[!is.na(own_date) & (is.na(cap_id) | cap_id != id_now) & !(.r %in% move$.r)]
  if (nrow(early)) {
    early[, note(id_now[1], 'fixes before logged capture kept (check)', .N,
                 paste0('collar ', collar[1], ': ', .N, ' fixes up to ', round(max(as.numeric(own_date - capture_date, units = 'days'))),
                        ' days before this animal\'s logged capture with it')), by = .(id_now, collar)]
  }
  # kept: the log gives the collar to another animal, without this one first
  other <- m[is.na(own_date) & !is.na(cap_id) & cap_id != id_now]
  if (nrow(other)) {
    other[, note(id_now[1], 'capture log disagrees, fixes kept (check)', .N,
                 paste0('collar ', collar[1], ' is logged on ', paste(unique(cap_id), collapse = '/'),
                        ', not on this animal; fixes left under this animal')), by = .(id_now, collar)]
  }
  DT[same$.r, deployment_start := same$cap_date]
  DT[move$.r, `:=`(id = move$cap_id, deployment_start = move$cap_date)]

  # sex from the log where missing
  if (!'sex' %in% names(DT)) DT[, sex := NA_character_]
  sx <- unique(cap[!is.na(sex), .(id, log_sex = sex)], by = 'id')
  DT[sx, on = 'id', .fill := i.log_sex]
  fill <- DT$source %in% src & (is.na(DT$sex) | !nzchar(trim_bytes(DT$sex))) & !is.na(DT$.fill)
  if (any(fill)) {
    note(NA_character_, 'sex filled from capture log', data.table::uniqueN(DT$id[fill]), 'animals whose sex was missing')
    DT[fill, sex := .fill]
  }
  DT[, c('.day', '.collar', '.fill') := NULL]
  data.table::setattr(DT, 'decisions', data.table::rbindlist(dec))
  DT
}

#' @title Cleaning step: apply recorded deaths
#' @description For animals in a mortality table ([read_mortality_records()]),
#'   matched on source and ID, adds `mortality_date` and `mortality_cause` and
#'   drops fixes more than `after_days` days after the death (a dead collar
#'   still sending fixes, or one put on another animal). Recorded deaths
#'   make `mortality_status` dead (see [mortality_status_rules()]). Records for
#'   animals not in the data are skipped with a warning.
#'
#'   Optional checks, each logged in the decisions (target `animal_decisions`):
#'   with `dead` set, only records whose outcome is one of those values are
#'   deaths (others with a date are listed, not applied); an animal with more
#'   than one record is listed (the first is used); with `qa` set, each record
#'   is compared with the animal's fixes (`mortality_record_flags()`) and
#'   records with a flag in `qa$skip` (default `moves_after`: the animal still
#'   moved after the date) are not applied, so their fixes are kept for review.
#'   Records for herds outside `locations: herd: keep` are dropped first
#'   ([add_record_tables()]) and only counted.
#' @inheritParams clean_bounds
#' @param params named list: `table` (all sources' records), `after_days`
#'   (default 1), `tz` (time zone of the recorded dates, default `UTC`: a death
#'   is taken at midnight of its date there), `dead` (optional outcome values
#'   that mean a death, e.g. `[Mortality]`), `qa` (optional: `max_fixes_after`
#'   (2), `max_path_m` (500), `max_step_m` (250), `skip` (`[moves_after]`)).
#' @return data.table, with attribute `decisions`.
#' @author Julie W. Turner
#' @keywords internal
#' @noRd
clean_mortality_records <- function(DT, params, crs) {
  rec <- params$table
  n_other <- attr(rec, 'other_herds') %||% 0L
  if (is.null(rec) || nrow(rec) == 0) return(DT)
  rec <- data.table::copy(rec)
  after <- (params$after_days %||% 1) * 86400
  tz <- params$tz %||% 'UTC'
  if (!'mortality_outcome' %in% names(rec)) rec[, mortality_outcome := NA_character_]
  dec <- list()
  if (n_other > 0) {
    dec[[1]] <- data.table::data.table(source = paste(unique(rec$source), collapse = ', '), id = NA_character_,
                                       action = 'mortality records for other herds skipped', fixes = 0L,
                                       detail = sprintf('%s records for herds not in locations: herd: keep', n_other),
                                       flag = 'other_herds')
  }
  dead <- unlist(params$dead)
  if (length(dead)) {
    other <- rec[!is.na(mortality_date) & !mortality_outcome %in% dead]
    if (nrow(other)) {
      dec[[length(dec) + 1]] <- other[, .(source, id, action = 'mortality record not applied', fixes = 0L,
                                          detail = paste0('has a date but its outcome (', mortality_outcome, ') is not a death'),
                                          flag = 'not_a_death')]
    }
    rec <- rec[mortality_outcome %in% dead]
  }
  rec <- rec[!is.na(mortality_date)]
  multi <- rec[, .(n = .N, dates = data.table::uniqueN(mortality_date)), by = .(source, id)][n > 1]
  if (nrow(multi)) {
    dec[[length(dec) + 1]] <- multi[, .(source, id, action = 'more than one mortality record', fixes = 0L,
                                        detail = sprintf('%s records with %s dates; the first listed is used', n, dates),
                                        flag = 'duplicate_records')]
  }
  rec <- unique(rec, by = c('source', 'id'))
  missing <- rec[!DT, on = c('source', 'id')]
  if (nrow(missing)) {
    warning(sprintf('[clean mortality_records] %s recorded deaths are for animals not in the data (skipped)', nrow(missing)))
    dec[[length(dec) + 1]] <- missing[, .(source, id, action = 'mortality record not matched', fixes = 0L,
                                          detail = 'animal not in the location data (e.g. never collared); record skipped',
                                          flag = 'not_matched')]
  }
  rec <- rec[unique(DT[, .(source, id)]), on = c('source', 'id'), nomatch = NULL]
  rec[, .death := local_to_utc(mortality_date, tz)]

  rec[, flag := NA_character_]
  if (!is.null(params$qa)) {
    fl <- mortality_record_flags(DT, rec, tz, params$qa)
    rec[, flag := NULL]
    rec <- fl[rec, on = c('source', 'id')]
    skip <- unlist(params$qa$skip %||% 'moves_after')
    held <- rec[flag %in% skip]
    if (nrow(held)) {
      dec[[length(dec) + 1]] <- held[, .(source, id, action = 'mortality record not applied', fixes = 0L,
                                         detail = sprintf('%s: %s fixes after the date, path %s m, longest step %s m; fixes kept, for review',
                                                          flag, fixes_after_date, path_m, max_step_m),
                                         flag, mortality_date = mortality_day, last_fix_date, fixes_after_date, path_m,
                                         max_step_m, step_into_m, days_last_fix_minus_date)]
    }
    rec <- rec[!flag %in% skip]
  }

  DT[rec, on = c('source', 'id'), `:=`(mortality_date = i.mortality_date, mortality_cause = i.mortality_cause,
                                       .death = i..death)]
  if (!'.death' %in% names(DT)) DT[, .death := as.POSIXct(NA)]
  late <- !is.na(DT$.death) & DT$t_ > DT$.death + after
  hit <- DT[!is.na(.death), .(fixes = sum(t_ > .death + after)), by = .(source, id)]
  hit <- rec[hit, on = c('source', 'id')]
  qa_cols <- intersect(c('mortality_day', 'last_fix_date', 'fixes_after_date', 'path_m', 'max_step_m', 'step_into_m',
                         'days_last_fix_minus_date'), names(hit))
  applied <- hit[, c(list(source = source, id = id, action = 'mortality record applied', fixes = fixes,
                          detail = paste0('status dead; fixes more than ', after / 86400,
                                          ' day(s) after the recorded death dropped',
                                          data.table::fifelse(is.na(flag), '', paste0(' (', flag, ')'))),
                          flag = flag),
                     .SD), .SDcols = qa_cols]
  if ('mortality_day' %in% names(applied)) data.table::setnames(applied, 'mortality_day', 'mortality_date')
  dec[[length(dec) + 1]] <- applied
  DT <- DT[!late]
  DT[, .death := NULL]
  data.table::setattr(DT, 'decisions', data.table::rbindlist(dec, fill = TRUE))
  DT
}

#' @title Compare recorded deaths with the animal's fixes
#' @description For each record, the fixes after the recorded date (local
#'   days in `tz`), and how far the animal went among them: the path through
#'   the fixes after the date, and its longest step. The step into the first
#'   fix after the date is not counted (it can be the animal's last move
#'   before it died) but is reported as `step_into_m`. Flags, in order:
#'   `ends_before` (last fix before the date, e.g. the collar dropped or failed
#'   first), `ends_on_date`, `moves_after` (path over `max_path_m` or a step
#'   over `max_step_m`: the date or the death is in doubt), `few_fixes_after`
#'   (at most `max_fixes_after`), and `many_fixes_no_move` (a collar lying
#'   still, e.g. on the carcass).
#' @param DT data.table of locations (`source`, `id`, `t_`, `x_`, `y_` in
#'   metres).
#' @param rec data.table of records (`source`, `id`, `mortality_date`),
#'   one per animal.
#' @param tz character. Time zone of the recorded dates.
#' @param qa named list: `max_fixes_after`, `max_path_m`, `max_step_m`.
#' @return data.table: `source`, `id`, `mortality_day`, `last_fix_date`,
#'   `fixes_after_date`, `path_m`, `max_step_m`, `step_into_m`, `flag`,
#'   `days_last_fix_minus_date`.
#' @keywords internal
#' @noRd
mortality_record_flags <- function(DT, rec, tz, qa) {
  max_n <- qa$max_fixes_after %||% 2
  max_path <- qa$max_path_m %||% 500
  max_step <- qa$max_step_m %||% 250
  r <- rec[, .(source, id, mdate = as.Date(format(mortality_date, '%Y-%m-%d', tz = 'UTC')))]
  d <- DT[r, on = c('source', 'id'), nomatch = NULL, .(source, id, t_, x_, y_, mdate)]
  data.table::setorder(d, source, id, t_)
  d[, day := as.Date(format(t_, '%Y-%m-%d', tz = tz))]
  out <- d[, {
    aft <- which(day > mdate[1])
    st <- if (length(aft) > 1) sqrt(diff(x_[aft])^2 + diff(y_[aft])^2) else 0
    into <- if (length(aft) && aft[1] > 1) {
      sqrt((x_[aft[1]] - x_[aft[1] - 1L])^2 + (y_[aft[1]] - y_[aft[1] - 1L])^2)
    } else {
      NA_real_
    }
    list(mortality_day = mdate[1], last_fix_date = max(day), fixes_after_date = length(aft),
         path_m = round(sum(st)), max_step_m = round(max(st)), step_into_m = round(into))
  }, by = .(source, id)]
  out[, flag := data.table::fcase(
    last_fix_date < mortality_day, 'ends_before',
    fixes_after_date == 0L, 'ends_on_date',
    path_m > max_path | max_step_m > max_step, 'moves_after',
    fixes_after_date <= max_n, 'few_fixes_after',
    default = 'many_fixes_no_move'
  )]
  out[, days_last_fix_minus_date := as.integer(last_fix_date - mortality_day)]
  out[]
}


#' @title Cleaning step: split tracks at long silences
#' @description A silence of more than `max_gap_days` inside an animal's
#'   track usually means two deployments filed under one ID (e.g. a collar
#'   put back on after years, or an ID reused). The track is split there into
#'   separate animals (`id`, `id_2`, ...); pieces with `min_fixes` fixes or
#'   fewer are dropped as stray fixes (often a wrong date).
#' @inheritParams clean_bounds
#' @param params named list: `max_gap_days` (default 365), `min_fixes` (default 3).
#' @return data.table, with attribute `decisions`.
#' @author Julie W. Turner
#' @keywords internal
#' @noRd
clean_long_gaps <- function(DT, params, crs) {
  gap <- (params$max_gap_days %||% 365) * 86400
  keep_min <- params$min_fixes %||% 3
  data.table::setorder(DT, id, t_)
  DT[, .piece := cumsum(c(0, diff(as.numeric(t_)) > gap)), by = id]
  pieces <- DT[, .(n = .N), by = .(source, id, .piece)]
  stray <- pieces[n <= keep_min & pieces[, .N, by = id][pieces, on = 'id']$N > 1]
  dec <- list()
  if (nrow(stray)) {
    dec[[1]] <- stray[, .(action = 'stray fixes after a long silence dropped', fixes = sum(n),
                          detail = paste0('pieces of ', keep_min, ' fixes or fewer, separated by more than ',
                                          gap / 86400, ' days from the rest')), by = .(source, id)]
    DT <- DT[!stray, on = c('id', '.piece')]
  }
  # renumber the remaining pieces and give later ones their own ID
  DT[, .piece := match(.piece, unique(.piece)), by = id]
  split <- DT[, .(pieces = max(.piece)), by = .(source, id)][pieces > 1]
  if (nrow(split)) {
    dec[[length(dec) + 1]] <- split[, .(source, id, action = 'track split at a long silence', fixes = pieces,
                                        detail = paste0('split into ', pieces, ' animals (', id, ', ', id, '_2, ...) at silences of more than ',
                                                        gap / 86400, ' days; fixes = number of pieces'))]
    DT[.piece > 1, id := paste0(id, '_', .piece)]
  }
  DT[, .piece := NULL]
  data.table::setattr(DT, 'decisions', data.table::rbindlist(dec, use.names = TRUE))
  DT
}

#' @title Lookup table of animals, IDs and collars
#' @description Links each animal across recaptures, new IDs, collars and
#'   study periods, so an animal studied at two different times can be
#'   followed. From a capture log (e.g. B.C.): one row per capture, with the
#'   fixes in the cleaned data under our ID(s) from that collar; plus our
#'   animals with more than one collar that the log does not show as
#'   recaptured. From Movebank: animals (`individual_local_identifier`) with
#'   more than one deployment, with the ID(s) we gave them.
#' @param captures data.table from [read_capture_log()], or NULL.
#' @param locations data.table of cleaned locations (`locations_clean`).
#' @param collar_column character. Collar column in `locations` for the
#'   capture log's source.
#' @return data.table: `source`, `kind`, `id` (the animal), `our_ids`,
#'   `capture_id`, `capture_date`, `collar`, `recapture`, `herd`, `sex`,
#'   `fixes`, `note`.
#' @author Julie W. Turner
#' @export
make_animal_lookup <- function(captures, locations, collar_column = 'collar_id') {
  loc <- data.table::as.data.table(locations)
  base_id <- function(x) sub('_[0-9]+$', '', x, useBytes = TRUE)
  # sort by bytes: some exports hold text that is not valid UTF-8
  srt <- function(x) sort(unique(x), method = 'radix')
  parts <- list()
  if (!is.null(captures) && nrow(captures)) {
    src <- unique(captures$source)
    b <- loc[source %in% src]
    b[, .collar := if (collar_column %in% names(b)) trim_bytes(get(collar_column)) else NA_character_]
    b[, .base := base_id(id)]
    fixes <- b[, .(fixes = .N, our_ids = paste(srt(id), collapse = '/')), by = .(.base, collar = .collar)]
    data.table::setnames(fixes, '.base', 'id')
    cl <- fixes[captures[, .(source, id, capture_id, capture_date, collar, recapture, herd, sex)], on = .(id, collar)]
    cl[, kind := 'capture log']
    multi <- b[!is.na(.collar), .(collars = data.table::uniqueN(.collar), fixes = .N,
                                  our_ids = paste(srt(id), collapse = '/')), by = .base][collars > 1]
    data.table::setnames(multi, '.base', 'id')
    extra <- multi[!id %in% captures[recapture %in% 'Y', id],
                   .(source = src[1], kind = 'several collars, no logged recapture', id, our_ids, fixes,
                     note = paste0(collars, ' collars in the data; not shown as recaptured in the capture log'))]
    parts <- c(parts, list(cl, extra))
  }
  if (all(c('individual_local_identifier', 'deployment_id') %in% names(loc))) {
    mb <- loc[!is.na(individual_local_identifier)]
    tag <- if ('tag_local_identifier' %in% names(mb)) mb$tag_local_identifier else NA_character_
    mb[, .tag := tag]
    mb[, `:=`(.animal = individual_local_identifier, .dep = as.character(deployment_id))]
    dep <- mb[, .(fixes = .N, our_ids = paste(srt(id), collapse = '/'), collar = paste(srt(.tag), collapse = '/'),
                  capture_date = min(t_)), by = .(source, .animal, deployment = .dep)]
    data.table::setnames(dep, '.animal', 'id')
    dep[, deployments := .N, by = .(source, id)]
    parts <- c(parts, list(dep[deployments > 1, .(source, kind = 'Movebank deployments', id, our_ids, collar,
                                                  capture_date, fixes, note = paste0('deployment ', deployment, ' of ', deployments,
                                                                                     '; capture_date = first fix'))]))
  }
  out <- data.table::rbindlist(parts, fill = TRUE)
  if (nrow(out)) data.table::setorder(out, source, id, capture_date)
  out[]
}

#' @title Write the decisions log and lookup table to restricted storage
#' @param x data.table.
#' @param file character. CSV to write (in `outputs/`, git-ignored and shared
#'   only to the restricted drive).
#' @return `file`.
#' @author Julie W. Turner
#' @export
write_decisions <- function(x, file) {
  reproducible::checkPath(dirname(file), create = TRUE)
  data.table::fwrite(x, file)
  file
}

#' @title Add record tables to the cleaning settings
#' @description The steps `captures` and `mortality_records` get their tables
#'   through their settings, so targets reruns cleaning when a table changes.
#' @param cleaning named list. The `locations: cleaning:` settings.
#' @param captures data.table from [read_capture_log()], or NULL.
#' @param mortality list of data.tables from [read_mortality_records()], or NULL.
#' @param capture_source character. Source the capture log belongs to.
#' @param collar_column character. Collar column for the captures step.
#' @param herd_cfg named list. The `locations: herd:` settings. With `keep`
#'   set, mortality records whose `herd` (in CamelCase, after `rename`) is not
#'   kept are dropped (records with no herd are kept); their number is logged
#'   by the step.
#' @return `cleaning`, with `params$captures` and `params$mortality_records` filled in.
#' @author Julie W. Turner
#' @export
add_record_tables <- function(cleaning, captures = NULL, mortality = NULL, capture_source = NULL,
                              collar_column = 'collar_id', herd_cfg = NULL) {
  if (!is.null(captures)) {
    cleaning$params$captures <- utils::modifyList(cleaning$params$captures %||% list(),
                                                  list(table = captures, source = capture_source, collar_column = collar_column))
  }
  if (length(mortality)) {
    cleaning$params$mortality_records <- utils::modifyList(cleaning$params$mortality_records %||% list(),
                                                           list(table = keep_record_herds(data.table::rbindlist(mortality, fill = TRUE),
                                                                                          herd_cfg)))
  }
  cleaning
}

#' @title Drop records for herds a project does not study
#' @param rec data.table of records, optionally with `herd`.
#' @param herd_cfg named list. The `locations: herd:` settings (`keep`, `rename`).
#' @return `rec` without records of other herds, with attribute `other_herds`
#'   (how many were dropped).
#' @keywords internal
#' @noRd
keep_record_herds <- function(rec, herd_cfg) {
  keep <- unlist(herd_cfg$keep)
  if (!length(keep) || !'herd' %in% names(rec)) return(rec)
  other <- !is.na(rec$herd) & !rename_names(to_camel(rec$herd), herd_cfg$rename) %in% keep
  out <- rec[!other]
  data.table::setattr(out, 'other_herds', sum(other))
  out
}
