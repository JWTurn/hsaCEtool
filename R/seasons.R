# === Seasons ------------------------------------------------------------------
# Seasons come from a season table (CSV), one row per season, grouped in sets:
# each set is one way of dividing the year (e.g. simplified or biological
# seasons). A project names its table and default set in `model:`; a model can
# use another set. See inst/config/README.md.

#' @title Read a season table
#' @description Reads and checks a season table (CSV), one row per season:
#'   `set` (a name for one way of dividing the year, e.g. simplified or bio),
#'   `season`, and its first and last day as `start_month`, `start_day`,
#'   `end_month` and `end_day`, inclusive. Months and days are separate
#'   numbers, so a spreadsheet cannot turn them into dates. A season may cross
#'   the new year (e.g. winter 11 1 to 4 15). Other columns (e.g. `source`) are
#'   kept but not used.
#' @param file character. The CSV file.
#' @return data.table: the table, with `start` and `end` added as month-day
#'   text (e.g. "04-15").
#' @author Julie W. Turner
#' @export
read_seasons <- function(file) {
  if (!file.exists(file)) stop('Season table not found: ', file, call. = FALSE)
  s <- data.table::fread(file, colClasses = 'character')
  need <- c('set', 'season', 'start_month', 'start_day', 'end_month', 'end_day')
  miss <- setdiff(need, names(s))
  if (length(miss)) stop('Season table ', file, ' needs columns: ', paste(miss, collapse = ', '), call. = FALSE)
  for (col in need[3:6]) data.table::set(s, j = col, value = suppressWarnings(as.integer(s[[col]])))

  problems <- character()
  add <- function(rows, msg) {
    if (any(rows)) problems <<- c(problems, paste0(msg, ': ', paste(unique(paste(s$set[rows], s$season[rows])), collapse = ', ')))
  }
  # a real day of the year (2020 is a leap year, so 2 29 is allowed)
  real_day <- function(m, d) !is.na(as.Date(sprintf('2020-%02d-%02d', m, d), format = '%Y-%m-%d'))
  add(!real_day(s$start_month, s$start_day), 'start is not a real day (month 1 to 12, then a day of that month)')
  add(!real_day(s$end_month, s$end_day), 'end is not a real day (month 1 to 12, then a day of that month)')
  name_ok <- function(x) grepl('^[A-Za-z][A-Za-z0-9_]*$', x)
  add(!name_ok(s$set) | !name_ok(s$season), 'set and season names must be letters, numbers and _, starting with a letter')
  add(duplicated(s[, c('set', 'season')]), 'seasons listed more than once in a set')
  if (length(problems)) {
    stop('Problems in season table ', file, ':\n  - ', paste(problems, collapse = '\n  - '), call. = FALSE)
  }
  s[, `:=`(start = sprintf('%02d-%02d', start_month, start_day), end = sprintf('%02d-%02d', end_month, end_day))]
  s[]
}

#' @title The seasons of one set
#' @description The seasons of one set of a season table, in table order, as
#'   [season_of()] takes them: the first season listed that holds a date wins.
#' @param seasons data.table from [read_seasons()].
#' @param set character. The set's name (column `set`).
#' @return named list: for each season, its first and last day as month-day
#'   text (e.g. `list('11-01', '04-15')`).
#' @author Julie W. Turner
#' @export
season_dates <- function(seasons, set) {
  rows <- which(seasons[['set']] == set)
  if (!length(rows)) {
    stop('No seasons in set "', set, '" (sets: ', paste(unique(seasons[['set']]), collapse = ', '), ')', call. = FALSE)
  }
  stats::setNames(purrr::map2(seasons$start[rows], seasons$end[rows], function(a, b) list(a, b)),
                  seasons$season[rows])
}
