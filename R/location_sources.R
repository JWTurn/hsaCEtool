# === Location sources table ------------------------------------------------
# Reading `locations: sources:` from a CSV (e.g. config/locations_NMC.csv)

# columns the location sources table may have
source_table_cols <- c('name', 'type', 'movebank_study', 'part', 'url', 'path', 'target_file', 'archive', 'fun',
                       'id_col', 'datetime_col', 'hour_col', 'x_col', 'y_col', 'input_crs', 'tz',
                       'date_only', 'id_from_deployment', 'refresh',
                       'study', 'study_column', 'herd', 'herd_column', 'juris', 'provider', 'terms', 'notes')

#' @title Read the location sources table
#' @description Reads a table of location sources, one row per source, in order
#'   of priority (as in `locations: sources:`; see `config/README.md`, "Location
#'   sources table"). The B.C. telemetry source has one row per part (`new`,
#'   `old`, `reference`), all with the same `name`. Blank cells are left out.
#'   Columns (only `name` is required):
#'   \describe{
#'     \item{name}{the source's name, used in its target names and the `source`
#'       column of the locations}
#'     \item{type}{`file` (the default), `movebank` or `bc`}
#'     \item{movebank_study}{Movebank study ID (or name), for `type` movebank}
#'     \item{part}{`new`, `old` or `reference`, for `type` bc}
#'     \item{url, path, target_file, archive, fun}{where the file is, as for any source}
#'     \item{id_col, datetime_col, hour_col, x_col, y_col}{the source's `columns`: names of
#'       its animal ID, date and time, hour, and coordinate columns}
#'     \item{input_crs, tz}{the source's coordinate system and time zone, if not the defaults}
#'     \item{date_only, id_from_deployment, refresh}{TRUE or FALSE (blank = FALSE)}
#'     \item{study, herd, juris}{the source's study, herd and jurisdiction, for
#'       fixes that have none from a column (the `by_source` settings)}
#'     \item{study_column, herd_column}{a column of the data checked first for
#'       each fix's study or herd in this source (before `locations: study:
#'       columns` or `herd: columns`)}
#'     \item{provider, terms}{who provided the data and on what terms (e.g. a
#'       data-sharing agreement), for the record of where the source came from;
#'       not used by the pipeline}
#'     \item{notes}{anything worth knowing about the source; not used by the pipeline}
#'   }
#' @param file character. Path to the CSV file.
#' @return data.table, one row per line of the file, blank cells as NA.
#' @author Julie W. Turner
#' @export
read_location_sources <- function(file) {
  if (!file.exists(file)) stop('Location sources table not found: ', file, call. = FALSE)
  tab <- data.table::fread(file, colClasses = 'character', na.strings = '')
  unknown <- setdiff(names(tab), source_table_cols)
  if (length(unknown)) {
    stop('Location sources table ', file, ' has unknown columns: ', paste(unknown, collapse = ', '),
         '. Columns can be: ', paste(source_table_cols, collapse = ', '), call. = FALSE)
  }
  for (col in setdiff(source_table_cols, names(tab))) tab[, (col) := NA_character_]
  tab[, (source_table_cols) := purrr::map(.SD, function(x) {
    x <- trimws(x)
    x[x == ''] <- NA_character_
    x
  }), .SDcols = source_table_cols]
  tab[is.na(type), type := 'file']

  problems <- character()
  add <- function(rows, msg) {
    if (any(rows)) problems <<- c(problems, paste0(msg, ': rows ', paste(which(rows) + 1L, collapse = ', ')))
  }
  add(is.na(tab$name), 'no name')
  add(!tab$type %in% c('file', 'movebank', 'bc'), 'type must be file, movebank or bc')
  add(tab$type == 'movebank' & is.na(tab$movebank_study), 'a movebank source needs movebank_study')
  add(tab$type != 'bc' & !is.na(tab$part), 'part is only for type bc')
  add(tab$type == 'bc' & !tab$part %in% c('new', 'old', 'reference'), 'a bc row needs part new, old or reference')
  add(tab$type == 'bc' & duplicated(tab[, .(name, type, part)]), 'a bc part listed twice')
  for (col in c('date_only', 'id_from_deployment', 'refresh')) {
    add(!is.na(tab[[col]]) & !tolower(tab[[col]]) %in% c('true', 'false', 'yes', 'no'),
        paste0(col, ' must be TRUE or FALSE'))
  }
  # a bc source's rows must agree on everything but their part's file
  file_cols <- c('part', 'url', 'path', 'target_file', 'archive', 'refresh', 'provider', 'terms', 'notes')
  for (nm in unique(tab[type == 'bc', name])) {
    rows <- tab[type == 'bc' & name == nm, setdiff(source_table_cols, file_cols), with = FALSE]
    differ <- names(rows)[purrr::map_lgl(rows, function(x) data.table::uniqueN(stats::na.omit(x)) > 1)]
    if (length(differ)) {
      problems <- c(problems, paste0('the rows of bc source ', nm, ' differ in: ', paste(differ, collapse = ', ')))
    }
  }
  if (length(problems)) {
    stop('Problems in the location sources table ', file, ':\n  - ', paste(problems, collapse = '\n  - '), call. = FALSE)
  }
  tab[]
}

#' @title Put a location sources table into the settings
#' @description Turns the table from [read_location_sources()] into the same
#'   settings as when the sources are typed in the YAML: the
#'   `locations: sources:` list, the `by_source` entries of `locations: study:`,
#'   `herd:` and `juris:` (added to, and replacing for the same source, any in the YAML),
#'   and `study_column` and `herd_column` as `columns` entries limited to their
#'   sources, put before those in the YAML. The table itself is kept as
#'   `cfg$location_sources` (e.g. for its provider, terms and notes).
#' @param cfg named list of settings.
#' @param tab data.table from [read_location_sources()].
#' @return `cfg`, changed.
#' @keywords internal
#' @noRd
apply_location_sources <- function(cfg, tab) {
  cfg$locations$sources <- source_list(tab)
  one <- tab[!duplicated(name)]
  for (what in c('study', 'herd', 'juris')) {
    has <- one[!is.na(get(what))]
    if (nrow(has)) {
      map <- stats::setNames(as.list(has[[what]]), has$name)
      cfg$locations[[what]]$by_source <- utils::modifyList(as.list(cfg$locations[[what]]$by_source %||% list()), map)
    }
  }
  for (what in c('study', 'herd')) {
    col <- paste0(what, '_column')
    has <- one[!is.na(get(col))]
    if (nrow(has)) {
      entries <- purrr::map(unique(has[[col]]), function(cl) list(column = cl, sources = has[get(col) == cl, name]))
      cfg$locations[[what]]$columns <- c(entries, as.list(cfg$locations[[what]]$columns))
    }
  }
  cfg$location_sources <- tab
  cfg
}

#' @title The `locations: sources:` list from a location sources table
#' @inheritParams apply_location_sources
#' @return list of sources, in the table's order; a bc source in the place of its first row.
#' @keywords internal
#' @noRd
source_list <- function(tab) {
  flag <- function(x) if (is.na(x)) NULL else tolower(x) %in% c('true', 'yes')
  # as in the YAML: whole numbers too large for R's integers stay numbers
  study_id <- function(x) {
    if (is.na(x)) return(NULL)
    n <- suppressWarnings(as.numeric(x))
    if (is.na(n)) return(x)
    i <- suppressWarnings(as.integer(x))
    if (is.na(i)) n else i
  }
  file_part <- function(r) {
    out <- list(url = r$url, path = r$path, target_file = r$target_file, archive = r$archive, fun = r$fun,
                refresh = flag(r$refresh))
    purrr::compact(purrr::map(out, function(v) if (length(v) && is.na(v[1])) NULL else v))
  }
  out <- list()
  done <- character()
  for (i in seq_len(nrow(tab))) {
    r <- as.list(tab[i])
    if (r$type == 'bc') {
      if (r$name %in% done) next
      done <- c(done, r$name)
      rows <- tab[type == 'bc' & name == r$name]
      src <- list(type = 'bc', name = r$name)
      for (p in c('new', 'old', 'reference')) {
        if (nrow(rows[part == p])) src[[p]] <- file_part(rows[part == p])
      }
      first <- function(col) stats::na.omit(rows[[col]])[1]
      r <- as.list(rows[1])
      for (col in c('id_col', 'datetime_col', 'hour_col', 'x_col', 'y_col', 'input_crs', 'tz')) r[[col]] <- first(col)
    } else {
      src <- c(list(type = r$type, name = r$name), file_part(r))
      if (r$type == 'movebank') src$study <- study_id(r$movebank_study)
    }
    cols <- c(id = r$id_col, datetime = r$datetime_col, hour = r$hour_col, x = r$x_col, y = r$y_col)
    cols <- cols[!is.na(cols)]
    if (length(cols)) src$columns <- as.list(cols)
    if (!is.na(r$input_crs)) src$input_crs <- r$input_crs
    if (!is.na(r$tz)) src$tz <- r$tz
    for (col in c('date_only', 'id_from_deployment')) src[[col]] <- flag(r[[col]])
    out <- c(out, list(src))
  }
  out
}
