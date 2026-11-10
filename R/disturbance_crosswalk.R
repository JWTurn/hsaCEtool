# === Disturbance crosswalk tables ---------------------------------------------
# Sorting a disturbance source's features into categories with a table (e.g.
# config/crosswalk_bc_NMC.csv) that lists every value of the fields used,
# instead of `rules:` in the YAML. read_config() turns each table into the same
# settings as rules typed in the YAML, one exact-value rule per row, so the
# rest of the pipeline is unchanged. Values in the data that a table doesn't
# list are found by check_disturbance_coverage() (target disturb_uncovered).

# columns every crosswalk table has; any other column is a field of the features
crosswalk_cols <- c('file', 'category', 'subtype', 'notes')

#' @title Read a disturbance crosswalk table
#' @description Reads one source's crosswalk: a table saying which category
#'   each kind of feature goes into, listing every value of the fields used
#'   (see `config/README.md`, "Crosswalk tables"). Columns:
#'   \describe{
#'     \item{file}{the file of the source the row is for}
#'     \item{category}{the category given (e.g. paved), or `drop` to leave
#'       those features out}
#'     \item{subtype}{optionally, a finer grouping within the category (e.g.
#'       for unpaved roads: winter, resource or all-season)}
#'     \item{notes}{anything worth knowing about the row; not used by the pipeline}
#'     \item{any other column}{a field of the features (e.g. `ReportClass`),
#'       holding the value the row is for}
#'   }
#'   A file uses the field columns that any of its rows fill in; a blank cell
#'   in a field the file uses stands for a missing value in the data. A file
#'   whose rows fill in no field has one row, giving all its features one
#'   category.
#' @param file character. Path to the CSV file.
#' @return data.table, one row per line of the file, blank cells as NA.
#' @author Julie W. Turner
#' @export
read_disturbance_crosswalk <- function(file) {
  if (!file.exists(file)) stop('Disturbance crosswalk table not found: ', file, call. = FALSE)
  tab <- data.table::fread(file, colClasses = 'character', na.strings = '')
  miss <- setdiff(c('file', 'category'), names(tab))
  if (length(miss)) {
    stop('Disturbance crosswalk table ', file, ' needs column(s) ', paste(miss, collapse = ', '), call. = FALSE)
  }
  for (col in setdiff(crosswalk_cols, names(tab))) tab[, (col) := NA_character_]
  cols <- names(tab)
  tab[, (cols) := purrr::map(.SD, function(x) {
    x <- trimws(x)
    x[x == ''] <- NA_character_
    x
  }), .SDcols = cols]

  problems <- character()
  add <- function(rows, msg) {
    if (any(rows)) problems <<- c(problems, paste0(msg, ': rows ', paste(which(rows) + 1L, collapse = ', ')))
  }
  add(is.na(tab$file), 'no file')
  add(is.na(tab$category), 'no category')
  fields <- setdiff(cols, crosswalk_cols)
  for (f in unique(stats::na.omit(tab$file))) {
    rows <- tab$file %in% f
    used <- crosswalk_fields(tab[rows], fields)
    if (length(used) == 0) {
      if (sum(rows) > 1) problems <- c(problems, paste0(f, ' fills in no field, so it needs exactly one row'))
    } else {
      add(rows & duplicated(tab[, c('file', used), with = FALSE]), paste0(f, ': the same values listed twice'))
    }
  }
  if (length(problems)) {
    stop('Problems in disturbance crosswalk table ', file, ':\n  - ', paste(problems, collapse = '\n  - '), call. = FALSE)
  }
  tab[]
}

#' @title The field columns a file's crosswalk rows fill in
#' @param rows data.table. One file's rows of a crosswalk table.
#' @param fields character. The table's field columns.
#' @return character.
#' @keywords internal
#' @noRd
crosswalk_fields <- function(rows, fields) {
  fields[purrr::map_lgl(fields, function(f) any(!is.na(rows[[f]])))]
}

#' @title Turn a crosswalk table into a disturbance source's category settings
#' @description Gives each of the source's files the settings its rows give
#'   (rows for other files are skipped: several sources can share a table):
#'   one `category` (and `subtype`) for a file whose rows fill in no field,
#'   otherwise one rule per row, `where` each field used has the row's value
#'   (`""` for a blank, matching a missing value). There is no rule catching
#'   the rest, so features with values the table doesn't list match no rule
#'   (see [check_disturbance_coverage()]).
#' @param src named list. The source's settings (`landscape: disturbance:
#'   sources: <name>`), with `crosswalk` naming the table.
#' @param tab data.table. From [read_disturbance_crosswalk()].
#' @param name character. The source's name, for messages.
#' @return `src`, its files with `category` or `rules`, and without `crosswalk`.
#' @author Julie W. Turner
#' @export
apply_disturbance_crosswalk <- function(src, tab, name) {
  where <- paste0('landscape: disturbance: sources: ', name)
  fields <- setdiff(names(tab), crosswalk_cols)
  src$files <- purrr::map(src$files, function(f) {
    if (!is.null(f$category) || length(f$rules)) {
      stop(where, ': ', f$file, ' has a category or rules and a crosswalk table; give only one', call. = FALSE)
    }
    rows <- tab[file %in% f$file]
    if (nrow(rows) == 0) stop(where, ': ', f$file, ' has no rows in crosswalk ', src$crosswalk, call. = FALSE)
    used <- crosswalk_fields(rows, fields)
    if (length(used) == 0) {
      f$category <- rows$category
      if (!is.na(rows$subtype)) f[['subtype']] <- rows$subtype
      return(f)
    }
    f$rules <- purrr::map(seq_len(nrow(rows)), function(i) {
      w <- purrr::map(stats::setNames(used, used), function(k) {
        v <- rows[[k]][i]
        if (is.na(v)) '' else v
      })
      r <- list(where = w, category = rows$category[i])
      if (!is.na(rows$subtype[i])) r[['subtype']] <- rows$subtype[i]
      r
    })
    f
  })
  src$crosswalk <- NULL
  src
}

#' @title Read the crosswalk tables of the disturbance sources that name one
#' @param dcfg named list. `landscape: disturbance:`.
#' @param config_dir character. Folder holding the project's settings files.
#' @return `dcfg`, each source with a `crosswalk` given its files' categories
#'   by [apply_disturbance_crosswalk()]. A table's rows must each be for a
#'   file of one of the sources naming it.
#' @keywords internal
#' @noRd
apply_disturbance_crosswalks <- function(dcfg, config_dir) {
  cws <- purrr::map(dcfg$sources, function(s) if (is.character(s$crosswalk) && length(s$crosswalk) == 1) s$crosswalk)
  cws <- unlist(cws[!purrr::map_lgl(cws, is.null)])
  for (cw in unique(cws)) {
    tab <- read_disturbance_crosswalk(file.path(config_dir, cw))
    users <- names(cws)[cws == cw]
    files <- unlist(purrr::map(users, function(nm) purrr::map_chr(dcfg$sources[[nm]]$files, function(f) f$file %||% NA_character_)))
    extra <- setdiff(unique(tab$file), files)
    if (length(extra)) {
      stop('Disturbance crosswalk ', cw, ' lists file(s) not in the files of ', paste(users, collapse = ' or '),
           ': ', paste(extra, collapse = ', '), call. = FALSE)
    }
    for (nm in users) dcfg$sources[[nm]] <- apply_disturbance_crosswalk(dcfg$sources[[nm]], tab, nm)
  }
  dcfg
}

#' @title Kinds of disturbance features that no rule or crosswalk row covers
#' @description Lists the kinds of feature (each combination of the values
#'   of the fields used) that matched no rule and so were left out of the
#'   disturbance layers: usually values a crosswalk table doesn't list yet
#'   (e.g. a new road class in a new data release). The list is written to
#'   `out_dir/disturbance_uncovered.csv` with one column per field, so its
#'   rows can be copied into the crosswalk table once given a category. If
#'   there are any, it then stops with an error, so that the layers are not
#'   built without them.
#' @param summary data.table. From [summarize_disturbance()].
#' @param out_dir character. Folder to write to.
#' @return character. The CSV file (with only its header when all are covered).
#' @author Julie W. Turner
#' @export
check_disturbance_coverage <- function(summary, out_dir) {
  reproducible::checkPath(out_dir, create = TRUE)
  file <- file.path(out_dir, 'disturbance_uncovered.csv')
  miss <- data.table::as.data.table(summary)[category == 'none (left out)']
  # the type ("field=value; field=value") as one column per field; "NA" = missing
  parts <- purrr::map(miss$type, function(s) {
    if (is.na(s)) return(list())
    kv <- strsplit(s, '; (?=[^;=]+=)', perl = TRUE)[[1]]
    v <- sub('^[^=]*=', '', kv)
    v[v == 'NA'] <- NA_character_
    as.list(stats::setNames(v, sub('=.*$', '', kv)))
  })
  vals <- data.table::rbindlist(parts, fill = TRUE)
  if (nrow(vals) == 0) vals <- data.table::data.table()
  out <- data.table::data.table(source = miss$source, file = miss$file)
  if (ncol(vals)) out <- cbind(out, vals)
  out <- cbind(out, miss[, .(features, length_km, area_km2, first_year, last_year)])
  data.table::fwrite(out, file)
  if (nrow(out)) {
    stop(nrow(out), ' kind(s) of disturbance feature (', sum(out$features), ' features, in source(s) ',
         paste(unique(out$source), collapse = ', '), ') match no rule or crosswalk row and would be left out. ',
         'Add them to the crosswalk table (or rules), with category drop to leave them out: see ', file,
         call. = FALSE)
  }
  file
}
