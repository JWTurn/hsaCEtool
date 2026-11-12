# === Inputs table ------------------------------------------------------------
# Inputs named once in a table (inputs_<project>.csv), with where each comes
# from and on what terms, and referred to by name in the YAML

# columns the inputs table may have
input_table_cols <- c('name', 'title', 'theme', 'url', 'path', 'target_file', 'archive',
                      'access', 'provider', 'terms', 'notes')

# the columns that say where the file is, put into the settings
input_file_cols <- c('url', 'path', 'target_file', 'archive')

#' @title Read an inputs table
#' @description Reads a table of input files, one row per input (see
#'   `config/README.md`, "Inputs table"). The settings refer to an input by its
#'   `name` (`input: <name>`) instead of repeating its url. Blank cells are left
#'   out. Columns (`name` and `access` are required, and `url` or `path`):
#'   \describe{
#'     \item{name}{the input's name, used in the settings and as its id in the
#'       input manifest}
#'     \item{title}{a short title (e.g. "SCANFI land cover v3")}
#'     \item{theme}{what kind of data it is, e.g. landcover, harvest, fire,
#'       forest_age, elevation, disturbance, boundary, study_area or caribou}
#'     \item{url, path, target_file, archive}{where the file is, as for any source}
#'     \item{access}{`public` (the url may be published) or `restricted` (the
#'       file is shared with named people only: its url is never published)}
#'     \item{provider, terms}{who provided the data and on what terms (e.g. a
#'       licence or data-sharing agreement)}
#'     \item{notes}{anything worth knowing about the input}
#'   }
#'   Only `url`, `path`, `target_file` and `archive` reach the pipeline; the
#'   other columns are a record of where the inputs come from, so editing them
#'   reruns nothing.
#' @param file character. Path to the CSV file.
#' @return data.table, one row per line of the file, blank cells as NA.
#' @author Julie W. Turner
#' @export
read_inputs_table <- function(file) {
  if (!file.exists(file)) stop('Inputs table not found: ', file, call. = FALSE)
  tab <- data.table::fread(file, colClasses = 'character', na.strings = '')
  unknown <- setdiff(names(tab), input_table_cols)
  if (length(unknown)) {
    stop('Inputs table ', file, ' has unknown columns: ', paste(unknown, collapse = ', '),
         '. Columns can be: ', paste(input_table_cols, collapse = ', '), call. = FALSE)
  }
  for (col in setdiff(input_table_cols, names(tab))) tab[, (col) := NA_character_]
  tab[, (input_table_cols) := purrr::map(.SD, function(x) {
    x <- trimws(x)
    x[x == ''] <- NA_character_
    x
  }), .SDcols = input_table_cols]
  data.table::setcolorder(tab, input_table_cols)

  problems <- character()
  add <- function(rows, msg) {
    if (any(rows)) problems <<- c(problems, paste0(msg, ': rows ', paste(which(rows) + 1L, collapse = ', ')))
  }
  add(is.na(tab$name), 'no name')
  add(!is.na(tab$name) & duplicated(tab$name), 'name listed twice')
  add(is.na(tab$url) & is.na(tab$path), 'give a url or a path')
  add(!is.na(tab$url) & !is.na(tab$path), 'give a url or a path, not both')
  add(!tab$access %in% c('public', 'restricted'), 'access must be public or restricted')
  if (length(problems)) {
    stop('Problems in the inputs table ', file, ':\n  - ', paste(problems, collapse = '\n  - '), call. = FALSE)
  }
  tab[]
}

#' @title Combine the package's and a project's inputs tables
#' @description A project's row replaces the package's row of the same name;
#'   its other rows are added after the package's.
#' @param defaults,project data.tables from [read_inputs_table()] (`project` may be NULL).
#' @return data.table.
#' @keywords internal
#' @noRd
merge_inputs_tables <- function(defaults, project = NULL) {
  if (is.null(project) || !nrow(project)) return(defaults)
  data.table::rbindlist(list(defaults[!name %in% project$name], project), use.names = TRUE)
}

#' @title Put the inputs table's files into the settings
#' @description Wherever the settings say `input: <name>`, that entry is
#'   replaced, in the same place, by the input's `url` or `path`, and its
#'   `target_file` and `archive` if it has them; settings given there as well
#'   (e.g. a project's own `url`) win. `<what>_input: <name>` (e.g.
#'   `type_input` in `landscape: harvest: canlad_old:`) becomes `<what>_url`.
#'   The settings then come out as if the urls were typed in the YAML. The
#'   table is kept as `cfg$inputs_table`, with a column `used_in` listing where
#'   in the settings each input is used (e.g. `landscape/fire/nbac`; blank if
#'   nowhere).
#' @param cfg named list of settings.
#' @param tab data.table from [read_inputs_table()] or [merge_inputs_tables()].
#' @return `cfg`, changed.
#' @keywords internal
#' @noRd
apply_inputs_table <- function(cfg, tab) {
  used <- list()
  missing <- character()
  row_of <- function(nm, where) {
    i <- match(nm, tab$name)
    if (is.na(i)) {
      missing <<- c(missing, paste0(nm, ' (', where, ')'))
      return(NULL)
    }
    used[[nm]] <<- c(used[[nm]], where)
    as.list(tab[i, input_file_cols, with = FALSE])
  }
  expand <- function(x, where) {
    if (!is.list(x) || is.data.frame(x) || !length(x)) return(x)
    nms <- names(x)
    if (is.null(nms)) {
      return(purrr::imap(x, function(el, i) expand(el, paste0(where, '/', i))))
    }
    out <- list()
    for (nm in nms) {
      val <- x[[nm]]
      is_ref <- (nm == 'input' || grepl('_input$', nm)) && is.character(val) && length(val) == 1
      if (!is_ref) {
        out[nm] <- list(expand(val, if (nzchar(where)) paste0(where, '/', nm) else nm))
        next
      }
      if (nm == 'input') {
        # settings given beside input: win
        if (any(c('url', 'path') %in% nms)) next
        r <- row_of(val, where)
        if (is.null(r)) next
        r <- r[!purrr::map_lgl(r, is.na) & !names(r) %in% nms]
        out <- c(out, r)
      } else {
        url_nm <- sub('_input$', '_url', nm)
        if (url_nm %in% nms) next
        r <- row_of(val, paste0(where, '/', url_nm))
        if (is.null(r)) next
        if (is.na(r$url)) {
          missing <<- c(missing, paste0(val, ' (', where, '/', nm, ': needs a url)'))
          next
        }
        out[[url_nm]] <- r$url
      }
    }
    out
  }
  skip <- c('location_sources', 'layers', 'seasons', 'inputs_table')
  for (nm in setdiff(names(cfg), skip)) {
    cfg[nm] <- list(expand(cfg[[nm]], nm))
  }
  if (length(missing)) {
    stop('These inputs are not in the inputs table: ', paste(unique(missing), collapse = ', '), call. = FALSE)
  }
  tab <- data.table::copy(tab)
  tab[, used_in := purrr::map_chr(name, function(n) {
    if (is.null(used[[n]])) NA_character_ else paste(used[[n]], collapse = '; ')
  })]
  cfg$inputs_table <- tab
  cfg
}
