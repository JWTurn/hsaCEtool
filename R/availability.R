# === Availability of each layer at used and available steps --------------------
# Summaries (counts, min, mean, median, max) to help choose and combine
# covariates before fitting models. No positions or times.

#' @title Grouping columns for the availability summaries
#' @description One row per row of `steps`, in the same order: `case_` (TRUE
#'   for observed, used steps; FALSE for random, available steps) and the `by`
#'   columns, stored as factors to save memory.
#' @param steps data.table from [prep_steps()].
#' @param by character. Columns to summarize by, broad to fine (e.g. juris,
#'   study, herd).
#' @return data.table.
#' @author Julie W. Turner
make_step_groups <- function(steps, by = NULL) {
  by <- unlist(by)
  missing <- setdiff(by, names(steps))
  if (length(missing)) {
    stop('extract: availability_by columns not in the steps: ', paste(missing, collapse = ', '),
         '. Add them to locations: keep_after_cleaning.', call. = FALSE)
  }
  g <- steps[, c('case_', by), with = FALSE]
  for (col in by) data.table::set(g, j = col, value = as.factor(g[[col]]))
  g[]
}

#' @title Summarize one layer's values at used and available steps
#' @description Reads only this layer's columns (`<name>_start`, `<name>_end`,
#'   and `ts_<name>_*` for time since disturbance) from every year's Parquet
#'   file, and summarizes each for used and available steps: overall, then by
#'   each level of `by` in turn (e.g. by juris, by juris and study, by juris,
#'   study and herd). `min`, `mean`, `median`, `max` and `share_above_0` use the
#'   steps with a value; `share_missing` is the share without one.
#' @param files character. Parquet files from [write_extracted_year()], one per year.
#' @param name character. Layer name.
#' @param groups data.table from [make_step_groups()].
#' @param by character. The grouping columns, broad to fine.
#' @return data.table: `layer` (`ts_<name>` for time since disturbance),
#'   `column`, `where` (start or end), `used`
#'   (TRUE for used steps), the `by` columns ("all" where not split by that
#'   column), `n`, `min`, `mean`, `median`, `max`, `share_above_0` and
#'   `share_missing`.
#' @author Julie W. Turner
summarize_availability <- function(files, name, groups, by = NULL) {
  by <- unlist(by)
  have <- nanoparquet::read_parquet_schema(files[1])$name
  cols <- intersect(c(paste0(name, c('_start', '_end')), paste0('ts_', name, c('_start', '_end'))), have)
  vals <- data.table::rbindlist(purrr::map(files, function(f) {
    data.table::as.data.table(nanoparquet::read_parquet(f, col_select = c('row', cols)))
  }))
  data.table::setorder(vals, row)
  if (!identical(as.integer(vals$row), seq_len(nrow(groups)))) {
    stop('The extracted rows do not match the steps; rerun extraction', call. = FALSE)
  }
  DT <- data.table::copy(groups)
  sets <- purrr::map(0:length(by), function(k) c('case_', by[seq_len(k)]))
  out <- data.table::rbindlist(purrr::map(cols, function(col) {
    data.table::set(DT, j = '.v', value = as.numeric(vals[[col]]))
    data.table::rbindlist(purrr::map(sets, function(s) {
      res <- DT[, .(n = .N,
                    min = if (all(is.na(.v))) NA_real_ else min(.v, na.rm = TRUE),
                    mean = mean(.v, na.rm = TRUE),
                    median = stats::median(.v, na.rm = TRUE),
                    max = if (all(is.na(.v))) NA_real_ else max(.v, na.rm = TRUE),
                    share_above_0 = mean(.v[!is.na(.v)] > 0),
                    share_missing = mean(is.na(.v))), keyby = s]
      for (b in by) {
        if (b %in% s) data.table::set(res, j = b, value = as.character(res[[b]]))
        else data.table::set(res, j = b, value = 'all')
      }
      # time since disturbance is listed as its own layer, ts_<name>
      res[, `:=`(layer = if (startsWith(col, 'ts_')) paste0('ts_', name) else name, column = col,
                 where = sub('.*_', '', col))]
    }), use.names = TRUE)
  }))
  data.table::setnames(out, 'case_', 'used')
  for (j in c('mean', 'share_above_0')) data.table::set(out, i = which(is.nan(out[[j]])), j = j, value = NA_real_)
  data.table::setcolorder(out, c('layer', 'column', 'where', 'used', by))
  out[]
}

#' @title Save the availability summaries as a CSV file
#' @param availability data.table from [summarize_availability()], all layers.
#' @param out_dir character. Folder to write to.
#' @return character. The file written.
#' @author Julie W. Turner
write_availability <- function(availability, out_dir) {
  reproducible::checkPath(out_dir, create = TRUE)
  file <- file.path(out_dir, 'availability.csv')
  data.table::fwrite(availability, file)
  file
}
