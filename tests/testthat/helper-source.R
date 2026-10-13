# Load the pipeline's functions (this project is not a package)
library(data.table)
for (f in list.files(testthat::test_path('..', '..', 'R'), pattern = '[.]R$', full.names = TRUE)) {
  source(f)
}

# Small set of standard locations: 2 animals, fixes every 2 hours, in EPSG:3154
toy_locations <- function(n = 50) {
  t0 <- as.POSIXct('2020-03-01', tz = 'UTC')
  data.table(
    id = rep(c('a', 'b'), each = n),
    t_ = rep(t0 + (seq_len(n) - 1) * 7200, 2),
    x_ = c(500000 + cumsum(rep(100, n)), 600000 + cumsum(rep(-100, n))),
    y_ = rep(6700000, 2 * n),
    gps_dop = rep(c(2, 12, NA, 3, 4), length.out = 2 * n)
  )
}

# Extract every year of made-up steps, as the extracted branches would, rows in step order
extract_all_years <- function(steps, index, where = 'both') {
  pts <- make_step_points(steps)
  out <- rbindlist(purrr::map(split(pts, by = 'tar_group'), extract_year, index = index, where = where, crs = 'EPSG:3154'))
  out[order(row)]
}
