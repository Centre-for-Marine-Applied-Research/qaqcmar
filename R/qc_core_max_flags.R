# Generated with Claude Opus 5.5

#' Add the maximum flag to long data
#'
#' For each observation, `qc_flag` is the worst (highest) flag across the test
#' flag columns in `dat`. `depth_crosscheck_flag` is not included. `NA` flags
#' are ignored; an observation with only `NA` flags gets `NA`.
#'
#' @param dat Long data with at least one `<test>_flag` column, e.g. from
#'   `qc_core_grossrange()`.
#'
#' @return `dat` with the same rows in the same order, plus `qc_flag` (integer,
#'   1 to 4).
#'
#' @importFrom dplyr mutate
#'
#' @noRd
qc_core_max_flags <- function(dat) {
  flag_cols <- grep("_flag$", colnames(dat), value = TRUE)
  # ignore the depth_cross_flag column
  flag_cols <- setdiff(flag_cols, c("depth_crosscheck_flag", "qc_flag"))

  if (length(flag_cols) == 0) {
    stop("<< dat >> has no flag columns.", call. = FALSE)
  }

  # pmax() takes the flag columns as separate arguments, so do.call() passes
  # each column of dat[flag_cols] as one argument
  mutate(dat, qc_flag = do.call(pmax, c(dat[flag_cols], na.rm = TRUE)))
}
