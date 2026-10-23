# Generated with Claude Opus 5.5

#' Add the thresholds for one QC test to long data
#'
#' Looks up the thresholds for each combination of `variable`, `sensor_key` and
#' `month` in `dat` with `qc_lookup_thresholds()`, and joins them.
#'
#' Rows whose key has no thresholds are kept, with `NA` in the threshold
#' columns. They will be assigned a QC flag 2 (Not Evaluated).
#'
#' @param dat Long data from `qc_core_prepare()`.
#' @param qc_test Character string. The QC test, as in `thresholds$qc_test`.
#' @param county Character string. One county, already checked by the exported
#'   wrapper (`assert_county()`, `assert_county_thresholds()`).
#' @param threshold_table Optional table of thresholds, passed to
#'   `qc_lookup_thresholds()`. Default `NULL` uses `qaqcmar::thresholds`.
#' @return `dat` with the same rows in the same order, plus the threshold
#'   columns of `qc_test` (e.g. `sensor_min`, `sensor_max`, `user_min`,
#'   `user_max` for gross range).
#'
#' @importFrom dplyr distinct left_join
#'
#' @noRd

qc_append_thresholds <- function(dat, qc_test, county, threshold_table = NULL) {
  keys <- distinct(dat, variable, sensor_key, month)

  thr <- qc_lookup_thresholds(
    keys,
    qc_test = qc_test,
    county = county,
    threshold_table = threshold_table
  )

  # many-to-one: errors if the lookup ever gives two rows for one key, which
  # would otherwise duplicate observations
  left_join(
    dat,
    thr,
    by = c("variable", "sensor_key", "month"),
    relationship = "many-to-one"
  )
}
