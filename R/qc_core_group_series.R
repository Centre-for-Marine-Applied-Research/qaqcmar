# Generated with Claude Opus 5.5

#' Group long data by series and sort each series by time
#'
#' A series is one sensor and variable within one deployment. Used by the
#' time-series tests (spike, rolling SD) before they compute anything.
#'
#' @param dat Long data from `qc_core_prepare()`.
#'
#' @return `dat` grouped by the series columns that are present
#'   (`county`, `region`, `station`, `deployment_range`,
#'   `sensor_serial_number`) plus `variable`, and sorted by `timestamp_utc`,
#'   then `row_id`, within each series.
#'
#' @importFrom dplyr across any_of arrange group_by
#'
#' @noRd
qc_core_group_series <- function(dat) {
  series_cols <- c(
    "county",
    "region",
    "station",
    "deployment_range",
    "sensor_serial_number"
  )

  dat |>
    group_by(across(any_of(c(series_cols, "variable")))) |>
    arrange(timestamp_utc, row_id, .by_group = TRUE)
}
