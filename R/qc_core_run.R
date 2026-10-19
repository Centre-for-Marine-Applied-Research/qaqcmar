# Generated with Claude Opus 5.5

#' Run QC tests on wide data and return long data
#'
#' Converts `dat` to long format once, then for each test
#' in `qc_tests` adds its thresholds and its flag column to the same long
#' table. There are no joins between tests.
#'
#' @param dat Wide data without flag columns (e.g. `dat` from
#'   `qc_core_set_aside_flags()`).
#' @param qc_tests Character vector of tests to apply: any of "climatology",
#'   "depth_crosscheck", "grossrange", "rolling_sd" and "spike".
#' @param county Character string. One county, already checked by the
#'   exported wrapper.
#' @param period_hours,max_interval_hours,align_window,keep_sd_cols Passed to
#'   `qc_core_rolling_sd()`.
#' @param keep_spike_cols Passed to `qc_core_spike()`.
#' @param max_flag Logical. If `TRUE`, adds `qc_flag` with
#'   `qc_core_max_flags()`.
#'
#' @return Long data: `dat` from `qc_core_prepare()`, plus one `<test>_flag`
#'   column per test (and `qc_flag` if `max_flag = TRUE`). Threshold columns
#'   are removed.
#'
#' @importFrom dplyr any_of filter pull select
#'
#' @noRd
qc_core_run <- function(
  dat,
  qc_tests,
  county,
  period_hours = 24,
  max_interval_hours = 2,
  align_window = "center",
  keep_sd_cols = FALSE,
  keep_spike_cols = FALSE,
  max_flag = FALSE
) {
  qc_tests <- match.arg(
    qc_tests,
    c("climatology", "depth_crosscheck", "grossrange", "rolling_sd", "spike"),
    several.ok = TRUE
  )

  dat <- qc_core_prepare(dat)

  if ("climatology" %in% qc_tests) {
    dat <- dat |>
      qc_append_thresholds("climatology", county) |>
      qc_core_climatology()
  }

  if ("depth_crosscheck" %in% qc_tests) {
    depth_diff_max <- qaqcmar::thresholds |>
      filter(qc_test == "depth_crosscheck") |>
      pull(threshold_value)

    dat <- qc_core_depth_crosscheck(dat, depth_diff_max = depth_diff_max)
  }

  if ("grossrange" %in% qc_tests) {
    dat <- dat |>
      qc_append_thresholds("grossrange", county) |>
      qc_core_grossrange()
  }

  if ("rolling_sd" %in% qc_tests) {
    dat <- dat |>
      qc_append_thresholds("rolling_sd", county) |>
      qc_core_rolling_sd(
        period_hours = period_hours,
        max_interval_hours = max_interval_hours,
        align_window = align_window,
        keep_sd_cols = keep_sd_cols
      )
  }

  if ("spike" %in% qc_tests) {
    dat <- dat |>
      qc_append_thresholds("spike", county) |>
      qc_core_spike(keep_spike_cols = keep_spike_cols)
  }

  if (isTRUE(max_flag)) {
    dat <- qc_core_max_flags(dat)
  }

  # remove threshold columns
  select(
    dat,
    -any_of(c(
      "season_min",
      "season_max",
      "sensor_min",
      "sensor_max",
      "user_min",
      "user_max",
      "rolling_sd_max",
      "spike_low",
      "spike_high"
    ))
  )
}
