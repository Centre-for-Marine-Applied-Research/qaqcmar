#' Apply the rolling standard deviation test
#'
#' The rolling standard deviation is calculated using the following algorithm:
#'
#' 1. Calculate the interval between the current and previous observation
#' (`int_sample``; in minutes).
#'
#' 2. Determine the number of samples in the specified time frame (`n_sample =
#' round((60 / int_sample) * period_hours)`)
#'
#' 3. If `int_sample` is really large (> `max_interval_hours`), or there are
#' duplicate timestamps (`int_sample = 0`), then set the effective sample number
#' (`n_sample_effective`) to 0. Assign any observation with `n_sample_effective
#' = 0` a flag of 2.
#'
#' 4. Calculate the rolling standard deviation, with the width =
#' `n_sample_effective` and alignment specified by `align_window`. `sd_roll` is
#' rounded to 2 decimal places. Rolling standard deviation is calculated from a
#' running-sum calculation in `qc_window_sd()`
#'
#' @param dat Data frame of sensor string data in wide format.
#'
#' @param rolling_sd_table Data frame with at least two columns: `variable`:
#'   must match the names of the variables being tested in `dat`.
#'   `rolling_sd_max`: maximum accepted value for the rolling standard
#'   deviation.
#'
#'   Default values are used if `rolling_sd_table = NULL`. To see the default
#'   `rolling_sd_table`, type `subset(thresholds, qc_test == "rolling_sd")` in
#'   the console.
#'
#' @param county Character string indicating the county from which `dat` was
#'   collected. Used to filter the default `rolling_sd_table`. Not required if
#'   there is a `county` column in `dat`.
#'
#' @param period_hours Length of a full cycle in hours. Default assumes a daily
#'   cycle of `period_hours = 24`.
#'
#' @param max_interval_hours Maximum accepted interval between two observations.
#'   If the interval between two observations is greater than this value, the
#'   rolling standard deviation will be set to `NA`. This is important because
#'   for large intervals, the number of observations in `period_hours` will be
#'   small. For example, if samples are collected every 6 hours, only 4
#'   observations would be used to calculate `roll_sd`.
#'
#' @param align_window Alignment for the window used to calculate the rolling
#'   standard deviation. Default is `align_window = "center"`. Other options are
#'   `"right"` (backward window) and `"left"` (forward window).
#'
#' @param keep_sd_cols Logical value. If `TRUE`, the columns used to produce the
#'   rolling standard deviation (`int_sample`, `n_sample`, and `sd_roll`) are
#'   returned in `dat`. Default is `FALSE`.
#'
#' @return Returns `dat` in a wide format, with rolling standard deviation flag
#'   columns for each variable in the form "rolling_sd_flag_variable". Variables
#'   with no observations in `dat` are dropped.
#'
#' @export
#' @examples
#' path <- system.file("testdata", package = "qaqcmar")
#' dat <- readRDS(file.path(path, "test_data_rolling_sd.RDS"))
#'
#' dat_sd <- qc_test_rolling_sd(dat)
#' table(dat_sd$rolling_sd_flag_dissolved_oxygen_percent_saturation)
#'
#' # keep the columns used to calculate the rolling standard deviation
#' dat_sd <- qc_test_rolling_sd(dat, keep_sd_cols = TRUE)
#' colnames(dat_sd)

qc_test_rolling_sd <- function(
  dat,
  rolling_sd_table = NULL,
  county = NULL,
  period_hours = 24,
  max_interval_hours = 2,
  align_window = "center",
  keep_sd_cols = FALSE
) {
  message("applying rolling_sd test. This could take several minutes")

  # check that not providing more than one county
  county <- assert_county(dat, county, "qc_test_rolling_sd()")
  if (is.null(rolling_sd_table)) {
    assert_county_thresholds(county, "rolling_sd")
  }
  dat <- qc_core_set_aside_flags(dat, "rolling_sd")

  # if rolling_sd_table is NULL, qc_core_run() uses the default thresholds
  dat$dat |>
    qc_core_run(
      qc_tests = "rolling_sd",
      county = county,
      rolling_sd_table = rolling_sd_table,
      period_hours = period_hours,
      max_interval_hours = max_interval_hours,
      align_window = align_window,
      keep_sd_cols = keep_sd_cols
    ) |>
    qc_core_to_wide(flags = dat$flags)
}
