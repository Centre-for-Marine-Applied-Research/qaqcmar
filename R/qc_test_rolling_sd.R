#' Apply the rolling standard deviation test
#'
#' The rolling standard deviation is calculated using the following algorithm:
#'
#' 1. Calculate the interval between the current and previous observation
#' (\code{int_sample}; in minutes).
#'
#' 2. Determine the number of samples in the specified time frame
#' (\code{n_sample = round((60 / int_sample) * period_hours)})
#'
#' 3. If \code{int_sample} is really large (> \code{max_interval_hours}), then
#' set the effective sample number (\code{n_sample_effective}) to 0.
#'
#' 4. Calculate the rolling standard deviation, with the width =
#' \code{n_sample_effective} and alignment specified by \code{align_window}.
#' \code{sd_roll} is rounded to 2 decimal places.
#'
#'
#' When int_sample = period_hours, n_sample = 1, so sd_roll = NA.
#'
#' @param dat Data frame of sensor string data in wide format.
#'
#' @param rolling_sd_table Data frame with at least two columns:
#'   \code{variable}: must match the names of the variables being tested in
#'   \code{dat}. \code{rolling_sd_max}: maximum accepted value for the rolling
#'   standard deviation.
#'
#'   Default values are used if \code{rolling_sd_table = NULL}. To see the
#'   default \code{rolling_sd_table}, type \code{subset(thresholds,
#'   qc_test == "rolling_sd")} in the console.
#'
#' @param county Character string indicating the county from which \code{dat}
#'   was collected. Used to filter the default \code{rolling_sd_table}. Not
#'   required if there is a \code{county} column in \code{dat}.
#'
#' @param period_hours Length of a full cycle in hours. Default assumes a daily
#'   cycle of \code{period_hours = 24}.
#'
#' @param max_interval_hours Maximum accepted interval between two observations.
#'   If the interval between two observations is greater than this value, the
#'   rolling standard deviation will be set to \code{NA}. This is important
#'   because for large intervals, the number of observations in
#'   \code{period_hours} will be small. For example, if samples are collected
#'   every 6 hours, only 4 observations would be used to calculate
#'   \code{roll_sd}.
#'
#' @param align_window Alignment for the window used to calculate the rolling
#'   standard deviation. Passed to \code{zoo::rollapply()}. Default is
#'   \code{align_window = "center"}. Other options are \code{"right"} (backward
#'   window) and \code{"left"} (forward window).
#'
#' @param keep_sd_cols Logical value. If \code{TRUE}, the columns used to
#'   produce the rolling standard deviation (\code{int_sample}, \code{n_sample},
#'   and \code{sd_roll}) are returned in \code{dat}. Default is \code{FALSE}.
#'
#' @return Returns \code{dat} in a wide format, with rolling standard deviation
#'   flag columns for each variable in the form "rolling_sd_flag_variable".
#'
#' @export

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
      rolling_sd_table = rolling_sd_table
    ) |>
    qc_core_to_wide(flags = dat$flags)
}
