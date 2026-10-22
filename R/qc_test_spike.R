#' Apply the spike test
#'
#' @param dat Data frame of sensor string data in wide format.
#'
#' @param spike_table Data frame with at least 3 columns: \code{variable}:
#'   should match the names of the variables being tested in \code{dat}.
#'   \code{spike_low}: maximum acceptable spike value to "Pass", and
#'   \code{spike_high}: maximum acceptable value to be flagged "Suspect/Of
#'   Interest". For variable = sensor_depth_measured_m, the column
#'   \code{sensor_type} is also required.
#'
#'   Default values are used if \code{spike_table = NULL}. To see the
#'   default \code{spike_table}, type \code{subset(thresholds,
#'   qc_test == "spike")} in the console.
#'
#' @param county Character string indicating the county from which \code{dat}
#'   was collected. Used to filter the default \code{spike_table}. Not
#'   required if there is a \code{county} column in \code{dat}.
#'
#' @param max_interval_hours Maximum accepted interval (hours) between two
#'   observations. The observations just before and after a longer gap get
#'   flag 2, because the observation across the gap is not a fair reference
#'   for the spike value. Data sampled less often than this get flag 2
#'   throughout. Default is \code{max_interval_hours = 2}.
#'
#' @param keep_spike_cols  Logical value. If \code{TRUE}, the columns used to
#'   produce the spike value are returned in \code{dat}. Default is
#'   \code{FALSE}.
#'
#' @return Returns \code{dat} in a wide format, with spike flag columns for each
#'   variable in the form "spike_flag_variable". Variables with no observations in `dat` are dropped.
#'
#' @export

qc_test_spike <- function(
  dat,
  county = NULL,
  max_interval_hours = 2,
  spike_table = NULL,
  keep_spike_cols = FALSE
) {
  message("applying spike test")

  # check that not providing more than one county
  county <- assert_county(dat, county, "qc_test_spike()")
  if (is.null(spike_table)) {
    assert_county_thresholds(county, "spike")
  }
  dat <- qc_core_set_aside_flags(dat, "spike")

  # if spike_table is NULL, qc_core_run() uses the default thresholds
  dat$dat |>
    qc_core_run(
      qc_tests = "spike",
      county = county,
      max_interval_hours = max_interval_hours,
      spike_table = spike_table,
      keep_spike_cols = keep_spike_cols
    ) |>
    qc_core_to_wide(flags = dat$flags)
}
