#' Apply the depth crosscheck test
#'
#' Sensor depth at low tide is estimated from tide charts and the position of
#' each sensor on the string. The estimated depth is recorded in in the
#' deployment log and included in the compiled data in the column
#' \code{sensor_depth_at_low_tide_m}.
#'
#' Some sensors are capable of recording depth (pressure). When this is the
#' case, the minimum sensor depth recorded (i.e., low tide) is compared to the
#' estimated sensor depth at low tide. In the case of large discrepancies, the
#' depth_crosscheck_flag will indicate that the observation is "Suspect / Of
#' Interest". In many cases, the measured sensor depth should be considered more
#' accurate than the estimated depth; however, the measured depth should be
#' carefully evaluated.
#'
#' Note that all observations from a deployment will have the same
#' \code{depth_crosscheck_flag}. If there is more than one sensor on the string
#' that measures depth, the worst (highest) flag will be assigned to the
#' deployment. If the depth crosscheck test identifies the estimated depth as
#' "Suspect", the other estimated sensor depths should also be considered
#' suspect. For example, if the string was moored in an area 10 m deeper than
#' anticipated, all sensors will likely be 10 m deeper than recorded in the
#' \code{sensor_depth_at_low_tide_m} column.
#'
#' @param dat Data frame of sensor string data in a wide format.
#'
#' @param depth_table Data frame with 1 column: \code{depth_diff_max}.
#'
#'   Default values are used if \code{depth_table = NULL}. To see the
#'   default \code{depth__table}, type \code{subset(thresholds,
#'   qc_test == "depth_crosscheck")} in the console.
#'
#' @param county Character string indicating the county from which \code{dat}
#'   was collected. Used to filter the default \code{depth_crosscheck_table}.
#'   Not required if there is a \code{county} column in \code{dat} or if
#'   \code{depth_table} is provided.
#'
#' @param return_sensor_table Logical value. If \code{TRUE}, the table used to
#'   evaluate the difference between measured depth and estimated depth
#'   (\code{min_measured} and \code{abs_diff}, one row per sensor) is
#'   returned instead of \code{dat}. Sensors that did not measure depth get
#'   flag 2. Default is \code{FALSE}.
#'
#' @return Returns \code{dat} in a wide format, with depth crosscheck flag
#'   column named "depth_crosscheck_flag".
#'
#' @importFrom dplyr filter group_by join_by left_join mutate rename right_join select summarise ungroup
#' @importFrom tidyr pivot_longer pivot_wider
#'
#' @export

qc_test_depth_crosscheck <- function(
  dat,
  depth_table = NULL,
  county = NULL,
  return_sensor_table = FALSE
) {
  message("applying depth_crosscheck test")

  # check that not providing more than one county
  county <- assert_county(dat, county, "qc_test_depth_crosscheck()")
  dat <- qc_core_set_aside_flags(dat, "depth_crosscheck")

  if (isTRUE(return_sensor_table)) {
    if (is.null(depth_table)) {
      depth_diff_max <- qaqcmar::thresholds |>
        filter(qc_test == "depth_crosscheck") |>
        pull(threshold_value)
    } else {
      depth_diff_max <- depth_table$depth_diff_max
    }

    out <- dat$dat |>
      qc_core_prepare() |>
      qc_core_depth_crosscheck(depth_diff_max, return_sensor_table = TRUE) |>
      mutate(
        depth_crosscheck_flag = ordered(depth_crosscheck_flag, levels = 1:4)
      )

    return(out)
  }

  # if depth_table is NULL, qc_core_run() uses the default threshold
  dat$dat |>
    qc_core_run(
      qc_tests = "depth_crosscheck",
      county = county,
      depth_table = depth_table
    ) |>
    qc_core_to_wide(flags = dat$flags)
}
