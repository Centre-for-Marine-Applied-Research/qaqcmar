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
#' @param crosscheck_table Logical value. If \code{TRUE}, the table used to
#'   evaluate the difference between measured depth and estimated depth
#'   (\code{min_measured} and \code{abs_diff}) is returned instead of
#'   \code{dat}. Default is \code{FALSE}.
#'
#' @return Returns \code{dat} in a wide format, with depth crosscheck flag
#'   column named "depth_crosscheck_flag_value".
#'
#' @importFrom dplyr filter group_by join_by left_join mutate rename right_join select summarise ungroup
#' @importFrom tidyr pivot_longer pivot_wider
#'
#' @export

qc_test_depth_crosscheck <- function(
  dat,
  depth_table = NULL,
  county = NULL,
  crosscheck_table = FALSE
) {
  message("applying depth_crosscheck test")

  # check that not providing more than one county
  county <- assert_county(dat, county, "qc_test_depth_crosscheck()")

  # import default thresholds from internal data file -----------------------
  if (is.null(depth_table)) {
    depth_table <- thresholds |>
      filter(
        qc_test == "depth_crosscheck",
        county == !!county | is.na(county)
      ) |>
      select(-c(qc_test, county, month, sensor_type)) |>
      pivot_wider(values_from = "threshold_value", names_from = threshold)
  }

  # add thresholds to dat and assign flags ---------------------------------------------------

  # create a table with the estimated (log) depth and minimum measured depth
  # for each deployment
  # apply flag
  dat_depth <- dat |>
    filter(!is.na(sensor_depth_measured_m)) |>
    group_by(
      county,
      station,
      deployment_range,
      sensor_serial_number,
      sensor_depth_at_low_tide_m
    ) |>
    summarise(min_measured = min(sensor_depth_measured_m)) |>
    ungroup() |>
    # make sure there is a row for every station, deployment, sensor
    right_join(
      dat |>
        distinct(
          county,
          station,
          deployment_range,
          sensor_serial_number,
          sensor_depth_at_low_tide_m
        ),
      by = join_by(
        county,
        station,
        deployment_range,
        sensor_serial_number,
        sensor_depth_at_low_tide_m
      )
    ) |>
    # calculate difference and apply preliminary flag
    mutate(
      abs_diff = abs(sensor_depth_at_low_tide_m - min_measured),

      depth_diff_max = depth_table$depth_diff_max,
      depth_crosscheck_flag = case_when(
        abs_diff > depth_diff_max ~ 3,
        abs_diff <= depth_diff_max ~ 1,
        # for sensors without measured depth, set flag to 0 for now so that these
        # deployments are not counted in the max() below
        # will be converted to 2 before export
        is.na(abs_diff) ~ 0
      )
    )

  # find the worst flag for each deployment (not counting "Not Evaluated")
  dat_depth2 <- dat_depth |>
    group_by(county, station, deployment_range) |>
    summarise(depth_crosscheck_flag = max(depth_crosscheck_flag)) |>
    ungroup()

  # join flags to data and convert 0 to 2
  dat <- dat |>
    left_join(dat_depth2, by = join_by(county, station, deployment_range)) |>
    mutate(
      depth_crosscheck_flag = if_else(
        depth_crosscheck_flag == 0,
        2,
        depth_crosscheck_flag
      ),
      depth_crosscheck_flag = ordered(depth_crosscheck_flag, levels = 1:4)
    )

  # clean up columns --------------------------------------------------------

  if (isTRUE(crosscheck_table)) {
    dat_depth |>
      mutate(
        depth_crosscheck_flag = if_else(
          depth_crosscheck_flag == 0,
          2,
          depth_crosscheck_flag
        ),
        depth_crosscheck_flag = ordered(depth_crosscheck_flag, levels = 1:4)
      )
  } else {
    dat
  }
}
