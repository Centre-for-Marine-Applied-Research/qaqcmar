#' Apply the gross range test
#'
#' The variable value must be greater than the threshold to trigger flag (e.g.,
#' value > sensor_max is assigned a flag of 4; value == sensor_max is assigned a
#' flag of 3).
#'
#' The sensor_type is reassigned as "hobo do" if sensor_type = hobo and
#' do_concentration is not \code{NA} so that the data can be joined with the
#' \code{grossrange_table} by \code{sensor_type} and \code{variable}.
#'
#' @param dat Data frame of sensor string data in wide format.
#'
#' @param grossrange_table Data frame with columns \code{variable},
#'   \code{sensor_type}, \code{sensor_min}, \code{sensor_max}, \code{user_min}
#'   and \code{user_max}. An \code{NA} in \code{sensor_type} means the row
#'   applies to any sensor. Use \code{sensor_type = "hobo do"} for hobo sensors
#'   that record dissolved oxygen.
#'
#'   Default values are used if \code{grossrange_table = NULL}. To see the
#'   default \code{grossrange_table}, type \code{subset(thresholds,
#'   qc_test == "grossrange")} in the console.
#'
#' @param county Character string indicating the county from which \code{dat}
#'   was collected. Used to filter the default \code{grossrange_table}. Not
#'   required if there is a \code{county} column in \code{dat}.
#'
#' @return Returns \code{dat} in a wide format, with grossrange flag columns for
#'   each variable in the form "grossrange_flag_variable".
#'
#' @family tests
#'
#' @importFrom dplyr case_when contains distinct inner_join left_join mutate select
#' @importFrom sensorstrings ss_pivot_longer
#' @importFrom stringr str_remove str_remove_all
#' @importFrom tidyr pivot_wider
#'
#' @export
#'
#' @examples
#' path <- system.file("testdata", package = "qaqcmar")
#' dat <- readRDS(file.path(path, "test_data_grossrange.RDS"))
#'
#' # default thresholds for the county in dat
#' dat_gr <- qc_test_grossrange(dat)
#' head(dat_gr)
#'
#' # user thresholds: one row for any sensor type and one for vr2ar sensors.
#' # Variables that are not in the table (here, dissolved oxygen) get flag 2.
#' my_table <- data.frame(
#'   variable = "temperature_degree_c",
#'   sensor_type = c(NA, "vr2ar"),
#'   sensor_min = c(-5, -5),
#'   sensor_max = c(35, 40),
#'   user_min = c(-1, 0),
#'   user_max = c(25, 20)
#' )
#'
#' dat_user <- qc_test_grossrange(dat, grossrange_table = my_table)
#' head(dat_user)
#'

qc_test_grossrange <- function(
  dat,
  grossrange_table = NULL,
  county = NULL
) {
  message("applying grossrange test")

  # check that not providing more than one county
  county <- assert_county(dat, county, "qc_test_grossrange()")
  if (is.null(grossrange_table)) {
    assert_county_thresholds(county, "grossrange")
  }
  dat <- qc_core_set_aside_flags(dat, "grossrange")

  # if grossrange_table is NULL, qc_core_run() uses the default thresholds
  dat$dat |>
    qc_core_run(
      qc_tests = "grossrange",
      county = county,
      grossrange_table = grossrange_table
    ) |>
    qc_core_to_wide(flags = dat$flags)
}
