#' Apply the climatology test
#'
#' @inheritParams qc_test_grossrange
#'
#' @param climatology_table Data frame with columns \code{variable},
#'   \code{month}: numeric values from 1 to 12 for each variable;
#'   \code{season_min}: minimum reasonable value for the corresponding variable
#'   and month; \code{season_max}: maximum reasonable value for the
#'   corresponding variable and month.
#'
#'   Default values are used if \code{climatology_table = NULL}. To see the
#'   default \code{climatology_table}, type \code{subset(thresholds,
#'   qc_test == "climatology")} in the console.
#'
#' @param county Character string indicating the county from which \code{dat}
#'   was collected. Used to filter the default \code{climatology_table}. Not
#'   required if there is a \code{county} column in \code{dat}.
#'
#' @return Returns \code{dat} in a wide format, with climatology flag columns
#'   for each variable in the form "climatology_flag_variable".
#'
#' @export

qc_test_climatology <- function(
  dat,
  climatology_table = NULL,
  county = NULL
) {
  message("applying climatology test")

  # check that not providing more than one county
  county <- assert_county(dat, county, "qc_test_climatology()")
  if (is.null(climatology_table)) {
    assert_county_thresholds(county, "climatology")
  }
  dat <- qc_core_set_aside_flags(dat, "climatology")

  # if climatology_table is NULL, qc_core_run() uses the default thresholds
  dat$dat |>
    qc_core_run(
      qc_tests = "climatology",
      county = county,
      climatology_table = climatology_table
    ) |>
    qc_core_to_wide(flags = dat$flags)
}
