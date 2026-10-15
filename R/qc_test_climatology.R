#' Apply the climatology test
#'
#' @inheritParams qc_test_grossrange
#'
#' @param climatology_table Data frame with at least 4 columns: \code{variable}:
#'   must match the names of the variables being tested in \code{dat};
#'   \code{month}: numeric values from 1 to 12 for each variable;
#'   \code{season_min}: minimum reasonable value for the corresponding variable
#'   and month; \code{season_max}: maximum reasonable value for the
#'   corresponding variable and month. Optional additional column(s) that is
#'   used to join with \code{dat}. This column must have the same name as the
#'   string \code{join_column}.
#'
#'   Default values are used if \code{climatology_table = NULL}. To see the
#'   default \code{climatology_table}, type \code{subset(thresholds,
#'   qc_test == "climatology")} in the console.
#'
#' @param county Character string indicating the county from which \code{dat}
#'   was collected. Used to filter the default \code{climatology_table}. Not
#'   required if there is a \code{county} column in \code{dat}.
#'
#' @param join_column Optional character string of a column name that is in both
#'   \code{dat} and \code{climatology_table}. The specified column will be used
#'   to join the two tables. Default is \code{join_column = NULL}, and the
#'   tables are joined only on the \code{month} and \code{variable} columns.
#'
#' @return Returns \code{dat} in a wide format, with climatology flag columns
#'   for each variable in the form "climatology_flag_variable".
#'
#' @family tests
#'
#' @importFrom dplyr as_tibble bind_rows case_when left_join mutate rename tibble
#' @importFrom lubridate month parse_date_time
#' @importFrom stringr str_detect str_remove_all str_replace
#' @importFrom tidyr pivot_wider
#'
#' @export

qc_test_climatology <- function(
  dat,
  climatology_table = NULL,
  join_column = NULL,
  county = NULL
) {
  message("applying climatology test")

  # check that not providing more than one county
  county <- assert_county(dat, county, "qc_test_climatology()")

  # import default thresholds from internal data file & format
  if (is.null(climatology_table)) {
    assert_county_thresholds(county, "climatology")

    climatology_table <- thresholds |>
      filter(qc_test == "climatology", county == !!county | is.na(county))

    # Inverness has separate thresholds for salinity compared to the other counties
    if (county == "Inverness") {
      climatology_table <- climatology_table |>
        filter(
          !(is.na(county) &
            variable == "salinity_psu" &
            (threshold == "season_min" | threshold == "season_max"))
        )
    }

    climatology_table <- climatology_table |>
      select(-c(qc_test, county, sensor_type)) |>
      tidyr::pivot_wider(
        names_from = "threshold",
        values_from = "threshold_value"
      )
  }

  colname_ts <- colnames(dat)[which(str_detect(colnames(dat), "timestamp"))]

  # add thresholds to dat and assign flags ---------------------------------------------------
  dat <- ss_pivot_longer(dat) |>
    rename(tstamp = contains("timestamp")) |>
    mutate(month = lubridate::month(tstamp))

  if (is.null(join_column)) {
    dat <- left_join(dat, climatology_table, by = c("month", "variable"))
  } else {
    dat <- left_join(
      dat,
      climatology_table,
      by = c("month", "variable", join_column)
    )
  }

  dat <- dat |>
    mutate(
      climatology_flag = case_when(
        value > season_max | value < season_min ~ 3,
        value <= season_max & value >= season_min ~ 1,
        TRUE ~ 2
      ),
      climatology_flag = ordered(climatology_flag, levels = 1:4)
    ) |>
    # remove extra columns
    select(-c(season_min, season_max, month)) |>
    pivot_wider(
      names_from = variable,
      values_from = c(value, climatology_flag),
      names_sort = TRUE
    )

  colnames(dat)[which(colnames(dat) == "tstamp")] <- colname_ts
  colnames(dat) <- str_remove_all(colnames(dat), pattern = "value_")

  dat
}
