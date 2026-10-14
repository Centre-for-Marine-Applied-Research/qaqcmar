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
#' @param grossrange_table Data frame with at least 7 columns: \code{variable}:
#'   entries must match the names of the variables being tested in \code{dat}.
#'   \code{sensor_type}: type of sensor that recorded the observation;
#'   \code{sensor_min}: minimum value the sensor can record; \code{sensor_max}:
#'   maximum value the sensor can record. \code{user_min}: minimum reasonable
#'   value; \code{user_max}: maximum reasonable value. Optional additional
#'   column(s) that is used to join with \code{dat}. This column must have the
#'   same name as the string \code{join_column}.
#'
#'   Default values are used if \code{grossrange_table = NULL}. To see the
#'   default \code{grossrange_table}, type \code{subset(thresholds,
#'   qc_test == "grossrange")} in the console.
#'
#' @param county Character string indicating the county from which \code{dat}
#'   was collected. Used to filter the default \code{grossrange_table}. Not
#'   required if there is a \code{county} column in \code{dat}.
#'
#' @param join_column Optional character string of a column name that is in both
#'   \code{dat} and \code{grossrange_table}. The specified column will be used
#'   to join the two tables. Default is \code{join_column = NULL}, and the
#'   tables are joined only on the \code{sensor_type} and \code{variable}
#'   columns.
#'
#' @return Returns \code{dat} in a wide format, with grossrange flag columns for
#'   each variable in the form "grossrange_flag_variable".
#'
#' @family tests
#'
#' @importFrom dplyr case_when contains distinct inner_join left_join mutate select
#' @importFrom sensorstrings ss_pivot_longer
#' @importFrom stringr str_detect str_remove str_remove_all
#' @importFrom tidyr pivot_wider separate
#'
#' @export

qc_test_grossrange <- function(
    dat,
    grossrange_table = NULL,
    county = NULL,
    join_column = NULL) {

  message("applying grossrange test")

  # check that not providing more than one county
  county <- assert_county(dat, county, "qc_test_grossrange()")

  # import default thresholds from internal data file -----------------------
  if (is.null(grossrange_table)) {

    grossrange_table <- thresholds |>
      filter(qc_test == "grossrange", county == !!county | is.na(county))

    # Inverness has separate thresholds for salinity compared to the other counties
    if (county == "Inverness") {
      grossrange_table <- grossrange_table |>
        filter(
          !(is.na(county) &
          variable == "salinity_psu" &
          (threshold == "user_min" | threshold == "user_max"))
        )
    }

    grossrange_table <- grossrange_table |>
      select(-c(qc_test, county, month)) |>
      pivot_wider(values_from = "threshold_value", names_from = threshold)

    # reformat
    user_thresh <- grossrange_table |>
      select(variable, contains("user")) |>
      filter(!is.na(user_min) & !is.na(user_max))

    sensor_thresh <- grossrange_table |>
      select(sensor_type, variable, contains("sensor")) |>
      filter(!is.na(sensor_type))

    grossrange_table <- sensor_thresh |>
      inner_join(user_thresh, by = "variable")
  }

  # variables in dat (could also pivot then use unique())
  dat_vars <- dat |>
    select(
      contains("depth_measured"),
      contains("dissolved_oxygen"),
      contains("ph"),
      contains("salinity"),
      contains("temperature")
    ) |>
    colnames()


  # if a hobo do sensor was used, change sensor_type from hobo to hobo do
  # because the temperature grossrange is different
  if ("dissolved_oxygen_uncorrected_mg_per_l" %in% dat_vars) {
    dat <- dat |>
      mutate(
        sensor_type = case_when(
          sensor_type == "hobo" &
            !is.na(dissolved_oxygen_uncorrected_mg_per_l) ~ "hobo do",
          TRUE ~ sensor_type
        )
      )
  }

  # add thresholds to dat and assign flags ---------------------------------------------------
  dat <- ss_pivot_longer(dat)

  if(is.null(join_column)) {
    dat <- left_join(dat, grossrange_table, by = c("sensor_type", "variable"))
  } else {
    dat <- left_join(dat, grossrange_table, by = c("sensor_type", "variable", join_column))
  }

  dat <- dat |>
    # sensor_max and sensor_min get evaluated first, so don't need to worry
    # about if user_min < sensor_min (it will get assigned a flag of 4)
    mutate(
      grossrange_flag = case_when(
        value > sensor_max | value < sensor_min ~ 4,
        (value <= sensor_max & value > user_max) |
          (value >= sensor_min & value < user_min) ~ 3,
        value <= user_max | value >= user_min ~ 1,
        TRUE ~ 2
      ),
      grossrange_flag = ordered(grossrange_flag, levels = 1:4),
      sensor_type = if_else(sensor_type == "hobo do", "hobo", sensor_type)
    ) |>
    # remove extra columns
    select(-c(sensor_max, sensor_min, user_max, user_min)) |>
    pivot_wider(
      names_from = variable,
      values_from = c(value, grossrange_flag),
      names_sort = TRUE
    )

  colnames(dat) <- str_remove_all(colnames(dat), pattern = "value_")

  dat
}
