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
#'   Optional additional column(s) that is used to join with \code{dat}. This
#'   column must have the same name as the string \code{join_column}.
#'
#'   Default values are used if \code{spike_table = NULL}. To see the
#'   default \code{spike_table}, type \code{subset(thresholds,
#'   qc_test == "spike")} in the console.
#'
#' @param county Character string indicating the county from which \code{dat}
#'   was collected. Used to filter the default \code{spike_table}. Not
#'   required if there is a \code{county} column in \code{dat}.
#'
#' @param join_column Optional character string of a column name that is in both
#'   \code{dat} and \code{spike_table}. The specified column will be used to
#'   join the two tables. Default is \code{join_column = NULL}, and the tables
#'   are joined only on the \code{variable} columns (unless the variable is
#'   sensor_depth_at_low_tide_m, in which case the tables are also joined by the
#'   \code{sensor_type} column).
#'
#' @param keep_spike_cols  Logical value. If \code{TRUE}, the columns used to
#'   produce the spike value are returned in \code{dat}. Default is
#'   \code{FALSE}.
#'
#' @return Returns \code{dat} in a wide format, with spike flag columns for each
#'   variable in the form "spike_flag_variable".
#'
#' @family tests
#'
#' @importFrom dplyr %>% arrange case_when contains group_by if_else lag lead
#'   left_join mutate select
#' @importFrom lubridate day
#' @importFrom sensorstrings ss_pivot_longer
#' @importFrom stringr str_detect str_remove_all
#' @importFrom tidyr pivot_wider separate
#' @importFrom tidyselect where
#'
#' @export

qc_test_spike <- function(
    dat,
    county = NULL,
    join_column = NULL,
    spike_table = NULL,

    keep_spike_cols = FALSE
) {

  message("applying spike test")

  # check that not providing more than one county
  county <- assert_county(dat, county, "qc_test_spike()")

  # import default thresholds from internal data file
  if (is.null(spike_table)) {

    spike_table <- thresholds %>%
      filter(qc_test == "spike", county == !!county | is.na(county))

    # Inverness has separate thresholds for salinity compared to the other counties
    if (county == "Inverness") {
      spike_table <- spike_table %>%
        filter(
          !(is.na(county) &
              variable == "salinity_psu" &
              (threshold == "spike_low" | threshold == "spike_high"))
        )
    }

    spike_table <- spike_table %>%
      select(-c(qc_test, county, month)) %>%
      pivot_wider(values_from = "threshold_value", names_from = threshold)
  }

  # add thresholds to dat and assign flags ---------------------------------------------------
  dat <- ss_pivot_longer(dat)

  # find a work around for this
  if("sensor_depth_measured_m" %in% unique(dat$variable)) {

    if(!("sensor_type" %in% join_column)) {

      stop("spike_table must include column << sensor_type >> for variable
            << sensor_depth_measured_m >>")
    }
  }

  # join
  if(is.null(join_column)) {

    dat <- left_join(dat, select(spike_table, -sensor_type), by = "variable")

  } else if("sensor_type" %in% join_column) {

    dat1 <- dat %>%
      filter(variable == "sensor_depth_measured_m") %>%
      left_join(
        filter(spike_table, variable == "sensor_depth_measured_m"),
        by = c("variable", join_column)
      )

    join2 <- join_column[-which(join_column == "sensor_type")]

    dat2 <- dat %>%
      filter(variable != "sensor_depth_measured_m") %>%
      left_join(
        spike_table %>%
        filter(variable != "sensor_depth_measured_m") %>%
          select(-sensor_type),
        by = c("variable", join2)
      )

    dat <- bind_rows(dat1, dat2)

  } else {


    dat <- left_join(
      dat,
      select(spike_table, -sensor_type),
      by = c("variable", join_column)
    )
   #  browser()
  }

  dat <- dat %>%
    group_by(
      county, station, deployment_range, sensor_serial_number, variable
    ) %>%
    dplyr::arrange(timestamp_utc, .by_group = TRUE) %>%
    mutate(
      lag_value = lag(value),
      lead_value = lead(value),
      spike_ref = (lag_value + lead_value) / 2,
      spike_value = abs(value - spike_ref),
      spike_flag = case_when(
        spike_value > spike_high ~ 4,
        (spike_value <= spike_high & spike_value > spike_low) ~ 3,
        spike_value <= spike_low ~ 1,
        TRUE ~ 2
      ),
      spike_flag = ordered(spike_flag, levels = 1:4)
    ) %>%
    ungroup()

  if(isFALSE(keep_spike_cols)) {
    dat <- dat %>%
      select(-c(lag_value, lead_value,
                spike_ref, spike_value,
                spike_high, spike_low))
  }

  dat <- dat %>%
    pivot_wider(
      names_from = variable,
      values_from = c(value, spike_flag),
      names_sort = TRUE
    )

  colnames(dat) <- str_remove_all(colnames(dat), pattern = "value_")

  dat
}
