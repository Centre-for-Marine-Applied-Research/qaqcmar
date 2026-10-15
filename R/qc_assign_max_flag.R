#' Assign each observation the maximum flag from applied QC tests.
#'
#' \code{depth_crosscheck_flag} is not considered when evaluating the maximum
#' flag value.  \code{depth_crosscheck_flag} remains its own column with a
#' single value for the whole deployment.
#'
#' @param dat Data frame in long or wide format with flag columns from multiple
#'   quality control tests.
#'
#' @param qc_tests Quality control tests included in \code{dat}. Default is
#'   \code{qc_tests = c("climatology", "grossrange", "rolling_sd", "spike")}.

#' @param return_all Logical value indicating whether to return all quality
#'   control flag columns or only the summary columns. If \code{TRUE}, all flag
#'   columns will be returned. If \code{FALSE}, only the summary columns will be
#'   returned. Default is \code{TRUE}.
#'
#' @return Returns \code{dat} in a wide format, with a single flag column for
#'   each variable column.
#'
#' @importFrom dplyr all_of any_of contains everything left_join mutate rename select
#' @importFrom purrr pmap
#' @importFrom stringr str_remove_all
#' @importFrom tidyr pivot_wider
#'
#' @export

qc_assign_max_flag <- function(dat, qc_tests = NULL, return_all = TRUE) {
  if (is.null(qc_tests)) {
    qc_tests <- c("climatology", "grossrange", "rolling_sd", "spike")
  }

  # use for the join and to order columns in output
  depl_cols <- c(
    "county",
    "waterbody",
    "station",
    "lease",
    "latitude",
    "longitude",
    "deployment_range",
    "string_configuration",
    "sensor_type",
    "sensor_serial_number",
    "timestamp_utc",
    "sensor_depth_at_low_tide_m",
    "depth_crosscheck_flag",
    "hil_comment"
  )

  qc_test_cols <- thresholds |>
    select(qc_test, variable) |>
    distinct() |>
    mutate(col_name = paste(qc_test, "flag", variable, sep = "_")) |>
    arrange(qc_test)
  qc_test_cols <- qc_test_cols$col_name

  qc_test_cols <- sort(c(
    qc_test_cols,
    "human_in_loop_flag_dissolved_oxygen_percent_saturation",
    "human_in_loop_flag_dissolved_oxygen_uncorrected_mg_per_l",
    "human_in_loop_flag_salinity_psu",
    "human_in_loop_flag_sensor_depth_measured_m",
    "human_in_loop_flag_temperature_degree_c"
  ))

  qc_max_cols <- c(
    "qc_flag_dissolved_oxygen_percent_saturation",
    "qc_flag_dissolved_oxygen_uncorrected_mg_per_l",
    "qc_flag_salinity_psu",
    "qc_flag_sensor_depth_measured_m",
    "qc_flag_temperature_degree_c"
  )

  # save the original data frame to join the qc_flag columns ti
  if ("variable" %in% colnames(dat)) {
    dat_og <- dat |> qc_pivot_wider()
  } else {
    dat_og <- dat
  }

  # pivot dat
  if (!("variable" %in% colnames(dat))) {
    dat <- qc_pivot_longer(dat, qc_tests = qc_tests)
  }

  # use to join and sort the columns of the output
  var_cols <- sort(unique(dat$variable))

  # rename columns so depth_crosscheck_test doesn't affect max_flag
  if ("depth_crosscheck_flag" %in% colnames(dat)) {
    dat <- rename(dat, depth_crosscheck = depth_crosscheck_flag)
  }

  # find the maximum flag for each variable
  dat <- dat |>
    mutate(
      qc_col = pmap(select(dat, contains("flag")), max, na.rm = TRUE),
      qc_col = unlist(qc_col),
      qc_col = ordered(qc_col, levels = 1:4)
    ) |>
    select(-contains("flag_value")) |>
    rename(qc_flag = qc_col) |>
    pivot_wider(
      names_from = variable,
      values_from = c(value, qc_flag)
    )

  if ("depth_crosscheck" %in% colnames(dat)) {
    dat <- rename(dat, depth_crosscheck_flag = depth_crosscheck)
  }

  colnames(dat) <- str_remove_all(colnames(dat), pattern = "value_")

  if (isTRUE(return_all)) {
    join_cols <- c(
      depl_cols[which(depl_cols %in% colnames(dat_og))],
      var_cols
    )

    dat <- dat_og |>
      left_join(dat, by = join_cols) |>
      select(
        any_of(depl_cols), # deployment columns
        all_of(var_cols), # variable values
        any_of(qc_test_cols), # qc flags
        any_of(qc_max_cols), # max qc flags
        everything() # anything left
      )
  } else {
    dat <- dat |>
      select(
        any_of(depl_cols), # deployment columns
        all_of(var_cols), # variable values
        any_of(qc_max_cols), # max qc flags
        everything() # anything left
      )
  }

  dat
}
