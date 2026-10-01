#' Apply multiple quality control tests
#'
#' @param dat Data frame of sensor string data in a wide format.
#'
#' @param qc_tests Character vector of quality control tests to apply to
#'   \code{dat}. Defaults to all available tests: \code{qc_tests =
#'   c("climatology", "depth_crosscheck", "grossrange", "rolling_sd", "spike")}.
#'
#' @param join_column_spike Optional character string of a column name that is in both
#'   \code{dat} and \code{spike_table}. The specified column will be used to
#'   join the two tables. Default is \code{join_column = NULL}, and the tables
#'   are joined only on the \code{sensor_type} and \code{variable} columns.
#'
#' @inheritParams qc_test_climatology
#' @inheritParams qc_test_depth_crosscheck
#' @inheritParams qc_test_grossrange
#' @inheritParams qc_test_rolling_sd
#' @inheritParams qc_test_spike
#'
#' @return Returns \code{dat} with additional quality control flag columns.
#'
#' @importFrom dplyr %>% arrange distinct left_join
#' @importFrom purrr reduce
#'
#' @export

qc_test_all <- function(
    dat,
    qc_tests = NULL,
    county = NULL,

    climatology_table = NULL,
    depth_table = NULL,
    grossrange_table = NULL,
    rolling_sd_table = NULL,
    spike_table = NULL,
    join_column = NULL,
    join_column_spike = NULL,

    period_hours = 24,
    max_interval_hours = 2,
    align_window = "center",
    keep_sd_cols = FALSE,
    keep_spike_cols = FALSE
) {

  if (is.null(qc_tests)) {
    qc_tests <- c("climatology", "depth_crosscheck",
                  "grossrange", "rolling_sd", "spike")
  }

  qc_tests <- tolower(qc_tests)

  # # remove columns that are all NA (because they will be dropped during testing,
  # # which causes a problem for the final join)
  # this doesn't work because lease columns gets dropped
  # dat <- dat %>% select_if(~ !all(is.na(.)))

  # # use for the join and to order columns in output
  depl_cols <- c(
    "county",
    "region",
    "waterbody",
    "station",
    "lease",
    "latitude" ,
    "longitude" ,
    "deployment_range"   ,
    "string_configuration",
    "sensor_type"     ,
    "sensor_serial_number"  ,
    "timestamp_utc"  ,
    "sensor_depth_at_low_tide_m",
    "depth_crosscheck_flag"
  )

  #  use for the join and to order columns in output
  var_cols <- dat %>%
    ss_pivot_longer() %>%
    distinct(variable) %>%
    arrange()
  var_cols <- var_cols$variable

  # apply tests
  dat_out <- list(NULL)

  if ("climatology" %in% qc_tests) {
    dat_out[[1]] <- qc_test_climatology(
      dat,
      climatology_table = climatology_table,
      county = county,
      join_column = join_column
    )
  }

  if ("depth_crosscheck" %in% qc_tests) {
    dat_out[[2]] <- qc_test_depth_crosscheck(
      dat,
      depth_table = depth_table,
      county = county
    )
  }

  if ("grossrange" %in% qc_tests) {
    dat_out[[3]] <- qc_test_grossrange(
      dat,
      grossrange_table = grossrange_table,
      county = county,
      join_column = join_column
    )
  }

  if ("rolling_sd" %in% qc_tests) {
    dat_out[[4]] <- qc_test_rolling_sd(
      dat,
      rolling_sd_table = rolling_sd_table,
      county = county,
      join_column = join_column,

      period_hours = period_hours,
      max_interval_hours = max_interval_hours,
      align_window = align_window,
      keep_sd_cols = keep_sd_cols
    )
  }

  if("spike" %in% qc_tests) {
    dat_out[[5]] <- qc_test_spike(
      dat,
      spike_table = spike_table,
      join_column = join_column_spike,
      keep_spike_cols = keep_spike_cols
    )
  }

  # remove empty list elements
  dat_out <- Filter(Negate(is.null), dat_out)

  # join results from each test
  join_cols <- c(
    depl_cols[which(depl_cols %in% colnames(dat))], var_cols)

  # join by all common columns
  dat_out <- dat_out %>%
    purrr::reduce(dplyr::left_join, by = join_cols)

  if("depth_crosscheck_flag" %in% colnames(dat_out)) {
    dat_out <- dat_out %>%
      relocate(depth_crosscheck_flag, .after = sensor_depth_at_low_tide_m)
  }

  dat_out
}

