#' Apply human in the loop flags and comments
#'
#' Only deployments that require additional human-in-loop QC will have non-NA
#' values in the human-in-loop columns. Observations in these deployments that
#' do not need additional QC will be assigned a \code{human_in_loop_flag_value}
#' of 1.
#'
#' Flags are assigned to each observation. For example, if a sensor measures
#' dissolved oxygen and temperature at the same time, and only the dissolved
#' oxygen observation is poor quality, then only the dissolved oxygen
#' observation will be flagged. The column
#' \code{human_in_loop_flag_value_dissolved_oxygen} will have a value of 3 or 4
#' (as specified in human_in_loop_table), while
#' \code{human_in_loop_flag_value_tempearture} will be 1.
#'
#' However, there is only one column for comments, \code{hil_comment}. This is
#' to reduce the total number of columns in the wide data set. The trade off is
#' that the comment will appear to apply to both variables. To avoid confusion
#' in this situation, comments should be pre-pended with the variable they refer
#' to, e.g., "dissolved oxygen: suspected biofouling not flagged by other
#' tests". This is not necessary for comments that apply to all variables (e.g.,
#' only one variable is measured).
#'
#' @param dat Data frame of flagged sensor string data in wide format.
#'
#' @param human_in_loop_table Data table with information required to assign the
#'   human in the loop flags. Must include the following columns: station,
#'   depl_range, variable, sensor_serial number (if \code{NA}, all sensors that
#'   recorded variable will be flagged), timestamp_prompt (indicates whether
#'   flags should be applied "between" the timestamp_utc_min
#'   and	timestamp_utc_max or "outside" these timestamps),
#'   timestamp_utc_min,	timestamp_utc_max, qc_test_column (qc flag column that
#'   is being upgraded), qc_flag_value (existing flag value for qc_test_column),
#'   human_in_loop_flag_value, and human_in_loop_comment (will be included in
#'   the dataset).
#'
#' @param qc_tests Character vector of quality control tests that have been
#'   applied to \code{dat}. Passed to \code{qc_pivot_longer()}. Default is:
#'   \code{qc_tests = c("climatology", "grossrange", "rolling_sd", "spike")}.
#'
#' @return Returns \code{dat} with a \code{human_in_loop_flag} column for each
#'   variable and a corresponding \code{hil_comment} column.

#' @export

qc_test_human_in_loop <- function(
    dat,
    human_in_loop_table = NULL,
    qc_tests = c("climatology", "grossrange", "rolling_sd", "spike")
) {

  if(nrow(human_in_loop_table) == 0) {
    message(
      paste(
        "no human in the loop flags to apply for <<",
        unique(dat$station),
        unique(dat$deployment_range), ">>"
      ))
  } else {

    message("applying human in the loop flags")

    for(i in seq_along(1:nrow(human_in_loop_table))) {

      human_in_loop_table_i <- human_in_loop_table[i, ]

      if(i > 1 && !("human_in_loop" %in% qc_tests)) {
        qc_tests = c(qc_tests, "human_in_loop")
      }

      dat_i <- qc_apply_human_in_loop(
        dat,
        human_in_loop_table = human_in_loop_table_i,
        qc_tests = qc_tests
      )

      dat <- dat_i
    }
  }

  dat
}

#' Apply human in the loop flags and comments
#'
#' @param dat Data frame of flagged sensor string data in wide format.
#'
#' @param human_in_loop_table One row of a data table with information required
#'   to assign the human in the loop flags. Must include the following columns:
#'   station, depl_range, variable, sensor_serial number (if \code{NA}, all
#'   sensors that recorded variable will be flagged), timestamp_prompt
#'   (indicates whether flags should be applied "between" the timestamp_utc_min
#'   and	timestamp_utc_max or "outside" these timestamps),
#'   timestamp_utc_min,	timestamp_utc_max, qc_test_column (qc flag column that
#'   is being upgraded), qc_flag_value (existing flag value for qc_test_column),
#'   human_in_loop_flag_value, and human_in_loop_comment (will be included in
#'   the dataset).
#'
#' @param qc_tests Character vector of quality control tests that have been
#'   applied to \code{dat}. Passed to \code{qc_pivot_longer()}. Default is:
#'   \code{qc_tests = c("climatology", "grossrange", "rolling_sd", "spike")}.
#'
#' @return Returns \code{dat} with a \code{human_in_loop_flag} column for each
#'   variable and a \code{human_in_loop_comment} column.
#'
#' @importFrom dplyr contains distinct if_any if_else filter last_col left_join relocate rename select
#' @importFrom data.table := between


qc_apply_human_in_loop <- function(
    dat,
    human_in_loop_table = NULL,
    qc_tests = c("climatology", "grossrange", "rolling_sd", "spike")
) {

  # checks ------------------------------------------------------------------

  if(length(unique(dat$station)) > 1 ||
     length(unique(dat$deployment_range)) > 1) {
    stop("more than one deployment found in dat")
  }

  # extract info from human_in_loop_table -----------------------------------
  hil_station <- human_in_loop_table$station
  hil_depl <- human_in_loop_table$depl_range
  hil_var <- human_in_loop_table$variable
  hil_sn <- human_in_loop_table$sensor_serial_number
  hil_timestamp_prompt <- human_in_loop_table$timestamp_prompt
  hil_timestamp_min <- human_in_loop_table$timestamp_utc_min
  hil_timestamp_max <- human_in_loop_table$timestamp_utc_max
  hil_qc_test_ref_column <- human_in_loop_table$qc_test_column
  hil_ref_flag_value <- human_in_loop_table$qc_flag_value
  hil_flag_value <- human_in_loop_table$human_in_loop_flag_value
  hil_flag_comment <-	human_in_loop_table$human_in_loop_comment


  # account for different input scenarios -----------------------------------
  if(is.na(hil_var) && is.na(hil_sn)) {
    var_sn <- dat %>%
      dplyr::select(-contains("flag")) %>%
      ss_pivot_longer()
    hil_var <- unique(var_sn$variable)
    hil_sn <- unique(var_sn$sensor_serial_number)
  } else if(is.na(hil_var) && !is.na(hil_sn)) {
    hil_var <- dat %>%
      dplyr::select(-contains("flag")) %>%
      ss_pivot_longer() %>%
      dplyr::filter(sensor_serial_number == hil_sn) %>%
      dplyr::distinct(variable)
    hil_var <- hil_var$variable
  } else if (!is.na(hil_var) && is.na(hil_sn)) {
    hil_sn <- dat %>%
      dplyr::select(-contains("flag")) %>%
      ss_pivot_longer() %>%
      dplyr::filter(variable == hil_var) %>%
      dplyr::distinct(sensor_serial_number)
    hil_sn <- hil_sn$sensor_serial_number
  }

  if(is.na(hil_timestamp_min)) hil_timestamp_min <- min(dat$timestamp_utc)
  if(is.na(hil_timestamp_max)) hil_timestamp_max <- max(dat$timestamp_utc)

  if (hil_timestamp_prompt == "between") {
    filter_text <- "between(timestamp_utc, hil_timestamp_min, hil_timestamp_max)"
  } else if (hil_timestamp_prompt == "outside") {
    filter_text <- "(timestamp_utc <= hil_timestamp_min |
    timestamp_utc >= hil_timestamp_max)"
  } else {
    stop(paste0("hil_timestamp_prompt must be << 'between' >> or << 'outside' >>,\nnot << ", hil_timestamp_prompt, " >>"))
  }

  if(is.na(hil_ref_flag_value)) hil_ref_flag_value <- c(1, 2, 3, 4)

  # apply human in the loop flags -------------------------------------------
  dat <- dat %>%
    qc_pivot_longer(qc_tests = qc_tests)

  # for instances where qc_test_human_in_loop() needs to be applied more than once
  # don't change to NA or there will be issues with qc_assign_max_flag()
  if(!("human_in_loop_flag_value" %in% colnames(dat))) {
    dat <- mutate(dat, human_in_loop_flag_value = 1)
  }

  if(!("hil_comment" %in% colnames(dat))) {
    dat <- mutate(dat, hil_comment = NA_character_)
  }

  dat_hil <- dat %>%
    rename(
      human_in_loop_reference_flag_col = all_of(hil_qc_test_ref_column)
    ) %>%
    mutate(
      human_in_loop_flag_value = as.numeric(human_in_loop_flag_value),
      human_in_loop_flag_value = if_else(
        (variable %in% hil_var &
           sensor_serial_number %in% hil_sn &
           eval(parse(text = filter_text)) &
           human_in_loop_reference_flag_col %in% hil_ref_flag_value),
        hil_flag_value,
        human_in_loop_flag_value
      ),
      human_in_loop_flag_value = ordered(human_in_loop_flag_value, levels = 1:4)
    ) %>%
    rename(
      "{hil_qc_test_ref_column}" := human_in_loop_reference_flag_col
    ) %>%
    qc_pivot_wider()

  # add comments (these will be applied to ALL variables measured by a sensor)
  join_cols <- colnames(dat_hil)[which(colnames(dat_hil) != "hil_comment")]

  dat_comment <- dat_hil %>%
    filter(
      if_any(contains("human_in_loop"), function (x) x == hil_flag_value)
    ) %>%
    mutate(
      hil_comment = if_else(is.na(hil_comment), hil_flag_comment, hil_comment)
    )

  dat_hil %>%
    select(-hil_comment) %>%
    left_join(dat_comment, by = join_cols) %>%
    relocate(hil_comment, .after = last_col())

}
