#' Calculate gross range test user thresholds
#'
#' The gross range user thresholds are calculated from historical data as the
#' mean +/- 3 standard deviations:
#'
#' \deqn{user_{min} = avg_{var} - 3 * stdev_{var}} \deqn{user_{max} = avg_{var} +
#' 3 * stdev_{var}}
#'
#' where \eqn{avg_{var}} is average of the variable of interest, and
#' \eqn{stdev_{var}} is the standard deviation of the variable of interest.
#'
#' This function does not automatically group by any variables. To calculate the
#' user thresholds by group (e.g., county) use
#' \code{dplyr::group_by(group_variable)}, and send the results to
#' \code{qc_calculate_user_thresholds()}.
#'
#' NA values are silently dropped before mean and standard deviation are
#' calculated.
#'
#' @param dat Data frame with at least one column named << var >>.
#'
#' @param var Unquoted character string indicating which column in \code{dat} to
#'   calculate the thresholds for, e.g., \code{var = temperature_degree_c}.
#'
#' @param n_sd Number of standard deviations to add / subtract from mean to
#'   calculate the user thresholds. Default is \code{n_sd = 3}.
#'
#' @param keep_stats Logical argument indicating whether to keep the calculated
#'   mean and standard deviation columns.
#'
#' @return Tibble with columns \code{qc_test}, \code{sensor_type},
#'   \code{variable}, \code{threshold}, and \code{threshold_value}.
#'
#' @importFrom dplyr everything mutate select summarise
#' @importFrom ggplot2 ensym
#' @importFrom stats sd
#' @importFrom stringr str_remove
#' @importFrom tidyr pivot_longer
#'
#' @export

qc_calculate_user_thresholds <- function(
  dat,
  var,
  n_sd = 3,
  keep_stats = FALSE
) {
  dat <- dat |>
    rename(variable = {{ var }}) |>
    summarise(
      mean_var = mean(variable, na.rm = TRUE),
      sd_var = sd(variable, na.rm = TRUE)
    ) |>
    mutate(
      qc_test = "grossrange",
      variable = as.character(ensym(var)),
      variable = str_remove(variable, "value_"),
      user_min = mean_var - n_sd * sd_var,
      user_max = mean_var + n_sd * sd_var
    ) |>
    select(qc_test, variable, everything())

  if (isFALSE(keep_stats)) {
    dat <- dat |> select(-c(mean_var, sd_var))
  }

  dat |>
    pivot_longer(
      cols = c(contains("user"), contains("_var")),
      values_to = "threshold_value",
      names_to = "threshold"
    ) |>
    mutate(threshold_value = round(threshold_value, digits = 3))
}


#' Calculate climatology test (monthly) thresholds
#'
#' The climatology thresholds are calculated from historical data for each month
#' as the mean +/- 3 standard deviations:
#'
#' \deqn{season_{min} = avg_{season} - 3 * stdev_{season}} \deqn{season_{max} =
#' avg_{season} + 3 * stdev_{season}}
#'
#' \eqn{avg_{season}} is calculated as the average of all observations for a
#' given month, and \code{stdev_{season}} is the associated standard deviation.
#'
#' This function only automatically groups by **month**. To calculate
#' the climatology thresholds by group (e.g., county) use
#' \code{dplyr::group_by(group_variable)}, and send the results to
#' \code{qc_calculate_climatology_thresholds()}.
#'
#' @inheritParams qc_calculate_user_thresholds
#'
#' @param dat Data frame with at least two columns: \code{timestamp_utc} or
#'   \code{month} and the variable for which to calculate the thresholds. If
#'   there is no column named \code{month}, the month will be extracted from
#'   \code{timestamp_utc}.
#'
#' @param ... Additional grouping variable(s).
#'
#' @return Tibble with columns \code{qc_test}, \code{variable}, \code{month},
#'   \code{threshold}, and \code{threshold_value}.
#'
#' @importFrom dplyr group_by mutate
#' @importFrom lubridate month
#'
#' @export

qc_calculate_climatology_thresholds <- function(
  dat,
  var,
  ...,
  n_sd = 3,
  keep_stats = FALSE
) {
  if (!("month" %in% colnames(dat))) {
    dat <- dat |>
      mutate(month = lubridate::month(timestamp_utc))
  }

  dat |>
    group_by(..., month) |>
    qc_calculate_user_thresholds(
      var = {{ var }},
      n_sd = n_sd,
      keep_stats = keep_stats
    ) |>
    mutate(
      qc_test = "climatology",
      threshold = case_when(
        threshold == "user_min" ~ "season_min",
        threshold == "user_max" ~ "season_max",
        TRUE ~ threshold
      )
    )
}

#' Calculate rolling standard deviation thresholds
#'
#' The rolling standard deviation thresholds are calculated from historical data
#' as the upper quartile of historical observations.
#'
#' This function does not calculate the rolling standard deviation - column
#' \code{sd_roll} must be included in \code{dat}.
#'
#' This function does not automatically group by any variables. To calculate the
#' thresholds by group (e.g., county) use
#' \code{dplyr::group_by(group_variable)}, and send the results to
#' \code{qc_calculate_rolling_sd_thresholds()}.
#'
#' @param dat data frame. Must include column sd_roll.
#'
#' @param var variable to calculate thresholds for.
#'
#' @param stat Statistic to calculate for the threshold. Options are: 1.
#'   "quartile", which uses the quantile function to calculate the value of the
#'   \code{prob} quartile. Used for variables with a right-tailed distribution
#'   for rolling standard deviation.
#'
#'   2. "mean_sd", which calculates the threshold as the mean + \code{n_sd}
#'   standard deviations. Used for variables with normal distributions for
#'   rolling standard deviation.
#'
#' @param prob Quantile to use for the threshold. Sent to quantile function.
#'   Only required when \code{stat = "quartile"}.
#'
#' @param n_sd Number of standard deviations to use. Default is \code{n_sd = 3}.
#'   Only required when \code{stat = "mean_sd"}.
#'
#' @return Tibble with columns \code{qc_test}, \code{variable},
#'   \code{threshold}, and \code{threshold_value}.
#'
#' @importFrom stats quantile
#'
#' @export

qc_calculate_rolling_sd_thresholds <- function(
  dat,
  var,
  stat = NULL,
  prob = 0.95,
  n_sd = 3
) {
  # turn this into proper error message
  # assert_that(length(prob) == 2)

  if (is.null(stat)) {
    stop("Argument stat must be 'quartile' or 'mean_sd', not  NULL")
  } else if (stat == "quartile") {
    dat <- dat |>
      summarise(
        q = quantile(sd_roll, probs = prob, na.rm = TRUE)
      ) |>
      rename(rolling_sd_max = q)
  } else if (stat == "mean_sd") {
    dat <- dat |>
      summarise(
        mean_sd_roll = mean(sd_roll, na.rm = TRUE),
        sd_sd_roll = sd(sd_roll, na.rm = TRUE)
      ) |>
      mutate(rolling_sd_max = mean_sd_roll + n_sd * sd_sd_roll) |>
      select(-c(mean_sd_roll, sd_sd_roll))
  } else {
    stop(paste("Argument stat must be 'quartile' or 'mean_sd', not", stat))
  }

  dat <- dat |>
    mutate(
      qc_test = "rolling_sd",
      variable = as.character(ensym(var)),
      variable = str_remove(variable, "value_"),
      rolling_sd_max = round(rolling_sd_max, digits = 2)
    ) |>
    select(qc_test, variable, everything()) |>
    pivot_longer(
      cols = rolling_sd_max,
      values_to = "threshold_value",
      names_to = "threshold"
    )

  attr(dat$threshold_value, "names") <- NULL

  dat
}


#' Calculate depth cross check thresholds
#'
#' @param dat Data frame.
#'
#'  Must include columns
#'   \code{sensor_depth_at_low_tide_m}, \code{sensor_depth_measured_m},
#'   \code{county}, \code{station}, \code{deployment_range}, and
#'   \code{sensor_serial_number}.
#'
#' @param prob Quantile to use for the threshold. Sent to \code{quantile()}.
#'
#' @return Tibble with columns \code{qc_test}, \code{variable},
#'   \code{threshold}, and \code{threshold_value}.
#'
#' @importFrom dplyr everything group_by mutate summarise ungroup
#' @importFrom tidyr pivot_longer
#' @importFrom sensorstrings ss_pivot_wider
#'
#' @export

qc_calculate_depth_crosscheck_thresholds <- function(
  dat,
  prob = 0.95
) {
  if ("variable" %in% colnames(dat)) {
    dat <- ss_pivot_wider(dat)
  }

  dat <- dat |>
    group_by(
      county,
      station,
      deployment_range,
      sensor_serial_number,
      sensor_depth_at_low_tide_m
    ) |>
    summarise(min_measured = min(sensor_depth_measured_m)) |>
    ungroup() |>
    mutate(abs_diff = abs(sensor_depth_at_low_tide_m - min_measured)) |>
    summarise(
      depth_diff_max = quantile(abs_diff, probs = prob, na.rm = TRUE)
    ) |>
    mutate(
      qc_test = "depth_crosscheck",
      variable = as.character("sensor_depth_measured_m"),
      depth_diff_max = round(depth_diff_max, digits = 2)
    ) |>
    select(qc_test, variable, everything()) |>
    pivot_longer(
      cols = depth_diff_max,
      values_to = "threshold_value",
      names_to = "threshold"
    )

  attr(dat$threshold_value, "names") <- NULL

  dat
}


#' Calculate spike test thresholds
#'
#' Returns 2 levels of spike threshold: \code{spike_low} is calculated from
#' historical data and the selected statistics. Values greater than
#' \code{spike_low} will be flagged as Suspect / Of Interest.
#'
#' \code{spike_high} is a user-specified multiple of \code{spike_low}. Values
#' greater than \code{spike_high} will be flagged Fail.
#'
#' @param dat Data frame of sensor string data in wide format. Must include
#'   columns \code{county}, \code{station}, \code{deployment_range}, var, and
#'   \code{sensor_serial_number}.
#'
#' @inheritParams qc_calculate_user_thresholds
#' @inheritParams qc_calculate_rolling_sd_thresholds
#'
#' @param prob Quantile to use for the threshold. Sent to \code{quantile()}.
#'
#' @param ... Optional grouping variable(s).
#'
#' @param fail_factor Numeric. Used to calculate \code{spike_high} based on
#'   \code{spike_low}: \code{spike_high} = \code{max_max} * \code{spike_low}.
#'
#' @return Tibble with columns \code{qc_test}, \code{variable},
#'   \code{threshold}, and \code{threshold_value}.
#'
#' @importFrom dplyr everything group_by mutate summarise ungroup
#' @importFrom stats sd
#' @importFrom tidyr pivot_longer
#' @importFrom sensorstrings ss_pivot_wider
#'
#' @export

qc_calculate_spike_thresholds <- function(
  dat,
  var,
  ...,
  stat = NULL,
  prob = 0.95,
  n_sd = 3,
  fail_factor = 3
) {
  if ("variable" %in% colnames(dat)) {
    dat <- ss_pivot_wider(dat)
  }

  dat <- dat |>
    rename(value = {{ var }}) |>
    group_by(county, station, deployment_range, sensor_serial_number) |>
    dplyr::arrange(timestamp_utc, .by_group = TRUE) |>
    mutate(
      lag_value = lag(value),
      lead_value = lead(value),
      spike_ref = (lag_value + lead_value) / 2,
      spike_value = abs(value - spike_ref)
    ) |>
    ungroup() |>
    group_by(...)

  if (is.null(stat)) {
    stop("Argument stat must be 'quartile' or 'mean_sd', not  NULL")
  } else if (stat == "quartile") {
    dat <- dat |>
      summarise(q = quantile(spike_value, probs = prob, na.rm = TRUE)) |>
      rename(spike_low = q)

    attr(dat$spike_low, "names") <- NULL
  } else if (stat == "mean_sd") {
    dat <- dat |>
      summarise(
        mean_spike = mean(spike_value, na.rm = TRUE),
        sd_spike = sd(spike_value, na.rm = TRUE)
      ) |>
      mutate(spike_low = mean_spike + n_sd * sd_spike) |>
      select(-c(mean_spike, sd_spike))
  } else {
    stop(paste("Argument stat must be 'quartile' or 'mean_sd', not", stat))
  }

  dat <- dat |>
    mutate(
      qc_test = "spike",
      variable = as.character(ensym(var)),
      variable = str_remove(variable, "value_"),
      spike_low = round(spike_low, digits = 2),
      spike_high = spike_low * fail_factor
    ) |>
    select(qc_test, variable, everything()) |>
    pivot_longer(
      cols = c(spike_low, spike_high),
      values_to = "threshold_value",
      names_to = "threshold"
    )

  # attr(dat$spike_low, "names") <- NULL
  #attr(dat$spike_high, "names") <- NULL

  dat
}
