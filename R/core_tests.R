# Generated with Claude Opus 5.5

#' Apply the grossrange test to long data
#'
#' @param dat Long data with gross range thresholds, from
#'   `qc_append_thresholds(dat, "grossrange", county)`.
#'
#' @return `dat` with the same rows in the same order, plus
#'   `grossrange_flag` (integer, 1 to 4). Rows with no thresholds get 2.
#'
#' @importFrom dplyr case_when mutate
#'
#' @noRd

qc_core_grossrange <- function(dat) {
  dat |>
    mutate(
      grossrange_flag = case_when(
        value > sensor_max | value < sensor_min ~ 4L,
        (value <= sensor_max & value > user_max) |
          (value >= sensor_min & value < user_min) ~ 3L,
        value <= user_max & value >= user_min ~ 1L,
        .default = 2L
      )
    )
}

#' Apply the climatology test to long data
#'
#' @param dat Long data with gross range thresholds, from
#'   `qc_append_thresholds(dat, "climatology", county)`.
#'
#' @return `dat` with the same rows in the same order, plus
#'   `climatology_flag` (integer, 1 to 4). Rows with no thresholds get 2.
#'
#' @importFrom dplyr case_when mutate
#'
#' @noRd

qc_core_climatology <- function(dat) {
  dat |>
    mutate(
      climatology_flag = case_when(
        value > season_max | value < season_min ~ 3L,
        value <= season_max & value >= season_min ~ 1L,
        .default = 2L
      )
    )
}

#' Apply the spike test to long data
#'
#' The spike value of an observation is its distance from the mean of the
#' observations before and after it in the same series (see
#' `qc_core_group_series()`). The first and last observation of each series
#' get flag 2.
#'
#'  Observations at a duplicate timestamp, and the observations just before and
#' after them, also get flag 2. This is because which duplicate counts as the
#' neighbour depends only on row order, so their spike value is arbitrary. A warning will be printed if this occurs.
#'
#' @param dat Long data with spike thresholds, from
#'   `qc_append_thresholds(dat, "spike", county)`.
#' @param max_interval_hours Largest accepted interval (hours) between two
#'   observations. Observations just before and after a longer gap get flag 2.
#' @param keep_spike_cols  Logical. If `TRUE`, the helper columns `lag_value`,
#'   `lead_value`, `spike_ref` and `spike_value` are kept.
#'
#' @return `dat` sorted by series and time, plus `spike_flag` (integer, 1 to
#'   4). `qc_core_to_wide()` restores the original order by `row_id`.
#'
#' @importFrom dplyr case_when coalesce if_else lag lead mutate ungroup select
#'
#' @noRd

qc_core_spike <- function(
  dat,
  max_interval_hours = 2,
  keep_spike_cols = FALSE
) {
  dat <- dat |>
    qc_core_group_series() |>
    mutate(
      lag_value = lag(value),
      lead_value = lead(value),
      spike_ref = (lag_value + lead_value) / 2,
      spike_value = abs(value - spike_ref),
      spike_flag = case_when(
        spike_value > spike_high ~ 4L,
        (spike_value <= spike_high & spike_value > spike_low) ~ 3L,
        spike_value <= spike_low ~ 1L,
        .default = 2L
      ),
      # duplicate timestamps, and the observations next to them, get 2
      dup_prev = coalesce(timestamp_utc == lag(timestamp_utc), FALSE), # FALSE for first
      dup = dup_prev | lead(dup_prev, default = FALSE),
      near_dup = dup | lag(dup, default = FALSE) | lead(dup, default = FALSE),

      gap_prev = coalesce(
        as.numeric(difftime(
          timestamp_utc,
          lag(timestamp_utc),
          units = "hours"
        )) >
          max_interval_hours,
        FALSE
      ), # FALSE for first
      near_gap = gap_prev | lead(gap_prev, default = FALSE),
      spike_flag = if_else(near_dup | near_gap, 2L, spike_flag)
    ) |>
    ungroup()

  n_dup <- sum(dat$dup_prev)
  if (n_dup > 0) {
    warning(
      n_dup,
      " duplicate timestamp(s) found; these observations and their ",
      "neighbours get spike_flag = 2.",
      call. = FALSE
    )
  }
  dat <- select(dat, -c(dup_prev, dup, near_dup, gap_prev, near_gap))

  if (isFALSE(keep_spike_cols)) {
    dat <- select(
      dat,
      -c(lag_value, lead_value, spike_ref, spike_value)
    )
  }
  dat
}

#' Apply the rolling sd test to long data
#'
#' The rolling standard deviation is calculated over a window of
#' `period_hours` in each series (see `qc_core_group_series()`). It is `NA`
#' (flag 2) for the first observation of a series, at duplicate timestamps,
#' and after a gap longer than `max_interval_hours`.
#'
#' @param dat Long data with rolling_sd thresholds, from `qc_append_thresholds(dat, "rolling_sd", county)`.
#'
#' @param period_hours Length of a full cycle in hours. Default assumes a daily
#'   cycle of \code{period_hours = 24}.
#'
#' @param max_interval_hours Maximum accepted interval between two observations.
#'   If the interval between two observations is greater than this value, the
#'   rolling standard deviation will be set to \code{NA}. This is important
#'   because for large intervals, the number of observations in
#'   \code{period_hours} will be small. For example, if samples are collected
#'   every 6 hours, only 4 observations would be used to calculate
#'   \code{roll_sd}.
#'
#' @param align_window Alignment for the window used to calculate the rolling
#'   standard deviation. Passed to \code{qc_window_sd()}. Default is
#'   \code{align_window = "center"}. Other options are \code{"right"} (backward
#'   window) and \code{"left"} (forward window).
#'
#' @param keep_sd_cols  Logical. If `TRUE`, the helper columns `int_sample`,
#'   `n_sample`, `n_sample_effective` and `sd_roll` are kept.
#'
#' @return `dat` sorted by series and time, plus `rolling_sd_flag` (integer, 1
#'   to 4). `qc_core_to_wide()` restores the original order by `row_id`.
#'
#' @importFrom dplyr case_when lag mutate ungroup select
#' @importFrom stats sd
#'
#' @noRd

qc_core_rolling_sd <- function(
  dat,
  period_hours = 24,
  max_interval_hours = 2,
  align_window = "center",
  keep_sd_cols = FALSE
) {
  align_window <- match.arg(align_window, c("center", "left", "right"))

  dat <- dat |>
    qc_core_group_series() |>
    mutate(
      # sample interval
      int_sample = as.numeric(difftime(
        timestamp_utc,
        lag(timestamp_utc),
        units = "mins"
      )),
      # number of samples in  period_hours
      # 60 mins / hour * 1 sample / int_sample mins * 24 hours / period
      n_sample = round((60 / int_sample) * period_hours),
      n_sample_effective = case_when(
        is.na(n_sample) ~ 0,
        int_sample <= 0 ~ 0, # duplicate timestamp
        # if the sample interval is greater than acceptable limit, set n_sample to 0
        # so that roll_sd will be NA
        int_sample > max_interval_hours * 60 ~ 0,
        TRUE ~ n_sample
      ),
      # rolling sd
      sd_roll = qc_window_sd(
        value,
        width = n_sample_effective,
        align = align_window
      ),
      sd_roll = round(sd_roll, digits = 2),
      rolling_sd_flag = case_when(
        sd_roll > rolling_sd_max ~ 3L,
        sd_roll <= rolling_sd_max ~ 1L,
        .default = 2L
      )
    ) |>
    ungroup()

  # count up the number of times int_sample = 0 (i.e., duplicated timestamp)
  n_dup <- sum(dat$int_sample == 0, na.rm = TRUE)
  if (n_dup > 0) {
    warning(
      n_dup,
      " duplicate timestamp(s) found; these observations get rolling_sd_flag = 2.",
      call. = FALSE
    )
  }

  if (isFALSE(keep_sd_cols)) {
    dat <- select(dat, -c(int_sample, n_sample, n_sample_effective, sd_roll))
  }

  dat
}

#' Calculate rolling standard deviation from running sums
#' @param x Numeric vector, sorted by time.
#' @param width Integer vector of window widths, the same length as `x` (or
#'   length 1).
#' @param align "center", "left" or "right".
#'
#' @return Numeric vector the same length as `x`.
#'
#' @noRd
qc_window_sd <- function(x, width, align = c("center", "left", "right")) {
  align <- match.arg(align)
  n <- length(x)
  i <- seq_len(n)
  width <- rep_len(width, n)

  # window bounds (indices into x)
  hi <- switch(
    align,
    center = i + floor(width / 2),
    left = i + width - 1,
    right = i
  )
  lo <- hi - width + 1

  out <- rep(NA_real_, n)
  ok <- width > 1 & lo >= 1 & hi <= n
  if (!any(ok)) {
    return(out)
  }

  # subtract the mean so the sums stay small (floating point accuracy), and
  # count NAs so that windows that contain one give NA, as sd() does
  is_na <- is.na(x)
  x <- x - mean(x, na.rm = TRUE)
  x[is_na] <- 0

  s1 <- c(0, cumsum(x))
  s2 <- c(0, cumsum(x^2))
  s_na <- c(0, cumsum(is_na))

  lo <- lo[ok]
  hi <- hi[ok]
  w <- width[ok]
  sum_x <- s1[hi + 1] - s1[lo]
  sum_x2 <- s2[hi + 1] - s2[lo]
  n_na <- s_na[hi + 1] - s_na[lo]

  # pmax(): rounding can make a zero variance very slightly negative
  sd_x <- sqrt(pmax((sum_x2 - sum_x^2 / w) / (w - 1), 0))
  sd_x[n_na > 0] <- NA_real_

  out[ok] <- sd_x
  out
}


#' Apply the depth crosscheck test to long data
#'
#' For each sensor with measured depth, compares the minimum measured depth
#' with `sensor_depth_at_low_tide_m`. Each deployment gets the worst of its
#' sensors' flags, repeated on every row of that deployment. Deployments with
#' no measured depth get 2. Not part of `qc_flag`.
#'
#' @param dat Long data from `qc_core_prepare()`, with a
#'   `sensor_depth_at_low_tide_m` column.
#' @param return_sensor_table Logical. If `TRUE`, returns the per-sensor table
#'   (one row per sensor: `min_measured`, `abs_diff`, `depth_diff_max`,
#'   `depth_crosscheck_flag`) instead of `dat`. Sensors with no measured
#'   depth get 2.
#'
#' @return `dat` with the same rows in the same order, plus
#'   `depth_crosscheck_flag` (integer, 1 to 4).
#'
#' @importFrom dplyr across all_of coalesce filter first group_by if_else left_join mutate summarise
#'
#' @noRd
qc_core_depth_crosscheck <- function(
  dat,
  return_sensor_table = FALSE
) {
  deployment_cols <- intersect(
    c("county", "region", "station", "deployment_range"),
    colnames(dat)
  )

  # one flag per sensor with measured depth
  sensor_flags <- dat |>
    filter(variable == "sensor_depth_measured_m") |>
    group_by(across(all_of(
      c(deployment_cols, "sensor_serial_number", "sensor_depth_at_low_tide_m")
    ))) |>
    # depth_diff_max comes from the thresholds (one value per sensor)
    summarise(
      min_measured = min(value),
      depth_diff_max = first(depth_diff_max),
      .groups = "drop"
    ) |>
    mutate(
      abs_diff = abs(sensor_depth_at_low_tide_m - min_measured),
      depth_crosscheck_flag = case_when(
        abs_diff > depth_diff_max ~ 3L,
        abs_diff <= depth_diff_max ~ 1L,
        .default = 2L
      )
    )

  if (isTRUE(return_sensor_table)) {
    # one row per sensor, including sensors that did not measure depth (flag 2)
    sensor_cols <- c(
      deployment_cols,
      "sensor_serial_number",
      "sensor_depth_at_low_tide_m"
    )
    sensor_table <- dat |>
      distinct(across(all_of(sensor_cols))) |>
      left_join(sensor_flags, by = sensor_cols) |>
      mutate(depth_crosscheck_flag = coalesce(depth_crosscheck_flag, 2L))

    return(sensor_table)
  }

  # worst sensor flag per deployment
  deployment_flags <- sensor_flags |>
    group_by(across(all_of(deployment_cols))) |>
    summarise(
      depth_crosscheck_flag = max(depth_crosscheck_flag),
      .groups = "drop"
    )

  # deployments with no measured depth get 2
  dat |>
    left_join(deployment_flags, by = deployment_cols) |>
    # find NAs and convert to flag 2
    mutate(depth_crosscheck_flag = coalesce(depth_crosscheck_flag, 2L))
}
