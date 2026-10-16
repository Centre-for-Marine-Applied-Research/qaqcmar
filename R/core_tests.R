#' Apply the grossrange test to long data
#'
#' # Generated with Claude Opus 5.5
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
        value <= user_max | value >= user_min ~ 1L,
        .default = 2L
      )
    )
}

#' Apply the climatology test to long data
#'
#' # Generated with Claude Opus 5.5
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
#' # Generated with Claude Opus 5.5
#' The spike value of an observation is its distance from the mean of the
#' observations before and after it in the same series (see
#' `qc_core_group_series()`). The first and last observation of each series
#' get flag 2.
#'
#' @param dat Long data with spike thresholds, from
#'   `qc_append_thresholds(dat, "spike", county)`.
#' @param keep_spike_cols  Logical. If `TRUE`, the helper columns `lag_value`,
#'   `lead_value`, `spike_ref` and `spike_value` are kept.
#'
#' @return  `dat` sorted by series and time, plus `spike_flag` (integer, 1 to
#'   4). `qc_core_to_wide()` restores the original order by `row_id`.
#'
#' @importFrom dplyr case_when lag lead mutate ungroup select
#'
#' @noRd

qc_core_spike <- function(dat, keep_spike_cols) {
  dat |>
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
      )
    ) |>
    ungroup()
}

#' Apply the rolling sd test to long data
#'
#' # Generated with Claude Opus 5.5
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
#'   standard deviation. Passed to \code{zoo::rollapply()}. Default is
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
#' @importFrom zoo rollapply
#'
#' @noRd

qc_core_rolling_sd <- function(
  dat,
  period_hours = 24,
  max_interval_hours = 2,
  align_window = "center",
  keep_sd_cols = FALSE
) {
  dat <- dat |>
    qc_core_group_series() |>
    mutate(
      # sample interval
      int_sample = as.numeric(difftime(
        timestamp_utc,
        lag(timestamp_utc),
        units = "mins"
      )),
      # int_sample = round(as.numeric(int_sample)),

      # number of samples in  period_hours
      # 60 mins / hour * 1 sample / int_sample mins * 24 hours / period
      n_sample = round((60 / int_sample) * period_hours),
      n_sample_effective = case_when(
        # first observation of each group is NA, which will give error in rollapply
        is.na(n_sample) ~ 0,
        int_sample <= 0 ~ 0, # duplicate timestamp
        # if the sample interval is greater than acceptable limit, set n_sample to 0
        # so that roll_sd will be NA
        int_sample > max_interval_hours * 60 ~ 0,
        TRUE ~ n_sample
      ),
      # rolling sd
      sd_roll = rollapply(
        value,
        width = n_sample_effective,
        align = align_window,
        FUN = sd,
        fill = NA
      ),
      sd_roll = round(sd_roll, digits = 2),
      rolling_sd_flag = case_when(
        sd_roll > rolling_sd_max ~ 3L,
        sd_roll <= rolling_sd_max ~ 1L,
        is.na(sd_roll) ~ 2L # change this to .default for Phase B
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
