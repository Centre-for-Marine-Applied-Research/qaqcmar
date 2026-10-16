#' Apply the grossrange test to long data
#'
#' # Generated with Claude Opus 5.5
#'
#' @param dat Long data from `qc_core_prepare()`.
#'
#' @return `dat` with the same rows in the same order, plus the columns for the grossrange thresholds and the grossrange_flag column, which has integers from 1 to 4.
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
#' @param dat Long data from `qc_core_prepare()`.
#' @return `dat` with the same rows in the same order, plus columns for the climatology thresholds and the climatology_flag column, which has integers from 1 to 4.
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
#'
#' @param dat Long data from `qc_core_prepare()`.
#' @return `dat` with the same rows in the same order, plus columns for the spike thresholds and the spike_flag column, which has integers from 1 to 4.
#'
#' @importFrom dplyr case_when mutate ungroup
#'
#' @noRd

qc_core_spike <- function(dat) {
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
#' @param dat Long data from `qc_core_prepare()`.
#' @return `dat` with the same rows in the same order, plus columns for the rolling sd thresholds and the rolling_sd_flag column, which has integers from 1 to 4.
#'
#' @importFrom dplyr case_when mutate
#'
#' @noRd

qc_core_rolling_sd <- function(
  dat,
  period_hours = 24,
  max_interval_hours = 2,
  align_window = "center",
  keep_sd_cols = FALSE
) {
  dat |>
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
        sd_roll > rolling_sd_max ~ 3,
        sd_roll <= rolling_sd_max ~ 1,
        is.na(sd_roll) ~ 2
      )
    ) |>
    ungroup()
}
