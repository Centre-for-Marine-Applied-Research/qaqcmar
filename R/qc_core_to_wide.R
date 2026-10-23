# Generated with Claude Opus 5.5

#' Convert long core output back to wide format
#'
#' The one pivot back, on `row_id`. Values go to one column per variable, flags
#' to `<test>_flag_<variable>`, and helper columns (from `keep_spike_cols` /
#' `keep_sd_cols`) to `<helper>_<variable>`. Flags become ordered factors with
#' levels 1 to 4.
#'
#' @param dat Long data from `qc_core_run()`.
#' @param flags Optional. The `flags` table from `qc_core_set_aside_flags()`
#'   (`row_id` plus flag columns that were already in the wide input). Joined
#'   back on `row_id`.
#'
#' @return Wide data with one row per `row_id`, in `row_id` order, without
#'   `row_id`, `sensor_key` and `month`.
#'
#' @importFrom dplyr across all_of any_of arrange everything left_join mutate select
#' @importFrom tidyr pivot_wider
#'
#' @noRd
qc_core_to_wide <- function(dat, flags = NULL) {
  depl_cols <- c(
    "county",
    "region",
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
    "depth_crosscheck_flag"
  )
  helper_cols <- c(
    "lag_value",
    "lead_value",
    "spike_ref",
    "spike_value",
    "int_sample",
    "n_sample",
    "n_sample_effective",
    "sd_roll"
  )

  # columns with one value per observation
  flag_cols <- setdiff(
    grep("_flag$", colnames(dat), value = TRUE),
    "depth_crosscheck_flag"
  )
  value_cols <- c("value", flag_cols, intersect(helper_cols, colnames(dat)))

  var_cols <- sort(unique(dat$variable))

  dat <- dat |>
    select(-any_of(c("sensor_key", "month"))) |>
    pivot_wider(
      names_from = variable,
      values_from = all_of(value_cols),
      names_glue = "{.value}_{variable}"
    ) |>
    arrange(row_id)

  colnames(dat) <- sub("^value_", "", colnames(dat))

  if (!is.null(flags)) {
    dat <- left_join(dat, flags, by = "row_id")
  }

  qc_flag_cols <- sort(grep("^qc_flag_", colnames(dat), value = TRUE))
  test_flag_cols <- sort(setdiff(
    grep("_flag_", colnames(dat), value = TRUE),
    qc_flag_cols
  ))

  dat |>
    mutate(
      across(
        any_of(c(test_flag_cols, qc_flag_cols, "depth_crosscheck_flag")),
        ~ ordered(.x, levels = 1:4)
      )
    ) |>
    select(
      any_of(depl_cols),
      all_of(var_cols),
      all_of(test_flag_cols),
      all_of(qc_flag_cols),
      everything(),
      -row_id
    )
}
