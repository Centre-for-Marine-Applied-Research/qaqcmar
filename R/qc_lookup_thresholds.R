#' Look up thresholds for given qc test and keys
#'
#' @param keys Variable, sensor_type, and month to look up in the thresholds table.
#' @param qc_test Quality control test to look up in the thresholds table.
#' @param county The county to look up in the threholds table.
#'
#' @returns A data frame of the thresholds for the given qc test, keys, and county.
#'
#' @noRd
#'

qc_core_thresholds <- function(keys, qc_test, county) {
  thr <- qaqcmar::thresholds |>
    filter(qc_test == !!qc_test, county == !!county | is.na(county)) |>
    pivot_wider(names_from = threshold, values_from = threshold_value) |>
    rename(
      thr_sensor_type = sensor_type,
      thr_month = month,
      thr_county = county
    )
  x <- keys |>
    inner_join(thr, by = "variable", relationship = "many-to-many") |>
    filter(
      is.na(thr_sensor_type) | thr_sensor_type == sensor_key,
      is.na(thr_month) | thr_month == month
    ) |>
    arrange(!is.na(thr_county), !is.na(thr_sensor_type), !is.na(thr_month)) |>
    # rows with the same key and specificity are combined column-wise
    # (grossrange stores sensor and user limits on separate rows)
    group_by(
      variable,
      sensor_key,
      month,
      spec = !is.na(thr_county) + !is.na(thr_sensor_type)
    ) |>
    summarise(
      across(where(is.numeric), ~ dplyr::last(na.omit(.x))),
      .groups = "drop"
    ) |>
    group_by(variable, sensor_key, month) |>
    arrange(spec, .by_group = TRUE) |>
    summarise(
      across(where(is.numeric), ~ dplyr::last(na.omit(.x))),
      .groups = "drop"
    ) |>
    select(-any_of(c("spec", "thr_month", "qc_test")))
}
