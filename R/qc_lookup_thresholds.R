# Generated with Claude Opus 5.5

#' Look up thresholds for one QC test
#'
#' Finds the thresholds that apply to each combination of variable, sensor and
#' month in the data, for one test and one county, from the long-format
#' `thresholds` table.
#'
#' @details In `thresholds`, an `NA` in `county`, `sensor_type` or `month`
#'   means "applies to any". A threshold row is a candidate for a key when its
#'   `county` is `NA` or matches `county`, its `sensor_type` is `NA` or matches
#'   the key's `sensor_key`, and its `month` is `NA` or matches the key's
#'   `month`.
#'
#'   When several candidate rows give a value for the same threshold, the most
#'   specific one wins, in this order:
#'   \enumerate{
#'     \item a county-specific row over a province-wide row (`county` `NA`);
#'     \item within that, a sensor-specific row over an any-sensor row;
#'     \item within that, a month-specific row over an any-month row.
#'   }
#'   This is how Inverness gets its own salinity thresholds: its rows are
#'   county-specific, so they win over the province-wide salinity rows.
#'
#'   The choice is made separately for each threshold column. If the most
#'   specific row has no value for a column (e.g. gross range stores
#'   `sensor_min`/`sensor_max` and `user_min`/`user_max` on different rows),
#'   the value comes from the next most specific row that has one. That is
#'   what combines sensor limits and user limits into one row per key.
#'
#'   Keys with no candidate rows at all are left out of the result, so after a
#'   `left_join()` their threshold columns are `NA` and the test gives flag 2
#'   (Not Evaluated).
#'
#' @param keys Data frame of the distinct combinations to look up, with
#'   columns `variable`, `sensor_key` and `month`, e.g.
#'   `dplyr::distinct(dat, variable, sensor_key, month)`.
#' @param qc_test Character string. The QC test, as in `thresholds$qc_test`.
#' @param county Character string. One county, as in `thresholds$county`.
#'   Rows for other counties are ignored; province-wide rows (`county` `NA`)
#'   are always used.
#' @param threhold_table Optional table of thresholds for `qc_test`, in wide
#'  format: `variable` plus one column per threshold (e.g. `season_min`,
#'  `season_max`), and optionally `sensor_type` and `month`. As in
#'  `thresholds`, `NA` (or a missing column) in `sensor_type` or `month`
#'  means "applies to any". Used instead of `thresholds`, as is: a
#'  `county` column is ignored. Default `NULL` uses `qaqcmar::thresholds`.
#'
#' @return A tibble with one row per key that has at least one candidate
#'   threshold row: `variable`, `sensor_key`, `month`, plus one column per
#'   threshold of `test` (e.g. `sensor_min`, `sensor_max`, `user_min`,
#'   `user_max` for gross range; `season_min`, `season_max` for climatology;
#'   `spike_low`, `spike_high` for spike; `rolling_sd_max` for rolling SD).
#'   Join it to the long data with
#'   `left_join(long, thr, by = c("variable", "sensor_key", "month"))`.
#'
#' @importFrom dplyr across arrange filter group_by inner_join last rename select summarise where
#' @importFrom tidyr pivot_wider
#'
#' @noRd

qc_lookup_thresholds <- function(
  keys,
  qc_test,
  county,
  threshold_table = NULL
) {
  if (is.null(table)) {
    thr <- qaqcmar::thresholds |>
      filter(qc_test == !!qc_test, county == !!county | is.na(county)) |>
      pivot_wider(names_from = threshold, values_from = threshold_value)
  } else {
    # binding to an empty template adds any missing county, sensor_type and
    # month columns as NA ("applies to any")
    template <- tibble(
      county = character(),
      sensor_type = character(),
      month = numeric()
    )
    thr <- bind_rows(template, select(threshold_table, -any_of("county")))
  }

  thr <- thr |>
    rename(
      thr_sensor_type = sensor_type,
      thr_month = month,
      thr_county = county
    )
  out <- keys |>
    inner_join(thr, by = "variable", relationship = "many-to-many") |>
    filter(
      is.na(thr_sensor_type) | thr_sensor_type == sensor_key,
      is.na(thr_month) | thr_month == month
    ) |>
    # this sorts from least specific to most specific (province -> county)
    arrange(!is.na(thr_county), !is.na(thr_sensor_type), !is.na(thr_month)) |>
    # rows with the same key and specificity are combined column-wise
    # (grossrange stores sensor and user limits on separate rows)
    group_by(
      variable,
      sensor_key,
      month,
      # specificity score for county and sensor
      spec = !is.na(thr_county) + !is.na(thr_sensor_type)
    ) |>
    summarise(
      across(where(is.numeric), ~ dplyr::last(.x[!is.na(.x)])),
      .groups = "drop"
    ) |>
    group_by(variable, sensor_key, month) |>
    arrange(spec, .by_group = TRUE) |>
    summarise(
      across(where(is.numeric), ~ dplyr::last(.x[!is.na(.x)])),
      .groups = "drop"
    ) |>
    select(-any_of(c("spec", "thr_month", "qc_test")))

  if (qc_test == "grossrange") {
    out <- filter(out, !is.na(sensor_min), !is.na(user_min), !is.na(user_max))
  }

  out
}
