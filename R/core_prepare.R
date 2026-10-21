# Generated with Claude Opus 5.5
# Long-format core: data contract ---------------------------------------------
#
# 1. Wide input (what qc_core_prepare() accepts) -------------------------------
#
#    Compiled sensor string data, as returned by
#    sensorstrings::ss_compile_deployment_data(): one row per sensor per
#    timestamp, one column per variable.
#
#    Required columns:
#      station, deployment_range, sensor_type, sensor_serial_number,
#      timestamp_utc (POSIXct, UTC), and at least one variable column.
#
#    Variable columns are any of sensorstrings' ss_vars$variable.
#
#    Needed only by some steps:
#      county                      default thresholds (or the county argument)
#      sensor_depth_at_low_tide_m  depth crosscheck
#
#    Every other column (region, waterbody, lease, latitude, longitude,
#    string_configuration, ...) is passed through untouched.
#
#    Flag columns already in the input are
#    set aside by row_id in qc_core_prepare() and joined back on row_id in
#    qc_core_to_wide(). They never enter the long data. If the input already
#    has flag columns for a test that is about to run, stop with a message. Human-in-the-loop works on the
#    wide output and stays outside the core.
#
# 2. Long data (what qc_core_prepare() returns; what every core function ------
#    receives and returns)
#
#    One row per non-NA observation. (row_id, variable) is unique.
#
#    row_id       integer  Row number of the wide input. The only key used to
#                          pivot back.
#    <pass-through columns>  Exactly as in the wide input, same type, and
#                          constant within a row_id.
#    variable     chr      One of ss_vars$variable.
#    value        dbl      The measurement. Never NA (values_drop_na = TRUE).
#    sensor_key   chr      sensor_type used for threshold lookup only.
#                          "hobo do" for a hobo, otherwise sensor_type.
#                         a row is "hobo do" for every row of a hobo that ever records
#                          dissolved_oxygen_uncorrected_mg_per_l within the
#                          deployment. sensor_type itself is never changed.
#    month        integer  1 to 12, from timestamp_utc, in UTC.
#
#    Series: the time-series tests (spike, rolling SD) work on one series at a
#    time. A series is one sensor and variable within one deployment:
#      the columns of c("county", "region", "station", "deployment_range",
#      "sensor_serial_number") present in the data, plus variable.
#    Within a series, rows are ordered by
#    timestamp_utc, then row_id. test functions group and sort the series themselves with qc_core_group_series()
#
#
# 3. Threshold columns (added by qc_append_thresholds()) -------------------------
#
#    Joined on variable, sensor_key and month.
#    Names are fixed and unique across tests, so all can sit in the data at
#    once:
#      grossrange        sensor_min, sensor_max, user_min, user_max
#      climatology       season_min, season_max
#      spike             spike_low, spike_high
#      rolling_sd        rolling_sd_max
#      depth_crosscheck  depth_diff_max
#    NA means no threshold for that row. Threshold columns are removed before
#    the result leaves qc_core_run(), unless a keep_* argument asks for them.
#
#
# 4. Flag columns (one per test) ------------------------------------------------
#
#    Each qc_core_<test>() adds exactly one column and changes nothing else:
#      grossrange_flag, climatology_flag, spike_flag, rolling_sd_flag
#    Type: integer, 1 = Pass, 2 = Not Evaluated, 3 = Suspect/Of Interest,
#    4 = Fail. Integers (not ordered factors) inside the core so pmax() works;
#    qc_core_to_wide() converts to ordered(levels = 1:4).
#    No NA: a row that can't be evaluated gets 2.
#
#    depth_crosscheck_flag: one value per deployment, repeated on every row of
#    that deployment. Integer Not per variable, and
#    not part of qc_flag.
#
#    Helper columns, kept only with keep_spike_cols / keep_sd_cols:
#      spike:      spike_value
#      rolling_sd: int_sample, n_sample, n_sample_effective, sd_roll
#
#
# 5. Max flag (qc_core_max_flag()) ---------------------------------------------
#
#    qc_flag = pmax() over the <test>_flag columns present, among
#    climatology, grossrange, rolling_sd, spike and human_in_loop, na.rm = TRUE.
#    depth_crosscheck_flag is never included (today's behaviour).
#    Only runs when qc_core_run(max_flag = TRUE). The exported qc_test_*()
#    functions, including qc_test_all(), use max_flag = FALSE so their output
#    has no qc_flag columns, as today; qc_assign_max_flag() adds them.
#
#
# 6. Wide output (qc_core_to_wide()) -------------------------------------------
#
#    One pivot on row_id. One row per row_id, in row_id order, so the output
#    has the input's rows in the input's order, minus rows with no values at
#    all (they have no long rows; today's code drops them too).
#    Columns, in this order:
#      deployment columns (county, region, waterbody, station, lease, latitude,
#        longitude, deployment_range, string_configuration, sensor_type,
#        sensor_serial_number, timestamp_utc, sensor_depth_at_low_tide_m,
#        depth_crosscheck_flag), those present
#      variables, sorted
#      <test>_flag_<variable>, sorted
#      qc_flag_<variable>, sorted
#      keep_* helper columns as <helper>_<variable> (names_glue, QC-5)
#      anything else
#    Flags are ordered factors, levels 1:4.
#

#' Set aside flag columns already in the wide input
#'
#' Adds `row_id` to `dat` and moves any flag columns (e.g. from an earlier
#' `qc_test_*()` call) into a separate table keyed by `row_id`, so
#' tests can still be chained. `qc_core_to_wide()` joins them back.
#'
#' @param dat Water quality data in a wide format, with at least one `qc_flag_*` column.
#' @param qc_tests Quality control tests that are going to be applied (NOT the tests that have already been applied).  Stops if `dat` already has flags for
#'   any of them. This is a required argument.
#'
#' @return A list: `dat` (wide data with `row_id`, without flag columns) and
#'   `flags` (`row_id` plus the flag columns, or `NULL` if there were none).
#'
#' @importFrom dplyr all_of mutate row_number select
#'
#' @noRd
qc_core_set_aside_flags <- function(dat, qc_tests) {
  if ("row_id" %in% colnames(dat)) {
    stop("<< dat >> already has a << row_id >> column.", call. = FALSE)
  }

  dat <- mutate(dat, row_id = row_number())
  flag_cols <- grep("_flag", colnames(dat), value = TRUE)

  if (length(flag_cols) == 0) {
    return(list(dat = dat, flags = NULL))
  }

  flagged_tests <- sub("_flag.*$", "", flag_cols)
  already_flagged <- intersect(qc_tests, flagged_tests)
  if (length(already_flagged) > 0) {
    stop(
      "<< dat >> already has flags for: ",
      paste(already_flagged, collapse = ", "),
      ".\nRemove those flag columns before applying the test again.",
      call. = FALSE
    )
  }

  list(
    dat = select(dat, -all_of(flag_cols)),
    flags = select(dat, all_of(c("row_id", flag_cols)))
  )
}


#' Convert wide sensor string data to the core's long format
#'
#' @param dat Data in a wide format (one column for each variable), without flag columns (see `qc_core_set_aside_flags()`). If it has no `row_id` column, one is added.
#'
#' @return Long data: one row per non-NA observation, with `row_id`,
#'   the pass-through columns, `variable`, `value`, `sensor_key` and `month`.
#'
#' @importFrom dplyr across all_of group_by if_else mutate row_number ungroup
#' @importFrom sensorstrings ss_pivot_longer
#' @importFrom rlang .data
#'
#' @noRd
qc_core_prepare <- function(dat) {
  required <- c(
    "station",
    "deployment_range",
    "sensor_type",
    "sensor_serial_number",
    "timestamp_utc"
  )
  missing <- setdiff(required, colnames(dat))
  if (length(missing) > 0) {
    stop(
      "<< dat >> is missing required column(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  if (!inherits(dat$timestamp_utc, "POSIXct")) {
    stop("<< timestamp_utc >> must be POSIXct.", call. = FALSE)
  }
  if (length(grep("_flag", colnames(dat))) > 0) {
    stop(
      "<< dat >> has flag columns; use qc_core_set_aside_flags() first.",
      call. = FALSE
    )
  }

  if (!("row_id" %in% colnames(dat))) {
    dat <- mutate(dat, row_id = row_number())
  }

  # sensor_key: sensor_type for the threshold lookup, with "hobo do" for hobo
  # DO loggers. A hobo is "hobo do"
  # for all its rows if it ever records DO. Decided in wide format, before the
  # pivot, because the rule looks at the DO column.
  do_col <- "dissolved_oxygen_uncorrected_mg_per_l"
  if (do_col %in% colnames(dat)) {
    dat <- dat |>
      group_by(station, deployment_range, sensor_serial_number) |>
      mutate(
        sensor_key = if_else(
          sensor_type == "hobo" & any(!is.na(.data[[do_col]])),
          "hobo do",
          sensor_type
        )
      ) |>
      ungroup()
  } else {
    dat <- mutate(dat, sensor_key = sensor_type)
  }

  # the one pivot longer; variables are whatever sensorstrings recognises
  ss_pivot_longer(dat) |>
    mutate(
      value = as.numeric(value),
      month = as.integer(lubridate::month(timestamp_utc))
    )
}
