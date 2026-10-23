# Generated with Claude Opus 5.5

#' Apply multiple quality control tests
#'
#' @param dat Data frame of sensor string data in a wide format.
#'
#' @param qc_tests Character vector of quality control tests to apply to
#'   \code{dat}. Defaults to all available tests: \code{qc_tests =
#'   c("climatology", "depth_crosscheck", "grossrange", "rolling_sd", "spike")}.
#'
#' @inheritParams qc_test_climatology
#' @inheritParams qc_test_depth_crosscheck
#' @inheritParams qc_test_grossrange
#' @inheritParams qc_test_rolling_sd
#' @inheritParams qc_test_spike
#'
#' @return Returns \code{dat} with additional quality control flag columns.
#'
#' @export
#'
#' @examples
#' path <- system.file("testdata", package = "qaqcmar")
#' dat <- readRDS(file.path(path, "test_data_rolling_sd.RDS"))
#'
#' # all tests
#' dat_qc <- qc_test_all(dat)
#' colnames(dat_qc)
#'
#' # some tests
#' dat_qc <- qc_test_all(dat, qc_tests = c("grossrange", "spike"))
#' colnames(dat_qc)

qc_test_all <- function(
  dat,
  qc_tests = NULL,
  county = NULL,

  climatology_table = NULL,
  depth_table = NULL,
  grossrange_table = NULL,
  rolling_sd_table = NULL,
  spike_table = NULL,

  period_hours = 24,
  max_interval_hours = 2,
  align_window = "center",
  keep_sd_cols = FALSE,
  keep_spike_cols = FALSE
) {
  if (is.null(qc_tests)) {
    qc_tests <- c(
      "climatology",
      "depth_crosscheck",
      "grossrange",
      "rolling_sd",
      "spike"
    )
  }
  qc_tests <- tolower(qc_tests)

  county <- assert_county(dat, county, "qc_test_all()")

  # default thresholds must exist for county (not needed for depth_crosscheck,
  # which has one province-wide threshold)
  threshold_table <- list(
    climatology = climatology_table,
    grossrange = grossrange_table,
    rolling_sd = rolling_sd_table,
    spike = spike_table
  )
  for (test in intersect(names(threshold_table), qc_tests)) {
    if (is.null(threshold_table[[test]])) {
      assert_county_thresholds(county, test)
    }
  }

  dat <- qc_core_set_aside_flags(dat, qc_tests)

  # tables that are NULL use the default thresholds
  # (qc_append_thresholds() -> qc_lookup_thresholds() -> qaqcmar::thresholds)
  dat$dat |>
    qc_core_run(
      qc_tests = qc_tests,
      county = county,
      climatology_table = climatology_table,
      depth_table = depth_table,
      grossrange_table = grossrange_table,
      rolling_sd_table = rolling_sd_table,
      spike_table = spike_table,
      period_hours = period_hours,
      max_interval_hours = max_interval_hours,
      align_window = align_window,
      keep_sd_cols = keep_sd_cols,
      keep_spike_cols = keep_spike_cols
    ) |>
    qc_core_to_wide(flags = dat$flags)
}
