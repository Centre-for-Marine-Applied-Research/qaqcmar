#' Assign each observation the maximum flag from applied QC tests.
#'
#' \code{depth_crosscheck_flag} is not considered when evaluating the maximum
#' flag value.  \code{depth_crosscheck_flag} remains its own column with a
#' single value for the whole deployment.
#'
#' @param dat Data frame in long or wide format with flag columns from multiple
#'   quality control tests.
#'
#' @param qc_tests Quality control tests included in \code{dat}. Default is
#'   \code{qc_tests = c("climatology", "grossrange", "rolling_sd", "spike")}.

#' @param return_all Logical value indicating whether to return all quality
#'   control flag columns or only the summary columns. If \code{TRUE}, all flag
#'   columns will be returned. If \code{FALSE}, only the summary columns will be
#'   returned. Default is \code{TRUE}.
#'
#' @return Returns \code{dat} in a wide format, with a single flag column for
#'   each variable column.
#'
#' @importFrom dplyr all_of any_of contains everything left_join mutate rename select
#' @importFrom stringr str_remove_all
#' @importFrom tidyr pivot_wider
#'
#' @export
#'
#' @examples
#' path <- system.file("testdata", package = "qaqcmar")
#' dat <- readRDS(file.path(path, "test_data_grossrange.RDS"))
#'
#' dat_qc <- dat |>
#'   qc_test_all(qc_tests = c("climatology", "grossrange")) |>
#'   qc_assign_max_flag()
#' colnames(dat_qc)
#'
#' # only the max flag columns
#' dat_qc <- dat |>
#'   qc_test_all(qc_tests = c("climatology", "grossrange")) |>
#'   qc_assign_max_flag(return_all = FALSE)
#' colnames(dat_qc)

qc_assign_max_flag <- function(dat, qc_tests = NULL, return_all = TRUE) {
  if ("variable" %in% colnames(dat)) {
    dat <- qc_pivot_wider(dat)
  }

  if (is.null(qc_tests)) {
    qc_tests <- c(
      "climatology",
      "grossrange",
      "rolling_sd",
      "spike",
      "human_in_loop"
    )
  }

  # flag columns are named <test>_flag_<variable>; depth_crosscheck_flag
  # does not match, so it is never included
  flag_cols <- grep(
    paste0("^(", paste(qc_tests, collapse = "|"), ")_flag_"),
    colnames(dat),
    value = TRUE
  )
  if (length(flag_cols) == 0) {
    stop("<< dat >> has no flag columns for << qc_tests >>.", call. = FALSE)
  }

  dat <- mutate(dat, row_id = row_number())

  # one row per observation and variable, one column per test, then the max
  max_flags <- dat |>
    select(row_id, all_of(flag_cols)) |>
    pivot_longer(
      -row_id,
      names_to = c(".value", "variable"),
      names_pattern = "^(.+_flag)_(.+)$"
    ) |>
    mutate(across(-c(row_id, variable), ~ as.integer(as.character(.x)))) |>
    qc_core_max_flags() |>
    select(row_id, variable, qc_flag) |>
    pivot_wider(
      names_from = variable,
      values_from = qc_flag,
      names_prefix = "qc_flag_"
    ) |>
    mutate(across(-row_id, ~ ordered(.x, levels = 1:4)))

  dat <- dat |>
    left_join(max_flags, by = "row_id") |>
    select(-row_id)

  if (!isTRUE(return_all)) {
    dat <- select(dat, -all_of(flag_cols))
  }

  # order columns: deployment, variables, test flags, max flags, anything left
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
    "depth_crosscheck_flag",
    "hil_comment"
  )
  qc_cols <- sort(grep("^qc_flag_", colnames(dat), value = TRUE))
  var_cols <- sort(sub("^qc_flag_", "", qc_cols))

  dat |>
    select(
      any_of(depl_cols),
      any_of(var_cols),
      any_of(sort(flag_cols)),
      all_of(qc_cols),
      everything()
    )
}
