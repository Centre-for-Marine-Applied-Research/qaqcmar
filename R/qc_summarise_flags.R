#' Generate summary table of flags
#'
#' @inheritParams qc_plot_flags
#'
#' @param ... Optional argument. Column names (not quoted) from \code{dat} to
#'   use as grouping variables.
#'
#' @return Returns a data frame with the number and percent of observations
#'   assigned each flag value for each variable, qc_test, and groups in
#'   \code{...}.
#'
#' @importFrom dplyr arrange group_by mutate n ungroup rename select
#' @importFrom tidyr pivot_longer
#'
#'
#' @export

qc_summarise_flags <- function(dat, qc_tests = NULL, ...) {
  if (!("variable" %in% colnames(dat))) {
    dat <- qc_pivot_longer(dat, qc_tests = qc_tests)
  }

  dat |>
    group_by(variable, ...) |>
    mutate(n_obs = n()) |>
    ungroup() |>
    pivot_longer(
      cols = contains("_flag_value"),
      names_to = "qc_test",
      names_prefix = "_flag_value",
      values_to = "flag_value"
    ) |>
    mutate(qc_test = str_remove(qc_test, "_flag_value")) |>
    group_by(qc_test, variable, flag_value, ..., n_obs) |>
    # don't use "flag" in column name because qc_assign_flag_labels will try to convert, resulting in NA
    summarise(n_fl = n()) |>
    ungroup() |>
    mutate(n_percent = round(100 * n_fl / n_obs, digits = 2)) |>
    qc_assign_flag_labels() |>
    select(-n_obs) |>
    rename(n_flag = n_fl) |>
    arrange(qc_test, variable, flag_value)
}
