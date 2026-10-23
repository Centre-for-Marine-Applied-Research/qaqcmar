#' Pivot flagged sensor string data from long to wide format by variable
#'
#' @param dat_long Data frame of flagged sensor string data in long format.
#'
#' @return Returns \code{dat_long}, with variables and flags pivoted to a wide
#'   format (a separate column for each variable and variable-qc_test
#'   combination).
#'
#' @importFrom dplyr contains
#' @importFrom tidyr pivot_wider
#' @importFrom stringr str_remove_all
#'
#' @export
#'
#' @examples
#' path <- system.file("testdata", package = "qaqcmar")
#' dat <- readRDS(file.path(path, "test_data_grossrange.RDS"))
#'
#' dat_long <- dat |>
#'   qc_test_grossrange() |>
#'   qc_pivot_longer(qc_tests = "grossrange")
#'
#' dat_wide <- qc_pivot_wider(dat_long)
#' colnames(dat_wide)

qc_pivot_wider <- function(dat_long) {
   dat <- dat_long |>
    pivot_wider(
      names_from = variable,
      values_from = c(value, contains("flag_value"))
    )

  colnames(dat) <- str_remove_all(colnames(dat), pattern = "value_")

  dat
}
