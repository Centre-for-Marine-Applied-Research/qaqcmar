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

qc_pivot_wider <- function(dat_long) {

  #check if depth_crosscheck will ever get caught in this

  dat <- dat_long |>
    pivot_wider(
      names_from = variable,
      values_from = c(value, contains("flag_value"))
    )

  colnames(dat) <- str_remove_all(colnames(dat), pattern = "value_")

  dat
}

