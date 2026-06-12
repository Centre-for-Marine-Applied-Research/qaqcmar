#' Assign text labels to flag values
#'
#' All columns with the string "flag" in the name will be converted from numeric
#' flags to text flags (e.g., 1 = Pass, 2 = Not Evaluated, 3 = Suspect/Of
#' Interest, 4 = Fail).
#'
#' @param dat Data frame of flagged sensor string data in long or wide format.
#'   Must include at least one column name with the string "flag".
#'
#' @return Returns \code{dat} with entries in the flag columns converted from
#'   numeric values to text labels (an ordered factor with levels "Pass", "Not
#'   Evaluated", "Suspect/Of Interest", "Fail", "Missing Data").
#'
#' @importFrom dplyr across case_when contains mutate
#'
#' @export

# @examples
# path <- system.file("testdata", package = "qaqcmar")
# dat <- readRDS(paste0(path, "/test_data.RDS"))
#
# dat_qc <- dat %>%
#  qc_test_grossrange() %>%
#  qc_assign_flag_labels()

qc_assign_flag_labels <- function(dat) {
  dat %>%
    mutate(
      across(
        contains("flag"),
        # add Pass and pass to accommodate db
        ~ case_when(
          .x == 1 | .x == "Pass" | .x == "pass" ~ "Pass",
          .x == 2 | .x == "Not Evaluated" | .x == "not evaluated" ~ "Not Evaluated",
          .x == 3 | .x == "suspect/of interest" |
            .x == "Suspect/Of Interest" ~ "Suspect/Of Interest",
          .x == 4 | .x == "Fail" | .x == "fail" ~ "Fail"
        )
      ),
      across(
        contains("flag"),
        ~ ordered(
          .x,
          levels = c("Pass", "Not Evaluated", "Suspect/Of Interest", "Fail")
        )
      )
    )
}
