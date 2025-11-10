#' Pivot flagged sensor string data longer by variable
#'
#' @param dat_wide Data frame of flagged sensor string data in wide format.
#'
#' @param qc_tests Quality control tests included in \code{dat_wide}. If
#'   \code{dat_wide} only includes the max flag, use \code{qc_tests = "qc"}.
#'
#' @return Returns \code{dat_wide}, with variables and flags pivoted to a long
#'   format.
#'
#' @importFrom dplyr %>% any_of arrange filter relocate select
#' @importFrom tidyr pivot_longer
#'
#' @export

# # turn this into example
# path <- system.file("testdata", package = "qaqcmar")
# dat <- read.csv(paste0(path, "/example_data.csv"))

# dat_long <- dat %>%
#   qc_test_all() %>%
#   qc_pivot_longer()
#
# dat_long <- dat %>%
#   qc_test_all() %>%
#   qc_assign_max_flag() %>%
#   qc_pivot_longer(qc_tests = "qc")


qc_pivot_longer <- function(dat_wide, qc_tests = NULL) {

  if (is.null(qc_tests)) {
    qc_tests <- c(
      "climatology",
      #"depth_crosscheck",
      "grossrange",
      "rolling_sd",
      "spike",
     "qc"
    )
  }

  qc_tests <- tolower(qc_tests)

  vars <- c(
    "dissolved_oxygen_percent_saturation",
    "dissolved_oxygen_uncorrected_mg_per_l",
    "salinity_psu",
    "ph_ph",
    "sensor_depth_measured_m",
    "temperature_degree_c"
  )

  # pivot the variables - this adds flags that shouldn't exist but will get cleaned up below
  dat <- dat_wide %>%
    pivot_longer(
      cols = any_of(vars),
      names_to = "variable",
      values_to = "value",
      values_drop_na = TRUE
    )

  # pivot the flags indicated
  if ("climatology" %in% qc_tests) {
    dat <- pivot_flags_longer(dat, qc_test = "climatology")
  }

  if ("grossrange" %in% qc_tests) {
    dat <- pivot_flags_longer(dat, qc_test = "grossrange")
  }

  if ("rolling_sd" %in% qc_tests) {
    dat <- pivot_flags_longer(dat, qc_test = "rolling_sd")
  }

  if ("spike" %in% qc_tests) {
    dat <- pivot_flags_longer(dat, qc_test = "spike")
  }

  if ("qc" %in% qc_tests) {
    dat <- pivot_flags_longer(dat, qc_test = "qc")
  }

  if ("human_in_loop" %in% qc_tests) {
    dat <- pivot_flags_longer(dat, qc_test = "human_in_loop")
  }

  # don't arrange by deployment_range (because it will be alphabetical not chronological)
  dat %>%
    arrange(station, sensor_type, variable, timestamp_utc)
}


#' Complete pivot_longer of flagged sensor string data
#'
#' @param dat_wide Data frame of flagged sensor string data with the variables
#'   in a long format and flags in wide format.
#'
#' @param qc_test Flag columns to pivot.
#'
#' @return Returns \code{dat_wide} with the qc_test flag columns pivoted to a
#'   long format.
#'
#' @importFrom dplyr %>% contains filter select
#' @importFrom rlang sym
#' @importFrom tidyr pivot_longer
#'
#' @export


pivot_flags_longer <- function(dat_wide, qc_test) {

  col_name <- paste0(qc_test, "_flag_variable")

  dat_wide %>%
    pivot_longer(
      cols = contains(qc_test),
      names_to = paste0(qc_test, "_flag_variable"),
      names_prefix = paste0(qc_test, "_flag_"),
      values_to = paste0(qc_test, "_flag_value"),
      values_drop_na = TRUE
    ) %>%
    # for sensors that measure two variables, the above adds a row for each var
    # e.g., temperature value + temperature flag AND temperature value + DO flag
    filter(!!sym(col_name) == variable) %>%
    select(-!!sym(col_name))
}
