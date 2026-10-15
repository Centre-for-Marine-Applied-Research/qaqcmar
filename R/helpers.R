#' Add lagged versions of column \code{var} to \code{dat}
#'
#' This is an internal function used in \code{qc_test_flat_line()}.
#'
#' @param dat Data frame with at least one column, \code{var}.
#'
#' @param var Column in \code{dat} that will be lagged.
#'
#' @param n_lags Vector of lagged versions of \code{var} to include. For
#'   example, if \code{n_lags} = 1, a lag-1 version named \code{var_lag1} will
#'   be added to \code{dat}. If \code{n_lags} = 2:3, the lag-2 and lag-3
#'   versions will be added and named \code{var_lag2} and \code{var_lag3},
#'   respectively.
#'
#' @return Returns \code{dat} with additional columns that are lagged versions
#'   of column \code{var}.
#'
#' @importFrom dplyr across lag mutate
#' @importFrom purrr map partial

add_n_lag_columns <- function(dat, var, n_lags) {
  map_lag <- n_lags |> map(~ partial(lag, n = .x))

  dat |>
    mutate(
      across(.cols = {{ var }}, .fns = map_lag, .names = "{.col}_lag{n_lags}")
    )
}


#' Ensure there is one and only one county specified for the qc_test_* functions
#'
#' @param dat Data frame sent to the qc_test_* function.
#'
#' @param county_arg Character string (or \code{NULL} value) in the county
#'   argument of the qc_test_*() function.
#'
#' @param foo Character string indicating which qc_test_* function is being
#'   checked.
#'
#' @return Returns a character string indicating the county for which to apply
#'   thresholds.

assert_county <- function(dat, county_arg, foo) {
  county <- NULL

  if (!is.null(county_arg) & !("county" %in% colnames(dat))) {
    county <- county_arg
  }

  if (!is.null(county_arg) & ("county" %in% colnames(dat))) {
    county_dat <- unique(dat$county)

    if (length(county_dat) > 1) {
      stop(
        paste(
          "More than one county found in << dat >>: ",
          paste(county_dat, collapse = " and ")
        )
      )
    }

    if (county_dat != county_arg) {
      stop(
        "Two different values for << county >> were found in << ",
        foo,
        " >>:\n",
        county_arg,
        " was provided in the function argument, \n and ",
        county_dat,
        " was found in << dat >>"
      )
    }

    county <- county_arg
  }

  if (is.null(county_arg) & ("county" %in% colnames(dat))) {
    county_dat <- unique(dat$county)

    if (length(county_dat) > 1) {
      stop(
        paste(
          "More than one county found in << dat >>: ",
          paste(county_dat, collapse = " and ")
        )
      )
    }

    county <- county_dat
  }

  if (is.null(county) || is.na(county)) {
    stop(
      "Must specify << county >> argument or include a << county >> column in << dat >>"
    )
  }

  county
}


#' Check that the county has default thresholds for a qc test
#'
#' @param county Character string returned by \code{assert_county()}.
#'
#' @param test Character string: the \code{qc_test} value in \code{thresholds}.
#'
#' @return Returns \code{county} invisibly, or stops with an error.
#'
#' @noRd

assert_county_thresholds <- function(county, test) {
  valid <- sort(unique(
    thresholds$county[thresholds$qc_test == test & !is.na(thresholds$county)]
  ))

  if (!(county %in% valid)) {
    close <- valid[tolower(valid) == tolower(county)]
    hint <- if (length(close) == 1) {
      paste0("\nDid you mean \"", close, "\"?")
    } else {
      paste0(
        "\nCounties with ",
        test,
        " thresholds: ",
        paste(valid, collapse = ", ")
      )
    }

    stop(
      "No default ",
      test,
      " thresholds for county \"",
      county,
      "\".",
      hint,
      "\nUse a valid county, or provide the thresholds in << ",
      test,
      "_table >>.",
      call. = FALSE
    )
  }

  invisible(county)
}
