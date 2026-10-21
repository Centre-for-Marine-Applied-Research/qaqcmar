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
#' @importFrom dplyr pull
#'
#' @noRd

assert_county_thresholds <- function(county, test) {
  valid_county <- qaqcmar::thresholds |>
    filter(qc_test == test & !is.na(county)) |>
    distinct(county) |>
    arrange(county) |>
    pull(county)

  if (!(county %in% valid_county)) {
    close <- valid_county[tolower(valid_county) == tolower(county)]
    hint <- if (length(close) == 1) {
      paste0("\nDid you mean \"", close, "\"?")
    } else {
      paste0(
        "\nCounties with ",
        test,
        " thresholds: ",
        paste(valid_county, collapse = ", ")
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
