#' testdata was generated such that we KNOW what flag should be assigned to each
#' observation (data-raw/test_data_climatology.R)
#'
#' Here, qc_test_climatology() is applied to the test data. The qc'd data is
#' then separated into a data frame for each flag value, based on
#' the timestamps used to generate the data in data-raw/test_data.R.
#'
#' The tests check that each data frame include the expected flag value, and
#' only the expected flag value.
#'

path <- system.file("testdata", package = "qaqcmar")

dat <- readRDS(paste0(path, "/test_data_climatology.RDS"))

dat_qc <- dat %>%
  qc_test_climatology(county = "Lunenburg") %>%
  mutate(day = lubridate::day(timestamp_utc)) %>%
  qc_pivot_longer(qc_tests = "climatology")

test_that("qc_test_climatology() assigns correct flags", {
  expect_equal(
    as.numeric(
      dat_qc |>
        filter(day == 10) |>
        distinct(climatology_flag_value) |>
        dplyr::pull(climatology_flag_value)
    ),
    1
  )

  expect_equal(
    as.numeric(
      dat_qc |>
        filter(day %in% c(1, 28)) |>
        distinct(climatology_flag_value) |>
        dplyr::pull(climatology_flag_value)
    ),
    3
  )

  expect_equal(sort(as.numeric(unique(dat_qc$climatology_flag_value))), c(1, 3))
})
