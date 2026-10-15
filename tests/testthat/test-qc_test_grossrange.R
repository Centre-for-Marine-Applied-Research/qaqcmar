# testdata was generated such that we KNOW what flag should be assigned to each
# observation (data-raw/test_data.R)

# Simulated data for the gross range test
# was constructed so there are 4 observations per month
# Day 1: Gross Range Flag 4 (low)
# Day 5: Gross Range Flag 3 (low)*
# Day 10: Gross Range Flag 1
# Day 15: Gross Range Flag 4 (high)
# Day 28: Gross Range Flag 3 (high)

# * for the aquameasure and vr2ar sensors, the user_min is less than the sensor_min,
# which means a flag of 3 (low) will not be assigned

# Here, qc_test_grossrange() is applied to the test data. The qc'd data is then
# separated by day.
# The tests check that each day include the expected flag value, and
# only the expected flag value.

path <- system.file("testdata", package = "qaqcmar")


test_that("qc_test_grossrange() assigns correct flags", {
  dat <- readRDS(paste0(path, "/test_data_grossrange.RDS"))

  dat_qc <- dat |>
    qc_test_grossrange(county = "Lunenburg") |>
    qc_pivot_longer(qc_tests = "grossrange") |>
    mutate(day = lubridate::day(timestamp_utc))

  expect_equal(
    as.numeric(
      dat_qc |>
        filter(day == 10) |>
        distinct(grossrange_flag_value) |>
        dplyr::pull(grossrange_flag_value)
    ),
    1
  )

  expect_equal(
    as.numeric(
      dat_qc |>
        filter(day == 5 & !(sensor_type %in% c("vr2ar", "aquameasure"))) |>
        distinct(grossrange_flag_value) |>
        dplyr::pull(grossrange_flag_value)
    ),
    3
  )

  expect_equal(
    as.numeric(
      dat_qc |>
        filter(day == 28) |>
        distinct(grossrange_flag_value) |>
        dplyr::pull(grossrange_flag_value)
    ),
    3
  )

  expect_equal(
    as.numeric(
      dat_qc |>
        filter(day == 1 | day == 15) |>
        distinct(grossrange_flag_value) |>
        dplyr::pull(grossrange_flag_value)
    ),
    4
  )

  expect_equal(
    sort(as.numeric(unique(dat_qc$grossrange_flag_value))),
    c(1, 3, 4)
  )
})
