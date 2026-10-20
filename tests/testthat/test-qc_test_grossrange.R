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
dat <- readRDS(paste0(path, "/test_data_grossrange.RDS"))
dat_qc <- dat |>
  qc_test_grossrange(county = "Lunenburg") |>
  qc_pivot_longer(qc_tests = "grossrange") |>
  mutate(day = lubridate::day(timestamp_utc))

test_that("qc_test_grossrange() assigns correct flags", {
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

test_that("qc_test_grossrange() uses hobo do limits on DO logger rows without DO", {
  dat <- data.frame(
    county = "Halifax",
    station = "A",
    deployment_range = "2023-Jan-01 to 2023-Dec-31",
    sensor_type = "hobo",
    sensor_serial_number = 1,
    timestamp_utc = as.POSIXct("2023-06-01", tz = "UTC") + 0:1 * 600,
    dissolved_oxygen_uncorrected_mg_per_l = c(8, NA),
    temperature_degree_c = c(10, 45)
  )

  out <- suppressMessages(qc_test_grossrange(dat))

  # 45 is above the hobo do sensor_max (40), but not the hobo sensor_max (70)
  expect_equal(
    out$grossrange_flag_temperature_degree_c,
    ordered(c(1, 4), levels = 1:4)
  )
})

test_that("qc_test_grossrange() stops for a misspelled sensor type", {
  dat <- data.frame(
    county = "Halifax",
    station = "A",
    deployment_range = "2023-Jan-01 to 2023-Dec-31",
    sensor_type = "aquameasur",
    sensor_serial_number = 1,
    timestamp_utc = as.POSIXct("2023-06-01", tz = "UTC"),
    temperature_degree_c = 60
  )

  expect_error(
    suppressMessages(qc_test_grossrange(dat)),
    'sensor_type "aquameasur"'
  )
})

test_that("qc_core_grossrange() gives 2, not 1, when a threshold is missing", {
  dat <- data.frame(
    value = c(10, 25),
    sensor_min = -5,
    sensor_max = NA,
    user_min = 0,
    user_max = 20
  )

  # 10 is within the user limits; 25 is above user_max but sensor_max is
  # missing, so it cannot be evaluated
  expect_equal(qc_core_grossrange(dat)$grossrange_flag, c(1L, 2L))
})
