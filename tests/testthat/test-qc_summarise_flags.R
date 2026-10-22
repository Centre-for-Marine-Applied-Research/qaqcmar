# Generated with Claude Opus 5.5

sum_dat <- data.frame(
  station = "A",
  sensor_type = c("hobo", "hobo", "hobo", "aquameasure"),
  sensor_serial_number = c(1, 1, 1, 2),
  timestamp_utc = as.POSIXct("2023-06-01", tz = "UTC") + c(0, 1, 2, 0) * 600,
  temperature_degree_c = c(10, 11, 30, 12),
  grossrange_flag_temperature_degree_c = ordered(c(1, 1, 4, 1), levels = 1:4)
)

test_that("qc_summarise_flags() counts each flag value per test and variable", {
  out <- qc_summarise_flags(sum_dat, qc_tests = "grossrange")

  expect_equal(out$qc_test, c("grossrange", "grossrange"))
  expect_equal(as.character(out$flag_value), c("Pass", "Fail"))
  expect_equal(out$n_flag, c(3, 1))
  expect_equal(out$n_percent, c(75, 25))
})

test_that("qc_summarise_flags() counts within the groups in ...", {
  out <- qc_summarise_flags(sum_dat, qc_tests = "grossrange", sensor_type)

  hobo <- filter(out, sensor_type == "hobo")
  expect_equal(hobo$n_flag, c(2, 1))
  expect_equal(hobo$n_percent, c(66.67, 33.33))
  expect_equal(filter(out, sensor_type == "aquameasure")$n_percent, 100)
})
