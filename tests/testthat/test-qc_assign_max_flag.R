# Generated with Claude Opus 5.5

# 3 observations, 2 variables. Expected max flags worked out by hand.
amf_dat <- data.frame(
  station = "s1",
  sensor_serial_number = "123",
  timestamp_utc = as.POSIXct("2023-01-01", tz = "UTC") + 3600 * 0:2,
  depth_crosscheck_flag = ordered(c(3, 3, 3), levels = 1:4),
  salinity_psu = c(30, 31, NA),
  temperature_degree_c = c(10, 11, 12),
  grossrange_flag_salinity_psu = ordered(c(1, 1, NA), levels = 1:4),
  grossrange_flag_temperature_degree_c = ordered(c(1, 3, 1), levels = 1:4),
  spike_flag_salinity_psu = ordered(c(1, 2, NA), levels = 1:4),
  spike_flag_temperature_degree_c = ordered(c(2, 1, NA), levels = 1:4),
  human_in_loop_flag_temperature_degree_c = ordered(c(1, 1, 4), levels = 1:4)
)

amf_out <- qc_assign_max_flag(amf_dat)

test_that("qc_assign_max_flag() gives the worst flag for each variable", {
  # depth_crosscheck_flag (3) is ignored; NA flags are ignored; human in the
  # loop flags are included by default
  expect_equal(
    amf_out$qc_flag_temperature_degree_c,
    ordered(c(2, 3, 4), levels = 1:4)
  )
  # no flags for an observation gives NA
  expect_equal(
    amf_out$qc_flag_salinity_psu,
    ordered(c(1, 2, NA), levels = 1:4)
  )
})

test_that("qc_assign_max_flag() keeps the rows and columns of dat", {
  expect_equal(nrow(amf_out), nrow(amf_dat))
  expect_equal(amf_out[colnames(amf_dat)], amf_dat)
})

test_that("qc_assign_max_flag() only uses the flags of qc_tests", {
  out <- qc_assign_max_flag(amf_dat, qc_tests = c("grossrange", "spike"))

  expect_equal(
    out$qc_flag_temperature_degree_c,
    ordered(c(2, 3, 1), levels = 1:4)
  )
})

test_that("qc_assign_max_flag(return_all = FALSE) drops the test flags", {
  out <- qc_assign_max_flag(amf_dat, return_all = FALSE)

  expect_equal(
    colnames(out),
    c(
      "station",
      "sensor_serial_number",
      "timestamp_utc",
      "depth_crosscheck_flag",
      "salinity_psu",
      "temperature_degree_c",
      "qc_flag_salinity_psu",
      "qc_flag_temperature_degree_c"
    )
  )
})

test_that("qc_assign_max_flag() stops when there are no flag columns", {
  expect_error(
    qc_assign_max_flag(amf_dat[, 1:6]),
    "no flag columns"
  )
})

test_that("qc_assign_max_flag() works on wide and long data", {
  expect_equal(
    amf_dat |>
      ss_pivot_longer() |>
      qc_assign_max_flag() |>
      as.data.frame(),
    amf_out
  )
})
