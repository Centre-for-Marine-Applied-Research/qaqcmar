hil_t0 <- as.POSIXct("2023-06-01", tz = "UTC")

# two sensors, two variables on sensor 1, three timestamps
hil_long <- data.frame(
  row_id = c(1, 1, 2, 2, 3, 3, 4, 5, 6),
  station = "A",
  deployment_range = "2023-Jun-01 to 2023-Jun-30",
  sensor_serial_number = c(1, 1, 1, 1, 1, 1, 2, 2, 2),
  timestamp_utc = hil_t0 + c(0, 0, 1, 1, 2, 2, 0, 1, 2) * 3600,
  variable = c(
    rep(c("temperature_degree_c", "salinity_psu"), 3),
    rep("temperature_degree_c", 3)
  ),
  value = c(10, 30, 11, 30, 12, 30, 10, 11, 12),
  grossrange_flag = c(1L, 1L, 3L, 1L, 1L, 1L, 1L, 1L, 1L)
)

hil_core_row <- data.frame(
  variable = NA,
  sensor_serial_number = NA,
  timestamp_utc_min = NA,
  timestamp_utc_max = NA,
  qc_test = NA,
  qc_flag_value = NA,
  human_in_loop_flag_value = 4,
  hil_comment = "comment 1"
)

test_that("qc_core_human_in_loop() treats NA in the table as any", {
  out <- qc_core_human_in_loop(hil_long, hil_core_row)

  expect_equal(out$human_in_loop_flag, rep(4L, 9))
  expect_equal(out$hil_comment, rep("comment 1", 9))
  # same rows in the same order
  expect_equal(select(out, -human_in_loop_flag, -hil_comment), hil_long)
})

test_that("qc_core_human_in_loop() matches variable, sensor and inclusive timestamps", {
  out <- qc_core_human_in_loop(
    hil_long,
    mutate(
      hil_core_row,
      variable = "temperature_degree_c",
      sensor_serial_number = 1,
      timestamp_utc_min = hil_t0 + 3600,
      timestamp_utc_max = hil_t0 + 2 * 3600
    )
  )

  expect_equal(out$human_in_loop_flag, c(1L, 1L, 4L, 1L, 4L, 1L, 1L, 1L, 1L))
  # the comment belongs to the observation, so salinity at the same time has it
  expect_equal(
    out$hil_comment,
    c(NA, NA, rep("comment 1", 4), NA, NA, NA)
  )
})

test_that("qc_core_human_in_loop() uses qc_flag_value to limit the flags", {
  out <- qc_core_human_in_loop(
    hil_long,
    mutate(hil_core_row, qc_test = "grossrange", qc_flag_value = 3)
  )

  expect_equal(out$human_in_loop_flag, c(1L, 1L, 4L, 1L, 1L, 1L, 1L, 1L, 1L))
})

test_that("qc_core_human_in_loop() gives the highest flag and pastes comments in table order", {
  hil <- rbind(
    mutate(
      hil_core_row,
      sensor_serial_number = 2,
      human_in_loop_flag_value = 4
    ),
    mutate(
      hil_core_row,
      timestamp_utc_min = hil_t0 + 2 * 3600,
      human_in_loop_flag_value = 3,
      hil_comment = "comment 2"
    )
  )

  out <- qc_core_human_in_loop(hil_long, hil)

  expect_equal(out$human_in_loop_flag, c(1L, 1L, 1L, 1L, 3L, 3L, 4L, 4L, 4L))
  expect_equal(
    out$hil_comment,
    c(
      NA,
      NA,
      NA,
      NA,
      "comment 2",
      "comment 2",
      "comment 1",
      "comment 1",
      "comment 1; comment 2"
    )
  )
})

test_that("qc_core_human_in_loop() warns for a row that matches nothing", {
  hil <- rbind(hil_core_row, mutate(hil_core_row, sensor_serial_number = 99))

  expect_warning(
    out <- qc_core_human_in_loop(hil_long, hil),
    "matched no observations"
  )
  expect_equal(out$human_in_loop_flag, rep(4L, 9))
})
