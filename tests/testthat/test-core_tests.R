# Generated with Claude Opus 5.5

# gross range -------------------------------------------------------------

test_that("qc_core_grossrange() flags values against sensor and user limits", {
  dat <- data.frame(
    value = c(-1, 11, 0, 1, 9, 10, 2, 5, 8, 5),
    sensor_min = c(rep(0, 9), NA),
    sensor_max = c(rep(10, 9), NA),
    user_min = c(rep(2, 9), NA),
    user_max = c(rep(8, 9), NA)
  )
  out <- qc_core_grossrange(dat)

  # outside sensor limits: 4; between sensor and user limits: 3;
  # inside user limits (limits included): 1; no thresholds: 2
  expect_equal(out$grossrange_flag, c(4L, 4L, 3L, 3L, 3L, 3L, 1L, 1L, 1L, 2L))
  expect_equal(out[colnames(dat)], dat)
})

# climatology -------------------------------------------------------------

test_that("qc_core_climatology() flags values outside the season limits", {
  dat <- data.frame(
    value = c(1, 9, 2, 5, 8, 5),
    season_min = c(rep(2, 5), NA),
    season_max = c(rep(8, 5), NA)
  )
  out <- qc_core_climatology(dat)

  expect_equal(out$climatology_flag, c(3L, 3L, 1L, 1L, 1L, 2L))
  expect_equal(out[colnames(dat)], dat)
})

# spike -------------------------------------------------------------------

# two sensors; sensor 2 is entered in reverse time order
spike_dat <- data.frame(
  row_id = 1:8,
  station = "A",
  deployment_range = "2023-Jan-01 to 2023-Dec-31",
  sensor_serial_number = c(1, 1, 1, 1, 1, 2, 2, 2),
  variable = "temperature_degree_c",
  timestamp_utc = as.POSIXct("2023-06-01", tz = "UTC") +
    c(0, 1, 2, 3, 4, 2, 1, 0) * 600,
  value = c(1, 1, 5, 1, 1, 1, 9, 1),
  spike_low = 1,
  spike_high = 3
)

test_that("qc_core_spike() flags spikes within each series", {
  out <- qc_core_spike(spike_dat) |>
    dplyr::arrange(row_id)

  # sensor 1: the spike (5) gets 4, its neighbours 3, the ends 2
  # sensor 2: the middle value in time (9) gets 4, the ends 2
  expect_equal(out$spike_flag, c(2L, 3L, 4L, 3L, 2L, 2L, 4L, 2L))
  expect_equal(nrow(out), nrow(spike_dat))
  expect_false(dplyr::is_grouped_df(out))
})

test_that("qc_core_spike() keeps the helper columns only when asked", {
  helpers <- c("lag_value", "lead_value", "spike_ref", "spike_value")

  expect_false(any(helpers %in% colnames(qc_core_spike(spike_dat))))
  expect_true(all(
    helpers %in% colnames(qc_core_spike(spike_dat, keep_spike_cols = TRUE))
  ))
})

test_that("qc_core_spike() gives 2 when there are no thresholds", {
  dat <- dplyr::mutate(spike_dat, spike_low = NA, spike_high = NA)

  expect_equal(unique(qc_core_spike(dat)$spike_flag), 2L)
})

test_that("qc_core_spike() warns about duplicate timestamps and gives them and their neighbours 2", {
  dat <- data.frame(
    row_id = 1:8,
    station = "A",
    deployment_range = "2023-Jan-01 to 2023-Dec-31",
    sensor_serial_number = 1,
    variable = "temperature_degree_c",
    timestamp_utc = as.POSIXct("2023-06-01", tz = "UTC") +
      c(0, 1, 2, 3, 3, 4, 5, 6) * 600,
    value = c(1, 1, 1, 1, 9, 1, 1, 1),
    spike_low = 1,
    spike_high = 3
  )

  expect_warning(
    out <- qc_core_spike(dat),
    "1 duplicate timestamp"
  )
  # rows 4 and 5 share a timestamp; rows 3 and 6 are their neighbours
  expect_equal(out$spike_flag, c(2L, 1L, 2L, 2L, 2L, 2L, 1L, 2L))
})


# rolling sd --------------------------------------------------------------

# one sensor, every 10 minutes, with a 3-hour gap before row 8
sd_dat <- data.frame(
  row_id = 1:9,
  station = "A",
  deployment_range = "2023-Jan-01 to 2023-Dec-31",
  sensor_serial_number = 1,
  variable = "temperature_degree_c",
  timestamp_utc = as.POSIXct("2023-06-01", tz = "UTC") +
    c(0, 1, 2, 3, 4, 5, 6, 24, 25) * 600,
  value = c(1, 1, 1, 1, 5, 1, 1, 1, 1),
  rolling_sd_max = 1
)

test_that("qc_core_rolling_sd() flags high rolling standard deviation", {
  # period_hours = 0.5 at 10-minute intervals: windows of 3 observations
  out <- qc_core_rolling_sd(sd_dat, period_hours = 0.5, keep_sd_cols = TRUE)

  # windows that include the 5 have sd 2.31; the others have sd 0
  expect_equal(out$sd_roll, c(NA, 0, 0, 2.31, 2.31, 2.31, 0, NA, NA))
  # first observation and after the gap: 2
  expect_equal(out$rolling_sd_flag, c(2L, 1L, 1L, 3L, 3L, 3L, 1L, 2L, 2L))
})

test_that("qc_core_rolling_sd() keeps the helper columns only when asked", {
  helpers <- c("int_sample", "n_sample", "n_sample_effective", "sd_roll")
  out <- qc_core_rolling_sd(sd_dat, period_hours = 0.5)

  expect_false(any(helpers %in% colnames(out)))
  expect_equal(colnames(out), c(colnames(sd_dat), "rolling_sd_flag"))
})

test_that("qc_core_rolling_sd() warns about duplicate timestamps and gives them 2", {
  dat <- sd_dat
  dat$timestamp_utc[3] <- dat$timestamp_utc[2]

  expect_warning(
    out <- qc_core_rolling_sd(dat, period_hours = 0.5),
    "1 duplicate timestamp"
  )
  expect_equal(out$rolling_sd_flag[out$row_id == 3], 2L)
})

# phase A: today's qc_test_rolling_sd() gives NA here. [phase B] 2
test_that("qc_core_rolling_sd() gives NA when there is no threshold", {
  dat <- dplyr::mutate(sd_dat, rolling_sd_max = NA)
  out <- qc_core_rolling_sd(dat, period_hours = 0.5)

  expect_equal(out$rolling_sd_flag[c(1, 2)], c(2L, NA))
})


test_that("qc_core_rolling_sd() supports left and right windows", {
  left <- qc_core_rolling_sd(
    sd_dat,
    period_hours = 0.5,
    align_window = "left",
    keep_sd_cols = TRUE
  )
  right <- qc_core_rolling_sd(
    sd_dat,
    period_hours = 0.5,
    align_window = "right",
    keep_sd_cols = TRUE
  )

  # left: window is the row and the next 2; right: the row and the previous 2
  expect_equal(left$sd_roll, c(NA, 0, 2.31, 2.31, 2.31, 0, 0, NA, NA))
  expect_equal(right$sd_roll, c(NA, NA, 0, 0, 2.31, 2.31, 2.31, NA, 0))
  expect_equal(left$rolling_sd_flag, c(2L, 1L, 3L, 3L, 3L, 1L, 1L, 2L, 2L))
  expect_equal(right$rolling_sd_flag, c(2L, 2L, 1L, 1L, 3L, 3L, 3L, 2L, 1L))
})

test_that("qc_core_rolling_sd() stops for an unknown align_window", {
  expect_error(
    qc_core_rolling_sd(sd_dat, align_window = "middle"),
    "should be one of"
  )
})

# qc_window_sd() ------------------------------------------------

# expected values come from sd() on the same windows that
# zoo::rollapply(x, width, FUN = sd, align = align, fill = NA) uses
roll_x <- c(1, 2, 4, 7, 11, 16)

test_that("qc_window_sd() matches sd() on centred windows", {
  # odd width: 1 before and 1 after
  expect_equal(
    qc_window_sd(roll_x, 3, "center"),
    c(
      NA,
      sd(roll_x[1:3]),
      sd(roll_x[2:4]),
      sd(roll_x[3:5]),
      sd(roll_x[4:6]),
      NA
    )
  )
  # even width: 1 before and 2 after (as zoo)
  expect_equal(
    qc_window_sd(roll_x, 4, "center"),
    c(NA, sd(roll_x[1:4]), sd(roll_x[2:5]), sd(roll_x[3:6]), NA, NA)
  )
})

test_that("qc_window_sd() matches sd() on left and right windows", {
  expect_equal(
    qc_window_sd(roll_x, 4, "left"),
    c(sd(roll_x[1:4]), sd(roll_x[2:5]), sd(roll_x[3:6]), NA, NA, NA)
  )
  expect_equal(
    qc_window_sd(roll_x, 4, "right"),
    c(NA, NA, NA, sd(roll_x[1:4]), sd(roll_x[2:5]), sd(roll_x[3:6]))
  )
})

test_that("qc_window_sd() uses a different width for each element", {
  out <- qc_window_sd(roll_x, c(0, 1, 3, 4, 2, 10), "right")

  # widths 0 and 1 give NA, as does a window longer than the data
  expect_equal(
    out,
    c(NA, NA, sd(roll_x[1:3]), sd(roll_x[1:4]), sd(roll_x[4:5]), NA)
  )
})

test_that("qc_window_sd() gives NA only for windows that contain an NA", {
  x <- roll_x
  x[2] <- NA
  out <- qc_window_sd(x, 2, "right")

  expect_equal(out, c(NA, NA, NA, sd(x[3:4]), sd(x[4:5]), sd(x[5:6])))
})

test_that("qc_window_sd() is accurate for large values with small variation", {
  # e.g. dissolved oxygen around 100 % saturation, or depth in metres
  set.seed(1)
  x <- 1000 + round(rnorm(500, sd = 0.05), 2)
  expected <- vapply(5:500, function(i) sd(x[(i - 4):i]), numeric(1))

  expect_equal(qc_window_sd(x, 5, "right")[5:500], expected, tolerance = 1e-10)
  # a constant series gives exactly 0, not a tiny positive number
  expect_identical(qc_window_sd(rep(12.34, 10), 3, "center")[2:9], rep(0, 8))
})

# depth crosscheck --------------------------------------------------

depth_dat <- data.frame(
  row_id = 1:9,
  station = "A",
  deployment_range = c("d1", "d1", "d1", "d2", "d2", "d2", "d2", "d3", "d3"),
  sensor_serial_number = c(1, 1, 2, 3, 3, 4, 4, 5, 5),
  sensor_depth_at_low_tide_m = c(5, 5, 10, 5, 5, 5, 5, 5, 5),
  variable = c(
    "sensor_depth_measured_m",
    "sensor_depth_measured_m",
    "temperature_degree_c",
    "sensor_depth_measured_m",
    "sensor_depth_measured_m",
    "sensor_depth_measured_m",
    "temperature_degree_c",
    "temperature_degree_c",
    "temperature_degree_c"
  ),
  value = c(9, 10, 12, 12, 13, 5, 11, 12, 13)
)

test_that("qc_core_depth_crosscheck() gives each deployment its worst sensor flag", {
  out <- qc_core_depth_crosscheck(depth_dat, depth_diff_max = 4)

  # d1: 1 (a diff equal to depth_diff_max passes)
  # d2: 3 (one failing sensor fails the deployment)
  # d3: 2 (no measured depth)
  expect_equal(out$depth_crosscheck_flag, c(1L, 1L, 1L, 3L, 3L, 3L, 3L, 2L, 2L))
})

test_that("qc_core_depth_crosscheck() only adds the flag column", {
  out <- qc_core_depth_crosscheck(depth_dat, depth_diff_max = 4)

  expect_equal(out[colnames(depth_dat)], depth_dat)
  expect_equal(colnames(out), c(colnames(depth_dat), "depth_crosscheck_flag"))
})
