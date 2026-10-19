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
