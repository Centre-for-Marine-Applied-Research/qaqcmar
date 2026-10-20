# testdata was generated such that we KNOW what flag should be assigned to each
# observation (data-raw/test_data.R)

path <- system.file("testdata", package = "qaqcmar")

dat <- readRDS(paste0(path, "/test_data_spike.RDS"))

test_that("qc_test_spike() assigns correct flags", {
  dat_qc <- dat %>%
    qc_test_spike(county = "Halifax") %>%
    mutate(day = lubridate::day(timestamp_utc)) %>%
    qc_pivot_longer(qc_tests = "spike")

  expect_equal(
    as.numeric(
      dat_qc |>
        mutate(month = month(timestamp_utc)) |>
        filter((month == 1 & day == 1) | (month == 2 & day == 28)) |>
        distinct(spike_flag_value) |>
        dplyr::pull(spike_flag_value)
    ),
    2
  )

  expect_equal(
    as.numeric(
      dat_qc |>
        filter(day %in% c(3, 9, 11, 17)) |>
        distinct(spike_flag_value) |>
        dplyr::pull(spike_flag_value)
    ),
    3
  )

  expect_equal(
    as.numeric(
      dat_qc |>
        filter(day == 10) |>
        distinct(spike_flag_value) |>
        dplyr::pull(spike_flag_value)
    ),
    4
  )

  expect_equal(
    as.numeric(
      dat_qc |>
        filter(!(day %in% c(1, 3, 9, 10, 11, 17, 28))) |>
        distinct(spike_flag_value) |>
        dplyr::pull(spike_flag_value)
    ),
    1
  )
})

test_that("qc_test_spike() uses sensor-specific depth thresholds by default", {
  # the same 0.8 m depth jump on an aquameasure and a vr2ar (Halifax:
  # aquameasure spike_low 0.35, spike_high 1.05; vr2ar spike_low 1, spike_high 3)
  dat <- data.frame(
    county = "Halifax",
    station = "A",
    deployment_range = "2023-Jan-01 to 2023-Dec-31",
    sensor_type = rep(c("aquameasure", "vr2ar"), each = 3),
    sensor_serial_number = rep(c(1, 2), each = 3),
    sensor_depth_at_low_tide_m = rep(c(2, 5), each = 3),
    timestamp_utc = as.POSIXct("2023-06-01", tz = "UTC") + rep(0:2, 2) * 600,
    sensor_depth_measured_m = c(2, 2.8, 2, 5, 5.8, 5)
  )

  out <- suppressMessages(qc_test_spike(dat))

  # middle observation: 3 for the aquameasure, 1 for the vr2ar
  expect_equal(
    out$spike_flag_sensor_depth_measured_m,
    ordered(c(2, 3, 2, 2, 1, 2), levels = 1:4)
  )
})
