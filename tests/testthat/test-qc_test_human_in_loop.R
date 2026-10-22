path <- system.file("testdata", package = "qaqcmar")

dat_hil <- readRDS(paste0(path, "/test_data_rolling_sd.RDS"))

hil_1 <- data.frame(
  station = "lighthouse",
  deployment_range = "2023-Jan-01 to 2023-Jan-15",
  variable = "dissolved_oxygen_percent_saturation",
  sensor_serial_number = 123,
  timestamp_utc_min = as_datetime("2023-01-15 12:30:00"),
  timestamp_utc_max = as_datetime("2023-01-22 00:00:00"),
  qc_test = "rolling_sd",
  qc_flag = 3,
  human_in_loop_flag_value = 4,
  hil_comment = "dissolved_oxygen_percent_saturation: test comment 1"
)

hil_2 <- data.frame(
  station = "lighthouse",
  deployment_range = "2023-Jan-01 to 2023-Jan-15",
  variable = "dissolved_oxygen_percent_saturation",
  sensor_serial_number = 123,
  timestamp_utc_min = as_datetime("2023-01-22 00:00:00"),
  timestamp_utc_max = NA,
  qc_test = "rolling_sd_flag_value",
  qc_flag = 2,
  human_in_loop_flag_value = 4,
  hil_comment = "dissolved_oxygen_percent_saturation: test comment 2"
)

hil_table <- rbind(hil_1, hil_2)

dat_qc <- dat_hil %>%
  qc_test_rolling_sd() %>%
  qc_test_human_in_loop(human_in_loop_table = hil_table)

dat_qc_4 <- dat_qc %>%
  filter(timestamp_utc >= as_datetime("2023-01-15 12:30:00"))

dat_qc_1 <- dat_qc %>%
  dplyr::anti_join(
    dat_qc_4,
    dplyr::join_by(
      county,
      waterbody,
      station,
      deployment_range,
      sensor_type,
      sensor_serial_number,
      timestamp_utc,
      sensor_depth_at_low_tide_m,
      dissolved_oxygen_percent_saturation,
      sensor_depth_measured_m,
      rolling_sd_flag_dissolved_oxygen_percent_saturation,
      rolling_sd_flag_sensor_depth_measured_m,
      human_in_loop_flag_dissolved_oxygen_percent_saturation,
      human_in_loop_flag_sensor_depth_measured_m,
      hil_comment
    )
  )

test_that("qc_test_human_in_loop() assigns correct flags", {
  expect_equal(
    as.numeric(
      unique(dat_qc_1$human_in_loop_flag_dissolved_oxygen_percent_saturation)
    ),
    1
  )

  expect_equal(
    as.numeric(
      unique(dat_qc_1$human_in_loop_flag_sensor_depth_measured_m)
    ),
    1
  )

  expect_equal(
    as.numeric(
      unique(dat_qc_4$human_in_loop_flag_dissolved_oxygen_percent_saturation)
    ),
    4
  )

  # sensor depth should still be flagged 1
  expect_equal(
    as.numeric(
      unique(dat_qc_4$human_in_loop_flag_sensor_depth_measured_m)
    ),
    1
  )
})

test_that("qc_test_human_in_loop() does not add rows", {
  expect_equal(nrow(dat_hil), nrow(dat_qc))
})
