path <- system.file("testdata", package = "qaqcmar")

dat_hil <- readRDS(paste0(path, "/test_data_rolling_sd.RDS"))

hil_1 <- data.frame(
  station = "lighthouse",
  depl_range = "2023-Jan-01 to 2023-Jan-15",
  variable = "dissolved_oxygen_percent_saturation",
  sensor_serial_number = 123,
  timestamp_prompt = "between",
  timestamp_utc_min = as_datetime("2023-01-15 12:30:00"),
  timestamp_utc_max = as_datetime("2023-01-22 00:00:00"),
  qc_test_column = "rolling_sd_flag_value",
  qc_flag_value = 3,
  human_in_loop_flag_value = 4,
  human_in_loop_comment = "dissolved_oxygen_percent_saturation: test comment 1"
)

hil_2 <- data.frame(
  station = "lighthouse",
  depl_range = "2023-Jan-01 to 2023-Jan-15",
  variable = "dissolved_oxygen_percent_saturation",
  sensor_serial_number = 123,
  timestamp_prompt = "between",
  timestamp_utc_min = as_datetime("2023-01-22 00:00:00"),
  timestamp_utc_max = NA,
  qc_test_column = "rolling_sd_flag_value",
  qc_flag_value = 2,
  human_in_loop_flag_value = 4,
  human_in_loop_comment = "dissolved_oxygen_percent_saturation: test comment 2"
)

hil_table <- rbind(hil_1, hil_2)

dat_qc <- dat_hil %>%
  qc_test_rolling_sd() %>%
  qc_test_human_in_loop(
    qc_tests = "rolling_sd",
    human_in_loop_table = hil_table
  )

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

test_that("qc_test_human_in_loop() assigns comments", {
  expect_true(
    all(
      unique(dat_qc_4$hil_comment) %in%
        unique(hil_table$human_in_loop_comment)
    )
  )

  expect_equal(unique(dat_qc_1$hil_comment), NA_character_)
})


test_that("qc_test_human_in_loop() does not add rows", {
  expect_equal(nrow(dat_hil), nrow(dat_qc))
})


# new tests (Claude Opus 5.5) --------------------------------------------

hil_t0 <- as.POSIXct("2023-06-01", tz = "UTC")

hil_small <- data.frame(
  county = "Halifax",
  station = "A",
  deployment_range = "2023-Jun-01 to 2023-Jun-30",
  sensor_type = rep(c("aquameasure", "hobo"), each = 4),
  sensor_serial_number = rep(c(1, 2), each = 4),
  sensor_depth_at_low_tide_m = rep(c(2, 5), each = 4),
  timestamp_utc = hil_t0 + rep(0:3, 2) * 3600,
  dissolved_oxygen_percent_saturation = c(100, 101, 102, 103, NA, NA, NA, NA),
  temperature_degree_c = c(10, 11, 12, 13, 10, 11, 12, 13),
  grossrange_flag_dissolved_oxygen_percent_saturation = ordered(
    c(1, 1, 1, 1, NA, NA, NA, NA),
    levels = 1:4
  ),
  grossrange_flag_temperature_degree_c = ordered(
    c(1, 3, 1, 1, 1, 1, 1, 1),
    levels = 1:4
  )
)

# flag sensor 1 temperature 4 between 00:30 and 02:30 (the 01:00 and 02:00
# observations)
hil_row <- data.frame(
  station = "A",
  depl_range = "2023-Jun-01 to 2023-Jun-30",
  variable = "temperature_degree_c",
  sensor_serial_number = 1,
  timestamp_prompt = "between",
  timestamp_utc_min = hil_t0 + 0.5 * 3600,
  timestamp_utc_max = hil_t0 + 2.5 * 3600,
  qc_test_column = "grossrange_flag_value",
  qc_flag_value = NA,
  human_in_loop_flag_value = 4,
  human_in_loop_comment = "test comment"
)

test_that("qc_test_human_in_loop() returns dat unchanged with no table", {
  expect_message(
    out <- qc_test_human_in_loop(hil_small, human_in_loop_table = NULL),
    "No flags applied"
  )
  expect_identical(out, hil_small)

  out <- suppressMessages(
    qc_test_human_in_loop(hil_small, human_in_loop_table = hil_row[0, ])
  )
  expect_identical(out, hil_small)
})

test_that("qc_test_human_in_loop() flags only the variable, sensor and time given", {
  out <- suppressMessages(
    qc_test_human_in_loop(hil_small, hil_row, qc_tests = "grossrange")
  ) |>
    arrange(sensor_serial_number, timestamp_utc)

  expect_equal(
    as.numeric(out$human_in_loop_flag_temperature_degree_c),
    c(1, 4, 4, 1, 1, 1, 1, 1)
  )
  expect_equal(
    as.numeric(out$human_in_loop_flag_dissolved_oxygen_percent_saturation),
    c(1, 1, 1, 1, NA, NA, NA, NA)
  )
  expect_equal(
    out$hil_comment,
    c(NA, "test comment", "test comment", NA, NA, NA, NA, NA)
  )
})

test_that("variable = NA flags every variable of that sensor", {
  out <- suppressMessages(
    qc_test_human_in_loop(
      hil_small,
      mutate(hil_row, variable = NA),
      qc_tests = "grossrange"
    )
  ) |>
    arrange(sensor_serial_number, timestamp_utc)

  expect_equal(
    as.numeric(out$human_in_loop_flag_temperature_degree_c),
    c(1, 4, 4, 1, 1, 1, 1, 1)
  )
  expect_equal(
    as.numeric(out$human_in_loop_flag_dissolved_oxygen_percent_saturation),
    c(1, 4, 4, 1, NA, NA, NA, NA)
  )
})

test_that("sensor_serial_number = NA flags that variable on every sensor", {
  out <- suppressMessages(
    qc_test_human_in_loop(
      hil_small,
      mutate(hil_row, sensor_serial_number = NA),
      qc_tests = "grossrange"
    )
  ) |>
    arrange(sensor_serial_number, timestamp_utc)

  expect_equal(
    as.numeric(out$human_in_loop_flag_temperature_degree_c),
    c(1, 4, 4, 1, 1, 4, 4, 1)
  )
})

test_that("timestamp_prompt = 'outside' flags only the observations outside the window", {
  out <- suppressMessages(
    qc_test_human_in_loop(
      hil_small,
      mutate(hil_row, timestamp_prompt = "outside"),
      qc_tests = "grossrange"
    )
  ) |>
    arrange(sensor_serial_number, timestamp_utc)

  expect_equal(
    as.numeric(out$human_in_loop_flag_temperature_degree_c),
    c(4, 1, 1, 4, 1, 1, 1, 1)
  )
})

test_that("qc_flag_value limits the flag to observations with that flag", {
  out <- suppressMessages(
    qc_test_human_in_loop(
      hil_small,
      mutate(hil_row, qc_flag_value = 3),
      qc_tests = "grossrange"
    )
  ) |>
    arrange(sensor_serial_number, timestamp_utc)

  # only 01:00 has grossrange flag 3
  expect_equal(
    as.numeric(out$human_in_loop_flag_temperature_degree_c),
    c(1, 4, 1, 1, 1, 1, 1, 1)
  )
})

test_that("qc_test_human_in_loop() stops for a bad timestamp_prompt or two deployments", {
  expect_error(
    suppressMessages(
      qc_test_human_in_loop(
        hil_small,
        mutate(hil_row, timestamp_prompt = "inside"),
        qc_tests = "grossrange"
      )
    ),
    "hil_timestamp_prompt must be"
  )
  expect_error(
    suppressMessages(
      qc_test_human_in_loop(
        mutate(hil_small, station = rep(c("A", "B"), each = 4)),
        hil_row,
        qc_tests = "grossrange"
      )
    ),
    "more than one deployment"
  )
})
