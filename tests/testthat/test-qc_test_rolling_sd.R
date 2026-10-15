path <- system.file("testdata", package = "qaqcmar")

dat <- readRDS(paste0(path, "/test_data_rolling_sd.RDS"))

test_that("qc_test_rolling_sd() assigns correct flags", {
  dat_qc <- dat %>%
    qc_test_rolling_sd(keep_sd_cols = TRUE) %>%
    qc_pivot_longer(qc_tests = "rolling_sd") %>%
    filter(variable == "dissolved_oxygen_percent_saturation")

  expect_equal(
    as.numeric(
      dat_qc |>
        filter(
          between(
            timestamp_utc,
            as_datetime("2023-01-01 23:30:00"),
            as_datetime("2023-01-07 13:30:00")
          )
        ) |>
        distinct(rolling_sd_flag_value) |>
        dplyr::pull(rolling_sd_flag_value)
    ),
    1
  )

  expect_equal(
    as.numeric(
      dat_qc |>
        filter(
          between(
            timestamp_utc,
            as_datetime("2023-01-01 12:00:00"),
            as_datetime("2023-01-01 23:00:00")
          ) |
            between(
              timestamp_utc,
              as_datetime("2023-01-22 00:30:00"),
              as_datetime("2023-02-15 12:00:00")
            )
        ) |>
        distinct(rolling_sd_flag_value) |>
        dplyr::pull(rolling_sd_flag_value)
    ),
    2
  )

  expect_equal(
    as.numeric(
      dat_qc |>
        filter(between(
          timestamp_utc,
          as_datetime("2023-01-07 14:00:00"),
          as_datetime("2023-01-22 00:00:00")
        )) |>
        distinct(rolling_sd_flag_value) |>
        dplyr::pull(rolling_sd_flag_value)
    ),
    3
  )
})

test_that("qc_test_rolling_sd() assigns 2 for intervals > min_sample_interval", {
  #dat <- readRDS(paste0(path, "/test_data_rolling_sd.RDS"))

  qc_roll_int <- dat %>%
    qc_test_rolling_sd(keep_sd_cols = TRUE, max_interval_hours = 0.25) %>%
    qc_pivot_longer(qc_tests = "rolling_sd")

  expect_equal(as.numeric(unique(qc_roll_int$rolling_sd_flag_value)), 2)
})


test_that("qc_test_rolling_sd() gives flag 2 to a duplicate timestamp", {
  #dat <- readRDS(file.path(path, "test_data_rolling_sd.RDS"))

  # duplicate one observation, with a different value so it can be found
  dup <- dat[100, ]
  dup$dissolved_oxygen_percent_saturation <-
    dup$dissolved_oxygen_percent_saturation + 1
  dat_dup <- dplyr::bind_rows(dat, dup)

  expect_warning(
    out <- suppressMessages(qc_test_rolling_sd(dat_dup)),
    "duplicate timestamp"
  )

  # no rows lost or merged, and no list-columns from pivot_wider()
  expect_equal(nrow(out), nrow(dat_dup))
  expect_false(any(vapply(out, is.list, logical(1))))

  flag_dup <- out$rolling_sd_flag_dissolved_oxygen_percent_saturation[
    out$timestamp_utc == dup$timestamp_utc &
      out$dissolved_oxygen_percent_saturation ==
        dup$dissolved_oxygen_percent_saturation
  ]
  expect_equal(as.numeric(flag_dup), 2)
})

test_that("qc_test_rolling_sd() keeps an exact duplicate row", {
  #dat <- readRDS(file.path(path, "test_data_rolling_sd.RDS"))
  dat_dup <- dplyr::bind_rows(dat, dat[100, ])

  expect_warning(
    out <- suppressMessages(qc_test_rolling_sd(dat_dup)),
    "duplicate timestamp"
  )

  expect_equal(nrow(out), nrow(dat_dup))
  expect_false(any(vapply(out, is.list, logical(1))))

  flags <- out$rolling_sd_flag_dissolved_oxygen_percent_saturation[
    out$timestamp_utc == dat$timestamp_utc[100]
  ]
  expect_true(2 %in% as.numeric(flags))
})

# 2. Sub-minute sampling ---------------------------------------------------

# added as an edge case. Should not matter for CMAR data
test_that("qc_test_rolling_sd() works with 30-second sampling", {
  n <- 6000 # 50 hours, so the middle rows have a full 24-hour window
  dat <- data.frame(
    county = "Lunenburg",
    station = "test",
    deployment_range = "2023-Jun-01 to 2023-Jun-03",
    sensor_type = "aquameasure",
    sensor_serial_number = "1",
    sensor_depth_at_low_tide_m = 2,
    timestamp_utc = as.POSIXct("2023-06-01", tz = "UTC") + 30 * (0:(n - 1)),
    temperature_degree_c = 10 + rep(c(-0.01, 0.01), n / 2)
  )

  out <- suppressMessages(
    qc_test_rolling_sd(dat, county = "Lunenburg", keep_sd_cols = TRUE)
  )

  expect_equal(nrow(out), n)
  # interval kept in minutes, not rounded to 0
  expect_equal(unique(out$int_sample[-1]), 0.5) #[-1] to remove first row, which is NA
  # 24 hours of 30-second samples
  expect_equal(unique(out$n_sample[-1]), 2880)

  # rows with a full centred window are evaluated (flag 1);
  # rows near the ends are not (flag 2)
  flag <- as.numeric(out$rolling_sd_flag_temperature_degree_c)
  expect_true(all(flag[1500:4500] == 1))
  expect_true(all(flag[c(1:100, (n - 100):n)] == 2))
})
