path <- system.file("testdata", package = "qaqcmar")

qc_tests <- c(
  "climatology",
  "grossrange",
  "rolling_sd",
  "spike",
  "depth_crosscheck"
)

n_var <- nrow(
  readRDS(paste0(path, "/test_data_rolling_sd.RDS")) %>%
    ss_pivot_longer() %>%
    distinct(variable)
)

dat_wide <- readRDS(paste0(path, "/test_data_rolling_sd.RDS")) %>%
  qc_test_all(
    qc_tests = qc_tests,
    county = "Halifax"
  )

dat_long <- dat_wide %>%
  qc_pivot_longer(qc_tests = qc_tests)

dat_wide2 <- dat_long %>%
  qc_pivot_wider()

dat_wide_max <- dat_wide %>%
  qc_assign_max_flag()

dat_wide_max2 <- dat_wide %>%
  qc_assign_max_flag(return_all = FALSE)

# pivot max flags ---------------------------------------------------------

# dat_pivot_long_max <- dat_wide_max %>%
#   qc_pivot_longer(qc_tests = c("qc", qc_tests))

# dat_pivot_long_max2 <- dat_wide_max2 %>%
#   qc_pivot_longer(qc_tests = "qc")

test_that("qc_pivot_longer() returns correct number of flag columns", {
  expect_equal(
    ncol(select(dat_long, contains("flag"))),
    length(qc_tests)
  )
})

test_that("qc_pivot_wider() returns dat_wide", {
  expect_equal(dat_wide, dat_wide2)
})

test_that("qc_assign_max_flags() has correct number of columns", {
  expect_equal(ncol(dat_wide) + n_var, ncol(dat_wide_max))
})


# variables with no thresholds pass through with flag 2 (not evaluated)
untested_dat <- data.frame(
  county = "Lunenburg",
  station = "A",
  deployment_range = "2023-Jan-01 to 2023-Dec-31",
  sensor_type = "aquameasure",
  sensor_serial_number = 1,
  sensor_depth_at_low_tide_m = 2,
  timestamp_utc = as.POSIXct("2023-06-01", tz = "UTC") + (0:5) * 600,
  temperature_degree_c = 10,
  chlorophyll_blue_ug_per_l = 1,
  tilt_degree = 5
)

untested_long <- untested_dat |>
  qc_test_all() |>
  qc_assign_max_flag() |>
  qc_pivot_longer(qc_tests = c(qc_tests[1:4], "qc"))

test_that("qc_pivot_longer() keeps variables that have no thresholds", {
  expect_setequal(
    unique(untested_long$variable),
    c("temperature_degree_c", "chlorophyll_blue_ug_per_l", "tilt_degree")
  )
  expect_equal(nrow(untested_long), 3 * nrow(untested_dat))
})

test_that("variables with no thresholds get flag 2 for every test", {
  untested_flags <- untested_long |>
    filter(variable != "temperature_degree_c") |>
    select(contains("flag_value")) |>
    distinct()

  expect_equal(ncol(untested_flags), 5)
  expect_equal(nrow(untested_flags), 1)
  expect_true(all(untested_flags == 2))
})
