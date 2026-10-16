# Generated with Claude Opus 5.5
# Tests for qc_core_thresholds(keys, qc_test, county) on qaqcmar branch new-core.
# Expected values are typed in from qaqcmar::thresholds (16 Oct 2026). If a
# threshold value changes, update the number here too.

sal <- data.frame(
  variable = "salinity_psu",
  sensor_key = "aquameasure",
  month = 7L
)

test_that("qc_core_thresholds() returns the threshold columns for each test", {
  key_cols <- c("variable", "sensor_key", "month")

  expect_setequal(
    colnames(qc_core_thresholds(sal, "grossrange", "Halifax")),
    c(key_cols, "sensor_min", "sensor_max", "user_min", "user_max")
  )
  expect_setequal(
    colnames(qc_core_thresholds(sal, "climatology", "Halifax")),
    c(key_cols, "season_min", "season_max")
  )
  expect_setequal(
    colnames(qc_core_thresholds(sal, "spike", "Halifax")),
    c(key_cols, "spike_low", "spike_high")
  )
  expect_setequal(
    colnames(qc_core_thresholds(sal, "rolling_sd", "Halifax")),
    c(key_cols, "rolling_sd_max")
  )
})

test_that("qc_core_thresholds() returns one row per key", {
  keys <- data.frame(
    variable = c(
      "temperature_degree_c",
      "temperature_degree_c",
      "salinity_psu"
    ),
    sensor_key = c("hobo", "hobo do", "aquameasure"),
    month = 7L
  )

  expect_equal(nrow(qc_core_thresholds(keys, "grossrange", "Halifax")), 3)
})

test_that("Inverness salinity uses the Inverness rows", {
  inv <- qc_core_thresholds(sal, "grossrange", "Inverness")
  hfx <- qc_core_thresholds(sal, "grossrange", "Halifax")

  # sensor limits are the same; user limits are Inverness vs province-wide
  expect_equal(inv$sensor_min, 0)
  expect_equal(inv$sensor_max, 40)
  expect_equal(inv$user_min, 19.8)
  expect_equal(inv$user_max, 24.703)
  expect_equal(hfx$user_min, 26.317)
  expect_equal(hfx$user_max, 34.655)

  expect_equal(
    qc_core_thresholds(sal, "rolling_sd", "Inverness")$rolling_sd_max,
    0.26
  )
  expect_equal(
    qc_core_thresholds(sal, "rolling_sd", "Halifax")$rolling_sd_max,
    0.42
  )
  expect_equal(qc_core_thresholds(sal, "spike", "Inverness")$spike_high, 0.9)
  expect_equal(qc_core_thresholds(sal, "spike", "Halifax")$spike_high, 1.35)
})

test_that("hobo do has its own temperature sensor limits", {
  keys <- data.frame(
    variable = "temperature_degree_c",
    sensor_key = c("hobo", "hobo do"),
    month = 7L
  )
  out <- qc_core_thresholds(keys, "grossrange", "Halifax")

  # rows come back sorted by sensor_key: hobo, then hobo do
  expect_equal(out$sensor_min, c(-40, -5))
  expect_equal(out$sensor_max, c(70, 40))
  expect_equal(out$user_min, c(-8.892, -8.892))
  expect_equal(out$user_max, c(23.298, 23.298))
})

test_that("climatology thresholds are looked up by month", {
  keys <- data.frame(
    variable = "temperature_degree_c",
    sensor_key = "hobo",
    month = c(1L, 7L)
  )
  out <- qc_core_thresholds(keys, "climatology", "Halifax")

  expect_equal(out$season_min, c(-2.112, 0.301))
  expect_equal(out$season_max, c(6.954, 22.309))
})

test_that("depth spike thresholds depend on sensor type", {
  keys <- data.frame(
    variable = "sensor_depth_measured_m",
    sensor_key = c("aquameasure", "vr2ar"),
    month = 7L
  )
  out <- qc_core_thresholds(keys, "spike", "Annapolis")

  expect_equal(out$spike_low, c(0.8, 1))
  expect_equal(out$spike_high, c(2.4, 3))
})

test_that("keys with no thresholds are left out", {
  chl <- data.frame(
    variable = "chlorophyll_blue_ug_per_l",
    sensor_key = "aquameasure",
    month = 7L
  )

  expect_equal(nrow(qc_core_thresholds(chl, "grossrange", "Halifax")), 0)
  expect_equal(nrow(qc_core_thresholds(chl, "rolling_sd", "Halifax")), 0)
})

# Phase A: today's qc_test_grossrange() has no thresholds for a sensor type
# without sensor limits, so it gets flag 2. [phase B] this becomes a stop().
test_that("gross range gives no thresholds for a sensor type without sensor limits", {
  abc <- data.frame(
    variable = "temperature_degree_c",
    sensor_key = "abc",
    month = 7L
  )

  expect_equal(nrow(qc_core_thresholds(abc, "grossrange", "Halifax")), 0)
})
