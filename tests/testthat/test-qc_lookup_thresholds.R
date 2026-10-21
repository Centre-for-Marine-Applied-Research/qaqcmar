# Generated with Claude Opus 5.5

sal <- data.frame(
  variable = "salinity_psu",
  sensor_key = "aquameasure",
  month = 7L
)

test_that("qc_lookup_thresholds() returns the threshold columns for each test", {
  key_cols <- c("variable", "sensor_key", "month")

  expect_setequal(
    colnames(qc_lookup_thresholds(sal, "grossrange", "Halifax")),
    c(key_cols, "sensor_min", "sensor_max", "user_min", "user_max")
  )
  expect_setequal(
    colnames(qc_lookup_thresholds(sal, "climatology", "Halifax")),
    c(key_cols, "season_min", "season_max")
  )
  expect_setequal(
    colnames(qc_lookup_thresholds(sal, "spike", "Halifax")),
    c(key_cols, "spike_low", "spike_high")
  )
  expect_setequal(
    colnames(qc_lookup_thresholds(sal, "rolling_sd", "Halifax")),
    c(key_cols, "rolling_sd_max")
  )
})

test_that("qc_lookup_thresholds() returns one row per key", {
  keys <- data.frame(
    variable = c(
      "temperature_degree_c",
      "temperature_degree_c",
      "salinity_psu"
    ),
    sensor_key = c("hobo", "hobo do", "aquameasure"),
    month = 7L
  )

  expect_equal(nrow(qc_lookup_thresholds(keys, "grossrange", "Halifax")), 3)
})

test_that("Inverness salinity uses the Inverness rows", {
  inv <- qc_lookup_thresholds(sal, "grossrange", "Inverness")
  hfx <- qc_lookup_thresholds(sal, "grossrange", "Halifax")

  # sensor limits are the same; user limits are Inverness vs province-wide
  expect_equal(inv$sensor_min, 0)
  expect_equal(inv$sensor_max, 40)
  expect_equal(inv$user_min, 19.8)
  expect_equal(inv$user_max, 24.703)
  expect_equal(hfx$user_min, 26.317)
  expect_equal(hfx$user_max, 34.655)

  expect_equal(
    qc_lookup_thresholds(sal, "rolling_sd", "Inverness")$rolling_sd_max,
    0.26
  )
  expect_equal(
    qc_lookup_thresholds(sal, "rolling_sd", "Halifax")$rolling_sd_max,
    0.42
  )
  expect_equal(qc_lookup_thresholds(sal, "spike", "Inverness")$spike_high, 0.9)
  expect_equal(qc_lookup_thresholds(sal, "spike", "Halifax")$spike_high, 1.35)
})

test_that("hobo do has its own temperature sensor limits", {
  keys <- data.frame(
    variable = "temperature_degree_c",
    sensor_key = c("hobo", "hobo do"),
    month = 7L
  )
  out <- qc_lookup_thresholds(keys, "grossrange", "Halifax")

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
  out <- qc_lookup_thresholds(keys, "climatology", "Halifax")

  expect_equal(out$season_min, c(-2.112, 0.301))
  expect_equal(out$season_max, c(6.954, 22.309))
})

test_that("depth spike thresholds depend on sensor type", {
  keys <- data.frame(
    variable = "sensor_depth_measured_m",
    sensor_key = c("aquameasure", "vr2ar"),
    month = 7L
  )
  out <- qc_lookup_thresholds(keys, "spike", "Annapolis")

  expect_equal(out$spike_low, c(0.8, 1))
  expect_equal(out$spike_high, c(2.4, 3))
})

test_that("keys with no thresholds are left out", {
  chl <- data.frame(
    variable = "chlorophyll_blue_ug_per_l",
    sensor_key = "aquameasure",
    month = 7L
  )

  expect_equal(nrow(qc_lookup_thresholds(chl, "grossrange", "Halifax")), 0)
  expect_equal(nrow(qc_lookup_thresholds(chl, "rolling_sd", "Halifax")), 0)
})

test_that("an unknown sensor type stops only where thresholds depend on the sensor", {
  abc <- data.frame(
    variable = c("temperature_degree_c", "sensor_depth_measured_m"),
    sensor_key = "abc",
    month = 7L
  )

  # gross range: every variable has sensor limits
  expect_error(
    qc_lookup_thresholds(abc[1, ], "grossrange", "Halifax"),
    'No grossrange thresholds for sensor_type << abc >>'
  )
  # spike: only depth thresholds depend on the sensor
  expect_error(
    qc_lookup_thresholds(abc[2, ], "spike", "Halifax"),
    'No spike thresholds for sensor_type << abc >>'
  )
  expect_equal(nrow(qc_lookup_thresholds(abc[1, ], "spike", "Halifax")), 1)
})

test_that("a known sensor type with no thresholds in the county warns, not stops", {
  # Colchester has a depth spike row for vr2ar but not for aquameasure
  aqm <- data.frame(
    variable = "sensor_depth_measured_m",
    sensor_key = "aquameasure",
    month = 7L
  )

  expect_warning(
    out <- qc_lookup_thresholds(aqm, "spike", "Colchester"),
    'No spike thresholds in Colchester for sensor_type << aquameasure >>'
  )
  # no thresholds: the key is left out, so the spike test gives flag 2
  expect_equal(nrow(out), 0)
  # in a county that has aquameasure rows, the thresholds are found
  expect_no_warning(halifax <- qc_lookup_thresholds(aqm, "spike", "Halifax"))
  expect_equal(nrow(halifax), 1)
  # a sensor type that is in no county still stops
  expect_error(
    qc_lookup_thresholds(
      dplyr::mutate(aqm, sensor_key = "abc"),
      "spike",
      "Colchester"
    ),
    'No spike thresholds for sensor_type << abc >>'
  )
})
