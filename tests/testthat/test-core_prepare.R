# Generated with Claude Opus 5.5
path <- system.file("testdata", package = "qaqcmar")

dat_gr <- readRDS(paste0(path, "/test_data_grossrange.RDS"))

var_cols <- function(dat) {
  intersect(
    colnames(dat),
    c(
      "dissolved_oxygen_percent_saturation",
      "dissolved_oxygen_uncorrected_mg_per_l",
      "ph_ph",
      "salinity_psu",
      "sensor_depth_measured_m",
      "temperature_degree_c"
    )
  )
}

test_that("qc_core_prepare() gives one row per non-NA value", {
  long <- qc_core_prepare(dat_gr)

  expect_equal(
    nrow(long),
    sum(
      !is.na(dat_gr[c(
        "dissolved_oxygen_percent_saturation",
        "temperature_degree_c"
      )])
    )
  )
  expect_equal(anyDuplicated(long[c("row_id", "variable")]), 0)
  expect_false(anyNA(long$value))
  expect_type(long$value, "double")
  expect_type(long$row_id, "integer")
  expect_type(long$month, "integer")
  expect_true(all(c("sensor_key", "month") %in% colnames(long)))
})

test_that("row_id maps every long value back to its wide cell", {
  long <- qc_core_prepare(dat_gr)

  wide <- long |>
    #dplyr::select(row_id, variable, value) |>
    tidyr::pivot_wider(names_from = variable, values_from = value) |>
    dplyr::arrange(row_id)
  vars <- setdiff(colnames(wide), c("row_id", "sensor_key", "month"))

  expect_equal(
    as.data.frame(wide[vars]),
    as.data.frame(dat_gr[wide$row_id, vars])
  )
})

test_that("pass-through columns are unchanged", {
  long <- qc_core_prepare(dat_gr)
  expect_equal(long$sensor_type, dat_gr$sensor_type[long$row_id])
  expect_equal(long$timestamp_utc, dat_gr$timestamp_utc[long$row_id])
})

test_that("sensor_key marks hobo DO rows, and sensor_type is unchanged", {
  dat <- data.frame(
    county = "Halifax",
    station = "A",
    deployment_range = "2023-Jan-01 to 2023-Dec-31",
    sensor_type = c("hobo", "hobo", "hobo", "tidbit"),
    sensor_serial_number = c(1, 1, 2, 3),
    timestamp_utc = as.POSIXct("2023-06-01", tz = "UTC") + 0:3 * 600,
    dissolved_oxygen_uncorrected_mg_per_l = c(8, NA, NA, NA),
    temperature_degree_c = c(10, 11, 12, 13)
  )

  # phase A: row by row
  long <- qc_core_prepare(dat)
  key <- long$sensor_key[long$variable == "temperature_degree_c"]
  expect_equal(key, c("hobo do", "hobo", "hobo", "tidbit"))
  expect_equal(unique(long$sensor_type), c("hobo", "tidbit"))

  # # phase B: the whole sensor (serial 1), but not the other hobo (serial 2)
  # long <- qc_core_prepare(dat, hobo_do = "sensor")
  # key <- long$sensor_key[long$variable == "temperature_degree_c"]
  # expect_equal(key, c("hobo do", "hobo do", "hobo", "tidbit"))
})

test_that("month comes from timestamp_utc", {
  dat <- dat_gr[1:2, ]
  dat$timestamp_utc <- as.POSIXct(
    c("2023-01-31 23:50:00", "2023-12-01 00:00:00"),
    tz = "UTC"
  )
  long <- qc_core_prepare(dat)
  expect_equal(sort(unique(long$month)), c(1L, 12L))
})

test_that("qc_core_prepare() stops on bad input", {
  expect_error(
    qc_core_prepare(dplyr::select(dat_gr, -station)),
    "missing required column\\(s\\): station"
  )

  dat <- dat_gr
  dat$timestamp_utc <- as.character(dat$timestamp_utc)
  expect_error(qc_core_prepare(dat), "must be POSIXct")

  dat <- dat_gr
  dat$grossrange_flag_temperature_degree_c <- 1
  expect_error(qc_core_prepare(dat), "qc_core_set_aside_flags")
})

test_that("qc_core_set_aside_flags() moves flag columns out by row_id", {
  dat <- dat_gr
  dat$grossrange_flag_temperature_degree_c <- 1
  dat$depth_crosscheck_flag <- 2

  parts <- qc_core_set_aside_flags(dat, qc_tests = "spike")

  expect_false(any(grepl("_flag", colnames(parts$dat))))
  expect_equal(parts$dat$row_id, seq_len(nrow(dat)))
  expect_equal(
    colnames(parts$flags),
    c("row_id", "grossrange_flag_temperature_degree_c", "depth_crosscheck_flag")
  )

  # no flags: nothing set aside
  expect_null(qc_core_set_aside_flags(dat_gr)$flags)
})

test_that("qc_core_set_aside_flags() stops if the test already ran", {
  dat <- dat_gr
  dat$grossrange_flag_temperature_degree_c <- 1

  expect_error(
    qc_core_set_aside_flags(dat, qc_tests = c("spike", "grossrange")),
    "already has flags for: grossrange"
  )
  expect_error(
    qc_core_set_aside_flags(dplyr::mutate(dat_gr, row_id = 1)),
    "already has a << row_id >>"
  )
})
