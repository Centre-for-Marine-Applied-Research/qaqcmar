# Generated with Claude Opus 5.5

dat_wide <- readRDS(
  paste0(
    system.file("testdata", package = "qaqcmar"),
    "/test_data_grossrange.RDS"
  )
)

test_that("qc_core_to_wide() undoes qc_core_prepare()", {
  out <- qc_core_to_wide(qc_core_prepare(dat_wide))

  has_values <- rowSums(
    !is.na(dat_wide[c(
      "dissolved_oxygen_percent_saturation",
      "temperature_degree_c"
    )])
  ) >
    0

  # same columns (in the standard order), same values, same row order
  expect_setequal(colnames(out), colnames(dat_wide))
  expect_equal(
    as.data.frame(out[colnames(dat_wide)]),
    as.data.frame(dat_wide[has_values, ]),
    ignore_attr = TRUE
  )
})

test_that("qc_core_to_wide() names flags by test and variable, as ordered factors", {
  out <- qc_core_run(dat_wide, "grossrange", county = "Lunenburg") |>
    qc_core_to_wide()

  expect_true(all(
    c(
      "grossrange_flag_dissolved_oxygen_percent_saturation",
      "grossrange_flag_temperature_degree_c"
    ) %in%
      colnames(out)
  ))
  expect_equal(
    levels(out$grossrange_flag_temperature_degree_c),
    as.character(1:4)
  )
  expect_true(is.ordered(out$grossrange_flag_temperature_degree_c))
})

test_that("qc_core_to_wide() joins set-aside flags back on row_id", {
  dat <- qc_test_grossrange(dat_wide, county = "Lunenburg")
  parts <- qc_core_set_aside_flags(dat, "spike")

  out <- qc_core_run(parts$dat, "spike", county = "Lunenburg") |>
    qc_core_to_wide(flags = parts$flags)

  expect_equal(
    out$grossrange_flag_temperature_degree_c,
    dat$grossrange_flag_temperature_degree_c
  )
  expect_false("row_id" %in% colnames(out))
})

test_that("qc_core_to_wide() gives helper columns one column per variable", {
  out <- qc_core_run(
    dat_wide,
    "spike",
    county = "Lunenburg",
    keep_spike_cols = TRUE
  ) |>
    qc_core_to_wide()

  # one row per timestamp and sensor, not one per variable (QC-5)
  expect_equal(nrow(out), nrow(qc_core_to_wide(qc_core_prepare(dat_wide))))
  expect_true(all(
    c(
      "spike_value_temperature_degree_c",
      "spike_value_dissolved_oxygen_percent_saturation"
    ) %in%
      colnames(out)
  ))
})

test_that("qc_core_to_wide() puts each flag in the right row and variable column", {
  # row 1 has temperature and salinity; row 2 has temperature only
  long <- data.frame(
    row_id = c(1L, 1L, 2L),
    station = "A",
    timestamp_utc = as.POSIXct("2023-06-01", tz = "UTC") + c(0, 0, 600),
    variable = c(
      "temperature_degree_c",
      "salinity_psu",
      "temperature_degree_c"
    ),
    value = c(10, 30, 11),
    grossrange_flag = c(1L, 3L, 4L),
    spike_flag = c(2L, 1L, 3L),
    qc_flag = c(2L, 3L, 4L)
  )
  out <- qc_core_to_wide(long)

  expect_equal(out$temperature_degree_c, c(10, 11))
  expect_equal(out$salinity_psu, c(30, NA))
  expect_equal(
    out$grossrange_flag_temperature_degree_c,
    ordered(c(1, 4), levels = 1:4)
  )
  expect_equal(
    out$grossrange_flag_salinity_psu,
    ordered(c(3, NA), levels = 1:4)
  )
  expect_equal(
    out$spike_flag_temperature_degree_c,
    ordered(c(2, 3), levels = 1:4)
  )
  expect_equal(out$qc_flag_salinity_psu, ordered(c(3, NA), levels = 1:4))
  expect_equal(
    colnames(out),
    c(
      "station",
      "timestamp_utc",
      "salinity_psu",
      "temperature_degree_c",
      "grossrange_flag_salinity_psu",
      "grossrange_flag_temperature_degree_c",
      "spike_flag_salinity_psu",
      "spike_flag_temperature_degree_c",
      "qc_flag_salinity_psu",
      "qc_flag_temperature_degree_c"
    )
  )
})
