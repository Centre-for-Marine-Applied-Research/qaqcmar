# Some tests generated with Claude Opus 5.5

path <- system.file("testdata", package = "qaqcmar")
dat <- readRDS(paste0(path, "/test_data_grossrange.RDS"))
dat_no_county <- dplyr::select(dat, -county)

test_that("assert_county() returns errors if more than one county is detected", {
  # different county in dat compared to function argument
  expect_error(
    assert_county(
      data.frame(county = rep("x", times = 10)),
      county = "y",
      "qc_test_grossrange()"
    )
  )

  # more than one county in dat
  expect_error(
    assert_county(
      data.frame(county = c(rep("x", 5), rep("y", 5))),
      county_arg = NULL,
      foo = "qc_test_grossrange()"
    )
  )

  # no county specified
  expect_error(
    assert_county(
      data.frame(col1 = seq(1:10)),
      county_arg = NULL,
      foo = "qc_test_grossrange()"
    ),
    "Must specify << county >>"
  )

  # county specified in dat
  expect_equal(
    assert_county(
      data.frame(county = rep("x", 10)),
      county_arg = NULL,
      foo = "qc_test_grossrange()"
    ),
    "x"
  )

  # county specified in arg
  expect_equal(
    assert_county(
      data.frame(col1 = seq(1:10)),
      county_arg = "y",
      foo = "qc_test_grossrange()"
    ),
    "y"
  )
})

test_that("a lower-case county stops and suggests the right spelling", {
  dat_lower <- dplyr::mutate(dat, county = "halifax")

  for (f in list(qc_test_grossrange, qc_test_spike, qc_test_rolling_sd)) {
    expect_error(
      suppressWarnings(f(dat_lower)),
      'Did you mean "Halifax"'
    )
  }
})

test_that("a county with no thresholds stops and lists the valid ones", {
  expect_error(
    qc_test_grossrange(dplyr::mutate(dat, county = "Kings")),
    'No default grossrange thresholds for county "Kings".*Counties with grossrange thresholds: Annapolis'
  )
  expect_error(
    qc_test_climatology(dplyr::mutate(dat, county = "Kings")),
    'No default climatology thresholds for county "Kings"'
  )
})

test_that("a county without default thresholds is fine with a user table", {
  tab <- data.frame(
    sensor_type = "aquameasure",
    variable = "temperature_degree_c",
    sensor_min = -5,
    sensor_max = 35,
    user_min = -1,
    user_max = 25
  )
  expect_no_error(
    qc_test_grossrange(
      dplyr::filter(
        dplyr::mutate(dat, county = "Kings"),
        sensor_type == "aquameasure"
      ),
      grossrange_table = tab
    )
  )
})
