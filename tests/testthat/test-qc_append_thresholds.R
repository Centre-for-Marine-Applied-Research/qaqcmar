# Generated with Claude Opus 5.5

dat <- data.frame(
  row_id = 1:4,
  variable = c(
    "temperature_degree_c",
    "temperature_degree_c",
    "salinity_psu",
    "chlorophyll_blue_ug_per_l"
  ),
  value = c(10, 11, 30, 2),
  sensor_key = c("hobo", "hobo do", "aquameasure", "aquameasure"),
  month = 7L
)

out <- qc_append_thresholds(dat, "grossrange", "Halifax")

test_that("qc_append_thresholds() keeps the rows and adds the threshold columns", {
  expect_equal(out[colnames(dat)], dat)
  expect_setequal(
    colnames(out),
    c(colnames(dat), "sensor_min", "sensor_max", "user_min", "user_max")
  )
})

test_that("each row gets the thresholds for its own key", {
  #out <- qc_append_thresholds(dat, "grossrange", "Halifax")

  expect_equal(out$sensor_min, c(-40, -5, 0, NA))
  expect_equal(out$user_min, c(-8.892, -8.892, 26.317, NA))
})

test_that("rows with no thresholds are kept with NA", {
  expect_true(all(is.na(c(
    out$sensor_min[4],
    out$sensor_maxp[4],
    out$user_min[4],
    out$user_max[4]
  ))))
})
