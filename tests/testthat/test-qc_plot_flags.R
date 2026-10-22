# Generated with Claude Opus 5.5

plot_dat <- readRDS(
  paste0(
    system.file("testdata", package = "qaqcmar"),
    "/test_data_rolling_sd.RDS"
  )
) |>
  qc_test_all(qc_tests = c("grossrange", "spike")) |>
  suppressMessages()

test_that("qc_plot_flags() gives one ggplot per variable and test", {
  p <- qc_plot_flags(plot_dat, qc_tests = c("grossrange", "spike"))

  expect_setequal(
    names(p),
    c("dissolved_oxygen_percent_saturation", "sensor_depth_measured_m")
  )
  expect_equal(names(p$sensor_depth_measured_m), c("grossrange", "spike"))
  expect_s3_class(p$sensor_depth_measured_m$spike, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p$sensor_depth_measured_m$spike))
})

test_that("qc_plot_flags() draws the depth crosscheck figure", {
  dat <- suppressMessages(qc_test_depth_crosscheck(plot_dat))
  p <- qc_plot_flags(dat, qc_tests = "depth_crosscheck")

  expect_s3_class(p$depth_crosscheck, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p$depth_crosscheck))
  expect_equal(p$depth_crosscheck$labels$title, "depth_crosscheck test")
})
