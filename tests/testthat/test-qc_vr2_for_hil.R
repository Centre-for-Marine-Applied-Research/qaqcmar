# Generated with Claude Opus 5.5

vr2_dat <- data.frame(
  county = "Halifax",
  station = "A",
  deployment_range = "2025-Jun-01 to 2025-Nov-30",
  sensor_type = c("aquameasure", "hobo", "vr2ar"),
  sensor_serial_number = c(1, 2, 3),
  sensor_depth_at_low_tide_m = c(2, 20, 20)
)

test_that("qc_vr2_for_hil() gives a row for a vr2ar paired with a hobo", {
  out <- qc_vr2_for_hil(vr2_dat)

  expect_equal(nrow(out), 1)
  expect_equal(out$sensor_serial_number, 3)
  expect_equal(out$variable, "temperature_degree_c")
  expect_equal(out$human_in_loop_flag_value, 4)
  # the columns qc_test_human_in_loop() needs
  expect_true(all(
    c(
      "station",
      "depl_range",
      "timestamp_prompt",
      "qc_test_column",
      "human_in_loop_comment"
    ) %in%
      colnames(out)
  ))
})

test_that("qc_vr2_for_hil() gives no rows for a vr2ar without a hobo", {
  out <- qc_vr2_for_hil(filter(vr2_dat, sensor_type != "hobo"))

  expect_equal(nrow(out), 0)
})
