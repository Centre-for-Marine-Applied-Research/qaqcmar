test_that("qc_test_human_in_loop() assigns correct flags", {
  expect_equal(
    as.numeric(
      unique(qc_hil_1$human_in_loop_flag_dissolved_oxygen_percent_saturation)),
    1)

  expect_equal(
    as.numeric(
      unique(qc_hil_1$human_in_loop_flag_sensor_depth_measured_m)),
    1)

  expect_equal(
    as.numeric(
      unique(qc_hil_4$human_in_loop_flag_dissolved_oxygen_percent_saturation)),
    4)

  # sensor depth should still be flagged 1
  expect_equal(
    as.numeric(
      unique(qc_hil_4$human_in_loop_flag_sensor_depth_measured_m)),
    1)

})

test_that("qc_test_human_in_loop() assigns comments", {
  expect_true(
    all(unique(qc_hil_4$hil_comment) %in%
          unique(hil_table$human_in_loop_comment))
  )

  expect_equal(unique(qc_hil_1$hil_comment), NA_character_)
})


test_that("qc_test_human_in_loop() does not add rows", {
  expect_equal(nrow(dat_hil), nrow(qc_hil))
})
