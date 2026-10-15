test_that("qc_test_depth_crosscheck() assigns correct flags", {
  expect_equal(as.numeric(unique(qc_depth_1$depth_crosscheck_flag)), 1)

  expect_equal(as.numeric(unique(qc_depth_2$depth_crosscheck_flag)), 2)

  expect_equal(as.numeric(unique(qc_depth_3$depth_crosscheck_flag)), 3)
})
