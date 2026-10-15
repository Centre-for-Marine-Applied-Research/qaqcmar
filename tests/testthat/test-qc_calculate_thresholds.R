test_that("qc_calculate_user_thresholds() drops NAs values before calculating mean and standard deviation", {
  dat <- data.frame(test_data = c(1, 2, 3, NA, 5))
  expect_equal(
    qc_calculate_user_thresholds(dat, var = "test_data")$threshold_value,
    c(-2.373, 7.873)
  )
})
