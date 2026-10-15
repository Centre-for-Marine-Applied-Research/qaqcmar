# qc_calculate_user_thresholds() ------------------------------------------
test_that("qc_calculate_user_thresholds() calculates correct thresholds", {
  expect_equal(round(user_temp$mean_var), temp_mean)
  expect_equal(round(user_temp$sd_var), temp_sd)

  expect_equal(user_temp$user_min, user_temp$check_user_min, tolerance = 0.1)
  expect_equal(user_temp$user_max, user_temp$check_user_max, tolerance = 0.1)
})

# qc_calculate_climatology_thresholds() -----------------------------------
test_that("qc_calculate_climatology_thresholds() calculates correct thresholds", {
  expect_equal(
    clim_temp$season_min,
    clim_temp$check_season_min,
    tolerance = 0.1
  )
  expect_equal(
    clim_temp$season_max,
    clim_temp$check_season_max,
    tolerance = 0.1
  )
})

# qc_calculate_rolling_sd_thresholds() ------------------------------------
test_that("qc_calculate_rolling_sd_thresholds() calculates correct thresholds", {
  expect_equal(rolling_sd_quantile$threshold_value, q_95, tolerance = 0.1)
  expect_equal(rolling_sd_mean_sd$threshold_value, mean_sd, tolerance = 0.1)
})

test_that("qc_calculate_rolling_sd_thresholds() returns correct errors", {
  expect_error(
    qc_calculate_rolling_sd_thresholds(rolling_sd_dat, var = "temperature")
  )
})


# qc_calculate_depth_crosscheck_thresholds --------------------------------
# the "expected" value is hard coded. Update if test_data_estimated_depth.RDS is updated
test_that("qc_calculate_depth_crosscheck_thresholds() calculates correct thresholds", {
  expect_equal(depth_crosscheck_quantile$threshold_value, 5.4, tolerance = 0.1)
})
