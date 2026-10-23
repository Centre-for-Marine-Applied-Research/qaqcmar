test_that("qc_calculate_user_thresholds() drops NAs values before calculating mean and standard deviation", {
  dat <- data.frame(test_data = c(1, 2, 3, NA, 5))
  expect_equal(
    qc_calculate_user_thresholds(dat, var = "test_data")$threshold_value,
    c(-2.373, 7.873)
  )
})

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

test_that("qc_calculate_rolling_sd_thresholds() uses the sd_roll_<var> column from qc_test_rolling_sd()", {
  dat_sd <- readRDS(paste0(path, "/test_data_rolling_sd.RDS")) |>
    qc_test_rolling_sd(keep_sd_cols = TRUE)

  expected <- dat_sd |>
    rename(sd_roll = sd_roll_dissolved_oxygen_percent_saturation) |>
    qc_calculate_rolling_sd_thresholds(
      var = dissolved_oxygen_percent_saturation,
      stat = "quartile"
    )

  dat_sd |>
    qc_calculate_rolling_sd_thresholds(
      var = dissolved_oxygen_percent_saturation,
      stat = "quartile"
    ) |>
    expect_equal(expected)
})

test_that("qc_calculate_rolling_sd_thresholds() stops when dat has no sd_roll column", {
  data.frame(sd_roll_salinity_psu = 1:5) |>
    qc_calculate_rolling_sd_thresholds(var = temperature_degree_c, stat = "quartile") |>
    expect_error("sd_roll_temperature_degree_c")
})

# qc_calculate_depth_crosscheck_thresholds --------------------------------
# the "expected" value is hard coded. Update if test_data_estimated_depth.RDS is updated
test_that("qc_calculate_depth_crosscheck_thresholds() calculates correct thresholds", {
  expect_equal(depth_crosscheck_quantile$threshold_value, 5.4, tolerance = 0.1)
})
