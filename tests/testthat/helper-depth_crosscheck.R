# Ocotber 4, 2023

# testdata was generated such that we KNOW what flag should be assigned to each
# observation (data-raw/test_data.R)

# dummy_thresholds --------------------------------------------------------
# library(ggplot2)
# library(plotly)
# library(dplyr)
# library(sensorstrings)
# library(zoo)

path <- system.file("testdata", package = "qaqcmar")

dat <- readRDS(paste0(path, "/test_data_estimated_depth.RDS")) %>%
  ss_pivot_wider()

# general test
qc_depth <- dat %>%
  qc_test_depth_crosscheck()

p <- qc_plot_flags(
  qc_depth,
  qc_tests = "depth_crosscheck",
  ncol = 1
)

# use this test data to make sure function works with other variables present
dat2 <- readRDS(paste0(path, "/test_data_rolling_sd.RDS"))

# general test
qc_depth2 <- dat2 %>%
  qc_test_depth_crosscheck()

p <- qc_plot_flags(
  qc_depth2,
  qc_tests = "depth_crosscheck",
  ncol = 1
)

# even though sensor depth was not measured at 10 m, the WHOLE deployment
# gets assigned depth_crosscheck_flag = 1 because both of the sensors that
# measured depth have a flag of 1
qc_depth_1 <- qc_depth %>%
  filter(sensor_depth_at_low_tide_m %in% c(2, 5, 10))

qc_depth_2 <- qc_depth %>%
  filter(sensor_depth_at_low_tide_m == 20)

qc_depth_3 <- qc_depth %>%
  filter(sensor_depth_at_low_tide_m == 15)
