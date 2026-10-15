# October 4, 2023

#' @importfrom dplyr %>% filter mutate relocate select
#' @importFrom here here
#' @importFrom lubridate as_datetime days
#' @importFrom sensorstrings  ss_convert_depth_to_ordered_factor ss_ggplot_variables

# Raw data ----------------------------------------------------------------

int <- 30 # generate simulated data every 30 minutes
n_int <- (60 / int) * 24 # number of intervals in 24 hours
amp <- 1 # tidal amplitude

ts <- data.frame(
  timestamp_utc = seq(
    as_datetime("2023-01-01 12:00:00"),
    as_datetime("2023-01-05 12:00:00"),
    by = paste0(int, " mins")
  )
)

dat2 <- ts %>%
  mutate(
    index = 0:(n() - 1),
    station = "Emonds_Field",
    sensor_depth_at_low_tide_m = rep(2, n()),
    sensor_type = rep("hobo", n()),
    sensor_serial_number = rep(123, n()),
    sensor_depth_measured_m = amp * sin(4 * pi / n_int * index) + 3
  )

dat5 <- ts %>%
  mutate(
    index = 0:(n() - 1),
    station = "Emonds_Field",
    sensor_depth_at_low_tide_m = rep(5, n()),
    sensor_type = rep("aquameasure", n()),
    sensor_serial_number = rep(456, n()),
    sensor_depth_measured_m = amp * sin(4 * pi / n_int * index) + 6
  )

dat10 <- ts %>%
  mutate(
    index = 0:(n() - 1),
    station = "Emonds_Field",
    sensor_depth_at_low_tide_m = rep(10, n()),
    sensor_type = rep("aquameasure", n()),
    sensor_serial_number = rep(789, n()),
    sensor_depth_measured_m = NA
  )


dat15 <- ts %>%
  mutate(
    index = 0:(n() - 1),
    station = "Taren_Ferry",
    sensor_depth_at_low_tide_m = rep(15, n()),
    sensor_type = rep("vemco", n()),
    sensor_serial_number = rep(012, n()),
    sensor_depth_measured_m = amp * sin(4 * pi / n_int * index) + 22
  )


dat20 <- ts %>%
  mutate(
    index = 0:(n() - 1),
    station = "White_Tower",
    sensor_depth_at_low_tide_m = rep(20, n()),
    sensor_type = rep("vemco", n()),
    sensor_serial_number = rep(345, n()),
    sensor_depth_measured_m = NA
  )

dat <- bind_rows(dat2, dat5, dat10, dat15, dat20) %>%
  mutate(
    county = "Halifax",
    deployment_range = "Third_Age"
  ) %>%
  pivot_longer(
    cols = "sensor_depth_measured_m",
    names_to = "variable",
    values_to = "value"
  ) %>%
  select(
    county,
    station,
    deployment_range,
    timestamp_utc,
    sensor_depth_at_low_tide_m,
    sensor_type,
    sensor_serial_number,
    variable,
    value
  ) %>%
  mutate(value = round(value, digits = 2))

# dat %>%
#   ss_pivot_wider() %>%
#   ss_ggplot_variables()

# Export rds file
saveRDS(dat, file = here("inst/testdata/test_data_estimated_depth.RDS"))
