# February 10, 2023

# Simulate data for the rolling standard deviation test

# Day 2: Spike Flag 4 (low)
# Day 5: Spike Flag 3 (low)
# Day 15: Spike Flag 4 (high)
# Day 27: Spike Flag 3 (high)

# library(dplyr)
# library(lubridate)
# library(here)
# library(sensorstrings)
# library(qaqcmar)

#' @importfrom dplyr %>% filter mutate relocate select
#' @importFrom here here
#' @importFrom lubridate as_datetime day
#' @importFrom sensorstrings  ss_convert_depth_to_ordered_factor ss_ggplot_variables

# Raw data ----------------------------------------------------------------

int <- 30 # generate simulated data every 30 minutes
n_int <- (60 / int) * 24 # number of intervals in 24 hours

amp1 <- 2 # no biofouling
amp2 <- 6 # moderate biofouling
amp3 <- 10 # substantial biofouling
trans <- 100 # oscillate around 100 %

# simulate data
dat <- data.frame(
  timestamp_utc = seq(
    as_datetime("2023-01-01 12:00:00"),
    as_datetime("2023-01-22 12:00:00"),
    by = paste0(int, " mins")
  )
) %>%
  mutate(
    index = 0:(n() - 1),
    # amplitude for different sections of the timeseries
    amp = case_when(
      timestamp_utc <= as_datetime("2023-01-07 12:00:00") ~ amp1,
      timestamp_utc >= as_datetime("2023-01-15 12:01:00") ~ amp3,
      TRUE ~ amp2
    ),
    # simulated data
    dissolved_oxygen_percent_saturation = amp *
      sin(2 * pi / n_int * index) +
      trans,
    # metadata (required for sensorstrings and qaqcmar plot functions)
    county = "Halifax",
    waterbody = "mcnabs",
    station = "lighthouse",
    deployment_range = "2023-Jan-01 to 2023-Jan-15",
    sensor_depth_at_low_tide_m = 5,
    sensor_depth_measured_m = 2 * sin(4 * pi / n_int * index) + 5,
    sensor_type = "aquameasure",
    sensor_serial_number = 123,
  ) %>%
  select(
    county,
    waterbody,
    station,
    deployment_range,
    sensor_type,
    sensor_serial_number,
    timestamp_utc,
    sensor_depth_at_low_tide_m,
    sensor_depth_measured_m,
    dissolved_oxygen_percent_saturation
  )

# Export rds file
saveRDS(dat, file = here("inst/testdata/test_data_rolling_sd.RDS"))
