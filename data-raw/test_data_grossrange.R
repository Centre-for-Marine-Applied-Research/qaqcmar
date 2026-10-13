# February 10, 2023

# Simulate data for the gross range test
# Data constructed so there are 4 observations per month
# Day 1: Gross Range Flag 4 (low)
# Day 5: Gross Range Flag 3 (low)*
# Day 10: Gross Range Flag 1
# Day 15: Gross Range Flag 4 (high)
# Day 28: Gross Range Flag 3 (high)

# * for the aquameasure and vr2ar sensors, the user_min is less than the sensor_min,
# which means a flag of 3 (low) will not be assigned

# library(dplyr)
# library(lubridate)
# library(here)
# library(sensorstrings)
# library(qaqcmar)
# library(tidyr)

#' @importfrom dplyr %>% filter mutate relocate select
#' @importFrom here here
#' @importFrom lubridate as_datetime days
#' @importFrom sensorstrings  ss_convert_depth_to_ordered_factor ss_ggplot_variables

# Raw data ----------------------------------------------------------------

# import the gross range table
grossrange_table <- thresholds %>%
  filter(qc_test == "grossrange", county == "Lunenburg" | is.na(county)) %>%
  select(-c(qc_test, county, month)) %>%
  pivot_wider(names_from = "threshold", values_from = "threshold_value") %>%
  filter(
    variable == "temperature_degree_c" |
      variable == "dissolved_oxygen_percent_saturation"
  )

# vectors used to create data frame
months <- seq(1, 12)
days <- rep(c(1, 5, 10, 15, 28), 12)
variables <- c("temperature_degree_c", "dissolved_oxygen_percent_saturation")
# sensors = na.omit(unique(grossrange_table$sensor_type))
sensors <- c("aquameasure", "hobo", "tidbit", "vr2ar")

dat <- expand.grid(
  month = months, day = days, variable = variables, sensor_type = sensors
) %>%
  distinct(month, day, variable, sensor_type) %>%
  # filter out impossible sensor-variable combinations
  filter(
    !((variable == "dissolved_oxygen_percent_saturation" |
      variable == "dissolved_oxygen_mg_per_l") &
      (sensor_type == "tidbit" | sensor_type == "hobo" | sensor_type == "vr2ar")),
    !(variable == "dissolved_oxygen_percent_saturation" & sensor_type == "hobo do")
  ) %>%
  # join with the sensor and user thresholds
  left_join(
    grossrange_table %>%
      select(sensor_type, variable, contains("sensor")) %>%
      filter(!is.na(sensor_type)),
    by = c("sensor_type", "variable")
  ) %>%
  left_join(
    grossrange_table %>%
      select(variable, contains("user")) %>%
      filter(!is.na(user_min) & !is.na(user_max)),
    by = "variable"
  ) %>%
  mutate(
    # add depths (so easier to view simulated data when plotted)
    sensor_depth_at_low_tide_m = case_when(
      sensor_type == "hobo" ~ 2,
      sensor_type == "aquameasure" ~ 5,
      sensor_type == "hobo do" ~ 10,
      sensor_type == "tidbit" ~ 15,
      sensor_type == "vr2ar" ~ 25,
      TRUE ~ NA_real_
    ),
    # sensor serial numbers
    sensor_serial_number = case_when(
      sensor_type == "hobo" ~ 123,
      sensor_type == "aquameasure" ~ 456,
      sensor_type == "hobo do" ~ 789,
      sensor_type == "tidbit" ~ 101,
      sensor_type == "vr2ar" ~ 112,
      TRUE ~ NA_real_
    ),

    timestamp_utc = as_datetime(paste("2023", month, day, sep = "-")),
    # add values with known flags
    value = case_when(
      day == 1 ~ sensor_min - 5, # flag 4
      day == 5 ~ sensor_min + 1, # flag 3
      day == 10 ~ case_when( # flag 1
        variable == "temperature_degree_c" & sensor_type == "hobo" ~ 2,
        variable == "temperature_degree_c" & sensor_type == "aquameasure" ~ 5,
        variable == "temperature_degree_c" & sensor_type == "hobo do" ~ 10,
        variable == "temperature_degree_c" & sensor_type == "tidbit" ~ 15,
        variable == "temperature_degree_c" & sensor_type == "vr2ar" ~ 19,
        variable == "dissolved_oxygen_percent_saturation" ~ 100,
        TRUE ~ NA_real_
      ),
      day == 15 ~ sensor_max + 5, # flag 4
      day == 28 ~ sensor_max - 1, # flag 3
      TRUE ~ NA_real_
    )
  ) %>%
  select(-c(month, sensor_min, sensor_max, user_min, user_max)) %>%
  ss_pivot_wider() %>%
  mutate(
    county = "Lunenburg",
    station = "White Tower",
    deployment_range = "2023-Jan-01 to 2023-Dec-28"
  )


dat[
  which((dat$sensor_type == "aquameasure" |
    dat$sensor_type == "vr2ar") & dat$day == 5),
  "temperature_degree_c"
] <- NA

dat[
  which(dat$sensor_type == "v2rar" & dat$day == 5),
  "temperature_degree_c"
] <- NA

attributes(dat$temperature_degree_c) <- NULL
attributes(dat$dissolved_oxygen_percent_saturation) <- NULL


dat <- dat %>%
  select(county, station, deployment_range,
         sensor_type, sensor_serial_number, sensor_depth_at_low_tide_m,
         timestamp_utc,
         dissolved_oxygen_percent_saturation,
         temperature_degree_c
  )

# Export rds file
saveRDS(dat, file = system.file("testdata/test_data_grossrange.RDS", package = "qaqcmar"))



