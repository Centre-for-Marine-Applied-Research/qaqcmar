library(readr)
library(dplyr)
library(lubridate)
library(here)
library(sensorstrings)
library(qaqcmar)
library(tidyr)

# Raw data ----------------------------------------------------------------

timestamp_utc <- seq(
  as_datetime("2023-04-01 12:00:00"),
  as_datetime("2023-04-15 12:00:00"),
  by = "6 hour"
)
variables <- c(
  "temperature_degree_c",
  "dissolved_oxygen_percent_saturation",
  "dissolved_oxygen_uncorrected_mg_per_l",
  "salinity_psu",
  "sensor_depth_measured_m"
)
sensors <- c("aquameasure", "hobo", "tidbit", "vr2ar")

# Suspect: 5 observations in a row
ts_3 <- c(timestamp_utc[7:11], timestamp_utc[35:39])

# Fail: 5 or more observations in a row
ts_4 <- c(timestamp_utc[15:21], timestamp_utc[43:55])


dat <- expand.grid(
  timestamp_utc = timestamp_utc,
  variable = variables,
  sensor_type = sensors
) %>%
  # only keep possible sensor-variable combinations
  filter(
    (sensor_type == "aquameasure" &
      variable %in%
        c(
          "temperature_degree_c",
          "dissolved_oxygen_percent_saturation",
          "salinity_psu",
          "sensor_depth_measured_m"
        )) |
      (sensor_type == "hobo" &
        variable %in%
          c(
            "temperature_degree_c",
            "dissolved_oxygen_uncorrected_mg_per_l"
          )) |
      (sensor_type == "tidbit" & variable == "temperature_degree_c") |
      (sensor_type == "vr2ar" &
        variable %in% c("temperature_degree_c", "sensor_depth_measured_m"))
  ) %>%
  mutate(
    variable = as.character(variable),
    sensor_type = as.character(sensor_type)
  )

# aquameasure data
set.seed(2154)
am <- dat %>%
  filter(sensor_type == "aquameasure") %>%
  mutate(
    sensor_depth_at_low_tide_m = 5,
    sensor_serial_number = 123,
    value = case_when(
      variable == "dissolved_oxygen_percent_saturation" ~
        round(rnorm(n(), 100, 5), digits = 1),
      variable == "salinity_psu" ~ round(rnorm(n(), 30, 2), digits = 1),
      variable == "sensor_depth_measured_m" ~ round(
        rnorm(n(), 7, 1),
        digits = 1
      ),
      variable == "temperature_degree_c" ~ round(rnorm(n(), 15, 2), digits = 2),
    )
  ) %>%
  ss_pivot_wider() %>%
  # stuck for 3 values in a row
  mutate(
    dissolved_oxygen_percent_saturation = if_else(
      timestamp_utc %in% ts_3,
      80.0,
      dissolved_oxygen_percent_saturation
    ),
    salinity_psu = if_else(timestamp_utc %in% ts_3, 20.0, salinity_psu),
    sensor_depth_measured_m = if_else(
      timestamp_utc %in% ts_3,
      5,
      sensor_depth_measured_m
    ),
    temperature_degree_c = if_else(
      timestamp_utc %in% ts_3,
      13,
      temperature_degree_c
    )
  ) %>%
  # stuck for 5 values in a row
  mutate(
    dissolved_oxygen_percent_saturation = if_else(
      timestamp_utc %in% ts_4,
      120.0,
      dissolved_oxygen_percent_saturation
    ),
    salinity_psu = if_else(timestamp_utc %in% ts_4, 40.0, salinity_psu),
    sensor_depth_measured_m = if_else(
      timestamp_utc %in% ts_4,
      9,
      sensor_depth_measured_m
    ),
    temperature_degree_c = if_else(
      timestamp_utc %in% ts_4,
      17,
      temperature_degree_c
    )
  )

# am %>%
#   ss_ggplot_variables() +
#   geom_point(size = 3)

# hobo data
set.seed(5464)
hobo <- dat %>%
  filter(sensor_type == "hobo") %>%
  mutate(
    sensor_depth_at_low_tide_m = 2,
    sensor_serial_number = 456,
    value = case_when(
      variable == "dissolved_oxygen_uncorrected_mg_per_l" ~
        round(rnorm(n(), 9, 0.5), digits = 1),
      variable == "temperature_degree_c" ~ round(rnorm(n(), 20, 2), digits = 3),
    )
  ) %>%
  ss_pivot_wider() %>%
  # stuck for 3 values in a row
  mutate(
    dissolved_oxygen_uncorrected_mg_per_l = if_else(
      timestamp_utc %in% ts_3,
      7,
      dissolved_oxygen_uncorrected_mg_per_l
    ),
    temperature_degree_c = if_else(
      timestamp_utc %in% ts_3,
      18,
      temperature_degree_c
    )
  ) %>%
  # stuck for 5 values in a row
  mutate(
    dissolved_oxygen_uncorrected_mg_per_l = if_else(
      timestamp_utc %in% ts_4,
      11,
      dissolved_oxygen_uncorrected_mg_per_l
    ),
    temperature_degree_c = if_else(
      timestamp_utc %in% ts_4,
      22,
      temperature_degree_c
    )
  )

# hobo[which(timestamp_utc %in% ts_3), "temperature_degree_c"] <-
#   seq(9.000, 10.999, length.out = length(ts_3))
#
# hobo[which(timestamp_utc %in% ts_4), "temperature_degree_c"] <-
#   rep(c(25.999, 25.000, 25.001, 25.002, 24.998), 2)
#
# hobo[which(timestamp_utc %in% ts_3), "dissolved_oxygen_uncorrected_mg_per_l"] <-
#   rep(c(8, 8.002, 7.998), 2)
#
# hobo[which(timestamp_utc %in% ts_4), "dissolved_oxygen_uncorrected_mg_per_l"] <-
#   rep(c(10, 10.002, 9.998, 10.001, 10.004), 2)

# hobo %>%
#   ss_ggplot_variables() +
#   geom_point(size = 3)

# tidbit data
set.seed(559)
tidbit <- dat %>%
  filter(sensor_type == "tidbit") %>%
  mutate(
    sensor_depth_at_low_tide_m = 10,
    sensor_serial_number = 789,
    value = case_when(
      variable == "temperature_degree_c" ~ round(rnorm(n(), 10, 2), digits = 3),
    )
  ) %>%
  ss_pivot_wider() %>%
  # stuck for 3 values in a row
  mutate(
    temperature_degree_c = if_else(
      timestamp_utc %in% ts_3,
      8,
      temperature_degree_c
    )
  ) %>%
  # stuck for 5 values in a row
  mutate(
    temperature_degree_c = if_else(
      timestamp_utc %in% ts_4,
      12,
      temperature_degree_c
    )
  )

# tidbit %>%
#   ss_ggplot_variables() +
#   geom_point(size = 3)

# vr2 data
set.seed(2154)
vem <- dat %>%
  filter(sensor_type == "vr2ar") %>%
  mutate(
    sensor_depth_at_low_tide_m = 25,
    sensor_serial_number = 1011,
    value = case_when(
      variable == "sensor_depth_measured_m" ~ round(
        rnorm(n(), 15, 3),
        digits = 1
      ),
      variable == "temperature_degree_c" ~ round(rnorm(n(), 5, 2), digits = 2),
    )
  ) %>%
  ss_pivot_wider() %>%
  # stuck for 3 values in a row
  mutate(
    sensor_depth_measured_m = if_else(
      timestamp_utc %in% ts_3,
      10,
      sensor_depth_measured_m
    ),
    temperature_degree_c = if_else(
      timestamp_utc %in% ts_3,
      3,
      temperature_degree_c
    )
  ) %>%
  # stuck for 5 values in a row
  mutate(
    sensor_depth_measured_m = if_else(
      timestamp_utc %in% ts_4,
      20.0,
      sensor_depth_measured_m
    ),
    temperature_degree_c = if_else(
      timestamp_utc %in% ts_4,
      8,
      temperature_degree_c
    )
  )


# vem %>%
#  ss_ggplot_variables() +
#  geom_point(size = 3)

dat_flat <- bind_rows(am, hobo, tidbit, vem)

dat_flat %>%
  ss_ggplot_variables() +
  geom_point(size = 3)


# Export rds file
saveRDS(dat_flat, file = here("inst/testdata/test_data_flat_line.RDS"))
