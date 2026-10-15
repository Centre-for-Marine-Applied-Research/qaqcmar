# library(readr)
# library(dplyr)
# library(lubridate)
# library(here)
# library(sensorstrings) # V. 0.1.0
# library(qaqcmar) # V. 0.0.1
# library(tidyr)

# Raw data ----------------------------------------------------------------

climatology_table <- thresholds %>%
  filter(qc_test == "climatology", county == "Lunenburg" | is.na(county)) %>%
  select(-c(qc_test, county, sensor_type)) %>%
  pivot_wider(names_from = "threshold", values_from = "threshold_value") #%>%
# bind_rows(
#   data.frame(
#     variable = rep("dissolved_oxygen_percent_saturation", 12),
#     month = seq(1, 12),
#     season_min = rep(95, 12),
#     season_max = rep(105, 12)
#   )
# )

months <- seq(1, 12)
days <- rep(c(1, 10, 28), 12)
variables <- c("temperature_degree_c", "dissolved_oxygen_percent_saturation")

dat <- expand.grid(month = months, day = days, variable = variables) %>%
  distinct(month, day, variable) %>%
  arrange(month) %>%
  left_join(climatology_table, by = c("variable", "month")) %>%
  mutate(
    sensor_depth_at_low_tide_m = 5,
    timestamp_utc = as_datetime(paste("2023", month, day, sep = "-")),
    value = case_when(
      day == 1 ~ season_min - 5,
      day == 10 ~ (season_min + season_max) / 2,
      day == 28 ~ season_max + 5,
      TRUE ~ NA_real_
    )
  ) %>%
  select(-c(month, day, season_min, season_max)) %>%
  ss_pivot_wider() %>%
  arrange(timestamp_utc) %>%
  mutate(
    county = "Lunenburg",
    station = "White Tower",
    deployment_range = "2023-Jan-01 to 2023-Dec-28",
    sensor_serial_number = "123",
    sensor_type = "abc"
  ) %>%
  select(
    county,
    station,
    deployment_range,
    sensor_type,
    sensor_serial_number,
    sensor_depth_at_low_tide_m,
    timestamp_utc,
    dissolved_oxygen_percent_saturation,
    temperature_degree_c
  )


attributes(dat$temperature_degree_c) <- NULL
attributes(dat$dissolved_oxygen_percent_saturation) <- NULL


# ss_ggplot_variables(dat) +
#   geom_point(size = 3)

# Export rds file
saveRDS(dat, file = here("inst/testdata/test_data_climatology.RDS"))
