# February 10, 2023

# Simulate data for the Spike test

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

# vectors used to create data frame
months <- seq(1, 2)
days <- rep(c(1:28), 2)
variables <- c("temperature_degree_c", "dissolved_oxygen_percent_saturation")

dat <- expand.grid(month = months, day = days, variable = variables) %>%
  distinct(month, day, variable) %>%
  mutate(
    timestamp_utc = as_datetime(paste("2023", month, day, sep = "-")),
    sensor_depth_at_low_tide_m = 2,
    sensor_serial_number = 123,
    sensor_type = "aquameasure",
    # add :"good values"
    value = case_when(
      variable == "temperature_degree_c" ~ 20,
      variable == "dissolved_oxygen_percent_saturation" ~ 100
    ),

    # add values with known flags
    value = case_when(
      day == 3 & variable == "temperature_degree_c" ~ value + 1,
      day == 3 & variable == "dissolved_oxygen_percent_saturation" ~ value +
        2.5,
      day == 10 & variable == "temperature_degree_c" ~ value + 2,
      day == 10 & variable == "dissolved_oxygen_percent_saturation" ~ value + 8,
      day == 17 & variable == "temperature_degree_c" ~ value - 1,
      day == 17 & variable == "dissolved_oxygen_percent_saturation" ~ value -
        2.5,
      TRUE ~ value
    )
  ) %>%
  select(-c(month, day)) %>%
  ss_pivot_wider() %>%
  mutate(
    county = "Halifax",
    station = "Emonds Field",
    deployment_range = "2019-Nov-01 to 2020-Jun-15"
  )

# Export rds file
saveRDS(dat, file = here("inst/testdata/test_data_spike.RDS"))

#
# dat %>%
#   ss_ggplot_variables() +
#   geom_point(size = 3)

# qc_gr <- dat %>%
#   qc_test_grossrange(county = "Lunenburg") %>%
#   mutate(sensor_serial_number = "")
#
#
# qc_plot_flags(
#   qc_gr,
#   qc_tests = "grossrange",
#   vars = "temperature_degree_c"
# )
#
# qc_plot_flags(
#   qc_gr,
#   qc_tests = "grossrange",
#   vars = "dissolved_oxygen_percent_saturation"
# )
