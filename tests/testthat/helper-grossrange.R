# February 10, 2023

# testdata was generated such that we KNOW what flag should be assigned to each
# observation (data-raw/test_data.R)

# Simulated data for the gross range test
# was constructed so there are 4 observations per month
# Day 1: Gross Range Flag 4 (low)
# Day 5: Gross Range Flag 3 (low)*
# Day 10: Gross Range Flag 1
# Day 15: Gross Range Flag 4 (high)
# Day 28: Gross Range Flag 3 (high)

# * for the aquameasure and vr2ar sensors, the user_min is less than the sensor_min,
# which means a flag of 3 (low) will not be assigned

# Here, qc_test_grossrange() is applied to the test data. The qc'd data is then
# separated by day.
# The tests check that each day include the expected flag value, and
# only the expected flag value.


#' @importFrom dplyr %>% anti_join

path <- system.file("testdata", package = "qaqcmar")

dat <- readRDS(paste0(path, "/test_data_grossrange.RDS"))

# sensorstrings::ss_ggplot_variables(dat)

qc_gr <- dat %>%
  qc_test_grossrange(county = "Lunenburg") %>%
  qc_pivot_longer(qc_tests = "grossrange") %>%
  mutate(day = lubridate::day(timestamp_utc))

# qc_plot_flags(
#   qc_gr,
#   qc_tests = "grossrange",
#   vars = "temperature_degree_c"
# )
qc_plot_flags(
  qc_gr,
  qc_tests = "grossrange",
  vars = "dissolved_oxygen_percent_saturation"
)

# filter by day flagged observations ------------------------------------------------

qc_gr_1 <- qc_gr %>%
  filter(day == 10)

qc_gr_3 <- qc_gr %>%
  filter(day == 5 | day == 28)

qc_gr_4 <- qc_gr %>%
  filter(day == 1 | day == 15)
