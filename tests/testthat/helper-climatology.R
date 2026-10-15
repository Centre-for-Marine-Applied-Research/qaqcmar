# November 9, 2022
## UPDATE THIS

#' testdata was generated such that we KNOW what flag should be assigned to each
#' observation (data-raw/test_data_climatology.R)
#'
#' Here, qc_test_climatology() is applied to the test data. The qc'd data is
#' then separated into a data frame for each flag value, based on
#' the timestamps used to generate the data in data-raw/test_data.R.
#'
#' The tests check that each data frame include the expected flag value, and
#' only the expected flag value.

#' @importFrom dplyr %>% anti_join
#' @importFrom lubridate day

path <- system.file("testdata", package = "qaqcmar")

dat <- readRDS(paste0(path, "/test_data_climatology.RDS"))

# ss_ggplot_variables(dat) + geom_point(size = 2)

qc_cl <- dat %>%
  qc_test_climatology(county = "Lunenburg") %>%
  mutate(day = lubridate::day(timestamp_utc)) %>%
  qc_pivot_longer(qc_tests = "climatology")

# qc_plot_flags(
#   qc_cl,
#   qc_tests = "climatology",
#   vars = "temperature_degree_c"
# )
# qc_plot_flags(
#   qc_cl,
#   qc_tests = "climatology",
#   vars = "dissolved_oxygen_percent_saturation"
# )

# # timestamps for flagged observations ------------------------------------------------

qc_cl_1 <- qc_cl %>%
  filter(day == 10)

qc_cl_3 <- qc_cl %>%
  filter(day == 1 | day == 28)
