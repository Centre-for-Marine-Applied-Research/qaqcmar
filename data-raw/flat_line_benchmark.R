library(lubridate)
library(microbenchmark)
library(sensorstrings)

dat <- readRDS(
  "Y:/coastal_monitoring_program/data_branches/water_quality/processed_data/deployment_data/halifax/new/beaver_point_2020-12-12.rds"
)

dat <- dat %>%
  filter(timestamp_utc < as_datetime("2021-01-15"))

ss_ggplot_variables(dat)


dat_cols <- dat %>%
  select(
    sensor_type,
    sensor_serial_number,
    timestamp_utc,
    dissolved_oxygen_percent_saturation,
    sensor_depth_measured_m,
    temperature_degree_c
  )


microbenchmark(
  x <- qc_test_flat_line(dat),
  y <- qc_test_flat_line(dat_cols),
  times = 10
)

# Unit: seconds
# expr                                min       lq      mean   median        uq
# x <- qc_test_flat_line(dat)      85.49152 88.13686 101.37094 99.86297 104.84726
# y <- qc_test_flat_line(dat_cols) 85.87375 87.33355  94.55601 91.07231  93.27132
# max neval
# 132.6567    10
# 126.5853    10

# takes a long time to run and sensors are not precise enough
