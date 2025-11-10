#' Identify redundant temperature data from VR2 sensors
#'
#' VR2AR temperature sensors are substantially less accurate than other
#' temperature sensors. Starting in 2025, VR2AR units will be used only for
#' acoustic detections and the release. A hobo sensor will be attached to each
#' VR2AR to record bottom temperature. The corresponding VR2AR temperature will
#' be assigned a human in the loop flag of 4 for CMAR records, and will *not* be
#' published on the Open Data Portal
#'
#' @param dat Data frame of compiled sensor string data for a single deployment.
#'
#' @returns A data frame row that can be appended to
#'   water_quality_human_in_loop.
#'
#' @importFrom dplyr distinct filter mutate n select
#' @importFrom lubridate as_datetime
#'
#' @export


qc_vr2_for_hil <- function(dat) {

  dat %>%
    distinct(
      county, station, depl_range = deployment_range,
      sensor_type, sensor_serial_number, sensor_depth_at_low_tide_m) %>%
    filter(
      sensor_type == "vr2ar" | sensor_type == "hobo",
      sensor_depth_at_low_tide_m == max(sensor_depth_at_low_tide_m)
    ) %>%
    mutate(n = n()) %>%
    ungroup() %>%
    filter(n >= 2, sensor_type == "vr2ar") %>%
    #pull(sensor_serial_number)
    mutate(
      variable = "temperature_degree_c",
      timestamp_prompt = "between",
      timestamp_utc_min = as_datetime(NA),
      timestamp_utc_max = as_datetime(NA),
      qc_test_column = "grossrange_flag_value",
      qc_flag_value = NA,
      human_in_loop_flag_value = 4,
      human_in_loop_comment = "vr2ar unit paired with hobo sensor for more accurate temperature data"
    ) %>%
    select(
      county, station, depl_range, variable, sensor_serial_number,
      timestamp_prompt, timestamp_utc_min, timestamp_utc_max,
      qc_test_column, qc_flag_value, human_in_loop_flag_value, human_in_loop_comment
    )
}
