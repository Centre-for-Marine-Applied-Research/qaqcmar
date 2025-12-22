#' Filter data to keep observations of acceptable quality
#'
#' @param dat Data frame with summary quality control flags in long format
#'   (e.g., output of `qc_pivot_longer()`).
#'
#' @param keep_dissolved_oxygen_percent_saturation Vector of acceptable flags
#'   for variable dissolved_oxygen_percent_saturation. Default is c(1, 2, NA).
#'   "Suspect/Of Interest" observations are considered biofouling and excluded
#'   from analysis.
#'
#' @param keep_dissolved_oxygen_uncorrected_mg_per_l Vector of acceptable flags
#'   for variable dissolved_oxygen_uncorrected_mg_per_l. Default is c(1, 2, NA).
#'   "Suspect/Of Interest" observations are considered biofouling and excluded
#'   from analysis.
#'
#' @param keep_sensor_depth_measured_m  Vector of acceptable flags for variable
#'   sensor_depth_measured_m. Default is c(1, 2, 3, NA).
#'
#' @param keep_salinity_psu Vector of acceptable flags for variable
#'   salinity_psu. Default is c(1, 2, 3, NA).
#'
#' @param keep_temperature_degree_c Vector of acceptable flags for variable
#'   temperature_degree_c. Default is c(1, 2, 3, NA).
#'
#' @param keep_sus_do_stations Character vector of stations for which dissolved
#'   oxygen (percent saturation) should be considered "Of Interest" (not
#'   "Suspect").
#'
#' @param keep_sus_do Vector of acceptable flags for variable
#'   dissolved_oxygen_percent_saturation for stations listed in
#'   \code{keep_sus_do_stations}. Default is c(1, 2, 3, NA).
#'
#'
#' @importFrom dplyr %>% filter
#'
#' @export

# qc_filter_summary_flags <- function(
#     dat,
#
#     keep_dissolved_oxygen_percent_saturation = c(1, 2, NA),
#     keep_dissolved_oxygen_uncorrected_mg_per_l = c(1, 2, NA),
#     keep_salinity_psu = c(1, 2, NA),
#     keep_sensor_depth_measured_m = c(1, 2, 3, NA),
#     keep_temperature_degree_c = c(1, 2, 3, NA),
#
#     keep_sus_do_stations = NULL,
#     keep_sus_do = c(1, 2, 3, NA)
#
#     ) {
#
#   if(is.null(keep_sus_do_stations)) {
#     keep_sus_do_stations <- c(
#       "0814x E",
#       "0814x W",
#       "Aberdeen",
#       "Deep Basin",
#       "Hourglass Lake",
#       "Piper Lake",
#       "Sissiboo",
#       "Tickle Island 1" # 60 m DO only
#     )
#   }
#
#   dat %>%
#     filter(
#       (variable == "dissolved_oxygen_percent_saturation" &
#          station %in% keep_sus_do_stations &
#          qc_flag_value %in% keep_sus_do) |
#
#       (variable == "dissolved_oxygen_percent_saturation" &
#         qc_flag_value %in% keep_dissolved_oxygen_percent_saturation) |
#
#       (variable == "dissolved_oxygen_uncorrected_mg_per_l" &
#         qc_flag_value %in% keep_dissolved_oxygen_uncorrected_mg_per_l) |
#
#       (variable == "sensor_depth_measured_m" &
#         qc_flag_value %in% keep_sensor_depth_measured_m) |
#
#       (variable == "salinity_psu" & qc_flag_value %in%  keep_salinity_psu) |
#
#       (variable == "temperature_degree_c" &
#          qc_flag_value %in% keep_temperature_degree_c)
#     )
#
#
# }


