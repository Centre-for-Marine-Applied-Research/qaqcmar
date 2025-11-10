#' Assemble county data
#'
#' Will add columns that do not already exist.
#'
#' @param prov Character string indicating which province the data to be
#'   assembled is from. Options are "ns" (the default) and "nb". This dictates
#'   the default file path.
#'
#' @param path File path the the folder with the rds files to be assembled.
#'
#' @param folder Name of the folder where the rds files are saved.
#'
#' @return Returns a data.frame with data from all deployments in folder.
#'
#' @importFrom dplyr %>% all_of arrange distinct mutate n row_number select
#' @importFrom purrr list_rbind map
#'
#' @export
#'

qc_assemble_county_data <- function(prov = "ns", path = NULL, folder) {

  if(prov == "ns") {
    path <- file.path(
      "R:/data_branches/water_quality/processed_data/qc_data")

    region_col <- "county"
  }
  if(prov == "nb") {
    path <- file.path(
      "R:/data_branches/nb_water_quality/processed_data/qc_data")

    region_col <- "region"
  }

  # column order ------------------------------------------------------------

  # use for the join and to order columns in output
  depl_cols <- c(
    region_col,
    "waterbody",
    "station",
    "lease",
    "latitude" ,
    "longitude" ,
    "deployment_range"   ,
    "string_configuration",
    "sensor_type",
    "sensor_serial_number",
    "timestamp_utc"  ,
    "sensor_depth_at_low_tide_m",
    "depth_crosscheck_flag",
    "hil_comment"
  )

  var_cols <- c(
    "dissolved_oxygen_percent_saturation"   ,
    "dissolved_oxygen_uncorrected_mg_per_l",
    "ph_ph",
    "salinity_psu",
    "sensor_depth_measured_m",
    "temperature_degree_c"
  )

  qc_test_cols <- thresholds %>%
    select(qc_test, variable) %>%
    distinct() %>%
    filter(qc_test != "depth_crosscheck") %>%
    mutate(col_name = paste(qc_test, "flag", variable, sep = "_")) %>%
    arrange(qc_test)

  qc_test_cols <- sort(c(
    qc_test_cols$col_name,
    "human_in_loop_flag_dissolved_oxygen_percent_saturation",
    "human_in_loop_flag_dissolved_oxygen_uncorrected_mg_per_l",
    "human_in_loop_flag_ph_ph",
    "human_in_loop_flag_salinity_psu",
    "human_in_loop_flag_sensor_depth_measured_m",
    "human_in_loop_flag_temperature_degree_c")
  )

  qc_max_cols <- c(
    "qc_flag_dissolved_oxygen_percent_saturation"   ,
    "qc_flag_dissolved_oxygen_uncorrected_mg_per_l",
    "qc_flag_salinity_psu",
    "qc_flag_ph_ph",
    "qc_flag_sensor_depth_measured_m",
    "qc_flag_temperature_degree_c"
  )

  # all columns that should be in the data
  all_cols <- c(depl_cols,  var_cols, qc_test_cols, qc_max_cols)
  df <- data.frame(matrix(nrow = 1, ncol = length(all_cols)))
  colnames(df) <- all_cols

  # list all files in region folder
  depls <- list.files(
    paste(path, folder, sep = "/"),
    pattern = ".rds",
    full.names = TRUE
  )

  # read in data, bind together
  dat <- depls %>%
    map(readRDS) %>%
    list_rbind()

  # if any needed columns are NOT in dat, add them as na
  dat %>%
    bind_rows(df) %>%
    filter(row_number() != n()) %>% # last row will be all NA, so need to remove it
    select(all_of(all_cols))  # fix the column order

}
