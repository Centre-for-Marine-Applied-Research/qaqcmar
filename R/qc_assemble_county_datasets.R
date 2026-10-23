#' Assemble county data
#'
#' Will add columns that do not already exist.
#'
#' Only the six variables published in the county datasets are kept (dissolved
#' oxygen percent saturation and uncorrected mg/L, pH, salinity, measured depth
#' and temperature). Any other variable in the files (e.g., chlorophyll), and
#' its flag columns, is dropped with a warning that names it.
#'
#' @param prov Character string indicating which province the data to be
#'   assembled is from. Options are "ns" (the default) and "nb". This dictates
#'   the default file path.
#'
#' @param path File path to the folder with the rds files to be assembled.
#'
#' @param folder Name of the folder where the rds files are saved.
#'
#' @return Returns a data.frame with data from all deployments in folder.
#'
#' @importFrom dplyr all_of arrange distinct mutate n row_number select
#' @importFrom purrr list_rbind map
#'
#' @export
#'
#' @examples
#' # save a flagged deployment in a temporary folder
#' path <- system.file("testdata", package = "qaqcmar")
#' folder <- file.path(tempdir(), "lunenburg")
#' dir.create(folder)
#'
#' readRDS(file.path(path, "test_data_grossrange.RDS")) |>
#'   qc_test_all(qc_tests = c("climatology", "grossrange")) |>
#'   qc_assign_max_flag() |>
#'   saveRDS(file.path(folder, "white_tower.rds"))
#'
#' # columns that were not in the file are added with NA values
#' dat <- qc_assemble_county_data(path = tempdir(), folder = "lunenburg")
#' colnames(dat)
#'
#' unlink(folder, recursive = TRUE)


qc_assemble_county_data <- function(prov = "ns", path = NULL, folder) {
  prov <- match.arg(prov, c("ns", "nb"))

  if (is.null(path)) {
    if (prov == "ns") {
      path <- file.path(
        "R:/data_branches/water_quality/processed_data/qc_data"
      )
    }
    if (prov == "nb") {
      path <- file.path(
        "R:/data_branches/nb_water_quality/processed_data/qc_data"
      )
    }
  }
  if (prov == "ns") {
    region_col <- "county"
  } else {
    region_col <- "region"
  }

  # column order ------------------------------------------------------------

  # use for the join and to order columns in output
  depl_cols <- c(
    region_col,
    "waterbody",
    "station",
    "lease",
    "latitude",
    "longitude",
    "deployment_range",
    "string_configuration",
    "sensor_type",
    "sensor_serial_number",
    "timestamp_utc",
    "sensor_depth_at_low_tide_m",
    "depth_crosscheck_flag",
    "hil_comment"
  )

  var_cols <- c(
    "dissolved_oxygen_percent_saturation",
    "dissolved_oxygen_uncorrected_mg_per_l",
    "ph_ph",
    "salinity_psu",
    "sensor_depth_measured_m",
    "temperature_degree_c"
  )

  qc_test_cols <- qaqcmar::thresholds |>
    select(qc_test, variable) |>
    distinct() |>
    filter(qc_test != "depth_crosscheck") |>
    mutate(col_name = paste(qc_test, "flag", variable, sep = "_")) |>
    arrange(qc_test)

  qc_test_cols <- sort(c(
    qc_test_cols$col_name,
    "grossrange_flag_ph_ph",
    "human_in_loop_flag_dissolved_oxygen_percent_saturation",
    "human_in_loop_flag_dissolved_oxygen_uncorrected_mg_per_l",
    "human_in_loop_flag_ph_ph",
    "human_in_loop_flag_salinity_psu",
    "human_in_loop_flag_sensor_depth_measured_m",
    "human_in_loop_flag_temperature_degree_c"
  ))

  qc_max_cols <- c(
    "qc_flag_dissolved_oxygen_percent_saturation",
    "qc_flag_dissolved_oxygen_uncorrected_mg_per_l",
    "qc_flag_salinity_psu",
    "qc_flag_ph_ph",
    "qc_flag_sensor_depth_measured_m",
    "qc_flag_temperature_degree_c"
  )

  # all columns that should be in the data
  all_cols <- c(depl_cols, var_cols, qc_test_cols, qc_max_cols)
  df <- data.frame(matrix(nrow = 1, ncol = length(all_cols)))
  colnames(df) <- all_cols

  # list all files in region folder
  depls <- list.files(
    paste(path, folder, sep = "/"),
    pattern = "\\.rds$",
    full.names = TRUE
  )

  if (length(depls) == 0) {
    stop("No .rds files found in ", file.path(path, folder))
  }

  # read in data, bind together
  dat <- depls |>
    map(readRDS) |>
    list_rbind()

  # variables with flag columns (e.g. qc_flag_chlorophyll_blue_ug_per_l) that
  # are not in var_cols are dropped by select() below
  dropped_vars <- grep("_flag_", colnames(dat), value = TRUE) |>
    sub(pattern = "^.*?_flag_", replacement = "") |>
    unique() |>
    setdiff(var_cols)

  if (length(dropped_vars) > 0) {
    warning(
      "Variables not included in the county dataset were dropped: ",
      paste(dropped_vars, collapse = ", "),
      call. = FALSE
    )
  }

  # if any needed columns are NOT in dat, add them as na
  dat |>
    bind_rows(df) |>
    filter(row_number() != n()) |> # last row will be all NA, so need to remove it
    select(all_of(all_cols)) # fix the column order
}
