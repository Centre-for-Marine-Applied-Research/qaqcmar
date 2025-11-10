# Need this so that package will play nice with dplyr package
# https://community.rstudio.com/t/how-to-solve-no-visible-binding-for-global-variable-note/28887
# more technical solution here: https://cran.r-project.org/web/packages/dplyr/vignettes/programming.html

# other technical solution here:
# https://dplyr.tidyverse.org/articles/programming.html


utils::globalVariables(
  c(
    "sensor_serial_number",
    "threshold_value",

    # qc_test_climatology
    "flag",
    "season_max",
    "season_min",
    "season",
    "tstamp",
    "climatology_flag",
    "numeric_month",
    "qc_test",
    "thresholds",
    "months_seasons",
    "threshold",

    # qc_test_flat_line
    "flat_line_flag",
    "suspect",
    "fail",

    # qc_test_grossrange
    "sensor",
    "sensor_type",
    "sensor_max",
    "sensor_min",
    "user_max",
    "user_min",
    "timestamp_utc",
    "grossrange_flag",
    "variable",
    "value",

    # qc_test_spike
    "lag_value",
    "lead_value",
    "spike_ref",
    "spike_flag",
    "spike_value",
    "spike_high",
    "spike_low",

    # qc_test_rolling_sd
    "county",
    "station",
    "deployment_range",
    "int_sample",
    "n_sample",
    "effective_sample",
    "rolling_sd_flag",
    "sd_roll",
    "n_sample_effective",

    # qc_assign_max_flag
    "qc_col",
    "qc_flag",
    "depth_crosscheck",
    "hil_comment",

    # ggplot_all_tests
    "depth",
    "sensor_depth_at_low_tide_m",
    "min_depth",
    "depth_label",
    "min_sensor_depth_measured_m",

    # qc_calculate_thresholds
    "rolling_sd_max",
    "mean_var",
    "sd_var",
    "threshold_value",
    "mean_sd_roll",
    "sd_sd_roll",
    "sensor_depth_measured_m",
    "min_measured",
    "abs_diff",
    "depth_diff_max",
    "mean_spike",
    "sd_spike",
    "spike_max",
    "spike_low",
    "spike_max",

    # qc_test_depth_crosscheck
    "depth_crosscheck_flag",
    "value_sensor_depth_measured_m",
    "depth_crosscheck_flag_sensor_depth_measured_m",

    # qc_summarise_flags
    "flag_value",
    "n_obs",
    "n_fl",

    # qc_pivot_longer
    "depth_crosscheck_flag_value",

    # qc_filter_flags
    # "keep_dissolved_oxygen_percent_saturation",
    # "keep_dissolved_oxygen_uncorrected_mg_per_l",
    # "keep_sensor_depth_measured_m",
    # "keep_salinity_psu",
    # "keep_temperature_degree_c"


    "qc_flag_dissolved_oxygen_percent_saturation",
    "qc_flag_dissolved_oxygen_uncorrected_mg_per_l",
    "qc_flag_sensor_depth_measured_m",
    "qc_flag_salinity_psu",
    "qc_flag_temperature_degree_c",
    "qc_flag_value",

    "filter_out_flag",
    "filter_out_timestamp",
    "filter_from_start",
    "start_date",
    "end_date",

    # qc_test_human_in_loop
    "hil_flag_comment",
    "human_in_loop_flag_value",
    "human_in_loop_reference_flag_col",

    # qc_vr2_for_hil
    "depl_range",
    "human_in_loop_comment",
    "qc_test_column",
    "timestamp_utc_min",
    "timestamp_utc_max",
    "timestamp_prompt"
  )
)
