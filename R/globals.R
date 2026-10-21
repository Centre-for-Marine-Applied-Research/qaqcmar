utils::globalVariables(
  c(
    "sensor_serial_number",
    "threshold_value",

    # qc_test_climatology
    "tstamp",
    "qc_test",
    "threshold",

    # qc_test_grossrange
    "sensor",
    "sensor_type",
    "sensor_max",
    "sensor_min",
    "user_max",
    "user_min",
    "timestamp_utc",
    "variable",
    "value",

    # qc_test_spike
    "lag_value",
    "lead_value",
    "spike_ref",
    "spike_value",
    "spike_high",
    "spike_flag",
    "dup",
    "dup_prev",
    "near_dup",
    "gap_prev",
    "near_gap",

    # qc_test_rolling_sd
    "county",
    "station",
    "deployment_range",
    "int_sample",
    "n_sample",
    "sd_roll",
    "n_sample_effective",
    "row_id",

    # qc_assign_max_flag
    "qc_flag",
    "hil_comment",

    # ggplot_all_tests
    "sensor_depth_at_low_tide_m",
    "depth_label",
    "min_sensor_depth_measured_m",

    # qc_calculate_thresholds
    "rolling_sd_max",
    "mean_var",
    "sd_var",
    "mean_sd_roll",
    "sd_sd_roll",
    "sensor_depth_measured_m",
    "min_measured",
    "abs_diff",
    "depth_diff_max",
    "mean_spike",
    "sd_spike",
    "spike_low",
    "spike_max",

    # qc_test_depth_crosscheck
    "depth_crosscheck_flag",

    # qc_summarise_flags
    "flag_value",
    "n_obs",
    "n_fl",

    # qc_test_human_in_loop
    "human_in_loop_flag_value",
    "human_in_loop_reference_flag_col",

    # qc_vr2_for_hil
    "depl_range",
    "qc_flag_value",
    "human_in_loop_comment",
    "qc_test_column",
    "timestamp_utc_min",
    "timestamp_utc_max",
    "timestamp_prompt",

    # core_prepare
    "sensor_key",

    # qc_lookup_thresholds
    "thr_county",
    "thr_month",
    "thr_sensor_type",
    "spec"
  )
)
