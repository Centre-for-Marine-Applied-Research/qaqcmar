path <- system.file("testdata", package = "qaqcmar")

dat_qc <- readRDS(paste0(path, "/test_data_estimated_depth.RDS")) |>
  ss_pivot_wider() |>
  mutate(temperature_degree_c = 10) |>
  qc_test_depth_crosscheck()

test_that("qc_test_depth_crosscheck() assigns correct flags", {
  expect_equal(
    as.numeric(
      dat_qc |>
        filter(sensor_depth_at_low_tide_m %in% c(2, 5, 10)) |>
        distinct(depth_crosscheck_flag) |>
        dplyr::pull(depth_crosscheck_flag)
    ),
    1
  )

  expect_equal(
    as.numeric(
      dat_qc |>
        filter(sensor_depth_at_low_tide_m == 20) |>
        distinct(depth_crosscheck_flag) |>
        dplyr::pull(depth_crosscheck_flag)
    ),
    2
  )

  expect_equal(
    as.numeric(
      dat_qc |>
        filter(sensor_depth_at_low_tide_m == 15) |>
        distinct(depth_crosscheck_flag) |>
        dplyr::pull(depth_crosscheck_flag)
    ),
    3
  )
})
