# testdata was generated such that we KNOW what flag should be assigned to each
# observation (data-raw/test_data.R)

path <- system.file("testdata", package = "qaqcmar")

dat <- readRDS(paste0(path, "/test_data_spike.RDS"))


test_that("qc_test_spike() assigns correct flags", {

  dat_qc <- dat %>%
    qc_test_spike(county = "Halifax") %>%
    mutate(day = day(timestamp_utc)) %>%
    qc_pivot_longer(qc_tests = "spike")

  expect_equal(
    as.numeric(
      dat_qc |>
        mutate(month = month(timestamp_utc)) |>
        filter((month == 1 & day == 1) | (month == 2 & day == 28)) |>
        distinct(spike_flag_value) |>
        dplyr::pull(spike_flag_value)), 2)

  expect_equal(
    as.numeric(
      dat_qc |>
        filter(day %in% c(3, 9, 11, 17)) |>
        distinct(spike_flag_value) |>
        dplyr::pull(spike_flag_value)), 3)

  expect_equal(
    as.numeric(
      dat_qc |>
        filter(day == 10) |>
        distinct(spike_flag_value) |>
        dplyr::pull(spike_flag_value)), 4)

  expect_equal(
    as.numeric(
      dat_qc |>
        filter(!(day %in% c(1 , 3, 9, 10, 11, 17, 28))) |>
        distinct(spike_flag_value) |>
        dplyr::pull(spike_flag_value)), 1)

})
