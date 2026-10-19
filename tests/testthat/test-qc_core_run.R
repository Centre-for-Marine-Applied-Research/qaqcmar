# Generated with Claude Opus 5.5

dat_run <- readRDS(
  paste0(
    system.file("testdata", package = "qaqcmar"),
    "/test_data_grossrange.RDS"
  )
)

out <- qc_core_run(dat_run, c("grossrange", "spike"), county = "Lunenburg")

test_that("qc_core_run() adds one flag column per test and no thresholds", {
  expect_true(all(c("grossrange_flag", "spike_flag") %in% colnames(out)))
  expect_false(any(
    c("climatology_flag", "rolling_sd_flag", "qc_flag") %in% colnames(out)
  ))
  expect_false(any(c("sensor_min", "user_max", "spike_low") %in% colnames(out)))
})

test_that("qc_core_run() keeps one row per observation", {
  expect_equal(nrow(out), nrow(qc_core_prepare(dat_run)))
  expect_equal(anyDuplicated(out[c("row_id", "variable")]), 0)
})

test_that("qc_core_run() adds qc_flag only when max_flag = TRUE", {
  out <- qc_core_run(
    dat_run,
    "grossrange",
    county = "Lunenburg",
    max_flag = TRUE
  )

  expect_equal(out$qc_flag, out$grossrange_flag)
})

test_that("qc_core_run() stops on an unknown test", {
  expect_error(qc_core_run(dat_run, "grossrnage", county = "Lunenburg"))
})
