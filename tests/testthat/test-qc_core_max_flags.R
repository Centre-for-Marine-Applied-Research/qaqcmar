# Generated with Claude Opus 5.5

max_dat <- data.frame(
  value = 1:5,
  grossrange_flag = c(1L, 4L, 2L, 1L, NA),
  spike_flag = c(3L, 1L, NA, 1L, NA),
  depth_crosscheck_flag = 4L
)

out <- qc_core_max_flags(max_dat)

test_that("qc_core_max_flags() gives the worst flag in each row", {
  expect_equal(out$qc_flag, c(3L, 4L, 2L, 1L, NA))
})

test_that("qc_core_max_flags() only adds qc_flag", {
  expect_equal(out[colnames(max_dat)], max_dat)
  expect_equal(colnames(out), c(colnames(max_dat), "qc_flag"))
})

test_that("qc_core_max_flags() stops when there are no test flags", {
  expect_error(
    qc_core_max_flags(max_dat[c("value", "depth_crosscheck_flag")]),
    "no flag columns"
  )
})
