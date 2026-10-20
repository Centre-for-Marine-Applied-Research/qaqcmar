path <- system.file("testdata", package = "qaqcmar")

qc_tests <- c(
  "climatology",
  "grossrange",
  "rolling_sd",
  "spike",
  "depth_crosscheck"
)

dat_wide <- readRDS(paste0(path, "/test_data_rolling_sd.RDS")) %>%
  qc_test_all(
    qc_tests = qc_tests,
    county = "Halifax"
  )

dat_long <- dat_wide %>%
  qc_pivot_longer(qc_tests = qc_tests)

dat_wide2 <- dat_long %>%
  qc_pivot_wider()

dat_long_max <- dat_long %>%
  qc_assign_max_flag()

dat_wide_max <- dat_wide %>%
  qc_assign_max_flag()

# dat_wide_max2 <- dat_wide %>%
#   qc_assign_max_flag(return_all = FALSE)

test_that("qc_test_all() returns correct dimensions", {
  expect_equal(nrow(dat_wide), 1009)
  expect_equal(ncol(dat_wide), 19)
})


test_that("qc_assign_max_flag() works on wide and long data", {
  expect_equal(dat_wide_max, dat_long_max)
})

test_that("qc_test_all() skips depth crosscheck when depth was not measured", {
  dat <- readRDS(paste0(path, "/test_data_grossrange.RDS"))
  out <- suppressMessages(qc_test_all(dat))

  expect_false("depth_crosscheck_flag" %in% colnames(out))
})
