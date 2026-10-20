path <- system.file("testdata", package = "qaqcmar")

qc_tests <- c(
  "climatology",
  "grossrange",
  "rolling_sd",
  "spike",
  "depth_crosscheck"
)

n_var <- nrow(
  readRDS(paste0(path, "/test_data_rolling_sd.RDS")) %>%
    ss_pivot_longer() %>%
    distinct(variable)
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

dat_wide_max <- dat_wide %>%
  qc_assign_max_flag()

dat_wide_max2 <- dat_wide %>%
  qc_assign_max_flag(return_all = FALSE)

# pivot max flags ---------------------------------------------------------

# dat_pivot_long_max <- dat_wide_max %>%
#   qc_pivot_longer(qc_tests = c("qc", qc_tests))

# dat_pivot_long_max2 <- dat_wide_max2 %>%
#   qc_pivot_longer(qc_tests = "qc")

test_that("qc_pivot_longer() returns correct number of flag columns", {
  expect_equal(
    ncol(select(dat_long, contains("flag"))),
    length(qc_tests)
  )
})

test_that("qc_pivot_wider() returns dat_wide", {
  expect_equal(dat_wide, dat_wide2)
})

test_that("qc_assign_max_flags() has correct number of columns", {
  expect_equal(ncol(dat_wide) + n_var, ncol(dat_wide_max))
})
