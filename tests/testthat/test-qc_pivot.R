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
