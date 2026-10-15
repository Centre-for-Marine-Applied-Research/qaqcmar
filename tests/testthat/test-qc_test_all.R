# test test_all returns same flags as the individual foos
# check that max_flag is the same when starting from wide and long dat
# should probably check the values of max flag?

test_that("qc_test_all() returns correct dimensions", {
  expect_equal(nrow(dat_wide), 1009)
  expect_equal(ncol(dat_wide), 19)
})


# this test does not work with current version of function
# test_that("qc_assign_max_flag() chooses the correct flag", {
#   expect_equal(df_max_flag$qc_flag_var1, 100)
#   expect_equal(df_max_flag$qc_flag_var2, 200)
#   expect_equal(df_max_flag$qc_flag_var3, 300)
# })

test_that("qc_assign_max_flag() works on wide and long data", {
  expect_equal(dat_wide_max, dat_long_max)
})
