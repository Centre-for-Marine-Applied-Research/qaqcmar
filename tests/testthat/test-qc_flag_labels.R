flag_labels <- data.frame(flag = c(1, 2, 3, 4)) %>%
  qc_assign_flag_labels()

test_that("qc_flag_labels() assigns correct flags", {
  expect_equal(
    as.character(flag_labels$flag),
    c("Pass", "Not Evaluated", "Suspect/Of Interest", "Fail")
  )
  expect_equal(class(flag_labels$flag), c("ordered", "factor"))

  expect_equal(
    levels(flag_labels$flag),
    c("Pass", "Not Evaluated", "Suspect/Of Interest", "Fail")
  )
})
