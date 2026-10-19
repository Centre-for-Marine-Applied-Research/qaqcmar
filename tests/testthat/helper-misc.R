# November 8, 2022

# labels ------------------------------------------------------------------

flag_labels <- data.frame(flag = c(1, 2, 3, 4)) %>%
  qc_assign_flag_labels()

# pivot -------------------------------------------------------------------

path <- system.file("testdata", package = "qaqcmar")

qc_tests <- c(
  "climatology",
  "grossrange",
  "rolling_sd",
  "spike",
  "depth_crosscheck"
)

n_var <- readRDS(paste0(path, "/test_data_rolling_sd.RDS")) %>%
  ss_pivot_longer() %>%
  distinct(variable)
n_var <- nrow(n_var)

dat_wide <- readRDS(paste0(path, "/test_data_rolling_sd.RDS")) %>%
  qc_test_all(
    qc_tests = qc_tests,
    county = "Halifax"
  )

dat_long <- dat_wide %>%
  qc_pivot_longer(qc_tests = qc_tests)

# dat_wide2 <- dat_long %>%
#   pivot_wider(
#   names_from = variable,
#   values_from = c(value, contains("flag_value"))
# )

dat_wide2 <- dat_long %>%
  qc_pivot_wider()

#qc_plot_flags(dat_long)

# max flag ----------------------------------------------------------------

dat_wide_max <- dat_wide %>%
  qc_assign_max_flag()

dat_wide_max2 <- dat_wide %>%
  qc_assign_max_flag(return_all = FALSE)

dat_long_max <- dat_long %>%
  qc_assign_max_flag()


# pivot max flags ---------------------------------------------------------

dat_pivot_long_max <- dat_wide_max %>%
  qc_pivot_longer(qc_tests = c("qc", qc_tests))

dat_pivot_long_max2 <- dat_wide_max2 %>%
  qc_pivot_longer(qc_tests = "qc")
