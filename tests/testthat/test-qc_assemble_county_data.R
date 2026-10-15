# generate by Claude Opus 5.5
# qc_assemble_county_data() reads every .rds file in file.path(path, folder)
# and binds them together, adding any expected columns that are missing.
# These tests write small deployment files to a temporary folder and pass it
# with the path argument, so they do not need the R: drive.

# write one small deployment file to dir
write_test_deployment <- function(
  dir,
  file,
  station,
  n = 3,
  region_col = "county"
) {
  dat <- data.frame(
    region = "Halifax",
    station = station,
    timestamp_utc = as.POSIXct("2023-06-01", tz = "UTC") +
      (seq_len(n) - 1) * 3600,
    temperature_degree_c = seq_len(n) + 10,
    qc_flag_temperature_degree_c = 1
  )
  names(dat)[names(dat) == "region"] <- region_col

  saveRDS(dat, file.path(dir, file))
}

# create path/folder in a new temporary directory and return path
make_test_path <- function(folder = "Halifax") {
  path <- tempfile("qc_assemble_")
  dir.create(file.path(path, folder), recursive = TRUE)
  path
}

test_that("qc_assemble_county_data() reads every .rds file in path/folder", {
  path <- make_test_path()
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  dir <- file.path(path, "Halifax")

  write_test_deployment(dir, "station_a.rds", "Station A", n = 3)
  write_test_deployment(dir, "station_b.rds", "Station B", n = 2)
  write.csv(data.frame(x = 1), file.path(dir, "not_a_deployment.csv"))

  dat <- qc_assemble_county_data(path = path, folder = "Halifax")

  expect_equal(nrow(dat), 5)
  expect_setequal(unique(dat$station), c("Station A", "Station B"))
  expect_equal(sort(dat$temperature_degree_c), c(11, 11, 12, 12, 13))
})

test_that("qc_assemble_county_data() adds missing columns as NA in a fixed order", {
  path <- make_test_path()
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  write_test_deployment(
    file.path(path, "Halifax"),
    "station_a.rds",
    "Station A"
  )

  dat <- qc_assemble_county_data(path = path, folder = "Halifax")

  expect_equal(colnames(dat)[1:3], c("county", "waterbody", "station"))
  expect_true(all(
    c(
      "dissolved_oxygen_percent_saturation",
      "grossrange_flag_temperature_degree_c",
      "human_in_loop_flag_temperature_degree_c",
      "qc_flag_temperature_degree_c"
    ) %in%
      colnames(dat)
  ))
  expect_true(all(is.na(dat$dissolved_oxygen_percent_saturation)))
  expect_equal(dat$qc_flag_temperature_degree_c, c(1, 1, 1))
})

test_that("qc_assemble_county_data() uses a region column when prov = 'nb'", {
  path <- make_test_path("Northumberland")
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  write_test_deployment(
    file.path(path, "Northumberland"),
    "station_a.rds",
    "Station A",
    region_col = "region"
  )

  dat <- qc_assemble_county_data(
    prov = "nb",
    path = path,
    folder = "Northumberland"
  )

  expect_equal(colnames(dat)[1], "region")
  expect_false("county" %in% colnames(dat))
  expect_equal(unique(dat$region), "Halifax")
})

test_that("qc_assemble_county_data() errors when no .rds files are found", {
  path <- make_test_path()
  on.exit(unlink(path, recursive = TRUE), add = TRUE)

  # folder exists but is empty
  expect_error(
    qc_assemble_county_data(path = path, folder = "Halifax"),
    "No .rds files found"
  )
  # folder does not exist
  expect_error(
    qc_assemble_county_data(path = path, folder = "Not a county"),
    "No .rds files found"
  )
})

test_that("qc_assemble_county_data() errors for an unknown province", {
  expect_error(qc_assemble_county_data(prov = "pei", folder = "Halifax"))
})
