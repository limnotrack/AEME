test_that("output_time_step defaults to daily and round-trips", {
  aeme <- suppressMessages(new_aeme())
  expect_equal(time(aeme)$time_step, 3600)
  expect_equal(time(aeme)$output_time_step, 86400)

  aeme <- suppressMessages(set_time(aeme, output_time_step = 3600))
  expect_equal(time(aeme)$output_time_step, 3600)

  # write_aeme_to_files() carries the new field in time.csv
  td <- withr::local_tempdir()
  ae <- readRDS(system.file("extdata/aeme.rds", package = "AEME"))
  ae <- suppressMessages(set_time(ae, output_time_step = 3600))
  suppressMessages(write_aeme_to_files(ae, path = td))
  tf <- list.files(td, pattern = "time.csv", recursive = TRUE, full.names = TRUE)
  time_csv <- read.csv(tf)
  expect_true("output_time_step" %in% names(time_csv))
  expect_equal(time_csv$output_time_step, 3600)

  # the yaml loader honours output_time_step, defaulting to daily when absent
  ae_yaml <- suppressMessages(
    yaml_to_aeme(path = system.file("extdata/lake/", package = "AEME"),
                 file = "aeme.yaml")
  )
  expect_equal(time(ae_yaml)$output_time_step, 86400)
})

test_that("set_time() rejects output_time_step < time_step", {
  aeme <- suppressMessages(new_aeme())
  expect_error(
    suppressMessages(set_time(aeme, time_step = 7200, output_time_step = 3600)),
    class = "aeme_error_output_time_step"
  )
})

test_that("is_subdaily() and .as_forcing_datetime() classify by spacing", {
  daily <- seq(as.POSIXct("2020-01-01", tz = "UTC"), by = "day", length.out = 10)
  hourly <- seq(as.POSIXct("2020-01-01", tz = "UTC"), by = "hour", length.out = 48)

  expect_false(is_subdaily(as.Date(daily)))
  expect_false(is_subdaily(daily))
  expect_true(is_subdaily(hourly))

  expect_s3_class(.as_forcing_datetime(format(daily, "%Y-%m-%d")), "Date")
  expect_s3_class(.as_forcing_datetime(format(hourly, "%Y-%m-%d %H:%M:%S")),
                  "POSIXct")
})

test_that(".collapse_output_date keeps Date for daily, POSIXct for sub-daily", {
  midnights <- seq(as.POSIXct("2020-01-01", tz = "UTC"), by = "day",
                   length.out = 5)
  hourly <- seq(as.POSIXct("2020-01-01", tz = "UTC"), by = "hour",
                length.out = 30)
  expect_s3_class(.collapse_output_date(midnights), "Date")
  expect_s3_class(.collapse_output_date(hourly), "POSIXct")
  expect_identical(.collapse_output_date(as.Date(midnights)), as.Date(midnights))
})

test_that("aeme_time_axis() reduces to the historical daily sequence", {
  aeme_time <- list(
    start = as.POSIXct("2020-02-01", tz = "UTC"),
    stop  = as.POSIXct("2020-03-01", tz = "UTC"),
    time_step = 3600, output_time_step = 86400,
    spin_up = list(glm_aed = 3, gotm_wet = 3)
  )
  ax_gotm <- aeme_time_axis(aeme_time, "gotm_wet")
  # daily grid from (start - 3 days) to stop, spin-up rows dropped
  ref <- seq.Date(as.Date("2020-02-01") - 3, as.Date("2020-03-01"), by = "day")
  expect_equal(length(ax_gotm$axis), length(ref))
  expect_equal(ax_gotm$index, which(seq_along(ref) > 3))

  # hourly output step -> 24x more rows, spin-up still 3 days
  aeme_time$output_time_step <- 3600
  ax_hr <- aeme_time_axis(aeme_time, "gotm_wet")
  expect_equal(length(ax_hr$axis), (length(ref) - 1) * 24 + 1)
  expect_equal(min(ax_hr$index), 3 * 24 + 1)
})

test_that("build_glm() propagates output_time_step to nsave/subdaily", {
  skip_if_models_unavailable("glm_aed")
  aeme <- suppressMessages(
    yaml_to_aeme(path = system.file("extdata/lake/", package = "AEME"),
                 file = "aeme.yaml")
  )
  path <- withr::local_tempdir()
  mc <- get_model_controls()

  # daily (default): shipped template values preserved
  a_daily <- suppressWarnings(suppressMessages(
    build_aeme(aeme = aeme, path = path, model = "glm_aed",
               model_controls = mc, ext_elev = 5)
  ))
  nml_file <- find_glm_nml(file.path(get_lake_dir(a_daily, path), "glm_aed"))
  nml_d <- read_nml(nml_file)
  expect_equal(nml_d$output$nsave, 24)
  expect_false(isTRUE(nml_d$meteorology$subdaily))
})
