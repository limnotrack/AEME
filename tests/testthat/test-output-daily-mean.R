test_that("set_output_time_step(daily_mean=) sets the time flag", {
  aeme <- new_aeme()
  expect_false(isTRUE(time(aeme)$output_daily_mean))

  aeme <- set_output_time_step(aeme, "hourly", daily_mean = TRUE)
  expect_identical(time(aeme)$output_time_step, 3600)
  expect_true(time(aeme)$output_daily_mean)

  aeme <- set_output_time_step(aeme, "hourly", daily_mean = FALSE)
  expect_false(time(aeme)$output_daily_mean)
})

test_that("set_output_time_step(daily_mean=) warns when frequency is not sub-daily", {
  aeme <- new_aeme()
  expect_warning(
    set_output_time_step(aeme, "daily", daily_mean = TRUE),
    class = "aeme_warn_output_daily_mean"
  )
})

test_that("set_output_time_step(daily_mean=) rejects a non-logical", {
  aeme <- new_aeme()
  expect_error(
    set_output_time_step(aeme, "hourly", daily_mean = "yes"),
    class = "aeme_error_output_daily_mean"
  )
})

test_that("output_daily_mean is serialised to the time data frame", {
  aeme <- new_aeme()
  aeme <- set_output_time_step(aeme, "hourly", daily_mean = TRUE)
  df <- as.data.frame(time(aeme))
  expect_true("output_daily_mean" %in% names(df))
  expect_true(isTRUE(as.logical(df$output_daily_mean[1])))
})

test_that("a time list without output_daily_mean defaults to FALSE", {
  aeme <- new_aeme()
  tt <- time(aeme)
  tt$output_daily_mean <- NULL
  time(aeme) <- tt
  expect_false(isTRUE(time(check_aeme(aeme))$output_daily_mean))
})

test_that(".aggregate_output_list_daily averages sub-daily records by calendar day", {
  # 3 days x 4 sub-daily steps
  times <- as.POSIXct("2020-01-01 00:00:00", tz = "UTC") +
    3600 * 6 * (0:11)
  day2 <- as.Date(times - 1, tz = "UTC")   # not used; sanity only

  temp <- matrix(seq_len(5 * 12), nrow = 5, ncol = 12)   # depth x time
  surft <- as.numeric(1:12)                               # 1-D over time
  z <- 1:5                                                # not time-indexed

  out <- list(Date = times, HYD_temp = temp, HYD_surft = surft, z = z)
  agg <- AEME:::.aggregate_output_list_daily(out)

  expect_s3_class(agg$Date, "Date")
  expect_length(agg$Date, 3)
  expect_equal(as.character(agg$Date),
               c("2020-01-01", "2020-01-02", "2020-01-03"))

  # day 1 = columns 1:4, day 2 = 5:8, day 3 = 9:12
  expect_equal(dim(agg$HYD_temp), c(5, 3))
  expect_equal(agg$HYD_temp[, 1], rowMeans(temp[, 1:4]))
  expect_equal(agg$HYD_temp[, 3], rowMeans(temp[, 9:12]))

  expect_equal(agg$HYD_surft, c(mean(1:4), mean(5:8), mean(9:12)))

  # non-time-indexed variable passes through untouched
  expect_equal(agg$z, z)
})

test_that(".aggregate_output_list_daily is a no-op when already daily", {
  d <- as.Date("2020-01-01") + 0:4
  out <- list(Date = as.POSIXct(d, tz = "UTC"),
              HYD_temp = matrix(1:15, nrow = 3))
  agg <- AEME:::.aggregate_output_list_daily(out)
  expect_length(agg$Date, 5)
  expect_equal(agg$HYD_temp, matrix(1:15, nrow = 3))
})

test_that(".glm_daily_keep_extra covers what read_glm_output() reads unconditionally", {
  keep <- AEME:::.glm_daily_keep_extra
  # the daily surface-flux / lake diagnostic block
  expect_true(all(c("daily_qe", "daily_qh", "daily_qlw", "daily_qsw",
                    "lake_volume", "evaporation", "evap_mass_flux",
                    "surface_area", "tot_inflow_vol", "overflow_vol",
                    "tot_outflow_vol", "precipitation", "surface_temp")
                  %in% keep))
  # layer structure + level
  expect_true(all(c("z", "NS", "lake_level") %in% keep))
})

test_that(".output_daily_path derives the companion path", {
  expect_equal(basename(AEME:::.output_daily_path("a/b/output/output.nc")),
               "output_daily.nc")
  expect_equal(AEME:::.output_daily_path("x/output_daily.nc"),
               "x/output_daily.nc")
  expect_null(AEME:::.output_daily_path(character(0)))
})
