make_dates <- function(start, n, by = "month") {
  seq.Date(as.Date(start), by = by, length.out = n)
}

test_that("suggest_kw_regime recommends static Kw when there are too few observations", {
  dates <- make_dates("2020-01-01", 5)
  kw <- c(1.0, 1.1, 0.9, 1.05, 0.95)

  rec <- suggest_kw_regime(dates, kw = kw, min_n = 12)

  testthat::expect_s3_class(rec, "kw_regime")
  testthat::expect_identical(rec$recommendation, "static")
  testthat::expect_false(rec$flags$sufficient_n)
  testthat::expect_equal(rec$mean_kw, mean(kw))
  testthat::expect_equal(rec$median_kw, stats::median(kw))
})

test_that("suggest_kw_regime falls back to static/single-year when years are too few", {
  dates <- make_dates("2020-01-01", 12)
  set.seed(1)
  kw <- 1 + 0.4 * sin(2 * pi * (as.integer(format(dates, "%m")) - 3) / 12) +
    stats::rnorm(length(dates), sd = 0.02)

  rec <- suggest_kw_regime(dates, kw = kw, min_n = 12, min_years = 3)

  testthat::expect_identical(rec$recommendation, "static_or_single_year_monthly")
  testthat::expect_true(rec$flags$sufficient_n)
  testthat::expect_false(rec$flags$sufficient_years)
  testthat::expect_equal(rec$n_years, 1L)
})

test_that("suggest_kw_regime recommends a dated time series when interannual variability dominates", {
  dates <- make_dates("2015-01-01", 84)
  years <- as.integer(format(dates, "%Y"))
  # A strong step-change trend across years swamps any seasonal cycle.
  kw <- (years - min(years)) * 2 + 1

  rec <- suggest_kw_regime(dates, kw = kw)

  testthat::expect_identical(rec$recommendation, "dated_timeseries")
  testthat::expect_true(rec$flags$interannual_dominant)
  testthat::expect_gte(rec$interannual_r2, 0.30)
})

test_that("suggest_kw_regime recommends a static Kw when there is no seasonal signal", {
  dates <- make_dates("2015-01-01", 84)
  set.seed(2)
  # Flat series with only noise -- no seasonal cycle, no interannual trend.
  kw <- rep(1, length(dates)) + stats::rnorm(length(dates), sd = 0.01)

  rec <- suggest_kw_regime(dates, kw = kw)

  testthat::expect_identical(rec$recommendation, "static")
  testthat::expect_true(rec$flags$sufficient_n)
  testthat::expect_true(rec$flags$sufficient_years)
  testthat::expect_true(rec$flags$sufficient_monthly_resolution)
  testthat::expect_false(rec$flags$interannual_dominant)
  testthat::expect_false(rec$flags$seasonal_signal_present)
})

test_that("suggest_kw_regime recommends a monthly climatology for a recurring seasonal cycle", {
  dates <- make_dates("2015-01-01", 84)
  set.seed(3)
  kw <- 1 + 0.4 * sin(2 * pi * (as.integer(format(dates, "%m")) - 3) / 12) +
    stats::rnorm(length(dates), sd = 0.05)

  rec <- suggest_kw_regime(dates, kw = kw)

  testthat::expect_identical(rec$recommendation, "monthly_climatology")
  testthat::expect_false(rec$flags$interannual_dominant)
  testthat::expect_true(rec$flags$seasonal_signal_present)
  testthat::expect_true(all(c("month", "n", "mean", "sd") %in%
                              names(rec$monthly_climatology)))
  testthat::expect_equal(nrow(rec$monthly_climatology), 12)
})

test_that("suggest_kw_regime accepts secchi depths and converts to Kd", {
  dates <- make_dates("2015-01-01", 84)
  set.seed(3)
  kw <- 1 + 0.4 * sin(2 * pi * (as.integer(format(dates, "%m")) - 3) / 12) +
    stats::rnorm(length(dates), sd = 0.05)
  secchi <- 1.7 / kw

  rec <- suggest_kw_regime(dates, secchi = secchi)

  testthat::expect_identical(rec$input_type, "secchi")
  testthat::expect_equal(rec$secchi_coef, 1.7)
  testthat::expect_equal(rec$data$kw, 1.7 / secchi[order(dates)])
})

test_that("suggest_kw_regime validates its inputs", {
  dates <- make_dates("2020-01-01", 5)
  kw <- c(1, 1.1, 0.9, 1.05, 0.95)
  secchi <- 1.7 / kw

  testthat::expect_error(suggest_kw_regime(dates), "exactly one")
  testthat::expect_error(suggest_kw_regime(dates, kw = kw, secchi = secchi),
                         "exactly one")
  testthat::expect_error(suggest_kw_regime(dates, kw = kw[-1]), "same length")
  testthat::expect_error(suggest_kw_regime(dates, kw = c(-1, kw[-1])),
                         "strictly positive")
  testthat::expect_error(suggest_kw_regime(dates, secchi = c(-1, secchi[-1])),
                         "strictly positive")
  testthat::expect_error(suggest_kw_regime(dates[1:2], kw = kw[1:2]),
                         "at least 3")
})

test_that("print and plot methods work for a kw_regime object", {
  dates <- make_dates("2015-01-01", 84)
  set.seed(3)
  kw <- 1 + 0.4 * sin(2 * pi * (as.integer(format(dates, "%m")) - 3) / 12) +
    stats::rnorm(length(dates), sd = 0.05)
  rec <- suggest_kw_regime(dates, kw = kw)

  testthat::expect_message(print(rec), "Recommendation")

  p <- plot(rec)
  testthat::expect_s3_class(p, "patchwork")
})
