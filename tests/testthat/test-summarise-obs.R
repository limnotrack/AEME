test_that("summarise_obs reports counts, dates and profile structure", {
  aeme <- readRDS(system.file("extdata/aeme.rds", package = "AEME"))

  s <- summarise_obs(aeme)

  testthat::expect_s3_class(s, "aeme_obs_summary")
  testthat::expect_true(all(c("variables", "years", "forcing", "window") %in%
                              names(s)))
  testthat::expect_gt(nrow(s$variables), 0)

  v <- s$variables
  testthat::expect_true(all(c("var_aeme", "n_obs", "n_dates", "first", "last",
                              "n_profiles", "profile_frac", "kind") %in%
                              names(v)))
  # Every classification is one of the three documented kinds.
  testthat::expect_true(all(v$kind %in% c("profile", "discrete", "scalar")))
  # A visit cannot be counted more often than it happened.
  testthat::expect_true(all(v$n_profiles <= v$n_dates))
  testthat::expect_true(all(v$n_dates <= v$n_obs))
  testthat::expect_true(all(v$first <= v$last))
  # Scalar series carry no depths by definition.
  testthat::expect_true(all(is.na(v$depth_min[v$kind == "scalar"])))
  testthat::expect_equal(sum(v$n_profiles[v$kind == "scalar"]), 0)

  # Per-year counts must reconcile with the per-variable totals.
  agg <- stats::aggregate(cbind(n_obs, n_dates) ~ var_aeme, data = s$years,
                          FUN = sum)
  m <- merge(agg, v[, c("var_aeme", "n_obs", "n_dates")], by = "var_aeme",
             suffixes = c("_year", "_var"))
  testthat::expect_equal(m$n_obs_year, m$n_obs_var)
  testthat::expect_equal(m$n_dates_year, m$n_dates_var)
})

test_that("summarise_obs filters to vars_sim and warns on absent variables", {
  aeme <- readRDS(system.file("extdata/aeme.rds", package = "AEME"))

  s <- summarise_obs(aeme, vars_sim = "HYD_temp")
  testthat::expect_identical(s$variables$var_aeme, "HYD_temp")

  testthat::expect_warning(
    summarise_obs(aeme, vars_sim = c("HYD_temp", "NOT_A_VAR")),
    "NOT_A_VAR"
  )
  # Warns about the absent variable first, then aborts because nothing is left.
  testthat::expect_error(
    suppressWarnings(summarise_obs(aeme, vars_sim = "NOT_A_VAR"))
  )
})

test_that("min_depths changes what counts as a profile", {
  aeme <- readRDS(system.file("extdata/aeme.rds", package = "AEME"))

  lo <- summarise_obs(aeme, min_depths = 2L)
  hi <- summarise_obs(aeme, min_depths = 20L)

  lo_n <- lo$variables$n_profiles[match(hi$variables$var_aeme,
                                        lo$variables$var_aeme)]
  # Demanding more depths can never find more profiles.
  testthat::expect_true(all(hi$variables$n_profiles <= lo_n))
})

test_that("suggest_sim_period respects the forcing window and spin-up", {
  aeme <- readRDS(system.file("extdata/aeme.rds", package = "AEME"))

  p <- suggest_sim_period(aeme, vars_sim = "HYD_temp", spin_up = 0)

  testthat::expect_s3_class(p, "aeme_sim_period")
  testthat::expect_true(p$start < p$stop)
  # The period, and its spin-up, must sit inside the runnable window.
  testthat::expect_gte(p$spin_up_start, p$window[["start"]])
  testthat::expect_lte(p$stop, p$window[["stop"]])

  # A longer spin-up eats into the front of the record, so the start can only
  # move later (or stay put).
  p_long <- suggest_sim_period(aeme, vars_sim = "HYD_temp", spin_up = 365)
  testthat::expect_gte(p_long$spin_up_start, p_long$window[["start"]])
  testthat::expect_gte(as.numeric(p_long$start), as.numeric(p$start) - 1)
})

test_that("suggest_sim_period errors when spin-up leaves no room", {
  aeme <- readRDS(system.file("extdata/aeme.rds", package = "AEME"))

  testthat::expect_error(
    suggest_sim_period(aeme, vars_sim = "HYD_temp", spin_up = 1e6),
    "no runnable period"
  )
})

test_that("align = 'year' snaps to the hydrological year", {
  aeme <- readRDS(system.file("extdata/aeme.rds", package = "AEME"))

  p <- suggest_sim_period(aeme, vars_sim = "HYD_temp", spin_up = 0,
                          align = "year", year_start_month = 7L)

  # Snapped ends land on 1 July / 30 June unless clamped by the window.
  if (p$start > p$window[["start"]]) {
    testthat::expect_equal(format(p$start, "%m-%d"), "07-01")
  }
  if (p$stop < p$window[["stop"]]) {
    testthat::expect_equal(format(p$stop, "%m-%d"), "06-30")
  }
})

test_that("set_sim_period writes the suggested period onto the object", {
  aeme <- readRDS(system.file("extdata/aeme.rds", package = "AEME"))

  p <- suggest_sim_period(aeme, vars_sim = "HYD_temp", spin_up = 30)
  out <- set_sim_period(aeme, p)

  tme <- AEME::time(out)
  testthat::expect_equal(as.Date(tme$start), p$start)
  testthat::expect_equal(as.Date(tme$stop), p$stop)

  testthat::expect_error(set_sim_period(aeme, "not a period"),
                         "suggest_sim_period")
})
