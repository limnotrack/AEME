subdaily_met <- function(rain) {
  data.frame(
    Date = seq(as.POSIXct("2020-01-01 00:00:00", tz = "UTC"),
               by = "1 hour", length.out = length(rain)),
    MET_radswd = 100, MET_tmpair = 15, MET_wndspd = 3, MET_pprain = rain
  )
}

test_that("sub-daily precip is rescaled to a mm/day rate exactly once", {
  rain <- c(0, 0.1, 0.2, 0, 0.05, rep(0, 19))
  once <- standardise_met(subdaily_met(rain), verbose = FALSE)
  expect_equal(as.numeric(once$MET_pprain), rain * 24)

  # build_aeme() re-reads already-standardised met on a rebuild
  twice <- standardise_met(once, verbose = FALSE)
  thrice <- standardise_met(twice, verbose = FALSE)
  expect_equal(as.numeric(twice$MET_pprain), rain * 24)
  expect_equal(as.numeric(thrice$MET_pprain), rain * 24)
})

test_that("small sub-daily precip is not mistaken for m/day", {
  rain <- c(rep(0, 12), 0.3, rep(0, 11))
  out <- standardise_met(subdaily_met(rain), verbose = FALSE)
  expect_equal(as.numeric(out$MET_pprain), rain * 24)
})

test_that("sub-daily rain supplied in metres triggers a magnitude warning", {
  n <- 24 * 60
  set.seed(1)
  rain_mm <- ifelse(runif(n) < 0.1, 0.5, 0)  # ~1200 mm/yr in mm/hr
  expect_no_warning(
    standardise_met(subdaily_met(rain_mm), verbose = FALSE)
  )
  expect_warning(
    standardise_met(subdaily_met(rain_mm / 1000), verbose = FALSE),
    class = "aeme_warn_met_precip_low"
  )
})
