test_that("silica inflow is not unit-converted", {
  for (v in c(1, 50, 150, 300)) {
    x <- v * c(0.9, 0.95, 1, 1.05, 1.1)
    out <- AEME:::.convert_inflow_units(data.frame(SIL_rsi = x), verbose = FALSE)
    expect_equal(out$SIL_rsi, x)
  }
})
