test_that("calc_lake_obs_deriv() does not re-append derived observations", {
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  aeme <- suppressWarnings(yaml_to_aeme(path = aeme_dir, file = "aeme.yaml"))
  a1 <- calc_lake_obs_deriv(aeme)
  a2 <- calc_lake_obs_deriv(a1)
  a3 <- calc_lake_obs_deriv(a2)
  expect_equal(nrow(observations(a2)$lake), nrow(observations(a1)$lake))
  expect_equal(nrow(observations(a3)$lake), nrow(observations(a1)$lake))
  expect_false(anyDuplicated(observations(a3)$lake[, c("Date", "var_aeme",
                                                       "depth")]) > 0)
})
