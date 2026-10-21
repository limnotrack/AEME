test_that("wbal_input_hash changes with inputs, not with identical ones", {
  a <- wbal_input_hash(x = 1:3, y = "a")
  expect_equal(a, wbal_input_hash(x = 1:3, y = "a"))
  expect_false(identical(a, wbal_input_hash(x = 1:4, y = "a")))
  expect_false(identical(a, wbal_input_hash(x = 1:3, y = "a",
                                            forced_params = c(C = 1))))
})

test_that("wbal_cache_valid needs data and a matching hash", {
  wb <- data.frame(Date = Sys.Date(), value = 1)
  w_bal <- list(data = list(wbal = wb), hash = "k")
  expect_true(wbal_cache_valid(w_bal, "k"))
  expect_false(wbal_cache_valid(w_bal, "other"))
  expect_false(wbal_cache_valid(list(hash = "k"), "k"))
})

test_that("build_aeme reuses the stored water balance unless inputs change", {
  skip_if_no_glm()
  path <- withr::local_tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  aeme <- yaml_to_aeme(path = aeme_dir, file = "aeme.yaml")

  n_calls <- 0
  real <- calc_water_balance
  local_mocked_bindings(calc_water_balance = function(...) {
    n_calls <<- n_calls + 1
    real(...)
  })
  build <- function(a, ...) {
    build_aeme(aeme = a, model = "glm_aed", path = path, ext_elev = 5,
               use_bgc = FALSE, ...)
  }

  a1 <- build(aeme)
  expect_equal(n_calls, 1)
  expect_type(water_balance(a1)$hash, "character")

  # identical inputs -> reused, identical result
  a2 <- build(a1)
  expect_equal(n_calls, 1)
  expect_equal(water_balance(a2)$data$wbal, water_balance(a1)$data$wbal)

  # forced refresh recalculates
  a3 <- build(a1, refresh_wbal = TRUE)
  expect_equal(n_calls, 2)

  # changed meteorology -> recalculated with a new hash
  inp <- input(a1)
  inp$meteo$MET_tmpair <- inp$meteo$MET_tmpair + 1
  input(a1) <- inp
  a4 <- build(a1)
  expect_equal(n_calls, 3)
  expect_false(identical(water_balance(a4)$hash, water_balance(a3)$hash))
})
