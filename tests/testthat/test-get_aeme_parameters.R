test_that("all parameters can be retrieved", {
  param <- get_aeme_parameters()
  testthat::expect_true(nrow(param) == 965)
})

test_that("DYRESM parameters can be retrieved", {
  param <- get_aeme_parameters(model = "dy_cd")
  testthat::expect_true(length(unique(param$model)) == 1)
  testthat::expect_true(nrow(param) == 16)
  param <- get_aeme_parameters(name = "vert_mix_coeff/15")
  
})

test_that("GLM Kw parameter can be retrieved", {
  param <- get_aeme_parameters(par = "Kw")
  testthat::expect_true(nrow(param) == 1)
  testthat::expect_true(param$model == "glm_aed")
})

test_that("GOTM k_min parameter can be retrieved", {
  param <- get_aeme_parameters(par = "k_min")
  testthat::expect_true(nrow(param) == 1)
  testthat::expect_true(param$model == "gotm_wet")
})

test_that("GOTM k_min parameter errors when misspelt", {
  testthat::expect_error(get_aeme_parameters(par = "k__min"))
})

test_that("all GOTM parameters can be retrieved", {
  param <- get_aeme_parameters(model = "gotm_wet")
  testthat::expect_true(length(unique(param$model)) == 1)
})

test_that("all oxygen module parameters can be retrieved", {
  param <- get_aeme_parameters(module = "oxygen")
  testthat::expect_true(length(unique(param$module)) == 1)
  testthat::expect_true(length(unique(param$model)) == 2)
})

test_that("all oxygen module parameters can be retrieved", {
  param <- get_aeme_parameters(model = "glm_aed", module = c("oxygen", "light"))
  testthat::expect_true(length(unique(param$module)) == 2)
})

test_that("multiple GOTM parameters can be retrieved", {
  tgt_names <- paste0(c("g2", "g1"), "/constant_value")
  param1 <- get_aeme_parameters(name = tgt_names)
  param2 <- get_aeme_parameters(par = "k_min")
  param <- rbind(param1, param2)
  testthat::expect_true(length(unique(param$model)) == 1)
})
