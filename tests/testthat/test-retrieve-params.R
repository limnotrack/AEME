test_that("all parameters can be retrieved", {
  param <- retrieve_params()
  testthat::expect_true(nrow(param) == 951)
})

test_that("errors when retrieving parameters for dy_cd", {
  testthat::expect_error(param <- retrieve_params(model = "dy_cd"))
})

test_that("GLM Kw parameter can be retrieved", {
  param <- retrieve_params(par = "Kw")
  testthat::expect_true(nrow(param) == 1)
  testthat::expect_true(param$model == "glm_aed")
})

test_that("GOTM k_min parameter can be retrieved", {
  param <- retrieve_params(par = "k_min")
  testthat::expect_true(nrow(param) == 1)
  testthat::expect_true(param$model == "gotm_wet")
})

test_that("GOTM k_min parameter errors when misspelt", {
  testthat::expect_error(retrieve_params(par = "k__min"))
})

test_that("all GOTM parameters can be retrieved", {
  param <- retrieve_params(model = "gotm_wet")
  testthat::expect_true(length(unique(param$model)) == 1)
})

test_that("all oxygen module parameters can be retrieved", {
  param <- retrieve_params(module = "oxygen")
  testthat::expect_true(length(unique(param$module)) == 1)
  testthat::expect_true(length(unique(param$model)) == 2)
})

test_that("all oxygen module parameters can be retrieved", {
  param <- retrieve_params(model = "glm_aed", module = c("oxygen", "light"))
  testthat::expect_true(length(unique(param$module)) == 2)
})



test_that("multiple GOTM parameters can be retrieved", {
  tgt_names <- paste0(c("g2", "g1"), "/constant_value")
  param1 <- retrieve_params(name = tgt_names)
  param2 <- retrieve_params(par = "k_min")
  param <- rbind(param1, param2)
  testthat::expect_true(length(unique(param$model)) == 1)
})
