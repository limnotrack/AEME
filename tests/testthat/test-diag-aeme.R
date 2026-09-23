test_that("model diagnostics work", {
  aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
  aeme <- readRDS(aeme_file)
  path <- tempdir()
  model_controls <- get_model_controls(use_bgc = TRUE)
  model <- c("glm_aed")
  sim_period <- suggest_sim_period(aeme)
  aeme <- set_sim_period(aeme, sim_period)
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, ext_elev = 5,
                     use_bgc = TRUE) |> 
    run_aeme()
  plot_output(aeme, "vol")
  diag <- diag_aeme(aeme, min_n = 8)
  testthat::expect_true(is(diag, "aeme_diag"))
  
  x <- recommend_calib_plan(diag)
  testthat::expect_true(is.data.frame(out))
})
