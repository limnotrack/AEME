test_that("plotting model output works", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme_data <- yaml_to_aeme(path = path, "aeme.yaml")
  mod_ctrls <- read.csv(file.path(path, "model_controls.csv"))
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("dy_cd", "glm_aed", "gotm_wet")
  aeme_data <- build_ensemble(path = path, aeme_data = aeme_data, model = model,
                              mod_ctrls = mod_ctrls, inf_factor = inf_factor,
                              ext_elev = 5, use_bgc = FALSE)

  plake <- function() plot(aeme_data, "lake")
  vdiffr::expect_doppelganger("lake plot", plake)

  poutf <- function() plot(aeme_data, "outflows")
  vdiffr::expect_doppelganger("outflow line plot", poutf)

  pinf <- function() plot(aeme_data, "inflows")
  vdiffr::expect_doppelganger("inflow line plot", pinf)


  aeme_data <- run_aeme(aeme_data = aeme_data, model = model, verbose = FALSE,
                        path = path, mod_ctrls = mod_ctrls, parallel = TRUE)

  p1 <- plot_output(aeme_data = aeme_data, model = model, var_sim = "HYD_temp",
                    level = TRUE, label = TRUE, print_plots = FALSE,
                    var_lims = c(0, 30), ylim = c(0, 16))
  testthat::expect_true(all(ggplot2::is.ggplot(p1[[1]]),
                            ggplot2::is.ggplot(p1[[2]])))

  p2 <- plot_output(aeme_data = aeme_data, model = model, var_sim = "HYD_evap",
                    print_plots = TRUE, ylim = c(0, 0.02))
  testthat::expect_true(ggplot2::is.ggplot(p2))

  p3 <- plot_output(aeme_data = aeme_data, model = model, var_sim = "HYD_wlev",
                    print_plots = TRUE)
  testthat::expect_true(ggplot2::is.ggplot(p2))
})
