test_that("plotting model met tile", {
  aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
  aeme <- readRDS(aeme_file)
  p1 <- plot_met_tile(aeme = aeme)
  testthat::expect_true(ggplot2::is.ggplot(p1))

  p2 <- plot_met_tile(aeme = aeme, var_inp = c("MET_tmpair", "MET_pprain"))
  testthat::expect_true(ggplot2::is.ggplot(p2))
  p3 <- plot_met_tile(aeme = aeme, var_inp = c("MET_pprain"),
                      use_hydro_year = FALSE)
  testthat::expect_true(ggplot2::is.ggplot(p3))

})

test_that("plotting hypsograph", {
  aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
  aeme <- readRDS(aeme_file)
  p1 <- plot_hyps(aeme = aeme)
  testthat::expect_true(ggplot2::is.ggplot(p1))
  p2 <- plot_hyps(aeme = aeme, y = "depth", add_surface = TRUE)
  testthat::expect_true(ggplot2::is.ggplot(p1))
  testthat::expect_error({
    p3 <- plot_hyps(aeme = aeme, y = "elevation", add_surface = TRUE)
  })
  p4 <- plot_hyps(aeme = aeme, y = "depth", add_surface = TRUE,
                  incl_ext_elev = TRUE)
  testthat::expect_true(ggplot2::is.ggplot(p4))
})

test_that("plotting observations", {
  aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
  aeme <- readRDS(aeme_file)
  p1 <- plot_obs(aeme = aeme, var_sim = c("HYD_temp", "LKE_lvlwtr"))
  testthat::expect_true(ggplot2::is.ggplot(p1))
  p2 <- plot_obs(aeme = aeme, var_sim = "LKE_lvlwtr", add_line = TRUE)
  testthat::expect_true(ggplot2::is.ggplot(p2))
  testthat::expect_error({
    p3 <- plot_obs(aeme = aeme, var_sim = "HYD_thmcln")
  })
})

test_that("plotting inflows and outflows", {
  aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
  aeme <- readRDS(aeme_file)
  p1 <- plot_flows(aeme = aeme)
  testthat::expect_true(ggplot2::is.ggplot(p1))
  p2 <- plot_flows(aeme = aeme, flow = "inflow", var_sim = "HYD_temp")
  testthat::expect_true(ggplot2::is.ggplot(p2))
  testthat::expect_error({
    p3 <- plot_flows(aeme = aeme, flow = "outflow", var_sim = "HYD_temp")
  })
})

test_that("plotting model output works", {
  aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
  aeme <- readRDS(aeme_file)
  path <- tempdir()
  model_controls <- get_model_controls()
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("dy_cd", "glm_aed", "gotm_wet")
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, inf_factor = inf_factor,
                     ext_elev = 5, use_bgc = FALSE)

  testthat::expect_error({
    p1 <- plot_output(aeme = aeme, model = model,
                      var_sim = "HYD_temp", level = TRUE,
                      print_plots = FALSE, var_lims = c(0, 30), ylim = c(0, 16))
  })

  plake <- plot(aeme, "lake")
  testthat::expect_true(ggplot2::is.ggplot(plake))

  pinput <- plot(aeme, "input")
  testthat::expect_true(ggplot2::is.ggplot(plake))


  poutf <- plot(aeme, "outflows")
  testthat::expect_true(ggplot2::is.ggplot(poutf))

  pinf <- plot(aeme, "inflows")
  testthat::expect_true(ggplot2::is.ggplot(pinf))

  pwbal <- plot(aeme, "water_balance")
  testthat::expect_true(ggplot2::is.ggplot(pwbal))

  # Run models
  aeme <- run_aeme(aeme = aeme, model = model, verbose = FALSE,
                   path = path, model_controls = model_controls,
                   parallel = TRUE, ncores = 2L)


  p1 <- plot(aeme, "output")
  testthat::expect_true(ggplot2::is.ggplot(p1))

  p1 <- plot_output(aeme = aeme, model = model, var_sim = "HYD_temp",
                    level = TRUE, print_plots = FALSE,
                    var_lims = c(0, 30), ylim = c(0, 16), facet = FALSE)
  testthat::expect_true(is.list(p1))
  testthat::expect_true(all(c(ggplot2::is.ggplot(p1[[1]]),
                              ggplot2::is.ggplot(p1[[2]]),
                              ggplot2::is.ggplot(p1[[3]]))))

  p2 <- plot_output(aeme = aeme, model = model, var_sim = "LKE_evpflx",
                    print_plots = FALSE, cumulative = TRUE, facet = FALSE)
  testthat::expect_true(ggplot2::is.ggplot(p2))

  p3 <- plot_output(aeme = aeme, model = model, var_sim = "LKE_lvlwtr",
                    facet = FALSE)
  testthat::expect_true(ggplot2::is.ggplot(p3))
})

test_that("plotting model summary output works", {
  aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
  aeme <- readRDS(aeme_file)
  obs_vars <- get_obs_vars(aeme)
  testthat::expect_true(is.vector(obs_vars))
  path <- tempdir()
  model_controls <- get_model_controls(use_bgc = TRUE)
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("glm_aed", "gotm_wet")
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, inf_factor = inf_factor,
                     ext_elev = 5, use_bgc = TRUE)

  # Run models
  aeme <- run_aeme(aeme = aeme, model = model, verbose = FALSE,
                   path = path, model_controls = model_controls,
                   parallel = TRUE, ncores = 2L)

  get_output_vars(aeme, model)
  aeme_summ <- summary(aeme)

  outp <- output(aeme_summ)
  testthat::expect_true(is.data.frame(outp$seasonal_profiles))
  testthat::expect_true(is.data.frame(outp$model_obs_df))
  p1 <- plot_output(aeme = aeme_summ, model = model, var_sim = "HYD_temp")
  testthat::expect_true(ggplot2::is.ggplot(p1))
  p2 <- plot_output(aeme = aeme_summ, model = model, var_sim = "HYD_thmcln")
  testthat::expect_true(ggplot2::is.ggplot(p2))
})


test_that("plotting model output works with no lake observations", {
  aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
  aeme <- readRDS(aeme_file)
  path <- tempdir()
  model_controls <- get_model_controls()
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("glm_aed", "gotm_wet")

  # Remove observations
  obs <- observations(aeme)
  obs$lake <- NULL
  observations(aeme) <- obs

  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, inf_factor = inf_factor,
                     ext_elev = 5, use_bgc = FALSE)


  # Run models
  aeme <- run_aeme(aeme = aeme, model = model, verbose = FALSE,
                   path = path, model_controls = model_controls, parallel = FALSE,
                   ncores = 2L)

  p1 <- plot_output(aeme = aeme, model = model, var_sim = "HYD_temp",
                    level = TRUE, print_plots = FALSE,
                    var_lims = c(0, 30), ylim = c(0, 16))
  testthat::expect_true(ggplot2::is.ggplot(p1))
})

test_that("plotting model output works with no lake & level observations", {
  aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
  aeme <- readRDS(aeme_file)
  path <- tempdir()
  model_controls <- get_model_controls()
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("glm_aed", "gotm_wet")

  # Remove observations
  obs <- observations(aeme)
  obs$lake <- NULL
  obs$level <- NULL
  observations(aeme) <- obs

  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, inf_factor = inf_factor,
                     ext_elev = 5, use_bgc = FALSE)


  # Run models
  aeme <- run_aeme(aeme = aeme, model = model, verbose = FALSE,
                   path = path, model_controls = model_controls,
                   parallel = TRUE, ncores = 2L)

  p1 <- plot_output(aeme = aeme, model = model, var_sim = "HYD_temp",
                    level = TRUE, print_plots = FALSE,
                    var_lims = c(0, 30), ylim = c(0, 16))
  testthat::expect_true(ggplot2::is.ggplot(p1))
})

test_that("plotting model residuals for 2d and 1d variables", {
  aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
  aeme <- readRDS(aeme_file)
  path <- tempdir()
  model_controls <- get_model_controls()
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("glm_aed", "gotm_wet")

  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, inf_factor = inf_factor,
                     ext_elev = 5, use_bgc = FALSE)
  # Run models
  aeme <- run_aeme(aeme = aeme, model = model, verbose = FALSE,
                   path = path, model_controls = model_controls,
                   parallel = TRUE, ncores = 2L)

  var_sim <- c("HYD_temp", "HYD_thmcln")
  p1 <- plot_resid(aeme = aeme, model = model, var_sim = "HYD_temp")
  testthat::expect_true(ggplot2::is.ggplot(p1$HYD_temp))

  p2 <- plot_resid(aeme = aeme, model = model, var_sim = "HYD_thmcln")
  testthat::expect_true(ggplot2::is.ggplot(p2$HYD_thmcln))
})

test_that("plotting phytoplankton model output works", {
  aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
  aeme <- readRDS(aeme_file)
  path <- tempdir()
  model_controls <- get_model_controls(use_bgc = TRUE)
  model_controls <- model_controls |>
    dplyr::mutate(simulate = dplyr::case_when(
      var_aeme == "ZOO_zoo1" ~ TRUE,
      .default = simulate
    ))
  model <- c("glm_aed", "gotm_wet")
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls,
                     ext_elev = 5, use_bgc = TRUE)

  # Run models
  aeme <- run_aeme(aeme = aeme, model = model, verbose = FALSE,
                   path = path, model_controls = model_controls,
                   parallel = TRUE, ncores = 2L)

  p1 <- plot_ts(aeme = aeme, model = model, var_sim = "HYD_temp")
  testthat::expect_true(ggplot2::is.ggplot(p1))

  p2 <- plot_phytos(aeme = aeme, model = model)
  testthat::expect_true(ggplot2::is.ggplot(p2))

  p3 <- plot_nit(aeme = aeme, model = model)
  testthat::expect_true(ggplot2::is.ggplot(p3))

  p4 <- plot_phs(aeme = aeme, model = model)
  testthat::expect_true(ggplot2::is.ggplot(p4))

  p5 <- plot_zoops(aeme = aeme, model = model)
  testthat::expect_true(ggplot2::is.ggplot(p5))
})
