test_that("plotting model met tile", {
  aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
  aeme <- readRDS(aeme_file)
  p1 <- plot_met_tile(aeme = aeme)
  testthat::expect_true(ggplot2::is_ggplot(p1))

  p2 <- plot_met_tile(aeme = aeme, var_inp = c("MET_tmpair", "MET_pprain"))
  testthat::expect_true(ggplot2::is_ggplot(p2))
  p3 <- plot_met_tile(aeme = aeme, var_inp = c("MET_pprain"),
                      use_hydro_year = FALSE)
  testthat::expect_true(ggplot2::is_ggplot(p3))

})

test_that("plotting hypsograph", {
  aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
  aeme <- readRDS(aeme_file)
  p1 <- plot_hyps(aeme = aeme)
  testthat::expect_true(ggplot2::is_ggplot(p1))
  p2 <- plot_hyps(aeme = aeme, y = "depth", add_surface = TRUE)
  testthat::expect_true(ggplot2::is_ggplot(p1))
  testthat::expect_error({
    p3 <- plot_hyps(aeme = aeme, y = "elevation", add_surface = TRUE)
  })
  p4 <- plot_hyps(aeme = aeme, y = "depth", add_surface = TRUE,
                  incl_ext_elev = TRUE)
  testthat::expect_true(ggplot2::is_ggplot(p4))
})

test_that("plotting observations", {
  aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
  aeme <- readRDS(aeme_file)
  p1 <- plot_obs(aeme = aeme, var_sim = c("HYD_temp", "LKE_lvlwtr"))
  testthat::expect_true(ggplot2::is_ggplot(p1))
  p2 <- plot_obs(aeme = aeme, var_sim = "LKE_lvlwtr", add_line = TRUE)
  testthat::expect_true(ggplot2::is_ggplot(p2))
  testthat::expect_error({
    p3 <- plot_obs(aeme = aeme, var_sim = "HYD_thmcln")
  })
})

test_that("plotting inflows and outflows", {
  aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
  aeme <- readRDS(aeme_file)
  p1 <- plot_flows(aeme = aeme)
  testthat::expect_true(ggplot2::is_ggplot(p1))
  p2 <- plot_flows(aeme = aeme, flow = "inflow", var_sim = "temp")
  testthat::expect_true(ggplot2::is_ggplot(p2))
  testthat::expect_error({
    p3 <- plot_flows(aeme = aeme, flow = "outflow", var_sim = "HYD_temp")
  })
  
  path <- tempdir()
  model_controls <- get_model_controls()
  model <- c("glm_aed")
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, ext_elev = 5) |> 
    run_aeme()
  
  p1 <- plot_flows(aeme = aeme)
  testthat::expect_true(ggplot2::is_ggplot(p1))
  p2 <- plot_flows(aeme = aeme, flow = "inflow", var_sim = "temp")
  testthat::expect_true(ggplot2::is_ggplot(p2))
  testthat::expect_error({
    p3 <- plot_flows(aeme = aeme, flow = "outflow", var_sim = "HYD_temp")
  })
})

test_that("plotting model output works", {
  aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
  aeme <- readRDS(aeme_file)
  path <- tempdir()
  model_controls <- get_model_controls(use_bgc = TRUE)
  model <- c("glm_aed", "gotm_wet")
  sys_OS <- AEME:::get_os()
  if (sys_OS == "osx") {
    model <- "glm_aed"
  }
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls,  ext_elev = 5, 
                     use_bgc = TRUE)

  testthat::expect_error({
    p1 <- plot_output(aeme = aeme, model = model,
                      var_sim = "HYD_temp", level = TRUE,
                      print_plots = FALSE, var_lims = c(0, 30), ylim = c(0, 16))
  })

  plake <- plot(aeme, "lake")
  testthat::expect_true(ggplot2::is_ggplot(plake))

  pinput <- plot(aeme, "input")
  testthat::expect_true(ggplot2::is_ggplot(plake))


  poutf <- plot(aeme, "outflows")
  testthat::expect_true(ggplot2::is_ggplot(poutf))

  pinf <- plot(aeme, "inflows")
  testthat::expect_true(ggplot2::is_ggplot(pinf))

  pwbal <- plot(aeme, "water_balance")
  testthat::expect_true(ggplot2::is_ggplot(pwbal))

  # Run models
  aeme <- run_aeme(aeme = aeme, model = model, verbose = FALSE,
                   path = path, model_controls = model_controls,
                   parallel = TRUE, ncores = 2L)


  p1 <- plot(aeme, "output")
  testthat::expect_true(ggplot2::is_ggplot(p1))

  p0 <- plot_output(aeme = aeme, var_sim = "temp")
  p0 <- plot_output(aeme = aeme, var_sim = "oxy")
  
  p1 <- plot_output(aeme = aeme, model = model, var_sim = "HYD_temp",
                    level = TRUE, print_plots = FALSE, point_size = 1,
                    var_lims = c(0, 30), ylim = c(0, 16), facet = FALSE)
  
  plot_output(aeme = aeme, model = model, var_sim = "HYD_temp")
  plot_output(aeme = aeme, model = model, var_sim = "HYD_dens")
  plot_output(aeme = aeme, model = model, var_sim = "CHM_oxy", var_lims = c(0, 14))
  plot_output(aeme = aeme, model = model, var_sim = "PHY_tchla")
  plot_output(aeme = aeme, model = model, var_sim = "PHY_cyano")
  plot_output(aeme = aeme, model = model, var_sim = "PHY_green")
  plot_output(aeme = aeme, model = model, var_sim = "NIT_tn")
  plot_output(aeme = aeme, model = model, var_sim = "PHS_tp")
  testthat::expect_true(is.list(p1))
  testthat::expect_true(all(c(ggplot2::is_ggplot(p1[[1]]),
                              ggplot2::is_ggplot(p1[[2]]))))

  p2 <- plot_output(aeme = aeme, model = model, var_sim = "LKE_evpflx",
                    print_plots = FALSE, cumulative = TRUE, facet = FALSE)
  testthat::expect_true(ggplot2::is_ggplot(p2))

  p3 <- plot_output(aeme = aeme, model = model, var_sim = "LKE_lvlwtr",
                    facet = FALSE)
  testthat::expect_true(ggplot2::is_ggplot(p3))
  p4 <- plot_wlev(aeme = aeme, model = model)
  testthat::expect_true(ggplot2::is_ggplot(p4))

  df <- get_var(aeme = aeme, model = model, var_sim = "HYD_temp")
  testthat::expect_true(is.data.frame(df))
  plist <- plot_var(df = df, facet = FALSE)
  testthat::expect_true(ggplot2::is_ggplot(plist[[1]]))
  p4 <- plot_var(df = df, facet = TRUE)
  testthat::expect_true(ggplot2::is_ggplot(p4))
})

test_that("plotting model summary output works", {
  aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
  aeme <- readRDS(aeme_file)
  obs_vars <- list_obs_vars(aeme)
  testthat::expect_true(is.vector(obs_vars))
  path <- tempdir()
  model_controls <- get_model_controls(use_bgc = TRUE)
  model_controls <- set_vars_sim(model_controls, c("HYD_thmcln"))
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("glm_aed", "gotm_wet")
  sys_OS <- AEME:::get_os()
  if (sys_OS == "osx") {
    model <- "glm_aed"
  }
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
  testthat::expect_true(ggplot2::is_ggplot(p1))
  p2 <- plot_output(aeme = aeme_summ, model = model, var_sim = "HYD_thmcln")
  testthat::expect_true(ggplot2::is_ggplot(p2))
})


test_that("plotting model output works with no lake observations", {
  aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
  aeme <- readRDS(aeme_file)
  path <- tempdir()
  model_controls <- get_model_controls()
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("glm_aed", "gotm_wet")
  sys_OS <- AEME:::get_os()
  if (sys_OS == "osx") {
    model <- "glm_aed"
  }

  # Remove observations
  obs <- observations(aeme)
  obs$lake <- NULL
  observations(aeme) <- obs

  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, inf_factor = inf_factor,
                     ext_elev = 5, use_bgc = FALSE)


  # Run models
  aeme <- run_aeme(aeme = aeme, model = model, verbose = FALSE,
                   path = path, model_controls = model_controls,
                   parallel = FALSE, ncores = 2L)

  p1 <- plot_output(aeme = aeme, model = model, var_sim = "HYD_temp",
                    level = TRUE, print_plots = FALSE,
                    var_lims = c(0, 30), ylim = c(0, 16))
  testthat::expect_true(ggplot2::is_ggplot(p1))
})

test_that("plotting model output works with no lake & level observations", {
  aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
  aeme <- readRDS(aeme_file)
  path <- tempdir()
  model_controls <- get_model_controls()
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("glm_aed", "gotm_wet")
  sys_OS <- AEME:::get_os()
  if (sys_OS == "osx") {
    model <- "glm_aed"
  }

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
                   parallel = F, ncores = 2L)

  p1 <- plot_output(aeme = aeme, model = model, var_sim = "HYD_temp",
                    level = TRUE, print_plots = FALSE,
                    var_lims = c(0, 30), ylim = c(0, 16))
  testthat::expect_true(ggplot2::is_ggplot(p1))
  
  p2 <- plot_wbal(aeme = aeme)
  testthat::expect_true(ggplot2::is_ggplot(p2))
  
  p3 <- plot_wbal_annual(aeme = aeme)
  testthat::expect_true(ggplot2::is_ggplot(p3))
})

test_that("plotting model residuals for 2d and 1d variables", {
  aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
  aeme <- readRDS(aeme_file)
  path <- tempdir()
  model_controls <- get_model_controls()
  var_sim <- c("HYD_temp", "HYD_thmcln")
  model_controls <- set_vars_sim(model_controls, var_sim)
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("glm_aed", "gotm_wet")
  sys_OS <- AEME:::get_os()
  if (sys_OS == "osx") {
    model <- "glm_aed"
  }

  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, inf_factor = inf_factor,
                     ext_elev = 5, use_bgc = FALSE)
  # Run models
  aeme <- run_aeme(aeme = aeme, model = model, verbose = FALSE,
                   path = path, model_controls = model_controls,
                   parallel = TRUE, ncores = 2L)

  p1 <- plot_resid(aeme = aeme, model = model, var_sim = "HYD_temp")
  testthat::expect_true(ggplot2::is_ggplot(p1))

  p2 <- plot_resid(aeme = aeme, model = model, var_sim = "HYD_thmcln")
  testthat::expect_true(ggplot2::is_ggplot(p2))
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
  sys_OS <- AEME:::get_os()
  if (sys_OS == "osx") {
    model <- "glm_aed"
  }
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls,
                     ext_elev = 5, use_bgc = TRUE)

  # Run models
  aeme <- run_aeme(aeme = aeme, model = model, verbose = FALSE,
                   path = path, model_controls = model_controls,
                   parallel = TRUE, ncores = 2L)

  p1 <- plot_ts(aeme = aeme, model = model, var_sim = "HYD_temp")
  testthat::expect_true(ggplot2::is_ggplot(p1))
  p1 <- plot_ts(aeme = aeme, model = model, var_sim = "HYD_temp", 
                depth_range = c(0, 1))
  testthat::expect_true(ggplot2::is_ggplot(p1))
  

  p2 <- plot_phytos(aeme = aeme, model = model)
  testthat::expect_true(ggplot2::is_ggplot(p2))

  p3 <- plot_nit(aeme = aeme, model = model)
  testthat::expect_true(ggplot2::is_ggplot(p3))

  p4 <- plot_phs(aeme = aeme, model = model)
  testthat::expect_true(ggplot2::is_ggplot(p4))

  p5 <- plot_zoops(aeme = aeme, model = model)
  testthat::expect_true(ggplot2::is_ggplot(p5))
})

test_that("plotting water balance components", {
  aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
  aeme <- readRDS(aeme_file)
  aeme <- aeme |> 
    set_time(stop = "2022-06-30")
  path <- tempdir()
  model_controls <- get_model_controls(use_bgc = TRUE)
  model_controls <- model_controls |>
    dplyr::mutate(simulate = dplyr::case_when(
      var_aeme == "ZOO_zoo1" ~ TRUE,
      .default = simulate
    ))
  model <- c("glm_aed")
  sys_OS <- AEME:::get_os()
  if (sys_OS == "osx") {
    model <- "glm_aed"
  }
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls,
                     ext_elev = 5, use_bgc = FALSE)
  
  wbal <- get_wbal_components(aeme = aeme)
  testthat::expect_true(is.list(wbal))
  p <- plot_wbal_comp(aeme = aeme)
  testthat::expect_true(ggplot2::is_ggplot(p))
  
  p1 <- plot_est_wbal(aeme = aeme, model = model, time_axis = "month")
  testthat::expect_true(ggplot2::is_ggplot(p1))
  
  p2 <- plot_weir_calibration(aeme = aeme)
  testthat::expect_true(ggplot2::is_ggplot(p2))
  
  # Run models
  aeme <- run_aeme(aeme = aeme, model = model, path = path,
                   parallel = TRUE, ncores = 2L)
  
  wbal <- get_wbal_components(aeme = aeme)
  testthat::expect_true(is.list(wbal))
  p <- plot_wbal_comp(aeme = aeme)
  testthat::expect_true(ggplot2::is_ggplot(p))
  
  p <- plot_wbal_summaries(aeme = aeme)
  testthat::expect_true(ggplot2::is_ggplot(p))
  
  # remove inflows & outflows
  aeme <- aeme |> 
    remove_inflow(all = TRUE) |> 
    remove_outflow(all = TRUE)
  
  p1 <- plot_est_wbal(aeme = aeme, model = model, time_axis = "month")
  testthat::expect_true(ggplot2::is_ggplot(p1))
  
  p2 <- plot_weir_calibration(aeme = aeme)
  testthat::expect_true(ggplot2::is_ggplot(p2))
  
  
})
