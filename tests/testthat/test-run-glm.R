test_that("running GLM works", {
  aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
  aeme <- readRDS(aeme_file)
  path <- tempdir()
  model_controls <- get_model_controls()
  model <- c("glm_aed")
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, ext_elev = 5,
                     use_bgc = FALSE)

  obs <- get_obs(aeme)
  mod_obs_vars <- get_mod_obs_vars(aeme)
  testthat::expect_true(all(mod_obs_vars$var_aeme %in% obs$var_aeme))
  aeme <- run_aeme(aeme = aeme, model = model, verbose = TRUE, path = path)
  # plot_wlev(aeme)
  # plot_wbal(aeme)
  lake_dir <- get_lake_dir(aeme = aeme)
  outfile <- get_model_outfile(aeme = aeme)
  outfile2 <- get_model_outfile(lake_dir = lake_dir, model = model)
  testthat::expect_equal(outfile, outfile2)

  wlev <- read_model_wlev(lake_dir = lake_dir, model = model)
  testthat::expect_true(is.data.frame(wlev))

  vars_sim <- get_vars_sim(aeme = aeme)

  # Read GLM output using ncdf4
  nc <- ncdf4::nc_open(outfile$glm_aed)
  wlev2 <- read_model_wlev(nc = nc, model = model)
  testthat::expect_true(is.data.frame(wlev2))
  testthat::expect_equal(nrow(wlev), nrow(wlev2))

  outp1 <- read_model_outputs(nc = nc, lake_dir = lake_dir, model = model,
                              vars_sim = vars_sim)
  testthat::expect_true(is.list(outp1))
  testthat::expect_true(nrow(outp1$HYD_temp) == 42)
  testthat::expect_true(length(outp1) >= length(vars_sim))

  outp2 <- read_model_outputs(nc = nc, lake_dir = lake_dir, model = model,
                              vars_sim = "HYD_temp", incl_fluxes = FALSE)
  testthat::expect_true(is.list(outp2))
  testthat::expect_true(nrow(outp2$HYD_temp) == 42)
  testthat::expect_true(length(outp2) == 6)

  ncdf4::nc_close(nc)


  out <- read_glm_output(file = outfile$glm_aed, vars_sim = vars_sim)
  testthat::expect_true(nrow(out$HYD_temp) > 2)

  var_indices <- get_var_indices(aeme = aeme, model = model, path = path,
                                 use_obs = TRUE, vars_sim = vars_sim)

  out <- read_glm_output(file = outfile$glm_aed, vars_sim = vars_sim,
                         depths = var_indices$HYD_temp$depths, incl_fluxes = FALSE,
                         date_index = var_indices$HYD_temp$date_index)

  out2 <- read_glm_output(file = outfile$glm_aed, vars_sim = "HYD_temp",
                          depths = c(0, 11), incl_fluxes = FALSE)
  testthat::expect_true(nrow(out2$HYD_temp) == 2)
  testthat::expect_true(all(out2$HYD_temp[1, ] >= out2$HYD_temp[2, ]))
  out3 <- read_glm_output(file = outfile$glm_aed, vars_sim = "HYD_temp",
                          depths = c(0, 11), incl_fluxes = FALSE,
                          dates = c("2020-09-01", "2020-12-02"))
  testthat::expect_true(ncol(out3$HYD_temp) == 2)

  # plot_output(aeme)
  outp <- output(aeme)
  file_chk <- file.exists(file.path(lake_dir,
                                    model, "output", "output.nc"))
  testthat::expect_true(file_chk)

  v <- get_var(aeme = aeme, var_sim = "HYD_temp", depth = 0)
  v2 <- get_var(aeme = aeme, var_sim = "HYD_temp", depth = 0,
                depth_ref = "bottom")
  lake_level <- get_var(aeme = aeme, var_sim = "LKE_lvlwtr")
  max_depth <- max(lake_level$value, na.rm = TRUE)
  testthat::expect_true(is.data.frame(v))
  testthat::expect_true(is.data.frame(v2))
  # testthat::expect_error({
  #   get_var(aeme = aeme, model = model, var_sim = "HYD_temp",
  #           depth = max_depth + 1)
  # })
})

test_that("running GLM with different exec works", {
  # Skip if not on Windows
  if (.Platform$OS.type != "windows") {
    testthat::skip("Skipping test on non-Windows OS")
  }
  aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
  aeme <- readRDS(aeme_file)
  path <- tempdir()
  model_controls <- get_model_controls()
  model <- c("glm_aed")

  path <- tempdir()  # or wherever you want to save

  glm_exec_url <- "https://github.com/AquaticEcoDynamics/Binaries/raw/master/windows/glm_3.9.016.zip"

  download.file(
    glm_exec_url,
    destfile = file.path(path, "glm_3.9.016.zip"),
    mode = "wb"
  )
  unzip(file.path(path, "glm_3.9.016.zip"), exdir = file.path(path, "glm_exec"))
  glm_exec <- file.path(path, "glm_exec", "glm_3.9.016", "glm.exe")
  testthat::expect_true(file.exists(glm_exec))
  options("AEME.glm_exec" = glm_exec)

  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, ext_elev = 5,
                     use_bgc = FALSE)
  aeme <- run_aeme(aeme = aeme, model = model, verbose = FALSE, path = path)

  glm_ver <- get_model_version(model = model)
  testthat::expect_true(any(grepl("3.9.016", glm_ver)))
  # plot_output(aeme, model = model)
  outp <- output(aeme)
  lake_dir <- get_lake_dir(aeme = aeme, path = path)
  file_chk <- file.exists(file.path(lake_dir,
                                    model, "output", "output.nc"))
  testthat::expect_true(file_chk)
  options("AEME.glm_exec" = NULL)

})

test_that("run GLM models with old object", {
  aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
  aeme <- readRDS(aeme_file)
  path <- tempdir()
  model_controls <- get_model_controls(use_bgc = TRUE)
  model <- c("glm_aed")
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, ext_elev = 5) |>
    run_aeme()
  outfile <- get_model_outfile(aeme = aeme)
  testthat::expect_true(all(file.exists(unlist(outfile))))

})

test_that("running GLM-AED works", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- yaml_to_aeme(path = path, "aeme.yaml")
  vars_sim <- c("HYD_strat", "HYD_temp", "HYD_thmcln", "HYD_schstb",
                "CHM_oxycln", "CHM_oxynal",
                "NIT_tn", "PHS_tp", "PHY_tchla")
  model_controls <- get_model_controls(use_bgc = TRUE)
  model_controls <- set_vars_sim(model_controls = model_controls,
                                 vars_sim = vars_sim)
  model <- c("glm_aed")
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls,
                     ext_elev = 5, use_bgc = TRUE) |>
    run_aeme(verbose = T)

  plot_output_base(aeme)
  plot_output_base(aeme, var_sim = c("evap"))
  plot_output_base(aeme, var_sim = c("qh"))
  plot_output(aeme, var_sim = c("temp", "oxy", "tp", "tn", "frp", "amm"), backend = "base")
  # aeme <- run_aeme(aeme, args = "--xdisp")
  html_file <- plot_glm_config(aeme = aeme)
  testthat::expect_true(file.exists(html_file))
  html_widget <- plot_glm_config(aeme = aeme, return_widget = TRUE)
  testthat::expect_true(!is.null(html_widget))


  out <- run_glm_aed_diagnostics(aeme = aeme)
  testthat::expect_true(is.list(out))
  testthat::expect_true(all(c("data", "plots", "summary") %in% names(out)))
  plt_chk <- sapply(out$plots, ggplot2::is_ggplot)
  testthat::expect_true(all(plt_chk))

  oxy_sdf <- out$data |>
    dplyr::filter(variable == "SDF_Fsed_oxy_Z", is.na(value))

  plot_output(aeme, var_sim = "CHM_oxy") /
    plot_output(aeme)/
    plot_output(aeme, var_sim = "PHY_tchla")

  diag_plot <- plot_glm_diagnostics(aeme = aeme)
  testthat::expect_true(is.list(diag_plot))
  chk <- sapply(diag_plot, \(x) ggplot2::is_ggplot(x))
  testthat::expect_true(all(chk))

  file <- get_model_outfile(aeme = aeme, model = model, path = path)

  plot_output(aeme, model = model)

  v1 <- get_var(aeme = aeme, model = model, var = "HYD_temp")
  v2 <- get_var(aeme = aeme, model = model, var = "CHM_oxynal",
                remove_spin_up = FALSE)
  testthat::expect_true(v1$Date[1] > v2$Date[1])

  plot_output(aeme, model = model, "HYD_temp", facet = TRUE, remove_spin_up = TRUE, level = FALSE) /
    plot_output(aeme, model = model, "CHM_oxy", facet = TRUE, remove_spin_up = FALSE)
  plot_output(aeme, model = model, "HYD_schstb", facet = FALSE) /
    plot_output(aeme, model = model, "CHM_oxycln", facet = FALSE) /
    plot_output(aeme, model = model, "HYD_thmcln", facet = FALSE)

  p1 <- plot_output(aeme, model = model, "PHY_tchla", facet = FALSE)
  p2 <- plot_output(aeme, model = model, "NIT_tn", facet = FALSE)
  p3 <- plot_output(aeme, model = model, "PHS_tp", facet = FALSE)
  plot_phytos(aeme)
  plot_phs(aeme)
  plot_nit(aeme)

  pstrat <- plot_output(aeme, model = model, var_sim = "HYD_strat",
                        facet = FALSE)
  testthat::expect_true(ggplot2::is_ggplot(pstrat))

  model_performance <- assess_model(aeme = aeme)
  testthat::expect_true(is.data.frame(model_performance))



  lke <- lake(aeme)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "output", "output.nc"))
  testthat::expect_true(file_chk)
})

test_that("running GLM with a spinup works", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- get_model_controls()
  inf_factor <- c("glm_aed" = 1)
  outf_factor <- c("glm_aed" = 1)
  model <- c("glm_aed")

  # Add spin up time
  tim <- time(aeme)
  tim[["spin_up"]][[model]] <- 100
  time(aeme) <- tim

  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, inf_factor = inf_factor,
                     ext_elev = 5, use_bgc = FALSE)
  aeme <- run_aeme(aeme = aeme, model = model,
                   model_controls = model_controls, path = path)
  lke <- lake(aeme)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "output", "output.nc"))
  testthat::expect_true(file_chk)
})

test_that("running ensemble works", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- get_model_controls()
  model <- c("glm_aed")
  model <- filter_platform_models(model)
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls,
                     ext_elev = 5, use_bgc = FALSE)
  aeme <- run_aeme(aeme = aeme, model = model, verbose = FALSE, path = path)

  aeme <- run_aeme(aeme = aeme, model = model, verbose = FALSE, path = path,
                   ens_n = 2)


  outp <- output(aeme)
  testthat::expect_true(check_all_model_outfiles(aeme))
  testthat::expect_true(outp$n_members > 1)
})

test_that("assess model with no lake level data", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- yaml_to_aeme(path = path, "aeme.yaml")

  obs <- observations(aeme)
  obs$level <- NULL
  observations(aeme) <- obs

  model_controls <- get_model_controls(use_bgc = TRUE)
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("glm_aed")
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, inf_factor = inf_factor,
                     ext_elev = 5, use_bgc = FALSE, calc_wbal = TRUE,
                     calc_wlev = FALSE)
  inp <- input(aeme)
  met <- inp$meteo
  aeme <- run_aeme(aeme = aeme, model = model, verbose = FALSE,
                   model_controls = model_controls, path = path,
                   parallel = FALSE)
  model_performance <- assess_model(aeme = aeme, model = model,
                                    var_sim = c("LKE_lvlwtr", "HYD_temp"))
  testthat::expect_true(is.data.frame(model_performance))

})

test_that("running GLM-AED with multiple aed models", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  yaml_path <- file.path(tmpdir, "lake")
  aeme <- yaml_to_aeme(path = yaml_path, "aeme.yaml")
  path <- file.path(tmpdir, "aeme")
  vars_sim <- c("HYD_strat", "HYD_temp", "HYD_thmcln", "HYD_schstb",
                "CHM_oxycln", "CHM_oxynal")
  model_controls <- get_model_controls(use_bgc = TRUE)
  model_controls <- set_vars_sim(model_controls = model_controls,
                                 vars_sim = vars_sim)
  model <- c("glm_aed")
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls,
                     ext_elev = 5, use_bgc = TRUE)
  aed_models = c("aed_sedflux", "aed_oxygen", "aed_silica", "aed_nitrogen",
                 "aed_phosphorus", "aed_organic_matter", "aed_phytoplankton",
                 "aed_zooplankton", "aed_macrophyte")
  for (i in seq_len(length(aed_models))) {
    sel_models <- aed_models[1:i]
    set_glm_aed_models(aeme = aeme, path = path,
                       aed_models = sel_models)
    aeme <- run_aeme(aeme = aeme, model = model, verbose = T, path = path)

    # Check output files
    lake_dir <- AEME::get_lake_dir(aeme = aeme, path = path)
    file_chk <- file.exists(file.path(lake_dir, model, "output", "output.nc"))
    testthat::expect_true(file_chk)
  }

})

test_that("updating AED sed params works", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  aeme <- yaml_to_aeme(path = aeme_dir, "aeme.yaml")
  path <- tempdir()
  vars_sim <- c("HYD_strat", "HYD_temp", "HYD_thmcln", "HYD_schstb",
                "CHM_oxycln", "CHM_oxynal")
  model_controls <- get_model_controls(use_bgc = TRUE)
  model_controls <- set_vars_sim(model_controls = model_controls,
                                 vars_sim = vars_sim)
  model <- c("glm_aed")
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls,
                     ext_elev = 5, use_bgc = TRUE)
  set_aed_sed_const2d(aeme = aeme, path = path)

  aeme <- run_aeme(aeme = aeme, model = model, path = path)

  sed_param <- get_aed_sed_const2d_param(aeme = aeme, path = path)
  testthat::expect_true(is.data.frame(sed_param))
  testthat::expect_true(max(sed_param$index, na.rm = TRUE) == 2)

  upd_sed_pars <- glm_sed_params(n_zones = 2, zone_heights = c(6, 14))

  aeme <- add_param(aeme, upd_sed_pars)
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     ext_elev = 5, use_bgc = TRUE)
  lake_dir <- get_lake_dir(aeme = aeme, path = path)
  cfg <- read_model_config(model = model, lake_dir = lake_dir)

  aeme <- aeme |>
    set_aed_sed_const2d(path = path)
  cfg2 <- read_model_config(model = model, lake_dir = lake_dir)

  testthat::expect_false(identical(cfg, cfg2))
  testthat::expect_true(cfg$bgc$aed$aed_sed_const2d$n_zones == 2)
  testthat::expect_true(cfg2$bgc$aed$aed_sed_const2d$n_zones == 2)

  aeme <- run_aeme(aeme = aeme, model = model, path = path)
  # Check output files
  lake_dir <- AEME::get_lake_dir(aeme = aeme, path = path)
  file_chk <- file.exists(file.path(lake_dir, model, "output", "output.nc"))
  testthat::expect_true(file_chk)

})
