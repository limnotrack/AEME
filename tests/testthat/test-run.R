# Functions to check for model output files
check_all_model_outfiles <- function(aeme) {
  lake_dir <- get_lake_dir(aeme)
  model_outfiles <- get_model_outfile(aeme) |> 
    unlist()
  file_chk <- all(file.exists(model_outfiles))
  return(file_chk)
}

test_that("package check is working", {
  sys_OS <- AEME:::get_os()
  if (sys_OS == "osx") {
    testthat::skip("Skipping test on macOS")
  }
  chk <- check_AEME_pkg()
  testthat::expect_true(chk)
})

test_that("running DYRESM works", {
  sys_OS <- AEME:::get_os()
  if (sys_OS == "osx") {
    testthat::skip("Skipping test on macOS")
  }
  aeme_yaml <- system.file("extdata/lake/aeme.yaml", package = "AEME")
  aeme <- yaml_to_aeme(file = aeme_yaml)
  model_controls <- get_model_controls(use_bgc = F)
  model <- c("dy_cd")
  path <- tempdir()
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, 
                     ext_elev = 5, use_bgc = FALSE)
  
  aeme <- run_aeme(aeme = aeme)
  lake_dir <- get_lake_dir(aeme = aeme)
  out_file <- get_model_outfile(aeme = aeme, model = model)[[model]]
  file_chk <- file.exists(out_file)
  testthat::expect_true(file_chk)
  outp <- output(aeme)
  testthat::expect_true(!is.null(outp$ens_001$dy_cd))
  
  lake_dir <- get_lake_dir(aeme = aeme, path = path)
  outfile <- get_model_outfile(aeme = aeme, model = model, path = path)
  
  vars_sim <- "HYD_temp"
  out <- read_dy_output(file = outfile$dy_cd, vars_sim = vars_sim)
  testthat::expect_true(nrow(out$HYD_temp) > 2)
  out2 <- read_dy_output(file = outfile$dy_cd, vars_sim = "HYD_temp", 
                         depths = c(0, 11))
  testthat::expect_true(nrow(out2$HYD_temp) == 2)
  testthat::expect_true(all(out2$HYD_temp[1, ] >= out2$HYD_temp[2, ]))
  out3 <- read_dy_output(file = outfile$dy_cd, vars_sim = "HYD_temp", 
                         depths = c(0, 11), dates = c("2020-09-01", "2020-12-02"))
  testthat::expect_true(ncol(out3$HYD_temp) == 2)
  
})

test_that("running GLM works", {
  sys_OS <- AEME:::get_os()
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
  aeme <- run_aeme(aeme = aeme, model = model, verbose = FALSE, path = path)
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
  testthat::expect_true(length(outp1) == 52)
  
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
  
  v <- get_var(aeme = aeme, model = model, var_sim = "HYD_temp", depth = 0)
  testthat::expect_true(is.data.frame(v))
  testthat::expect_error(get_var(aeme = aeme, model = model, var_sim = "HYD_temp",
                                 depth = 15))
})

test_that("running GLM with different exec works", {
  sys_OS <- AEME:::get_os()
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

test_that("running GOTM works", {
  sys_OS <- AEME:::get_os()
  if (sys_OS == "osx") {
    testthat::skip("Skipping test on macOS")
  }
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- get_model_controls()
  model_controls <- set_vars_sim(model_controls = model_controls,
                                 vars_sim = c("CHM_oxynal", "CHM_oxymom",
                                              "LKE_tli4", "LKE_tli3"))
  inf_factor = c("gotm_wet" = 1)
  outf_factor = c("gotm_wet" = 1)
  model <- c("gotm_wet")
  aeme <- build_aeme(path = path, aeme = aeme,
                     model = model, model_controls = model_controls,
                     inf_factor = inf_factor, ext_elev = 5,
                     use_bgc = F)
  aeme <- run_aeme(aeme = aeme, model = model, path = path, verbose = F)
  plot_output(aeme)
  plot_output(aeme, var_sim = "LKE_evpflx")
  lake_dir <- get_lake_dir(aeme = aeme, path = path)
  wlev <- read_model_wlev(lake_dir = lake_dir, model = model)
  # p1 <- plot_output(aeme, var_sim = "CHM_oxynal")
  # testthat::expect_true(ggplot2::is.ggplot(p1))
  # p2 <- plot_output(aeme, var_sim = "LKE_tli4")
  # testthat::expect_true(ggplot2::is.ggplot(p2))
  lake_dir <- get_lake_dir(aeme = aeme, path = path)
  outfile <- get_model_outfile(aeme = aeme, model = model, path = path)
  
  vars_sim <- "HYD_temp"
  out <- read_gotm_output(file = outfile$gotm_wet["output"], 
                          vars_sim = vars_sim)
  testthat::expect_true(nrow(out$HYD_temp) > 2)
  out2 <- read_gotm_output(file = outfile$gotm_wet["output"], 
                           vars_sim = "HYD_temp", depths = c(0, 11))
  testthat::expect_true(nrow(out2$HYD_temp) == 2)
  testthat::expect_true(all(out2$HYD_temp[1, ] >= out2$HYD_temp[2, ]))
  out3 <- read_gotm_output(file = outfile$gotm_wet["output"], 
                           vars_sim = "HYD_temp", depths = c(0, 11), 
                           dates = c("2020-09-01", "2020-12-02"))
  testthat::expect_true(ncol(out3$HYD_temp) == 2)
  
  lke <- lake(aeme)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "output", "output.nc"))
  testthat::expect_true(file_chk)
  
  outp1 <- output(aeme)
  testthat::expect_true(outp1$n_members > 0)
  aeme <- remove_output(aeme)
  outp2 <- output(aeme)
  testthat::expect_true(outp2$n_members == 0)
})

test_that("run GLM models with old object", {
  aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
  aeme <- readRDS(aeme_file)
  path <- tempdir()
  model_controls <- get_model_controls(use_bgc = TRUE)
  model <- c("dy_cd", "glm_aed", "gotm_wet")
  model <- c("glm_aed")
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, ext_elev = 5) |> 
    run_aeme()
  outfile <- get_model_outfile(aeme = aeme)
  testthat::expect_true(all(file.exists(unlist(outfile))))
  
})

test_that("running all models with running out of water works", {
  sys_OS <- AEME:::get_os()
  if (sys_OS == "osx") {
    testthat::skip("Skipping test on macOS")
  }
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- get_model_controls()
  outf_factor = c("dy_cd" = 1.5, "glm_aed" = 1.5, "gotm_wet" = 1.5)
  outf_param <- aeme_parameters |> 
    dplyr::filter(name == "outflow") |> 
    dplyr::mutate(value = 2)
  aeme <- add_param(aeme, param = outf_param)
  model <- c("dy_cd", "glm_aed", "gotm_wet")
  aeme <- build_aeme(path = path, aeme = aeme,
                     model = model, model_controls = model_controls,
                     outf_factor = outf_factor,
                     ext_elev = 5, use_bgc = FALSE)
  aeme <- run_aeme(aeme = aeme, model = model, path = path)
  lake_dir <- get_lake_dir(aeme = aeme, path = path)
  wlev <- read_model_wlev(lake_dir = lake_dir, model = "gotm_wet")
  
  plot_output(aeme)
  plot_output(aeme, var_sim = "LKE_lvlwtr")
  outfile <- get_model_outfile(aeme = aeme, model = model, path = path)
  testthat::expect_true(all(file.exists(unlist(outfile))))
})

test_that("running DYRESM-CAEDYM works", {
  sys_OS <- AEME:::get_os()
  if (sys_OS == "osx") {
    testthat::skip("Skipping test on macOS")
  }
  aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
  aeme <- readRDS(aeme_file)
  path <- tempdir()
  model_controls <- get_model_controls(use_bgc = TRUE)
  model <- c("dy_cd")
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls,
                     ext_elev = 5, use_bgc = TRUE)
  aeme <- run_aeme(aeme = aeme, verbose = FALSE)
  
  outfile <- get_model_outfile(aeme = aeme)
  file_chk <- file.exists(outfile[["dy_cd"]])
  testthat::expect_true(file_chk)
  
  outp <- output(aeme)
  testthat::expect_true(!is.null(outp$ens_001$dy_cd))
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
  path = "aeme"
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls,
                     ext_elev = 5, use_bgc = TRUE) |> 
    run_aeme()
  
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

test_that("running GOTM-WET works", {
  sys_OS <- AEME:::get_os()
  if (sys_OS == "osx") {
    testthat::skip("Skipping test on macOS")
  }
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- get_model_controls(use_bgc = TRUE)
  model_controls <- model_controls |>
    dplyr::mutate(simulate = dplyr::case_when(
      var_aeme == "ZOO_zoo1" ~ TRUE,
      .default = simulate
    ))
  inf_factor = c("gotm_wet" = 1)
  outf_factor = c("gotm_wet" = 1)
  model <- c("gotm_wet")
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, inf_factor = inf_factor,
                     ext_elev = 5, use_bgc = TRUE)
  aeme <- run_aeme(aeme = aeme, model = model, verbose = FALSE,
                   model_controls = model_controls, path = path)
  plot_output(aeme = aeme, model = model)
  # plot_output(aeme = aeme, model = model, var_sim = "CHM_oxy")
  lke <- lake(aeme)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "output", "output.nc"))
  testthat::expect_true(file_chk)
})

test_that("running models in parallel works", {
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  path <- tempdir()
  aeme <- yaml_to_aeme(path = aeme_dir, "aeme.yaml")
  model_controls <- get_model_controls(use_bgc = TRUE)
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("dy_cd", "glm_aed", "gotm_wet")
  sys_OS <- AEME:::get_os()
  if (sys_OS == "osx") {
    model <- c("glm_aed")
  }
  
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, inf_factor = inf_factor,
                     ext_elev = 5, use_bgc = TRUE, calc_wbal = TRUE,
                     calc_wlev = FALSE)
  inp <- input(aeme)
  met <- inp$meteo
  aeme <- run_aeme(aeme = aeme, model = model, verbose = FALSE,
                   model_controls = model_controls, path = path,
                   parallel = TRUE, ncores = 2)
  
  lke <- lake(aeme)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model[1], "DYsim.nc"))
  testthat::expect_true(file_chk)
  
  file_chk <- all(file.exists(file.path(path, paste0(lke$id, "_",
                                                     tolower(lke$name)),
                                        model[-1], "output", "output.nc")))
  testthat::expect_true(file_chk)
  
  var_sim <- c("LKE_lvlwtr", "HYD_temp")
  
  model_performance <- assess_model(aeme = aeme, model = model,
                                    var_sim = var_sim)
  testthat::expect_true(is.data.frame(model_performance))
  
  pl <- plot_resid(aeme = aeme, model = model, var_sim = var_sim[1])
  testthat::expect_true(ggplot2::is_ggplot(pl))
})

test_that("running models with wbal method = 1", {
  aeme_yaml <- system.file("extdata/lake/aeme.yaml", package = "AEME")
  aeme <- yaml_to_aeme(file = aeme_yaml)
  path <- tempdir()
  lke <- lake(aeme)
  model_controls <- get_model_controls()
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("dy_cd", "glm_aed", "gotm_wet")
  sys_OS <- AEME:::get_os()
  if (sys_OS == "osx") {
    model <- c("dy_cd")
  }
  lake_dir <- get_lake_dir(aeme = aeme, path = path)
  # Delete all files in lake_dir
  unlink(list.files(lake_dir, full.names = TRUE), recursive = TRUE)
  
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, inf_factor = inf_factor,
                     ext_elev = 5, use_bgc = FALSE, calc_wbal = T,
                     wb_method = 1, calc_wlev = F)
  inp <- input(aeme)
  met <- inp$meteo
  aeme <- run_aeme(aeme = aeme, verbose = FALSE,
                   parallel = TRUE, ncores = 2L)

  file_chk <- check_all_model_outfiles(aeme)
  testthat::expect_true(file_chk)

  model_performance <- assess_model(aeme = aeme, model = model,
                                    var_sim = c("LKE_lvlwtr", "HYD_temp"))
  testthat::expect_true(is.data.frame(model_performance))
  
  # DYRESM - Check for number of inflow and outflow files
  lake_dir <- get_lake_dir(aeme = aeme)
  inflow_files <- list.files(file.path(lake_dir, "dy_cd"), pattern = "inf",
                             full.names = TRUE)
  n_inf <- as.numeric(strsplit(readLines(inflow_files)[2], "#")[[1]][1])
  inf <- read.delim(inflow_files, skip = 3, sep = "\t")
  testthat::expect_equal(n_inf, max(inf$InfNum))
  
  outflow_files <- list.files(file.path(lake_dir, "dy_cd"), pattern = "wdr",
                              full.names = TRUE)
  n_wdr <- as.numeric(strsplit(readLines(outflow_files)[2], "#")[[1]][1])
  wdr <- read.delim(outflow_files, skip = 2, sep = "\t")
  testthat::expect_equal(n_wdr, ncol(wdr) - 1)
  
  # GLM - Check for number of inflow and outflow files
  inflow_files <- list.files(file.path(lake_dir, "glm_aed", "bcs"), 
                             pattern = "inf")
  outflow_files <- list.files(file.path(lake_dir, "glm_aed", "bcs"), 
                              pattern = "outf")
  testthat::expect_equal(length(inflow_files), 1)
  testthat::expect_equal(length(outflow_files), 1)
  
  # GOTM - Check for number of inflow and outflow files
  inflow_files <- list.files(file.path(lake_dir, "gotm_wet", "inputs"), 
                             pattern = "inf_")
  outflow_files <- list.files(file.path(lake_dir, "gotm_wet", "inputs"),
                              pattern = "outf_")
  testthat::expect_equal(length(inflow_files), 3)
  testthat::expect_equal(length(outflow_files), 1)
})

test_that("running models with wbal method = 3", {
  aeme_yaml <- system.file("extdata/lake/aeme.yaml", package = "AEME")
  aeme <- yaml_to_aeme(file = aeme_yaml)
  path <- tempdir()
  lke <- lake(aeme)
  model_controls <- get_model_controls()
  model <- c("dy_cd", "glm_aed", "gotm_wet")
  sys_OS <- AEME:::get_os()
  if (sys_OS == "osx") {
    model <- c("glm_aed")
  }
  
  infl <- inflows(aeme)
  infl$data <- NULL
  inflows(aeme) <- infl
  outf <- outflows(aeme)
  outf$data <- NULL
  outflows(aeme) <- outf
  lake_dir <- get_lake_dir(aeme = aeme, path = path)
  # Delete all files in lake_dir
  unlink(list.files(lake_dir, full.names = TRUE), recursive = TRUE)
  
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, 
                     ext_elev = 5, use_bgc = FALSE, calc_wbal = T,
                     wb_method = 3, calc_wlev = F) |> 
    run_aeme(parallel = F, ncores = 2L)

  file_chk <- check_all_model_outfiles(aeme = aeme)
  testthat::expect_true(file_chk)
  # DYRESM - Check for number of inflow and outflow files
  inflow_files <- list.files(file.path(lake_dir, "dy_cd"), 
                             pattern = "inf", full.names = TRUE)
  n_inf <- as.numeric(strsplit(readLines(inflow_files)[2], "#")[[1]][1])
  inf <- read.delim(inflow_files, skip = 3, sep = "\t")
  testthat::expect_equal(n_inf, max(inf$InfNum))
  
  outflow_files <- list.files(file.path(lake_dir, "dy_cd"), pattern = "wdr",
                              full.names = TRUE)
  n_wdr <- as.numeric(strsplit(readLines(outflow_files)[2], "#")[[1]][1])
  wdr <- read.delim(outflow_files, skip = 2, sep = "\t")
  testthat::expect_equal(n_wdr, ncol(wdr) - 1)
  
  # GLM - Check for number of inflow and outflow files
  inflow_files <- list.files(file.path(lake_dir, "glm_aed", "bcs"), 
                             pattern = "inf")
  outflow_files <- list.files(file.path(lake_dir, "glm_aed", "bcs"), 
                              pattern = "outf")
  testthat::expect_equal(length(inflow_files), 1)
  testthat::expect_equal(length(outflow_files), 1)
  
  # GOTM - Check for number of inflow and outflow files
  inflow_files <- list.files(file.path(lake_dir, "gotm_wet", "inputs"), 
                             pattern = "inf_")
  outflow_files <- list.files(file.path(lake_dir, "gotm_wet", "inputs"), 
                              pattern = "outf_")
  testthat::expect_equal(length(inflow_files), 3)
  testthat::expect_equal(length(outflow_files), 1)
})


test_that("running models in parallel with no wbal calculated", {
  aeme_yaml <- system.file("extdata/lake/aeme.yaml", package = "AEME")
  aeme <- yaml_to_aeme(file = aeme_yaml)
  path <- tempdir()
  model_controls <- get_model_controls()
  model <- c("dy_cd", "glm_aed", "gotm_wet")
  sys_OS <- AEME:::get_os()
  if (sys_OS == "osx") {
    model <- c("glm_aed")
  }
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls,
                     ext_elev = 5, use_bgc = FALSE, calc_wbal = FALSE)
  outf <- outflows(aeme)
  names(outf$data)
  
  aeme <- run_aeme(aeme = aeme, model = model, verbose = FALSE,
                   model_controls = model_controls, path = path,
                   parallel = TRUE, ncores = 2)
  plot_output(aeme = aeme, model = model, var_sim = "LKE_lvlwtr",
              add_obs = FALSE, facet = FALSE)
  plot_output(aeme = aeme, model = model, var_sim = "LKE_outflow",
              add_obs = FALSE, facet = FALSE)
  plot_wbal(aeme = aeme)
  
  file_chk <- check_all_model_outfiles(aeme)
  testthat::expect_true(file_chk)
})


test_that("running models with no wbal/outflows calculated", {
  aeme_yaml <- system.file("extdata/lake/aeme.yaml", package = "AEME")
  aeme <- yaml_to_aeme(file = aeme_yaml)
  path <- tempdir()
  model_controls <- get_model_controls()
  model <- c("dy_cd", "glm_aed", "gotm_wet")
  sys_OS <- AEME:::get_os()
  if (sys_OS == "osx") {
    model <- c("glm_aed")
  }
  
  outf <- outflows(aeme)
  outf$data <- NULL
  outflows(aeme) <- outf
  
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls,
                     ext_elev = 5, use_bgc = FALSE, calc_wbal = F)
  outf <- outflows(aeme)
  names(outf$data)
  
  aeme <- run_aeme(aeme = aeme, model = model, verbose = T,
                   model_controls = model_controls, path = path,
                   parallel = F, ncores = 2L)
  plot_output(aeme = aeme, model = model, var_sim = "LKE_lvlwtr",
              add_obs = F)
  
  lke <- lake(aeme)
  file_chk <- all(file.exists(file.path(path, paste0(lke$id, "_",
                                                     tolower(lke$name)),
                                        model[1], "DYsim.nc")),
                  file.exists(file.path(path, paste0(lke$id, "_",
                                                     tolower(lke$name)),
                                        model[2:3], "output", "output.nc")))
  testthat::expect_true(file_chk)
})

test_that("running models in parallel with no wbal & no wlev calculated", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- yaml_to_aeme(path = path, "aeme.yaml")
  inp <- input(aeme)
  summary(inp$meteo)
  
  model_controls <- get_model_controls()
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("dy_cd", "glm_aed", "gotm_wet")
  sys_OS <- AEME:::get_os()
  if (sys_OS == "osx") {
    model <- c("glm_aed")
  }
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, inf_factor = inf_factor,
                     ext_elev = 5, use_bgc = FALSE, calc_wbal = TRUE,
                     calc_wlev = FALSE)
  
  aeme <- run_aeme(aeme = aeme, model = model, verbose = FALSE,
                   model_controls = model_controls, path = path,
                   parallel = TRUE, ncores = 2)
  
  plot_output(aeme = aeme, model = model, var_sim = "LKE_lvlwtr",
              add_obs = F)
  plot_output(aeme = aeme, model = model, var_sim = "LKE_outflow",
              add_obs = F)
  
  lke <- lake(aeme)
  file_chk <- all(file.exists(file.path(path, paste0(lke$id, "_",
                                                     tolower(lke$name)),
                                        model[1], "DYsim.nc")),
                  file.exists(file.path(path, paste0(lke$id, "_",
                                                     tolower(lke$name)),
                                        model[2:3], "output", "output.nc")))
  testthat::expect_true(file_chk)
})

test_that("getting model output works", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- get_model_controls(use_bgc = TRUE)
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("glm_aed", "gotm_wet")
  sys_OS <- AEME:::get_os()
  if (sys_OS == "osx") {
    model <- c("glm_aed")
  }
  aeme <- build_aeme(path = path, aeme = aeme, model = model, ext_elev = 5,
                     model_controls = model_controls, use_bgc = TRUE)
  run_aeme(aeme = aeme, model = model, verbose = FALSE, path = path,
           parallel = TRUE, return_type = "none", ncores = 2)
  
  aeme <- load_output(model = model, aeme = aeme, path = path,
                      model_controls = model_controls, parallel = FALSE)
  
  outp <- output(aeme)
  output_chk <- !all(is.null(unlist(outp)))
  testthat::expect_true(output_chk)
})

test_that("getting model output in parallel works", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- get_model_controls(use_bgc = TRUE)
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("glm_aed", "gotm_wet")
  sys_OS <- AEME:::get_os()
  if (sys_OS == "osx") {
    model <- c("glm_aed")
  }
  build_aeme(path = path, aeme = aeme, model = model,
             model_controls = model_controls, inf_factor = inf_factor, ext_elev = 5,
             use_bgc = TRUE)
  aeme <- run_aeme(aeme = aeme, model = model, verbose = FALSE,
                   model_controls = model_controls, path = path,
                   parallel = TRUE, ncores = 2)
  
  outp <- output(aeme)
  output_chk <- !all(is.null(unlist(outp)))
  testthat::expect_true(output_chk)
})

test_that("running DYRESM with a spinup works", {
  sys_OS <- AEME:::get_os()
  if (sys_OS == "osx") {
    testthat::skip("Skipping test on macOS")
  }
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- get_model_controls()
  inf_factor <- c("dy_cd" = 1)
  outf_factor <- c("dy_cd" = 1)
  model <- c("dy_cd")
  
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
                                    model, "DYsim.nc"))
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

test_that("running GOTM with a spinup works", {
  sys_OS <- AEME:::get_os()
  if (sys_OS == "osx") {
    testthat::skip("Skipping test on macOS")
  }
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- get_model_controls()
  inf_factor <- c("gotm_wet" = 1)
  outf_factor <- c("gotm_wet" = 1)
  model <- c("gotm_wet")
  
  
  tim <- time(aeme)
  tim[["spin_up"]][[model]] <- 200
  time(aeme) <- tim
  
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, inf_factor = inf_factor,
                     ext_elev = 5, use_bgc = FALSE)
  aeme <- run_aeme(aeme = aeme, model = model, verbose = FALSE,
                   model_controls = model_controls, path = path)
  lke <- lake(aeme)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "output", "output.nc"))
  
  plot_output(aeme = aeme, model = model, var_sim = "LKE_outflow",
              level = TRUE, print_plots = FALSE,
              var_lims = c(0, 30))
  
  p1 <- plot_output(aeme = aeme, model = model, var_sim = "HYD_temp",
                    level = TRUE, print_plots = FALSE,
                    var_lims = c(0, 30), ylim = c(0, 16))
  testthat::expect_true(all(ggplot2::is_ggplot(p1)))
  
  testthat::expect_true(file_chk)
})

test_that("can build all models, run and write to new directory & re-run", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- get_model_controls(use_bgc = TRUE)
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("dy_cd", "glm_aed", "gotm_wet")
  sys_OS <- AEME:::get_os()
  if (sys_OS == "osx") {
    model <- c("glm_aed")
  }
  
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, inf_factor = inf_factor,
                     ext_elev = 5, use_bgc = TRUE)
  
  aeme <- run_aeme(aeme = aeme, model = model, parallel = TRUE, ncores = 2,
                   model_controls = model_controls, path = path)
  
  plot_output_base(aeme, var_sim = "qe")
  
  path2 <- file.path(tmpdir, "lake-rewrite")
  aeme <- write_configuration(aeme = aeme, model = model, path = path2)
  
  # Check DYRESM files
  lke <- lake(aeme)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    "dy_cd", "dyresm3p1.par"))
  testthat::expect_true(file_chk)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    "dy_cd", paste0(tolower(lke$name), ".con")))
  testthat::expect_true(file_chk)
  
  
  caedym_fils <- c("bio", "chm", "sed")
  sapply(caedym_fils, \(f) {
    file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                   tolower(lke$name)),
                                      "dy_cd", paste0("caedym3p1.", f)))
    testthat::expect_true(file_chk)
  })
  
  # Check GLM files
  file_chk <- file.exists(file.path(path2, paste0(lke$id, "_",
                                                  tolower(lke$name)),
                                    "glm_aed", "glm3.nml"))
  testthat::expect_true(file_chk)
  file_chk <- file.exists(file.path(path2, paste0(lke$id, "_",
                                                  tolower(lke$name)),
                                    "glm_aed", "aed", "aed.nml"))
  testthat::expect_true(file_chk)
  
  
  # Check GOTM files
  file_chk <- file.exists(file.path(path2, paste0(lke$id, "_",
                                                  tolower(lke$name)),
                                    "gotm_wet", "gotm.yaml"))
  testthat::expect_true(file_chk)
  file_chk <- file.exists(file.path(path2, paste0(lke$id, "_",
                                                  tolower(lke$name)),
                                    "gotm_wet", "fabm.yaml"))
  testthat::expect_true(file_chk)
  
  
  #
  aeme <- build_aeme(path = path2, aeme = aeme,
                     model = model, model_controls = model_controls,
                     inf_factor = inf_factor, ext_elev = 5,
                     use_bgc = TRUE)
  aeme <- run_aeme(aeme = aeme, model = model, verbose = FALSE,
                   model_controls = model_controls, path = path2)
  
  file_chk <- file.exists(file.path(path2, paste0(lke$id, "_",
                                                  tolower(lke$name)),
                                    model[1], "DYsim.nc"))
  testthat::expect_true(file_chk)
  
  file_chk <- all(file.exists(file.path(path2, paste0(lke$id, "_",
                                                      tolower(lke$name)),
                                        model[-1], "output", "output.nc")))
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
  inf_factor <- c("glm_aed" = 1)
  outf_factor <- c("glm_aed" = 1)
  model <- c("glm_aed", "gotm_wet")
  sys_OS <- AEME:::get_os()
  if (sys_OS == "osx") {
    model <- c("glm_aed")
  }
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, inf_factor = inf_factor,
                     ext_elev = 5, use_bgc = FALSE)
  aeme <- run_aeme(aeme = aeme, model = model, verbose = FALSE, path = path)
  
  model <- c("gotm_wet")
  sys_OS <- AEME:::get_os()
  if (sys_OS == "osx") {
    model <- c("glm_aed")
  }
  aeme <- run_aeme(aeme = aeme, model = model, verbose = FALSE, path = path,
                   ens_n = 2)
  
  
  outp <- output(aeme)
  lke <- lake(aeme)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "output", "output.nc"))
  testthat::expect_true(file_chk)
  
  testthat::expect_true(outp$n_members > 1)
})

test_that("running all models with new parameters works", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- yaml_to_aeme(path = path, "aeme.yaml")
  
  utils::data("aeme_parameters")
  aeme_parameters <- aeme_parameters |>
    dplyr::mutate(
      value = dplyr::case_when(
        model == "dy_cd" & name == "light_extinction_coefficient/7" ~ 1,
        model == "glm_aed" & name == "light/Kw" ~ 5,
        model == "gotm_wet" & name == "light_extinction/g2/constant_value" ~ 5,
        # name == "MET_radswd" ~ 0,
        .default = value
      )
    )
  
  parameters(aeme) <- aeme_parameters
  # parameters(aeme) <- aeme_parameters |>
  #   dplyr::filter( model == "glm")
  
  model_controls <- get_model_controls()
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("dy_cd", "glm_aed", "gotm_wet")
  sys_OS <- AEME:::get_os()
  if (sys_OS == "osx") {
    model <- c("glm_aed")
  }
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, inf_factor = inf_factor,
                     use_bgc = FALSE, ext_elev = 5)
  
  aeme <- run_aeme(aeme = aeme, model = model, verbose = FALSE, path = path)
  
  lake_dir <- get_lake_dir(aeme = aeme, path = path)
  
  file_chk <- file.exists(file.path(lake_dir, model[1], "DYsim.nc"))
  testthat::expect_true(file_chk)
  
  file_chk <- all(file.exists(file.path(lake_dir, model[-1], "output",
                                        "output.nc")))
  testthat::expect_true(file_chk)
})

test_that("can get variable indices after running the model", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- get_model_controls()
  inf_factor <- c("gotm_wet" = 1)
  outf_factor <- c("gotm_wet" = 1)
  model <- c("gotm_wet")
  sys_OS <- AEME:::get_os()
  if (sys_OS == "osx") {
    model <- c("glm_aed")
  }
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls,
                     ext_elev = 5, use_bgc = FALSE)
  aeme <- run_aeme(aeme = aeme, model = model, verbose = FALSE,
                   model_controls = model_controls, path = path)
  var_indices <- get_var_indices(model = model, aeme = aeme, path = path,
                                 vars_sim = "HYD_temp", use_obs = TRUE)
  testthat::expect_true(length(var_indices) > 0)
  testthat::expect_true(is.list(var_indices))
  testthat::expect_true(length(var_indices$HYD_temp$date_index) == 10)
  
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

test_that("summarise multi-year output", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- yaml_to_aeme(path = path, "aeme.yaml")
  
  aeme_time <- time(aeme)
  aeme_time$start <- as.POSIXct("2020-01-01 00:00:00")
  time(aeme) <- aeme_time
  
  model_controls <- get_model_controls(use_bgc = TRUE)
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("glm_aed", "gotm_wet")
  sys_OS <- AEME:::get_os()
  if (sys_OS == "osx") {
    model <- c("glm_aed")
  }
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, inf_factor = inf_factor,
                     ext_elev = 5, use_bgc = TRUE, calc_wbal = TRUE,
                     calc_wlev = TRUE)
  
  tgt_vars <- list_mod_obs_vars(aeme = aeme, model = model)
  testthat::expect_true(length(tgt_vars) == 0)
  testthat::expect_true(is.vector(tgt_vars))
  s1 <- object.size(aeme)
  aeme <- run_aeme(aeme = aeme, model = model,
                   model_controls = model_controls, path = path,
                   parallel = TRUE)
  plot_output(aeme)
  tgt_vars <- list_mod_obs_vars(aeme = aeme, model = model)
  testthat::expect_true(length(tgt_vars) > 0)
  s2 <- object.size(aeme)
  aeme_summ <- summary(aeme)
  s3 <- object.size(aeme_summ)
  s3 / s2
  s1 / s2
  testthat::expect_true(is(aeme_summ, "Aeme"))
  testthat::expect_true(s3 < s2)
  
})

test_that("can run with generated hypsgraph", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- yaml_to_aeme(path = path, "aeme.yaml")
  
  hyps <- generate_hypsograph(aeme = aeme, ext_elev = 5, mean_depth = 4.2)
  inp <- input(aeme)
  inp$hypsograph <- hyps
  input(aeme) <- inp
  
  model_controls <- get_model_controls()
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("dy_cd", "glm_aed", "gotm_wet")
  sys_OS <- AEME:::get_os()
  if (sys_OS == "osx") {
    model <- c("glm_aed")
  }
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, inf_factor = inf_factor,
                     use_bgc = FALSE, calc_wbal = TRUE,
                     calc_wlev = TRUE)
  aeme <- run_aeme(aeme = aeme, model = model, verbose = FALSE,
                   model_controls = model_controls, path = path,
                   parallel = FALSE)
  
  lake_dir <- get_lake_dir(aeme = aeme, path = path)
  file_chk <- file.exists(file.path(lake_dir, model[1], "DYsim.nc"))
  testthat::expect_true(file_chk)
  
  file_chk <- all(file.exists(file.path(lake_dir, model[-1], "output",
                                        "output.nc")))
  testthat::expect_true(file_chk)
  
})

test_that("add AEME output as inflow", {
  sys_OS <- AEME:::get_os()
  aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
  aeme <- readRDS(aeme_file)
  path <- tempdir()
  model_controls <- get_model_controls()
  model <- c("glm_aed", "gotm_wet")
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, ext_elev = 5,
                     use_bgc = FALSE)
  
  obs <- get_obs(aeme)
  mod_obs_vars <- get_mod_obs_vars(aeme)
  testthat::expect_true(all(mod_obs_vars$var_aeme %in% obs$var_aeme))
  
  # cfg <- configuration(aeme)
  # cfg$model_controls <- NULL
  # configuration(aeme) <- cfg
  aeme <- run_aeme(aeme = aeme, model = model, verbose = FALSE, path = path)
  # plot_output(aeme, model = model)
  outp <- output(aeme)
  lake_dir <- get_lake_dir(aeme = aeme, path = path)
  file_chk <- file.exists(file.path(lake_dir,
                                    model, "output", "output.nc"))
  testthat::expect_true(all(file_chk))
  
  v <- get_var(aeme = aeme, model = model, var_sim = "HYD_temp", depth = 0)
  testthat::expect_true(is.data.frame(v))
  testthat::expect_error(get_var(aeme = aeme, model = model, 
                                 var_sim = "HYD_temp", depth = 15))
  
  outflow_inflow <- aeme_to_inflow(aeme)
  
  testthat::expect_true(is.data.frame(outflow_inflow))
  testthat::expect_true("model" %in% names(outflow_inflow))
  
  aeme2 <- add_inflow(aeme, inflow = outflow_inflow, 
                      inflow_id = "outflow_inflow")
  aeme2 <- build_aeme(path = path, aeme = aeme2, model = model,
                      model_controls = model_controls,
                      ext_elev = 5, use_bgc = FALSE)
  aeme2 <- run_aeme(aeme2, model, verbose = FALSE, path = path)
  inf <- inflows(aeme2)
  testthat::expect_true("outflow_inflow" %in% names(inf$data))
})

test_that("running GLM-AED with multiple aed models", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  yaml_path <- file.path(tmpdir, "lake")
  aeme <- yaml_to_aeme(path = yaml_path, "aeme.yaml")
  path <- "aeme"
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
