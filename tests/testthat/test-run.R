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
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- get_model_controls(use_bgc = F)
  inf_factor = c("dy_cd" = 1)
  outf_factor = c("dy_cd" = 1)
  model <- c("dy_cd")
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, inf_factor = inf_factor,
                     ext_elev = 5, use_bgc = FALSE)
  aeme <- run_aeme(aeme = aeme, model = model, verbose = F,
                   model_controls = model_controls, path = path)
  lke <- lake(aeme)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "DYsim.nc"))
  testthat::expect_true(file_chk)
  outp <- output(aeme)
  testthat::expect_true(!is.null(outp$ens_001$dy_cd))
})

test_that("running GLM works", {
  sys_OS <- AEME:::get_os()
  # if (sys_OS == "windows") {
  #   bin_exec <- file.path(bin_path, "glm_aed", "glm.exe")
  # } else if (sys_OS == "osx") {
  #   bin_exec <- file.path(bin_path, "glm_aed", "glm")
  # }
  # timeout <- 0
  # system2(bin_exec,
  #         wait = TRUE, stdout = "",
  #         stderr = "", timeout = timeout)

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
  
  # cfg <- configuration(aeme)
  # cfg$model_controls <- NULL
  # configuration(aeme) <- cfg
  aeme <- run_aeme(aeme = aeme, model = model, verbose = TRUE, path = path)
  # plot_output(aeme, model = model)
  outp <- output(aeme)
  lake_dir <- get_lake_dir(aeme = aeme, path = path)
  file_chk <- file.exists(file.path(lake_dir,
                                    model, "output", "output.nc"))
  testthat::expect_true(file_chk)

  v <- get_var(aeme = aeme, model = model, var_sim = "HYD_temp", depth = 0)
  testthat::expect_true(is.data.frame(v))
  testthat::expect_error(get_var(aeme = aeme, model = model, var_sim = "HYD_temp",
                                 depth = 15))
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
  inf_factor = c("gotm_wet" = 1)
  outf_factor = c("gotm_wet" = 1)
  model <- c("gotm_wet")
  aeme <- build_aeme(path = path, aeme = aeme,
                     model = model, model_controls = model_controls,
                     inf_factor = inf_factor, ext_elev = 5,
                     use_bgc = FALSE)
  aeme <- run_aeme(aeme = aeme, model = model, verbose = FALSE,
                   model_controls = model_controls, path = path)
  plot_output(aeme, model = model)

  # inp <- input(aeme)
  # outp <- output(aeme)
  # filled.contour(t(outp$ens_001$gotm_wet$HYD_temp))
  # wb <- water_balance(aeme)
  # head(outp[[model]]$Date)
  # head(outp[[model]]$LKE_V)
  # head(outp[[model]]$HYD_Ts)
  # head(outp[[model]]$LKE_lvlwtr) + min(inp$hypsograph$elev)
  # head(wb$data$wbal)
  # AEME:::calc_V(wb$data$wbal$value[1], hyps = inp$hypsograph, h = 0.01)


  lke <- lake(aeme)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "output", "output.nc"))
  testthat::expect_true(file_chk)
})

test_that("running DYRESM-CAEDYM works", {
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
  inf_factor = c("dy_cd" = 1)
  outf_factor = c("dy_cd" = 1)
  model <- c("dy_cd")
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, inf_factor = inf_factor,
                     ext_elev = 5, use_bgc = TRUE)
  aeme <- run_aeme(aeme = aeme, model = model, verbose = TRUE,
                   model_controls = model_controls, path = path)
  lke <- lake(aeme)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "DYsim.nc"))
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
  model_controls <- get_model_controls(use_bgc = TRUE)
  inf_factor = c("glm_aed" = 1)
  outf_factor = c("glm_aed" = 1)
  model <- c("glm_aed")
  tim <- time(aeme)
  tim$start <- tim$start + (100 * 86400)
  tim$spin_up$glm_aed <- 100
  time(aeme) <- tim
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, inf_factor = inf_factor,
                     ext_elev = 5, use_bgc = TRUE)
  aeme <- run_aeme(aeme = aeme, model = model, verbose = TRUE,
                   path = path)
  # plot_output(aeme, model = model, c("LKE_tli3"), facet = FALSE)

  v1 <- get_var(aeme = aeme, model = model, var = "HYD_temp")
  v2 <- get_var(aeme = aeme, model = model, var = "HYD_temp",
                remove_spin_up = FALSE)
  testthat::expect_true(v1$Date[1] > v2$Date[1])

  plot_output(aeme, model = model, "HYD_temp", facet = TRUE, remove_spin_up = TRUE, level = FALSE) /
    plot_output(aeme, model = model, "CHM_oxy", facet = TRUE, remove_spin_up = FALSE)
  plot_output(aeme, model = model, "HYD_schstb", facet = FALSE) /
    plot_output(aeme, model = model, "CHM_oxycln", facet = FALSE) /
    plot_output(aeme, model = model, "HYD_thmcln", facet = FALSE)

  var_sim <- c("LKE_lvlwtr", "HYD_temp", "HYD_thmcln", "HYD_schstb")
  model_performance <- assess_model(aeme = aeme, model = model,
                                    var_sim = var_sim)



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
  aeme <- run_aeme(aeme = aeme, model = model, verbose = TRUE,
                   model_controls = model_controls, path = path)
  # plot_output(aeme = aeme, model = model)
  # plot_output(aeme = aeme, model = model, var_sim = "CHM_oxy")
  lke <- lake(aeme)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "output", "output.nc"))
  testthat::expect_true(file_chk)
})

test_that("running models in parallel works", {
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
                     ext_elev = 5, use_bgc = TRUE, calc_wbal = TRUE,
                     calc_wlev = FALSE)
  inp <- input(aeme)
  met <- inp$meteo
  aeme <- run_aeme(aeme = aeme, model = model, verbose = TRUE,
                   model_controls = model_controls, path = path,
                   parallel = TRUE, ncores = 2)
  plot_output(aeme = aeme, model = model)

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

  pl <- plot_resid(aeme = aeme, model = model, var_sim = var_sim)
  testthat::expect_true(is.list(pl))
  testthat::expect_true(all(sapply(pl, ggplot2::is_ggplot)))
})

test_that("running models with wbal method = 1", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- yaml_to_aeme(path = path, "aeme.yaml")
  lke <- lake(aeme)
  unlink(file.path(path, paste0(lke$id, "_", tolower(lke$name))),
         recursive = TRUE)
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
                     ext_elev = 5, use_bgc = FALSE, calc_wbal = T,
                     wb_method = 1, calc_wlev = F)
  inp <- input(aeme)
  met <- inp$meteo
  aeme <- run_aeme(aeme = aeme, model = model, verbose = TRUE,
                   model_controls = model_controls, path = path,
                   parallel = TRUE, ncores = 2)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model[1], "DYsim.nc"))
  testthat::expect_true(file_chk)

  file_chk <- all(file.exists(file.path(path, paste0(lke$id, "_",
                                                     tolower(lke$name)),
                                        model[-1], "output", "output.nc")))
  testthat::expect_true(file_chk)

  model_performance <- assess_model(aeme = aeme, model = model,
                                    var_sim = c("LKE_lvlwtr", "HYD_temp"))
  testthat::expect_true(is.data.frame(model_performance))

  # DYRESM - Check for number of inflow and outflow files
  inflow_files <- list.files(file.path(path, paste0(lke$id, "_",
                                                    tolower(lke$name)),
                                       "dy_cd"), pattern = "inf", full.names = TRUE)
  n_inf <- as.numeric(strsplit(readLines(inflow_files)[2], "#")[[1]][1])
  inf <- read.delim(inflow_files, skip = 3, sep = "\t")
  testthat::expect_equal(n_inf, max(inf$InfNum))

  outflow_files <- list.files(file.path(path, paste0(lke$id, "_",
                                                     tolower(lke$name)),
                                        "dy_cd"), pattern = "wdr", full.names = TRUE)
  n_wdr <- as.numeric(strsplit(readLines(outflow_files)[2], "#")[[1]][1])
  wdr <- read.delim(outflow_files, skip = 2, sep = "\t")
  testthat::expect_equal(n_wdr, ncol(wdr) - 1)

  # GLM - Check for number of inflow and outflow files
  inflow_files <- list.files(file.path(path, paste0(lke$id, "_",
                                                    tolower(lke$name)),
                                       "glm_aed", "bcs"), pattern = "inf")
  outflow_files <- list.files(file.path(path, paste0(lke$id, "_",
                                                     tolower(lke$name)),
                                        "glm_aed", "bcs"), pattern = "outf")
  testthat::expect_equal(length(inflow_files), 1)
  testthat::expect_equal(length(outflow_files), 1)

  # GOTM - Check for number of inflow and outflow files
  inflow_files <- list.files(file.path(path, paste0(lke$id, "_",
                                                    tolower(lke$name)),
                                       "gotm_wet", "inputs"), pattern = "inf")
  outflow_files <- list.files(file.path(path, paste0(lke$id, "_",
                                                     tolower(lke$name)),
                                        "gotm_wet", "inputs"), pattern = "outf")
  testthat::expect_equal(length(inflow_files), 3)
  testthat::expect_equal(length(outflow_files), 1)
})

test_that("running models with wbal method = 3", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  # unlink(path, recursive = TRUE)
  aeme <- yaml_to_aeme(path = path, "aeme.yaml")
  lke <- lake(aeme)
  unlink(file.path(path, paste0(lke$id, "_", tolower(lke$name))),
         recursive = TRUE)
  model_controls <- get_model_controls()
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
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

  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, inf_factor = inf_factor,
                     ext_elev = 5, use_bgc = FALSE, calc_wbal = T,
                     wb_method = 3, calc_wlev = F, hum_type = 1)
  aeme <- run_aeme(aeme = aeme, model = model, verbose = TRUE,
                   model_controls = model_controls, path = path,
                   parallel = TRUE, ncores = 2)

  # plot_output(aeme, model)
  # plot_output(aeme = aeme, model = model, var_sim = "LKE_lvlwtr",
  #             facet = F) /
  # plot_output(aeme = aeme, model = model, var_sim = "LKE_netwbl",
  #             facet = F, cumulative = T)
  # plot_wbal(aeme = aeme)
  #
  #   tst <- get_var(aeme = aeme, model = model, var = "LKE_netwbl")


  # w_bal <- water_balance(aeme)
  #
  # gotm_evap <- get_var(aeme = aeme, model = "dy_cd",
  #                      var = "LKE_evpflx")
  #
  # ggplot() +
  #   geom_line(data = w_bal$data$wbal, aes(x = Date, y = -dy_cd_evap_flux,
  #                                         colour = "Est")) +
  #   geom_line(data = gotm_evap, aes(x = Date, y = value))
  #
  # gotm_ts <- get_var(aeme = aeme, model = "dy_cd",
  #                    var = "HYD_Ts")
  #
  # ggplot() +
  #   geom_line(data = w_bal$data$wbal, aes(x = Date, y = Ts,
  #                                         colour = "Est")) +
  #   geom_line(data = gotm_ts, aes(x = Date, y = value))

  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model[1], "DYsim.nc"))
  testthat::expect_true(file_chk)

  file_chk <- all(file.exists(file.path(path, paste0(lke$id, "_",
                                                     tolower(lke$name)),
                                        model[-1], "output", "output.nc")))
  testthat::expect_true(file_chk)

  # DYRESM - Check for number of inflow and outflow files
  inflow_files <- list.files(file.path(path, paste0(lke$id, "_",
                                                    tolower(lke$name)),
                                       "dy_cd"), pattern = "inf", full.names = TRUE)
  n_inf <- as.numeric(strsplit(readLines(inflow_files)[2], "#")[[1]][1])
  inf <- read.delim(inflow_files, skip = 3, sep = "\t")
  testthat::expect_equal(n_inf, max(inf$InfNum))

  outflow_files <- list.files(file.path(path, paste0(lke$id, "_",
                                                     tolower(lke$name)),
                                        "dy_cd"), pattern = "wdr", full.names = TRUE)
  n_wdr <- as.numeric(strsplit(readLines(outflow_files)[2], "#")[[1]][1])
  wdr <- read.delim(outflow_files, skip = 2, sep = "\t")
  testthat::expect_equal(n_wdr, ncol(wdr) - 1)

  # GLM - Check for number of inflow and outflow files
  inflow_files <- list.files(file.path(path, paste0(lke$id, "_",
                                                    tolower(lke$name)),
                                       "glm_aed", "bcs"), pattern = "inf")
  outflow_files <- list.files(file.path(path, paste0(lke$id, "_",
                                                     tolower(lke$name)),
                                        "glm_aed", "bcs"), pattern = "outf")
  testthat::expect_equal(length(inflow_files), 1)
  testthat::expect_equal(length(outflow_files), 1)

  # GOTM - Check for number of inflow and outflow files
  inflow_files <- list.files(file.path(path, paste0(lke$id, "_",
                                                    tolower(lke$name)),
                                       "gotm_wet", "inputs"), pattern = "inf")
  outflow_files <- list.files(file.path(path, paste0(lke$id, "_",
                                                     tolower(lke$name)),
                                        "gotm_wet", "inputs"), pattern = "outf")
  testthat::expect_equal(length(inflow_files), 3)
  testthat::expect_equal(length(outflow_files), 1)
})


test_that("running models in parallel with no wbal calculated", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- yaml_to_aeme(path = path, "aeme.yaml")
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
                     ext_elev = 5, use_bgc = FALSE, calc_wbal = FALSE)
  outf <- outflows(aeme)
  names(outf$data)

  aeme <- run_aeme(aeme = aeme, model = model, verbose = TRUE,
                   model_controls = model_controls, path = path,
                   parallel = TRUE, ncores = 2)
  plot_output(aeme = aeme, model = model, var_sim = "LKE_lvlwtr",
              add_obs = FALSE, facet = FALSE)

  lke <- lake(aeme)
  file_chk <- all(file.exists(file.path(path, paste0(lke$id, "_",
                                                     tolower(lke$name)),
                                        model[1], "DYsim.nc")),
                  file.exists(file.path(path, paste0(lke$id, "_",
                                                     tolower(lke$name)),
                                        model[2:3], "output", "output.nc")))
  testthat::expect_true(file_chk)
})


test_that("running models with no wbal/outflows calculated", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- get_model_controls()
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("dy_cd", "glm_aed", "gotm_wet")
  sys_OS <- AEME:::get_os()
  if (sys_OS == "osx") {
    model <- c("glm_aed")
  }

  outf <- outflows(aeme)
  outf$data <- NULL
  outflows(aeme) <- outf

  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, inf_factor = inf_factor,
                     ext_elev = 5, use_bgc = FALSE, calc_wbal = F)
  outf <- outflows(aeme)
  names(outf$data)

  aeme <- run_aeme(aeme = aeme, model = model, verbose = TRUE,
                   model_controls = model_controls, path = path,
                   parallel = TRUE, ncores = 2)
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

  aeme <- run_aeme(aeme = aeme, model = model, verbose = TRUE,
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
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, use_bgc = TRUE)
  run_aeme(aeme = aeme, model = model, verbose = TRUE, path = path,
           parallel = TRUE, return = FALSE, ncores = 2)

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
  aeme <- run_aeme(aeme = aeme, model = model, verbose = TRUE,
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

  path2 <- file.path(tmpdir, "lake-rewrite")
  aeme <- write_configuration(model = model, aeme = aeme,
                              path = path2)

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
                                    "glm_aed", "aed2", "aed2.nml"))
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
  aeme <- run_aeme(aeme = aeme, model = model, verbose = TRUE,
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
  aeme <- run_aeme(aeme = aeme, model = model, verbose = TRUE, path = path)

  model <- c("gotm_wet")
  sys_OS <- AEME:::get_os()
  if (sys_OS == "osx") {
    model <- c("glm_aed")
  }
  aeme <- run_aeme(aeme = aeme, model = model, verbose = TRUE, path = path,
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

  aeme <- run_aeme(aeme = aeme, model = model, verbose = TRUE, path = path)

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
  aeme <- run_aeme(aeme = aeme, model = model, verbose = TRUE,
                   model_controls = model_controls, path = path)
  var_indices <- get_var_indices(model = model, aeme = aeme, path = path,
                                 vars_sim = "HYD_temp")
  testthat::expect_true(length(var_indices) > 0)
  testthat::expect_true(is.list(var_indices))
  testthat::expect_true(length(var_indices$HYD_temp$time) == 10)

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
  aeme <- run_aeme(aeme = aeme, model = model, verbose = TRUE,
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
  aeme <- run_aeme(aeme = aeme, model = model, verbose = TRUE, path = path)
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
  aeme2 <- run_aeme(aeme2, model, verbose = TRUE, path = path)
  inf <- inflows(aeme2)
  testthat::expect_true("outflow_inflow" %in% names(inf$data))
})
