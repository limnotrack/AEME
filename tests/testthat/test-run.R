test_that("running DYRESM works", {
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
  testthat::expect_true(!is.null(outp$dy_cd))
})

test_that("running GLM works", {
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
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, inf_factor = inf_factor,
                     ext_elev = 5, use_bgc = FALSE)
  # cfg <- configuration(aeme)
  # cfg$model_controls <- NULL
  # configuration(aeme) <- cfg
  aeme <- run_aeme(aeme = aeme, model = model, verbose = TRUE, path = path)
  outp <- output(aeme)
  lke <- lake(aeme)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "output", "output.nc"))
  testthat::expect_true(file_chk)
})

test_that("running GOTM works", {
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
  inp <- input(aeme)
  outp <- output(aeme)
  wb <- water_balance(aeme)
  head(outp[[model]]$Date)
  head(outp[[model]]$LKE_V)
  head(outp[[model]]$HYD_Ts)
  head(outp[[model]]$LKE_lvlwtr) + min(inp$hypsograph$elev)
  head(wb$data$wbal)
  AEME:::calc_V(wb$data$wbal$value[1], hyps = inp$hypsograph, h = 0.01)


  lke <- lake(aeme)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "output", "output.nc"))
  testthat::expect_true(file_chk)
})

test_that("running DYRESM-CAEDYM works", {
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
  testthat::expect_true(!is.null(outp$dy_cd))
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
                   model_controls = model_controls, path = path)
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
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, inf_factor = inf_factor,
                     ext_elev = 5, use_bgc = TRUE, calc_wbal = TRUE,
                     calc_wlev = FALSE)
  inp <- input(aeme)
  met <- inp$meteo
  aeme <- run_aeme(aeme = aeme, model = model, verbose = TRUE,
                   model_controls = model_controls, path = path,
                   parallel = TRUE)
  plot_output(aeme = aeme, model = model, var_sim = "CHM_oxy")

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
  testthat::expect_true(all(sapply(pl, ggplot2::is.ggplot)))
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

  w_bal <- water_balance(aeme)
  w_bal$method <- 1
  water_balance(aeme) <- w_bal

  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, inf_factor = inf_factor,
                     ext_elev = 5, use_bgc = FALSE, calc_wbal = T,
                     calc_wlev = F)
  inp <- input(aeme)
  met <- inp$meteo
  aeme <- run_aeme(aeme = aeme, model = model, verbose = TRUE,
                   model_controls = model_controls, path = path,
                   parallel = TRUE)
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

  w_bal <- water_balance(aeme)
  w_bal$method <- 3
  water_balance(aeme) <- w_bal
  infl <- inflows(aeme)
  infl$data <- NULL
  inflows(aeme) <- infl
  outf <- outflows(aeme)
  outf$data <- NULL
  outflows(aeme) <- outf

  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, inf_factor = inf_factor,
                     ext_elev = 5, use_bgc = FALSE, calc_wbal = T,
                     calc_wlev = F, hum_type = 1)
  aeme <- run_aeme(aeme = aeme, model = model, verbose = TRUE,
                   model_controls = model_controls, path = path,
                   parallel = TRUE)

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
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, inf_factor = inf_factor,
                     ext_elev = 5, use_bgc = FALSE, calc_wbal = FALSE)
  outf <- outflows(aeme)
  names(outf$data)

  aeme <- run_aeme(aeme = aeme, model = model, verbose = TRUE,
                   model_controls = model_controls, path = path,
                   parallel = TRUE)
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
                   parallel = TRUE)
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
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, inf_factor = inf_factor,
                     ext_elev = 5, use_bgc = FALSE, calc_wbal = TRUE,
                     calc_wlev = FALSE)

  aeme <- run_aeme(aeme = aeme, model = model, verbose = TRUE,
                   model_controls = model_controls, path = path,
                   parallel = TRUE)

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
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, use_bgc = TRUE)
  run_aeme(aeme = aeme, model = model, verbose = TRUE, path = path,
           parallel = TRUE, return = FALSE)

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
  build_aeme(path = path, aeme = aeme, model = model,
             model_controls = model_controls, inf_factor = inf_factor, ext_elev = 5,
             use_bgc = TRUE)
  aeme <- run_aeme(aeme = aeme, model = model, verbose = FALSE,
                   model_controls = model_controls, path = path,
                   parallel = TRUE)

  outp <- output(aeme)
  output_chk <- !all(is.null(unlist(outp)))
  testthat::expect_true(output_chk)
})

test_that("running DYRESM with a spinup works", {
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
  testthat::expect_true(all(ggplot2::is.ggplot(p1)))

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
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, inf_factor = inf_factor,
                     ext_elev = 5, use_bgc = TRUE)

  aeme <- run_aeme(aeme = aeme, model = model, parallel = TRUE,
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
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, inf_factor = inf_factor,
                     ext_elev = 5, use_bgc = FALSE)
  aeme <- run_aeme(aeme = aeme, model = model, verbose = TRUE, path = path)

  model <- c("gotm_wet")
  aeme <- run_aeme(aeme = aeme, model = model, verbose = TRUE, path = path,
                   ens_n = 2)


  outp <- output(aeme)
  lke <- lake(aeme)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "output", "output.nc"))
  testthat::expect_true(file_chk)

  testthat::expect_true(outp$n_members)
})
