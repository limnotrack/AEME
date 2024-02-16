test_that("running DYRESM works", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme_data <- yaml_to_aeme(path = path, "aeme.yaml")
  mod_ctrls <- read.csv(file.path(path, "model_controls.csv"))
  inf_factor = c("dy_cd" = 1)
  outf_factor = c("dy_cd" = 1)
  model <- c("dy_cd")
  aeme_data <- build_ensemble(path = path, aeme_data = aeme_data, model = model,
                              mod_ctrls = mod_ctrls, inf_factor = inf_factor,
                              ext_elev = 5, use_bgc = FALSE)
  aeme_data <- run_aeme(aeme_data = aeme_data, model = model, verbose = TRUE,
                        mod_ctrls = mod_ctrls, path = path)
  lke <- lake(aeme_data)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "DYsim.nc"))
  testthat::expect_true(file_chk)
})

test_that("running GLM works", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme_data <- yaml_to_aeme(path = path, "aeme.yaml")
  mod_ctrls <- read.csv(file.path(path, "model_controls.csv"))
  inf_factor <- c("glm_aed" = 1)
  outf_factor <- c("glm_aed" = 1)
  model <- c("glm_aed")
  aeme_data <- build_ensemble(path = path, aeme_data = aeme_data, model = model,
                              mod_ctrls = mod_ctrls, inf_factor = inf_factor,
                              ext_elev = 5, use_bgc = FALSE)
  aeme_data <- run_aeme(aeme_data = aeme_data, model = model, verbose = TRUE,
                        mod_ctrls = mod_ctrls, path = path)
  lke <- lake(aeme_data)
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
  aeme_data <- yaml_to_aeme(path = path, "aeme.yaml")
  mod_ctrls <- read.csv(file.path(path, "model_controls.csv"))
  inf_factor = c("gotm_wet" = 1)
  outf_factor = c("gotm_wet" = 1)
  model <- c("gotm_wet")
  aeme_data <- build_ensemble(path = path, aeme_data = aeme_data,
                              model = model, mod_ctrls = mod_ctrls,
                              inf_factor = inf_factor, ext_elev = 5,
                              use_bgc = FALSE)
  aeme_data <- run_aeme(aeme_data = aeme_data, model = model, verbose = TRUE,
                        mod_ctrls = mod_ctrls, path = path)
  lke <- lake(aeme_data)
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
  aeme_data <- yaml_to_aeme(path = path, "aeme.yaml")
  mod_ctrls <- read.csv(file.path(path, "model_controls.csv"))
  inf_factor = c("dy_cd" = 1)
  outf_factor = c("dy_cd" = 1)
  model <- c("dy_cd")
  aeme_data <- build_ensemble(path = path, aeme_data = aeme_data, model = model,
                              mod_ctrls = mod_ctrls, inf_factor = inf_factor,
                              ext_elev = 5, use_bgc = TRUE)
  aeme_data <- run_aeme(aeme_data = aeme_data, model = model, verbose = TRUE,
                        mod_ctrls = mod_ctrls, path = path)
  lke <- lake(aeme_data)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "DYsim.nc"))
  testthat::expect_true(file_chk)

  outp <- output(aeme_data)
  testthat::expect_true(!is.null(outp$dy_cd))
})

test_that("running GLM-AED works", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme_data <- yaml_to_aeme(path = path, "aeme.yaml")
  mod_ctrls <- read.csv(file.path(path, "model_controls.csv"))
  inf_factor = c("glm_aed" = 1)
  outf_factor = c("glm_aed" = 1)
  model <- c("glm_aed")
  aeme_data <- build_ensemble(path = path, aeme_data = aeme_data, model = model,
                              mod_ctrls = mod_ctrls, inf_factor = inf_factor,
                              ext_elev = 5, use_bgc = TRUE)
  aeme_data <- run_aeme(aeme_data = aeme_data, model = model, verbose = TRUE,
                        mod_ctrls = mod_ctrls, path = path)
  lke <- lake(aeme_data)
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
  aeme_data <- yaml_to_aeme(path = path, "aeme.yaml")
  mod_ctrls <- read.csv(file.path(path, "model_controls.csv"))
  mod_ctrls <- mod_ctrls |>
    dplyr::mutate(simulate = dplyr::case_when(
      name == "ZOO_zoo1" ~ 1,
      .default = simulate
    ))
  inf_factor = c("gotm_wet" = 1)
  outf_factor = c("gotm_wet" = 1)
  model <- c("gotm_wet")
  aeme_data <- build_ensemble(path = path, aeme_data = aeme_data, model = model,
                              mod_ctrls = mod_ctrls, inf_factor = inf_factor,
                              ext_elev = 5, use_bgc = TRUE)
  aeme_data <- run_aeme(aeme_data = aeme_data, model = model, verbose = TRUE,
                        mod_ctrls = mod_ctrls, path = path)
  lke <- lake(aeme_data)
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
  aeme_data <- yaml_to_aeme(path = path, "aeme.yaml")
  mod_ctrls <- read.csv(file.path(path, "model_controls.csv"))
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("dy_cd", "glm_aed", "gotm_wet")
  aeme_data <- build_ensemble(path = path, aeme_data = aeme_data, model = model,
                              mod_ctrls = mod_ctrls, inf_factor = inf_factor,
                              ext_elev = 5, use_bgc = F, calc_wbal = T,
                              calc_wlev = F)
  inp <- input(aeme_data)
  met <- inp$meteo
  aeme_data <- run_aeme(aeme_data = aeme_data, model = model, verbose = TRUE,
                        mod_ctrls = mod_ctrls, path = path, parallel = FALSE,
                        ncores = 2L)
  # plot_output(aeme_data = aeme_data, model = model)

  lke <- lake(aeme_data)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model[1], "DYsim.nc"))
  testthat::expect_true(file_chk)

  file_chk <- all(file.exists(file.path(path, paste0(lke$id, "_",
                                                     tolower(lke$name)),
                                        model[-1], "output", "output.nc")))
  testthat::expect_true(file_chk)

  var_sim <- c("LKE_lvlwtr", "HYD_temp")

  model_performance <- assess_model(aeme_data = aeme_data, model = model,
                                    var_sim = var_sim)
  testthat::expect_true(is.data.frame(model_performance))

  pl <- plot_resid(aeme_data = aeme_data, model = model, var_sim = var_sim)
  testthat::expect_true(is.list(pl))
  testthat::expect_true(all(sapply(pl, ggplot2::is.ggplot)))
})

test_that("running models with wbal method = 1", {
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

  w_bal <- water_balance(aeme_data)
  w_bal$method <- 1
  water_balance(aeme_data) <- w_bal

  aeme_data <- build_ensemble(path = path, aeme_data = aeme_data, model = model,
                              mod_ctrls = mod_ctrls, inf_factor = inf_factor,
                              ext_elev = 5, use_bgc = FALSE, calc_wbal = T,
                              calc_wlev = F)
  inp <- input(aeme_data)
  met <- inp$meteo
  aeme_data <- run_aeme(aeme_data = aeme_data, model = model, verbose = TRUE,
                        mod_ctrls = mod_ctrls, path = path, parallel = FALSE,
                        ncores = 2L)
  lke <- lake(aeme_data)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model[1], "DYsim.nc"))
  testthat::expect_true(file_chk)

  file_chk <- all(file.exists(file.path(path, paste0(lke$id, "_",
                                                     tolower(lke$name)),
                                        model[-1], "output", "output.nc")))
  testthat::expect_true(file_chk)

  model_performance <- assess_model(aeme_data = aeme_data, model = model,
                                    var_sim = c("LKE_lvlwtr", "HYD_temp"))
  testthat::expect_true(is.data.frame(model_performance))
})

test_that("running models with wbal method = 3", {
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

  w_bal <- water_balance(aeme_data)
  w_bal$method <- 3
  water_balance(aeme_data) <- w_bal
  infl <- inflows(aeme_data)
  infl$data <- NULL
  inflows(aeme_data) <- infl
  outf <- outflows(aeme_data)
  outf$data <- NULL
  outflows(aeme_data) <- outf

  aeme_data <- build_ensemble(path = path, aeme_data = aeme_data, model = model,
                              mod_ctrls = mod_ctrls, inf_factor = inf_factor,
                              ext_elev = 5, use_bgc = FALSE, calc_wbal = T,
                              calc_wlev = F, hum_type = 1)
  aeme_data <- run_aeme(aeme_data = aeme_data, model = model, verbose = TRUE,
                        mod_ctrls = mod_ctrls, path = path, parallel = FALSE,
                        ncores = 2L)

  # plot_output(aeme_data, model)
  # plot_output(aeme_data = aeme_data, model = model, var_sim = "LKE_lvlwtr",
  #             facet = F) /
  # plot_output(aeme_data = aeme_data, model = model, var_sim = "LKE_netwbl",
  #             facet = F, cumulative = T)
  # plot_wbal(aeme_data = aeme_data)
#
#   tst <- get_var(aeme_data = aeme_data, model = model, var = "LKE_netwbl")


  # w_bal <- water_balance(aeme_data)
  #
  # gotm_evap <- get_var(aeme_data = aeme_data, model = "dy_cd",
  #                      var = "LKE_evpflx")
  #
  # ggplot() +
  #   geom_line(data = w_bal$data$wbal, aes(x = Date, y = -dy_cd_evap_flux,
  #                                         colour = "Est")) +
  #   geom_line(data = gotm_evap, aes(x = Date, y = value))
  #
  # gotm_ts <- get_var(aeme_data = aeme_data, model = "dy_cd",
  #                    var = "HYD_Ts")
  #
  # ggplot() +
  #   geom_line(data = w_bal$data$wbal, aes(x = Date, y = Ts,
  #                                         colour = "Est")) +
  #   geom_line(data = gotm_ts, aes(x = Date, y = value))

  lke <- lake(aeme_data)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model[1], "DYsim.nc"))
  testthat::expect_true(file_chk)

  file_chk <- all(file.exists(file.path(path, paste0(lke$id, "_",
                                                     tolower(lke$name)),
                                        model[-1], "output", "output.nc")))
  testthat::expect_true(file_chk)
})


test_that("running models in parallel with no wbal calculated", {
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
                              ext_elev = 5, use_bgc = FALSE, calc_wbal = FALSE)
  outf <- outflows(aeme_data)
  names(outf$data)

  aeme_data <- run_aeme(aeme_data = aeme_data, model = model, verbose = TRUE,
                        mod_ctrls = mod_ctrls, path = path, parallel = FALSE,
                        ncores = 2L)
  plot_output(aeme_data = aeme_data, model = model, var_sim = "LKE_lvlwtr",
              add_obs = FALSE, facet = FALSE)

  lke <- lake(aeme_data)
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
  aeme_data <- yaml_to_aeme(path = path, "aeme.yaml")
  mod_ctrls <- read.csv(file.path(path, "model_controls.csv"))
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("dy_cd", "glm_aed", "gotm_wet")

  outf <- outflows(aeme_data)
  outf$data <- NULL
  outflows(aeme_data) <- outf

  aeme_data <- build_ensemble(path = path, aeme_data = aeme_data, model = model,
                              mod_ctrls = mod_ctrls, inf_factor = inf_factor,
                              ext_elev = 5, use_bgc = FALSE, calc_wbal = F)
  outf <- outflows(aeme_data)
  names(outf$data)

  aeme_data <- run_aeme(aeme_data = aeme_data, model = model, verbose = TRUE,
                        mod_ctrls = mod_ctrls, path = path, parallel = FALSE,
                        ncores = 2L)
  plot_output(aeme_data = aeme_data, model = model, var_sim = "LKE_lvlwtr",
              add_obs = F)

  lke <- lake(aeme_data)
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
  aeme_data <- yaml_to_aeme(path = path, "aeme.yaml")
  inp <- input(aeme_data)
  summary(inp$meteo)

  mod_ctrls <- read.csv(file.path(path, "model_controls.csv"))
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("dy_cd", "glm_aed", "gotm_wet")
  aeme_data <- build_ensemble(path = path, aeme_data = aeme_data, model = model,
                              mod_ctrls = mod_ctrls, inf_factor = inf_factor,
                              ext_elev = 5, use_bgc = FALSE, calc_wbal = TRUE,
                              calc_wlev = FALSE)

  aeme_data <- run_aeme(aeme_data = aeme_data, model = model, verbose = TRUE,
                        mod_ctrls = mod_ctrls, path = path, parallel = FALSE,
                        ncores = 2L)

  plot_output(aeme_data = aeme_data, model = model, var_sim = "LKE_lvlwtr",
              add_obs = F)
  plot_output(aeme_data = aeme_data, model = model, var_sim = "LKE_outflow",
              add_obs = F)

  lke <- lake(aeme_data)
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
  aeme_data <- yaml_to_aeme(path = path, "aeme.yaml")
  mod_ctrls <- read.csv(file.path(path, "model_controls.csv"))
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("glm_aed", "gotm_wet")
  build_ensemble(path = path, aeme_data = aeme_data, model = model,
                 mod_ctrls = mod_ctrls, inf_factor = inf_factor, ext_elev = 5,
                 use_bgc = TRUE)
  run_aeme(aeme_data = aeme_data, model = model, verbose = TRUE, path = path,
           parallel = FALSE, ncores = 2L, return = FALSE)

  aeme_data <- load_output(model = model, aeme_data = aeme_data, path = path,
                           mod_ctrls = mod_ctrls, parallel = FALSE)

  outp <- output(aeme_data)
  output_chk <- !all(is.null(unlist(outp)))
  testthat::expect_true(output_chk)
})

test_that("getting model output in parallel works", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme_data <- yaml_to_aeme(path = path, "aeme.yaml")
  mod_ctrls <- read.csv(file.path(path, "model_controls.csv"))
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("glm_aed", "gotm_wet")
  build_ensemble(path = path, aeme_data = aeme_data, model = model,
                 mod_ctrls = mod_ctrls, inf_factor = inf_factor, ext_elev = 5,
                 use_bgc = TRUE)
  aeme_data <- run_aeme(aeme_data = aeme_data, model = model, verbose = FALSE,
                        mod_ctrls = mod_ctrls, path = path, parallel = FALSE,
                        ncores = 2L)

  outp <- output(aeme_data)
  output_chk <- !all(is.null(unlist(outp)))
  testthat::expect_true(output_chk)
})

test_that("running DYRESM with a spinup works", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme_data <- yaml_to_aeme(path = path, "aeme.yaml")
  mod_ctrls <- read.csv(file.path(path, "model_controls.csv"))
  inf_factor <- c("dy_cd" = 1)
  outf_factor <- c("dy_cd" = 1)
  model <- c("dy_cd")

  # Add spin up time
  tim <- time(aeme_data)
  tim[["spin_up"]][[model]] <- 100
  time(aeme_data) <- tim

  aeme_data <- build_ensemble(path = path, aeme_data = aeme_data, model = model,
                              mod_ctrls = mod_ctrls, inf_factor = inf_factor,
                              ext_elev = 5, use_bgc = FALSE)
  aeme_data <- run_aeme(aeme_data = aeme_data, model = model, verbose = TRUE,
                        mod_ctrls = mod_ctrls, path = path)
  lke <- lake(aeme_data)
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
  aeme_data <- yaml_to_aeme(path = path, "aeme.yaml")
  mod_ctrls <- read.csv(file.path(path, "model_controls.csv"))
  inf_factor <- c("glm_aed" = 1)
  outf_factor <- c("glm_aed" = 1)
  model <- c("glm_aed")

  # Add spin up time
  tim <- time(aeme_data)
  tim[["spin_up"]][[model]] <- 100
  time(aeme_data) <- tim

  aeme_data <- build_ensemble(path = path, aeme_data = aeme_data, model = model,
                              mod_ctrls = mod_ctrls, inf_factor = inf_factor,
                              ext_elev = 5, use_bgc = FALSE)
  aeme_data <- run_aeme(aeme_data = aeme_data, model = model, verbose = TRUE,
                        mod_ctrls = mod_ctrls, path = path)
  lke <- lake(aeme_data)
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
  aeme_data <- yaml_to_aeme(path = path, "aeme.yaml")
  mod_ctrls <- read.csv(file.path(path, "model_controls.csv"))
  inf_factor <- c("gotm_wet" = 1)
  outf_factor <- c("gotm_wet" = 1)
  model <- c("gotm_wet")


  tim <- time(aeme_data)
  tim[["spin_up"]][[model]] <- 200
  time(aeme_data) <- tim

  aeme_data <- build_ensemble(path = path, aeme_data = aeme_data, model = model,
                              mod_ctrls = mod_ctrls, inf_factor = inf_factor,
                              ext_elev = 5, use_bgc = FALSE)
  aeme_data <- run_aeme(aeme_data = aeme_data, model = model, verbose = TRUE,
                        mod_ctrls = mod_ctrls, path = path)
  lke <- lake(aeme_data)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "output", "output.nc"))

  plot_output(aeme_data = aeme_data, model = model, var_sim = "LKE_outflow",
              level = TRUE, print_plots = FALSE,
              var_lims = c(0, 30))

  p1 <- plot_output(aeme_data = aeme_data, model = model, var_sim = "HYD_temp",
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
  aeme_data <- yaml_to_aeme(path = path, "aeme.yaml")
  mod_ctrls <- read.csv(file.path(path, "model_controls.csv"))
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("dy_cd", "glm_aed", "gotm_wet")
  aeme_data <- build_ensemble(path = path, aeme_data = aeme_data, model = model,
                              mod_ctrls = mod_ctrls, inf_factor = inf_factor, ext_elev = 5,
                              use_bgc = TRUE)

  aeme_data <- run_aeme(aeme_data = aeme_data, model = model, verbose = TRUE,
                        mod_ctrls = mod_ctrls, path = path)


  path2 <- file.path(tmpdir, "lake-rewrite")
  aeme_data <- write_configuration(model = model, aeme_data = aeme_data,
                                   path = path2)

  # Check DYRESM files
  lke <- lake(aeme_data)
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
  aeme_data <- build_ensemble(path = path2, aeme_data = aeme_data,
                              model = model, mod_ctrls = mod_ctrls,
                              inf_factor = inf_factor, ext_elev = 5,
                              use_bgc = TRUE)
  aeme_data <- run_aeme(aeme_data = aeme_data, model = model, verbose = TRUE,
                        mod_ctrls = mod_ctrls, path = path2)

  file_chk <- file.exists(file.path(path2, paste0(lke$id, "_",
                                                  tolower(lke$name)),
                                    model[1], "DYsim.nc"))
  testthat::expect_true(file_chk)

  file_chk <- all(file.exists(file.path(path2, paste0(lke$id, "_",
                                                      tolower(lke$name)),
                                        model[-1], "output", "output.nc")))
  testthat::expect_true(file_chk)

})
