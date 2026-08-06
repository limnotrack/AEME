test_that("package check is working", {
  chk <- check_AEME_pkg()
  testthat::expect_true(chk)
})

test_that("running all models with running out of water works", {
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
  skip_if_models_unavailable(model)
  aeme <- build_aeme(path = path, aeme = aeme,
                     model = model, model_controls = model_controls,
                     outf_factor = outf_factor,
                     ext_elev = 5, use_bgc = FALSE)
  aeme <- run_aeme(aeme = aeme, model = model, path = path)
  lake_dir <- get_lake_dir(aeme = aeme, path = path)
  wlev <- read_model_wlev(lake_dir = lake_dir, model = "gotm_wet")

  plot_output(aeme)
  plot_output(aeme, var_sim = "LKE_lvlwtr")
  outfile <- get_model_outfile(aeme = aeme, model = model)
  testthat::expect_true(all(file.exists(unlist(outfile))))
})

test_that("running models in parallel works", {
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  path <- tempdir()
  aeme <- yaml_to_aeme(path = aeme_dir, "aeme.yaml")
  model_controls <- get_model_controls(use_bgc = TRUE)
  model <- c("dy_cd", "glm_aed", "gotm_wet", "simstrat_aed2")
  model <- filter_platform_models(model)

  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls,
                     ext_elev = 5, use_bgc = TRUE, calc_wbal = TRUE,
                     calc_wlev = FALSE)
  aeme <- run_aeme(aeme = aeme, parallel = TRUE, ncore = getOption("ncore"))
  plot_wlev(aeme)

  testthat::expect_true(check_all_model_outfiles(aeme))

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
  skip_if_models_unavailable(model)
  lake_dir <- get_lake_dir(aeme = aeme, path = path)
  # Delete all files in lake_dir
  unlink(list.files(lake_dir, full.names = TRUE), recursive = TRUE)

  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, inf_factor = inf_factor,
                     ext_elev = 5, use_bgc = FALSE, calc_wbal = T,
                     wb_method = 1, calc_wlev = F)

  plot_est_wbal(aeme, model = "glm_aed", time_axis = "monthly")

  inp <- input(aeme)
  met <- inp$meteo
  aeme <- run_aeme(aeme = aeme, verbose = FALSE,
                   parallel = TRUE, ncore = getOption("ncore")L)

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
  skip_if_models_unavailable(model)

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
    run_aeme(parallel = F, ncore = getOption("ncore")L)

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
  model <- filter_platform_models(model)
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls,
                     ext_elev = 5, use_bgc = FALSE, calc_wbal = FALSE)
  outf <- outflows(aeme)
  names(outf$data)

  aeme <- run_aeme(aeme = aeme, model = model, verbose = FALSE,
                   model_controls = model_controls, path = path,
                   parallel = TRUE, ncore = getOption("ncore"))
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
  model <- filter_platform_models(model)

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
                   parallel = F, ncore = getOption("ncore")L)
  plot_output(aeme = aeme, model = model, var_sim = "LKE_lvlwtr",
              add_obs = F)

  testthat::expect_true(check_all_model_outfiles(aeme))
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
  model <- filter_platform_models(model)
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, inf_factor = inf_factor,
                     ext_elev = 5, use_bgc = FALSE, calc_wbal = TRUE,
                     calc_wlev = FALSE)

  aeme <- run_aeme(aeme = aeme, model = model, verbose = FALSE,
                   model_controls = model_controls, path = path,
                   parallel = TRUE, ncore = getOption("ncore"))

  plot_output(aeme = aeme, model = model, var_sim = "LKE_lvlwtr",
              add_obs = F)
  plot_output(aeme = aeme, model = model, var_sim = "LKE_outflow",
              add_obs = F)

  testthat::expect_true(check_all_model_outfiles(aeme))
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
  model <- filter_platform_models(model)
  aeme <- build_aeme(path = path, aeme = aeme, model = model, ext_elev = 5,
                     model_controls = model_controls, use_bgc = TRUE)
  run_aeme(aeme = aeme, model = model, verbose = FALSE, path = path,
           parallel = TRUE, return_type = "none", ncore = getOption("ncore"))

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
  model <- filter_platform_models(model)
  build_aeme(path = path, aeme = aeme, model = model,
             model_controls = model_controls, inf_factor = inf_factor, ext_elev = 5,
             use_bgc = TRUE)
  aeme <- run_aeme(aeme = aeme, model = model, verbose = FALSE,
                   model_controls = model_controls, path = path,
                   parallel = TRUE, ncore = getOption("ncore"))

  outp <- output(aeme)
  output_chk <- !all(is.null(unlist(outp)))
  testthat::expect_true(output_chk)
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
  skip_if_models_unavailable(model)
  model <- filter_platform_models(model)

  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, inf_factor = inf_factor,
                     ext_elev = 5, use_bgc = TRUE)

  aeme <- run_aeme(aeme = aeme, model = model, parallel = TRUE, ncore = getOption("ncore"),
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
  model <- c("dy_cd", "glm_aed", "gotm_wet")
  model <- filter_platform_models(model)
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls,
                     use_bgc = FALSE, ext_elev = 5)

  aeme <- run_aeme(aeme = aeme, model = model, verbose = FALSE, path = path)
  testthat::expect_true(check_all_model_outfiles(aeme))
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
  model <- filter_platform_models(model)
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

test_that("can run with generated hypsograph", {
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
  model <- filter_platform_models(model)
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, inf_factor = inf_factor,
                     use_bgc = FALSE, calc_wbal = TRUE,
                     calc_wlev = TRUE)
  aeme <- run_aeme(aeme = aeme, model = model, verbose = FALSE,
                   model_controls = model_controls, path = path,
                   parallel = FALSE)

  testthat::expect_true(check_all_model_outfiles(aeme))

})

test_that("add AEME output as inflow", {
  aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
  aeme <- readRDS(aeme_file)
  path <- tempdir()
  model_controls <- get_model_controls()
  model <- c("glm_aed", "gotm_wet")
  model <- filter_platform_models(model)
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
