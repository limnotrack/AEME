test_that("GLM parameters can be input", {
  path <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  aeme <- yaml_to_aeme(path = aeme_dir, "aeme.yaml")
  model_controls <- get_model_controls()
  model <- c("glm_aed")
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, ext_elev = 3)
  # Get parameters for calibration
  data("aeme_parameters", package = "AEME")
  param <- dplyr::bind_rows(aeme_parameters)
  param <- param |> 
    dplyr::mutate(
      value = dplyr::case_when(
        name == "light/Kw" ~ 1.5,
        TRUE ~ value
      )
    )
  input_model_parameters(aeme = aeme, model = model, param = param, path = path)
  cfg_files <- get_model_config_files(aeme = aeme, model = model, path = path)
  nml <- read_nml(cfg_files$glm_aed[find_glm_nml_key(names(cfg_files$glm_aed))])
  kw_value <- nml$light$Kw
  testthat::expect_equal(kw_value, 1.5)
})

test_that("GLM-AED parameters can be input", {
  path <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  aeme <- yaml_to_aeme(path = aeme_dir, "aeme.yaml")
  model_controls <- get_model_controls()
  model <- c("glm_aed")
  aeme <- build_aeme(path = path, aeme = aeme, model = model, ext_elev = 3,
                     model_controls = model_controls, use_bgc = TRUE)
  cfg_files <- get_model_config_files(aeme = aeme, model = model, path = path)
  phy_pars1 <- read_aed_param_csv(cfg_files$glm_aed["aed_phyto_pars"])
  zoo_pars1 <- read_aed_param_csv(cfg_files$glm_aed["aed_zoop_pars"])
  # Get parameters for calibration
  data("aeme_parameters", package = "AEME")
  phy_param <- get_aeme_parameters(model = model, 
                                   file = "aed_phyto_pars.csv", 
                                   module = "phytoplankton") |> 
    dplyr::mutate(
      value = dplyr::case_when(
        name == "p_initial" ~ 25,
        .default = value
      )
    )
  # phy_param <- phy_param |>
  #   dplyr::mutate(
  #     value = dplyr::case_when(
  #       name == "phyto_data/pd%R_growth" ~ 2.0,
  #       TRUE ~ value
  #     )
  #   )
  param <- dplyr::bind_rows(aeme_parameters, phy_param)
  param |> 
    dplyr::distinct(model, file, name, group)
  input_model_parameters(aeme = aeme, model = model, param = param, path = path)
  phy_pars2 <- read_aed_param_csv(cfg_files$glm_aed["aed_phyto_pars"])
  testthat::expect_equal(nrow(phy_pars1), nrow(phy_pars2))
  init2 <- phy_pars2 |> 
    dplyr::filter(p_name == "p_initial") |> 
    dplyr::select(-p_name) |> 
    unlist()
  # testthat::expect_true(all(init2 == 25))
  
  aeme <- run_aeme(aeme = aeme, model = model, path = path, verbose = TRUE)
  
  testthat::expect_true(file.exists(get_model_outfile(aeme = aeme, 
                                                       model = model)$glm_aed))
  
  # Zooplankton parameters
  zoo_param <- get_aeme_parameters(model = model, 
                                   file = "aed_zoop_pars.csv", 
                                   module = "zooplankton") |> 
    dplyr::mutate(
      value = dplyr::case_when(
        name == "zoop_initial" ~ 5,
        .default = value
      )
    )
  param <- dplyr::bind_rows(aeme_parameters, zoo_param)
  input_model_parameters(aeme = aeme, model = model, param = param, path = path)
  zoo_pars2 <- read_aed_param_csv(cfg_files$glm_aed["aed_zoop_pars"])
  init2 <- zoo_pars2 |> 
    dplyr::filter(zoop_name == "zoop_initial") |> 
    dplyr::select(-zoop_name) |> 
    unlist()
  testthat::expect_true(all(init2 == 5))
  
})

test_that("GLM sediment parameters can be input", {
  path <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  aeme <- yaml_to_aeme(path = aeme_dir, "aeme.yaml")
  model_controls <- get_model_controls()
  model <- c("glm_aed")
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, ext_elev = 3)
  sed_params <- glm_sed_params(n_zones = 1, sed_temp_mean = 16.5)
  input_model_parameters(aeme = aeme, model = model, param = sed_params,
                         path = path)
  cfg_files <- get_model_config_files(aeme = aeme, model = model, path = path)
  nml <- read_nml(cfg_files$glm_aed[find_glm_nml_key(names(cfg_files$glm_aed))])
  sed_temp <- get_nml_value(nml, "sed_temp_mean")
  testthat::expect_equal(sed_temp, 16.5)
})

test_that("GLM sediment parameters can be input and run", {
  path <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  aeme <- yaml_to_aeme(path = aeme_dir, "aeme.yaml")
  model_controls <- get_model_controls()
  model <- c("glm_aed")
  sed_params <- glm_sed_params(n_zones = 2, zone_heights = c(5, 14))
  aeme <- AEME::add_param(aeme, sed_params)
  # input_model_parameters(aeme = aeme, model = model, param = sed_params,
  #                        path = path)
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, ext_elev = 5)
  
  cfg_files <- get_model_config_files(aeme = aeme, model = model, path = path)
  nml <- read_nml(cfg_files$glm_aed[find_glm_nml_key(names(cfg_files$glm_aed))])
  zone_heights <- get_nml_value(nml, "zone_heights")
  testthat::expect_equal(zone_heights, c(5, 14))
  
  aeme <- run_aeme(aeme = aeme, model = model, path = path, verbose = T)

  outfiles <- get_model_outfile(aeme = aeme, model = model)
  testthat::expect_true(file.exists(outfiles$glm_aed))
  
  sed_params <- glm_sed_params(n_zones = 3, zone_heights = c(5, 10, 14), 
                               sed_temp_mean = c(10, 12, 16))
  input_model_parameters(aeme = aeme, model = model, param = sed_params,
                         path = path)
  cfg_files <- get_model_config_files(aeme = aeme, model = model, path = path)
  nml <- read_nml(cfg_files$glm_aed[find_glm_nml_key(names(cfg_files$glm_aed))])
  zone_heights <- get_nml_value(nml, "zone_heights")
  testthat::expect_equal(zone_heights, c(5, 10, 14))
  
  aeme <- run_aeme(aeme = aeme, model = model, path = path)
  
  outfiles <- get_model_outfile(aeme = aeme, model = model)
  testthat::expect_true(file.exists(outfiles$glm_aed))
})

test_that("GLM sediment parameters can be input and run with bgc", {
  path <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  aeme <- yaml_to_aeme(path = aeme_dir, "aeme.yaml")
  model_controls <- get_model_controls()
  model <- c("glm_aed")
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, use_bgc = TRUE, 
                     ext_elev = 3)
  sed_params <- glm_sed_params(n_zones = 2, zone_heights = c(5, 14))
  input_model_parameters(aeme = aeme, model = model, param = sed_params,
                         path = path)
  cfg_files <- get_model_config_files(aeme = aeme, model = model, path = path)
  nml <- read_nml(cfg_files$glm_aed[find_glm_nml_key(names(cfg_files$glm_aed))])
  zone_heights <- get_nml_value(nml, "zone_heights")
  testthat::expect_equal(zone_heights, c(5, 14))
  
  aeme <- run_aeme(aeme = aeme, model = model, path = path, verbose = T)
  
  outfiles <- get_model_outfile(aeme = aeme, model = model)
  testthat::expect_true(file.exists(outfiles$glm_aed))
  
  sed_params <- glm_sed_params(n_zones = 3, zone_heights = c(5, 10, 14), 
                               sed_temp_mean = c(10, 12, 16))
  input_model_parameters(aeme = aeme, model = model, param = sed_params,
                         path = path)
  cfg_files <- get_model_config_files(aeme = aeme, model = model, path = path)
  nml <- read_nml(cfg_files$glm_aed[find_glm_nml_key(names(cfg_files$glm_aed))])
  zone_heights <- get_nml_value(nml, "zone_heights")
  testthat::expect_equal(zone_heights, c(5, 10, 14))
  
  aeme <- run_aeme(aeme = aeme, model = model, path = path, verbose = T)
  
  outfiles <- get_model_outfile(aeme = aeme, model = model)
  testthat::expect_true(file.exists(outfiles$glm_aed))
})

test_that("GLM sediment parameters can be input and run with bgc", {
  path <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  aeme <- yaml_to_aeme(path = aeme_dir, "aeme.yaml")
  model_controls <- get_model_controls()
  model <- c("glm_aed")
  aeme <- build_aeme(path = path, aeme = aeme, model = model, ext_elev = 5,
                     model_controls = model_controls, use_bgc = TRUE)
  
  glm_pattern <- pattern <- paste0(
    "p_initial|p0|Xcc|",
    "R_growth|theta_growth|T_opt|T_max|",
    "I_K|KePHY|",
    "f_pr|R_resp|k_fres|k_fdom|",
    # "salTol|S_bep|S_opt|S_maxsp|",
    # "simDINUptake|simDONUptake|simNFixation|simINDynamics|",
    "K_N|",
    "K_P|",
    "w_p"
  )
  glm_phy_param <- AEME::get_aeme_parameters(model = "glm_aed",
                                             module = "phytoplankton",
                                             file = "aed_phyto_pars.csv") |> 
    # dplyr::select(dplyr::all_of(par_cols)) |> 
    dplyr::filter(grepl(glm_pattern, name)) |> 
    dplyr::mutate(
      value = 10,
      min = dplyr::case_when(
        value < 0 ~ value * 2,
        value > 0 ~ value * 0.1,
        TRUE ~ value * 0.5
      ),
      max = dplyr::case_when(
        value < 0 ~ value * 0.1,
        value > 0 ~ value * 2,
        TRUE ~ value * 1.5
      )
    )
  
  input_model_parameters(aeme = aeme, model = model, param = glm_phy_param,
                         path = path)
  n_zones <- get_glm_sed_zones(aeme = aeme)
  testthat::expect_equal(n_zones, 2)
  glm_sed_pars <- get_glm_sed_params(aeme = aeme)
  # One row per value across the &sediment block; the shipped glm4.nml
  # template carries more sediment keys than glm3.nml did (sed_heat_model,
  # sed_spinup_days, sed_deep_temp, ...), so this tracks that template.
  testthat::expect_equal(nrow(glm_sed_pars), 30)
  
  lake_dir <- get_lake_dir(aeme = aeme, path = path)
  glm_cfg <- read_model_config(model = model, lake_dir = lake_dir)
  n_vals <- sum(glm_cfg$bgc$aed_phyto_pars$cyano == 10)
  testthat::expect_equal(n_vals, nrow(glm_phy_param) / 3)
  
  
  aed_param <- AEME::get_aeme_parameters(model = "glm_aed",
                                             module = "oxygen",
                                             file = "aed.nml") |> 
    dplyr::mutate(value = 10)
  
  input_model_parameters(aeme = aeme, model = model, param = aed_param,
                         path = path)
  lake_dir <- get_lake_dir(aeme = aeme, path = path)
  glm_cfg <- read_model_config(model = model, lake_dir = lake_dir)
  testthat::expect_true(glm_cfg$bgc$aed$aed_oxygen$oxy_initial == 10)
  testthat::expect_true(glm_cfg$bgc$aed$aed_oxygen$ksed_oxy == 10)
})
  