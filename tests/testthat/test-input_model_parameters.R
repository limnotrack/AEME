test_that("GLM parameters can be input", {
  path <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  aeme <- yaml_to_aeme(path = aeme_dir, "aeme.yaml")
  model_controls <- get_model_controls()
  model <- c("glm_aed")
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls)
  # Get parameters for calibration
  utils::data("aeme_parameters", package = "AEME")
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
  nml <- read_nml(cfg_files$glm_aed["glm3"])
  kw_value <- nml$light$Kw
  testthat::expect_equal(kw_value, 1.5)
})

test_that("GLM-AED parameters can be input", {
  path <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  aeme <- yaml_to_aeme(path = aeme_dir, "aeme.yaml")
  model_controls <- get_model_controls()
  model <- c("glm_aed")
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, use_bgc = TRUE)
  # Get parameters for calibration
  utils::data("aeme_parameters", package = "AEME")
  phy_param <- AEME::get_aeme_parameters(model = model,
                                         module = "phytoplankton")
  phy_param <- phy_param |>
    dplyr::mutate(
      value = dplyr::case_when(
        name == "phyto_data/pd%R_growth" ~ 2.0,
        TRUE ~ value
      )
    )
  param <- dplyr::bind_rows(aeme_parameters, phy_param)
  input_model_parameters(aeme = aeme, model = model, param = param, path = path)
  cfg_files <- get_model_config_files(aeme = aeme, model = model, path = path)
  nml <- read_nml(cfg_files$glm_aed["aed2_phyto_pars"])
  testthat::expect_equal(sum(nml$phyto_data[["pd%R_growth"]] == 2.0), 3)
})

test_that("GLM sediment parameters can be input", {
  path <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  aeme <- yaml_to_aeme(path = aeme_dir, "aeme.yaml")
  model_controls <- get_model_controls()
  model <- c("glm_aed")
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls)
  sed_params <- glm_sed_params(n_zones = 1, sed_temp_mean = 16.5)
  input_model_parameters(aeme = aeme, model = model, param = sed_params,
                         path = path)
  cfg_files <- get_model_config_files(aeme = aeme, model = model, path = path)
  nml <- read_nml(cfg_files$glm_aed["glm3"])
  sed_temp <- get_nml_value(nml, "sed_temp_mean")
  testthat::expect_equal(sed_temp, 16.5)
})

test_that("GLM sediment parameters can be input and run", {
  path <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  aeme <- yaml_to_aeme(path = aeme_dir, "aeme.yaml")
  model_controls <- get_model_controls()
  model <- c("glm_aed")
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls)
  sed_params <- glm_sed_params(n_zones = 2, zone_heights = c(5, 14))
  input_model_parameters(aeme = aeme, model = model, param = sed_params,
                         path = path)
  cfg_files <- get_model_config_files(aeme = aeme, model = model, path = path)
  nml <- read_nml(cfg_files$glm_aed["glm3"])
  zone_heights <- get_nml_value(nml, "zone_heights")
  testthat::expect_equal(zone_heights, c(5, 14))
  
  aeme <- run_aeme(aeme = aeme, model = model, path = path)
  
  outfiles <- get_model_outfile(aeme = aeme, model = model, path = path)
  testthat::expect_true(file.exists(outfiles$glm_aed))
  
  sed_params <- glm_sed_params(n_zones = 3, zone_heights = c(5, 10, 14), 
                               sed_temp_mean = c(10, 12, 16))
  input_model_parameters(aeme = aeme, model = model, param = sed_params,
                         path = path)
  cfg_files <- get_model_config_files(aeme = aeme, model = model, path = path)
  nml <- read_nml(cfg_files$glm_aed["glm3"])
  zone_heights <- get_nml_value(nml, "zone_heights")
  testthat::expect_equal(zone_heights, c(5, 10, 14))
  
  aeme <- run_aeme(aeme = aeme, model = model, path = path)
  
  outfiles <- get_model_outfile(aeme = aeme, model = model, path = path)
  testthat::expect_true(file.exists(outfiles$glm_aed))
})
