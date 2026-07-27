test_that("building DYRESM works", {
  skip_if_models_unavailable(c("dy_cd"))
  path <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  aeme <- yaml_to_aeme(path = aeme_dir, file = "aeme.yaml")
  model_controls <- get_model_controls()
  model <- c("dy_cd")
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls,
                     ext_elev = 5, use_bgc = FALSE)
  lke <- lake(aeme)
  lake_dir <- get_lake_dir(aeme = aeme, path = path)
  file_chk <- file.exists(file.path(lake_dir, model, "dyresm3p1.par"))
  testthat::expect_true(file_chk)
  
  file_chk <- file.exists(file.path(lake_dir, model, "wainamu.wdr"))
  testthat::expect_true(file_chk)
  
  file_chk <- file.exists(file.path(lake_dir, model, "wainamu.met"))
  testthat::expect_true(file_chk)
  
  file_chk <- file.exists(file.path(lake_dir, model, "wainamu.inf"))
  testthat::expect_true(file_chk)
})

test_that("building DYRESM-CAEDYM works", {
  skip_if_models_unavailable(c("dy_cd"))
  path <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  aeme <- yaml_to_aeme(path = aeme_dir, file = "aeme.yaml")
  model_controls <- get_model_controls(use_bgc = TRUE)
  inf_factor = c("dy_cd" = 1)
  outf_factor = c("dy_cd" = 1)
  model <- c("dy_cd")
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, 
                     ext_elev = 5, use_bgc = TRUE)
  lake_dir <- get_lake_dir(aeme = aeme, path = path)
  file_chk <- file.exists(file.path(lake_dir, model, "dyresm3p1.par"))
  testthat::expect_true(file_chk)
  
  model_controls2 <- get_model_controls(aeme = aeme)
  
  # test that model controls are equal
  testthat::expect_equal(model_controls, model_controls2)
  
})

test_that("building GLM works", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  list.files(tmpdir, full.names = TRUE, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- yaml_to_aeme(path = path, "aeme.yaml")
  
  wbal <- water_balance(aeme)
  testthat::expect_true(is.null(wbal$params))
  
  model_controls <- get_model_controls()
  model <- c("glm_aed")
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls,
                     ext_elev = 5, use_bgc = FALSE)
  
  # Test water balance
  wbal <- water_balance(aeme)
  testthat::expect_true(!is.null(wbal$params))
  
  wb_params1 <- get_wbal_param(aeme)
  
  aeme <- reset_wbal_param(aeme)
  wbal2 <- water_balance(aeme)
  testthat::expect_true(is.null(wbal2$params))
  
  aeme <- set_wbal_param(aeme, params = wb_params1)
  wbal3 <- water_balance(aeme)
  testthat::expect_equal(wbal3$params, wb_params1)
  
  lke <- lake(aeme)
  testthat::expect_true(is.character(lke$id))
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "glm3.nml"))
  testthat::expect_true(file_chk)
  
  obs <- observations(aeme)
  thmcln <- obs$lake |>
    dplyr::filter(var_aeme == "HYD_thmcln")
  testthat::expect_true(all(!is.na(thmcln$value)))
  
  tli4 <- obs$lake |>
    dplyr::filter(var_aeme == "LKE_tli4")
  testthat::expect_true(all(!is.na(tli4$value)))
  
  oxysat <- obs$lake |> 
    dplyr::filter(var_aeme == "CHM_oxysat")
  testthat::expect_true(all(!is.na(oxysat$value)))
})

test_that("building GLM-AED works", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  list.files(tmpdir, full.names = TRUE, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- get_model_controls(use_bgc = TRUE)
  model <- c("glm_aed")
  # path = "aeme"
  aeme <- build_aeme(path = path, aeme = aeme, model = model, 
                     model_controls = model_controls,
                     ext_elev = 5, use_bgc = TRUE)
  lke <- lake(aeme)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "aed", "aed.nml"))
  testthat::expect_true(file_chk)
})

test_that("building GLM with fixed outlets", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  list.files(tmpdir, full.names = TRUE, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- get_model_controls()
  inf_factor = c("glm_aed" = 1)
  outf_factor = c("glm_aed" = 1)
  model <- c("glm_aed")
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls,
                     ext_elev = 5, use_bgc = FALSE)
  aeme <- run_aeme(aeme)
  cfg <- configuration(aeme)
  cfg$glm_aed$hydrodynamic$outflow$outl_elvs
  out_file <- get_model_outfile(aeme = aeme)
  testthat::expect_true(file.exists(out_file$glm_aed))
  inp <- input(aeme)
  
  outf <- outflows(aeme)
  outf$elevation$outflow <- 11
  aeme <- add_outflows(aeme = aeme, elevation = outf$elevation)
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, 
                     ext_elev = 5, use_bgc = FALSE)
  aeme <- run_aeme(aeme)
  cfg2 <- configuration(aeme)
  testthat::expect_true(cfg$glm_aed$hydrodynamic$outflow$outl_elvs[1] >
                         cfg2$glm_aed$hydrodynamic$outflow$outl_elvs[1])
  
})

test_that("building GOTM works", {
  skip_if_models_unavailable(c("gotm_wet"))
  path <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  aeme <- yaml_to_aeme(path = aeme_dir, file = "aeme.yaml")
  model_controls <- get_model_controls()
  inf_factor = c("gotm_wet" = 1)
  outf_factor = c("gotm_wet" = 1)
  model <- c("gotm_wet")
  aeme <- build_aeme(path = path, aeme = aeme, model = model, 
                     model_controls = model_controls,  
                     ext_elev = 5, use_bgc = FALSE)
  lke <- lake(aeme)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "gotm.yaml"))
  testthat::expect_true(file_chk)
})

test_that("building GOTM-WET works", {
  skip_if_models_unavailable(c("gotm_wet"))
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- get_model_controls(use_bgc = TRUE)
  inf_factor = c("gotm_wet" = 1)
  outf_factor = c("gotm_wet" = 1)
  model <- c("gotm_wet")
  lake_dir <- get_lake_dir(aeme = aeme, path = path)
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, 
                     ext_elev = 5, use_bgc = TRUE, wb_method = 3)
  lke <- lake(aeme)
  file_chk <- file.exists(file.path(lake_dir, model, "fabm.yaml"))
  testthat::expect_true(file_chk)
  
  # Check inflow files are generated
  file_chk <- file.exists(file.path(lake_dir, model, "inputs",
                                    "inf_flow_inf_water_bal.dat"))
  testthat::expect_true(file_chk)
  
  # Check outflow files are generated
  file_chk <- file.exists(file.path(lake_dir, model, "inputs",
                                    "outf_outflow.dat"))
  testthat::expect_true(file_chk)
  
  # Check met file is generated
  file_chk <- file.exists(file.path(lake_dir, model, "inputs", "meteo.dat"))
  testthat::expect_true(file_chk)
  
})

test_that("building all models with minimum met variables", {
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  aeme <- yaml_to_aeme(path = aeme_dir, file = "aeme.yaml")
  path <- tempdir()
  req_met1 <- c("Date", "MET_tmpair", "MET_tmpdew", "MET_wnduvu", "MET_wnduvv", 
                "MET_pprain", "MET_radswd")
  inp <- input(aeme)
  met <- inp$met |> 
    dplyr::select(dplyr::all_of(req_met1))
  aeme <- add_met(aeme = aeme, met = met)
  model_controls <- get_model_controls(use_bgc = TRUE)
  model <- c("dy_cd", "glm_aed", "gotm_wet")
  skip_if_models_unavailable(model)
  aeme <- build_aeme(path = path, aeme = aeme, model = model, ext_elev = 3,
                     model_controls = model_controls, use_bgc = FALSE)
  
  cfg_upd <- cfg <- configuration(aeme)
  all_models <- list_models()
  for (m in all_models) {
    cfg_upd[[m]] <- NULL
  }
  configuration(aeme) <- cfg_upd
  cfg2 <- configuration(aeme)
  testthat::expect_equal(length(cfg2), 10)
  aeme <- load_configuration(aeme = aeme, model = model, path = path)
  cfg3 <- configuration(aeme)
  testthat::expect_equal(names(cfg), names(cfg3))
  
  lke <- lake(aeme)
  exp_met <- met |> 
    expand_met(lat = lke$latitude, lon = lke$longitude, elev = lke$elev)
  
  req_met2 <- c("Date", "MET_tmpair", "MET_humrel", "MET_wndspd", "MET_pprain",
                "MET_radswd")
  testthat::expect_true(all(req_met2 %in% colnames(exp_met)))
  met <- exp_met |> 
    dplyr::select(dplyr::all_of(req_met2))
  aeme <- add_met(aeme = aeme, met = met)
  aeme <- build_aeme(path = path, aeme = aeme, model = model, ext_elev = 3,
                     model_controls = model_controls, 
                     use_bgc = FALSE)
  inp <- input(aeme)
  met <- inp$meteo
  testthat::expect_true(all(req_met2 %in% colnames(met)))
})

test_that("building all models in a different dir", {
  tmpdir <- tempdir()
  path <- file.path(tmpdir, "lake")
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  aeme <- yaml_to_aeme(path = aeme_dir, file = "aeme.yaml")
  model_controls <- get_model_controls(use_bgc = TRUE)
  model <- c("dy_cd", "glm_aed", "gotm_wet")
  model <- filter_platform_models(model)
  aeme <- build_aeme(path = path, aeme = aeme, model = model, ext_elev = 3,
                     model_controls = model_controls, 
                     use_bgc = FALSE)
  files1 <- list.files(path, recursive = TRUE)
  testthat::expect_true(length(files1) > 0)
  
  path <- file.path(tmpdir, "lake_new")
  aeme <- build_aeme(path = path, aeme = aeme, model = model, ext_elev = 3,
                     model_controls = model_controls, 
                     use_bgc = FALSE, use_aeme = TRUE)
  
  files2 <- list.files(path, recursive = TRUE)
  testthat::expect_true(length(files2) > 0)
  testthat::expect_true(length(files1) >= length(files2))
  
})

test_that("building all models with the same hypsograph", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- get_model_controls(use_bgc = TRUE)
  model <- c("dy_cd", "glm_aed", "gotm_wet")
  skip_if_models_unavailable(model)
  aeme <- build_aeme(path = path, aeme = aeme, model = model, ext_elev = 3,
                     model_controls = model_controls, 
                     use_bgc = FALSE)
  
  inp <- input(aeme)
  lke <- lake(aeme)
  inp$init_depth
  lake_dir <- get_lake_dir(aeme = aeme, path = path)
  
  dy_hyps <- read_model_hypsograph(model = "dy_cd", lake_dir = lake_dir)
  glm_hyps <- read_model_hypsograph(model = "glm_aed", lake_dir = lake_dir)
  gotm_hyps <- read_model_hypsograph(model = "gotm_wet", lake_dir = lake_dir)
  
  testthat::expect_true(all(gotm_hyps$area %in% glm_hyps$area))
  testthat::expect_true(all(gotm_hyps$area %in% dy_hyps$area))
  testthat::expect_true(all(glm_hyps$area %in% dy_hyps$area))
  
  testthat::expect_true(all.equal(glm_hyps$depth, dy_hyps$depth))
  testthat::expect_true(all.equal(gotm_hyps$depth, dy_hyps$depth))
  testthat::expect_true(all.equal(glm_hyps$depth, gotm_hyps$depth))
})

test_that("can build all models with the generated hypsograph", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- get_model_controls(use_bgc = TRUE)
  model <- c("dy_cd", "glm_aed", "gotm_wet")
  skip_if_models_unavailable(model)
  
  hyps <- generate_hypsograph(aeme = aeme, ext_elev = 5,
                              volume_development = 1.2)
  inp <- input(aeme)
  inp$hypsograph <- hyps
  input(aeme) <- inp
  
  aeme <- build_aeme(path = path, aeme = aeme, model = model, ext_elev = 3,
                     model_controls = model_controls, 
                     use_bgc = FALSE)
  
  inp <- input(aeme)
  lke <- lake(aeme)
  inp$init_depth
  
  lake_dir <- get_lake_dir(aeme = aeme, path = path)
  dy_hyps <- read_model_hypsograph(model = "dy_cd", lake_dir = lake_dir)
  glm_hyps <- read_model_hypsograph(model = "glm_aed", lake_dir = lake_dir)
  gotm_hyps <- read_model_hypsograph(model = "gotm_wet", lake_dir = lake_dir)
  
  testthat::expect_true(all(gotm_hyps$area %in% glm_hyps$area))
  testthat::expect_true(all(gotm_hyps$area %in% dy_hyps$area))
  testthat::expect_true(all(glm_hyps$area %in% dy_hyps$area))
  
  testthat::expect_true(all(round(glm_hyps$depth, 2) %in%
                              round(hyps$depth, 2)))
  testthat::expect_true(all(round(gotm_hyps$depth, 2) %in%
                              round(dy_hyps$depth, 2)))
  testthat::expect_true(all(round(glm_hyps$depth, 2) %in%
                              round(gotm_hyps$depth, 2)))
})

test_that("building all models with same initial depth", {
  skip_if_models_unavailable(c("dy_cd", "glm_aed", "gotm_wet"))
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- get_model_controls(use_bgc = TRUE)
  model <- c("dy_cd", "glm_aed", "gotm_wet")
  aeme <- build_aeme(path = path, aeme = aeme, model = model, ext_elev = 3,
                     model_controls = model_controls, 
                     use_bgc = FALSE)
  
  inp <- input(aeme)
  lke <- lake(aeme)
  inp$init_depth
  dy_init <- readLines(file.path(path, paste0(lke$id, "_", lke$name), "dy_cd",
                                 "wainamu.stg"))
  dy_depth <- as.numeric(strsplit(dy_init[4], "#" )[[1]][1]) -
    as.numeric(strsplit(dy_init[7], "#" )[[1]][1])
  glm_init <- read_nml(file.path(path, paste0(lke$id, "_", lke$name), "glm_aed",
                                 "glm3.nml"))
  glm_depth <- glm_init$init_profiles$lake_depth
  gotm_init <- read.delim(file.path(path, paste0(lke$id, "_", lke$name),
                                    "gotm_wet", "inputs", "hypsograph.dat"),
                          header = FALSE)
  gotm_depth <- abs(min(gotm_init[, 1]))
  testthat::expect_equal(inp$init_depth, dy_depth)
  testthat::expect_equal(inp$init_depth, glm_depth)
  testthat::expect_equal(inp$init_depth, gotm_depth)
  
  inp$init_depth <- 10
  input(aeme) <- inp
  aeme <- build_aeme(path = path, aeme = aeme, model = model, ext_elev = 3,
                     model_controls = model_controls, 
                     use_bgc = FALSE)
  
  inp <- input(aeme)
  lke <- lake(aeme)
  inp$init_depth
  dy_init <- readLines(file.path(path, paste0(lke$id, "_", lke$name), "dy_cd",
                                 "wainamu.stg"))
  dy_depth <- as.numeric(strsplit(dy_init[4], "#" )[[1]][1]) -
    as.numeric(strsplit(dy_init[7], "#" )[[1]][1])
  glm_init <- read_nml(file.path(path, paste0(lke$id, "_", lke$name), "glm_aed",
                                 "glm3.nml"))
  glm_depth <- glm_init$init_profiles$lake_depth
  gotm_init <- read.delim(file.path(path, paste0(lke$id, "_", lke$name),
                                    "gotm_wet", "inputs", "hypsograph.dat"),
                          header = FALSE)
  gotm_depth <- abs(min(gotm_init[, 1]))
  testthat::expect_equal(inp$init_depth, dy_depth)
  testthat::expect_equal(inp$init_depth, glm_depth)
  testthat::expect_equal(inp$init_depth, gotm_depth)
  
})

test_that("building all models and loading to aeme works", {
  skip_if_models_unavailable(c("dy_cd", "glm_aed", "gotm_wet"))
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- get_model_controls(use_bgc = TRUE)
  model <- c("dy_cd", "glm_aed", "gotm_wet")
  build_aeme(path = path, aeme = aeme, model = model,
             model_controls = model_controls,  ext_elev = 5,
             use_bgc = TRUE)
  aeme <- load_configuration(model = model, aeme = aeme,
                             model_controls = model_controls, path = path)
  cfg <- configuration(aeme)
  mod_cfg_chk <- sapply(model, \(m) is.list(cfg[[m]]))
  mod_bgc_cfg_chk <- sapply(model, \(m) is.list(cfg[[m]][["bgc"]]))
  chk <- all(mod_cfg_chk) & (is.vector(cfg$dy_cd$bgc)) &
    all(mod_bgc_cfg_chk)
  
  testthat::expect_true(chk)
})

test_that("can build all models and write to new directory", {
  skip_if_models_unavailable(c("dy_cd", "glm_aed", "gotm_wet"))
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- get_model_controls()
  model <- c("dy_cd", "glm_aed", "gotm_wet")
  build_aeme(path = path, aeme = aeme, model = model,
             model_controls = model_controls, 
             ext_elev = 5, use_bgc = TRUE)
  aeme <- load_configuration(model = model, aeme = aeme,
                             path = path)
  
  path2 <- file.path(tmpdir, "lake-rewrite")
  aeme <- write_configuration(model = model, aeme = aeme,
                              path = path2)
  
  # Check DYRESM files
  lke <- lake(aeme)
  file_chk <- file.exists(file.path(path2, paste0(lke$id, "_",
                                                  tolower(lke$name)),
                                    "dy_cd", "dyresm3p1.par"))
  testthat::expect_true(file_chk)
  file_chk <- file.exists(file.path(path2, paste0(lke$id, "_",
                                                  tolower(lke$name)),
                                    "dy_cd", paste0(tolower(lke$name), ".con")))
  testthat::expect_true(file_chk)
  
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
                                    "gotm_wet", "output.yaml"))
  testthat::expect_true(file_chk)
  file_chk <- file.exists(file.path(path2, paste0(lke$id, "_",
                                                  tolower(lke$name)),
                                    "gotm_wet", "fabm.yaml"))
  testthat::expect_true(file_chk)
})

test_that("building all models with new parameters works", {
  skip_if_models_unavailable(c("dy_cd", "glm_aed", "gotm_wet"))
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- yaml_to_aeme(path = path, "aeme.yaml")
  
  data("aeme_parameters")
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
  
  model_controls <- get_model_controls(use_bgc = TRUE)
  model <- c("dy_cd", "glm_aed", "gotm_wet")
  aeme <- build_aeme(path = path, aeme = aeme, model = model, ext_elev = 5,
                     model_controls = model_controls, 
                     use_bgc = FALSE)
  
  lake_dir <- get_lake_dir(aeme = aeme, path = path)
  dy_cfg <- readLines(file.path(lake_dir, "dy_cd", "wainamu.cfg"))
  testthat::expect_true(as.numeric(substr(dy_cfg[7], 1, 2)) == 1)
  
  glm_cfg <- read_nml(file.path(lake_dir, "glm_aed", "glm3.nml"))
  testthat::expect_true(glm_cfg$light$Kw == 5)
  
  gotm_cfg <- yaml::read_yaml(file.path(lake_dir, "gotm_wet", "gotm.yaml"))
  testthat::expect_true(gotm_cfg$light_extinction$g2$constant_value == 5)
  
  aeme_parameters <- aeme_parameters |>
    dplyr::mutate(
      value = dplyr::case_when(
        name == "MET_radswd" ~ 0,
        .default = value
      )
    )
  
  parameters(aeme) <- aeme_parameters
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, 
                     use_bgc = FALSE)
  
  glm_met <- read.csv(file.path(lake_dir, "glm_aed", "bcs", "meteo_glm.csv"))
  testthat::expect_true(all(glm_met$ShortWave == 0))
  
  gotm_swr <- read.delim(file.path(lake_dir, "gotm_wet", "inputs",
                                   "meteo_swr.dat"), header = FALSE)
  testthat::expect_true(all(gotm_swr[, 2] == 0))
  
  dy_met <- read.delim(file.path(lake_dir, "dy_cd", "wainamu.met"),
                       header = FALSE, skip = 6)
  testthat::expect_true(all(dy_met[, 2] == 0))
})

test_that("building models with parameters for only one model", {
  skip_if_models_unavailable(c("dy_cd", "glm_aed", "gotm_wet"))
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- yaml_to_aeme(path = path, "aeme.yaml")
  
  data("aeme_parameters")
  aeme_parameters <- aeme_parameters |>
    dplyr::mutate(
      value = dplyr::case_when(
        model == "dy_cd" & name == "light_extinction_coefficient/7" ~ 1,
        model == "glm_aed" & name == "light/Kw" ~ 5,
        model == "gotm_wet" & name == "light_extinction/g2/constant_value" ~ 5,
        # name == "MET_radswd" ~ 0,
        .default = value
      )
    ) |>
    dplyr::filter(model == "glm_aed")
  
  parameters(aeme) <- aeme_parameters
  # parameters(aeme) <- aeme_parameters |>
  #   dplyr::filter( model == "glm")
  
  model_controls <- get_model_controls(use_bgc = TRUE)
  model <- c("dy_cd", "glm_aed", "gotm_wet")
  aeme <- build_aeme(path = path, aeme = aeme, model = model, ext_elev = 5,
                     model_controls = model_controls, 
                     use_bgc = FALSE)
  
  lake_dir <- get_lake_dir(aeme = aeme, path = path)
  glm_cfg <- read_nml(file.path(lake_dir, "glm_aed", "glm3.nml"))
  testthat::expect_true(glm_cfg$light$Kw == 5)
  
})

test_that("derived variables are in aeme object", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- get_model_controls()
  model <- c("glm_aed")
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, ext_elev = 5, use_bgc = FALSE)
  
  vars_chk <- c("HYD_temp", "HYD_strat", "HYD_thmcln", "CHM_oxycln")
  chk <- check_obs_var(aeme = aeme, var_sim = vars_chk)
  testthat::expect_true(length(chk$vars_present) == 4)
  testthat::expect_true(all(chk$obs$n > 0))
  
  obs <- observations(aeme)
  thmcln1 <- obs$lake |>
    dplyr::filter(var_aeme == "HYD_thmcln")
  testthat::expect_true(all(!is.na(thmcln1$value)))
  
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls,
                     ext_elev = 5, use_bgc = FALSE)
  
  obs <- observations(aeme)
  thmcln2 <- obs$lake |>
    dplyr::filter(var_aeme == "HYD_thmcln")
  testthat::expect_true(nrow(thmcln2) == nrow(thmcln1))
})

test_that("can update initial profile with obs", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  path <- tmpdir
  aeme <- yaml_to_aeme(path = aeme_dir, "aeme.yaml")
  model_controls <- get_model_controls()
  model <- c("dy_cd", "glm_aed", "gotm_wet")
  skip_if_models_unavailable(model)
  
  inp <- input(aeme)
  
  aeme <- update_init(aeme)
  mod_ctrls <- get_model_controls(aeme = aeme)
  inp2 <- input(aeme)
  testthat::expect_true(is.null(inp$init_profile))
  testthat::expect_true(is.data.frame(inp2$init_profile))
  
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls,
                     ext_elev = 5, use_bgc = FALSE)
  # obs <- get_obs(aeme = aeme)
  
  model_files <- get_model_config_files(aeme = aeme, model = model, path = path)
  testthat::expect_true(length(unlist(model_files)) > 0)
  testthat::expect_true(all(file.exists(unlist(model_files))))
  
  glm_nml <- read_nml(model_files$glm_aed["glm3"])
  testthat::expect_true(all(glm_nml$init_profiles$the_temps %in%
                              inp2$init_profile$temperature))
  
  lake_dir <- AEME::get_lake_dir(aeme = aeme, path = path)
  gotm_yaml <- yaml::read_yaml(model_files$gotm_wet["gotm"])
  init_tprof_file <- file.path(lake_dir, "gotm_wet", 
                               gotm_yaml$temperature$file)
  init_sprof_file <- file.path(lake_dir, "gotm_wet", 
                               gotm_yaml$salinity$file)
  gotm_tprof <- read_gotm_profile(init_tprof_file)
  gotm_sprof <- read_gotm_profile(init_sprof_file)
  testthat::expect_true(all(gotm_tprof$temperature %in%
                              inp2$init_profile$temperature))
  testthat::expect_true(all(abs(gotm_sprof$depth_m) %in%
                              inp2$init_profile$depth))
  testthat::expect_true(all(gotm_tprof$salinity %in%
                              inp2$init_profile$salinity))
  
  aeme <- run_aeme(aeme = aeme, model = model, path = path)
  
  lke <- lake(aeme)
  file_chk <- file.exists(file.path(lake_dir, "dy_cd", "DYsim.nc"))
  testthat::expect_true(file_chk)
  
  file_chk <- all(file.exists(file.path(lake_dir, model[-1], "output", 
                                        "output.nc")))
  testthat::expect_true(file_chk)
})
