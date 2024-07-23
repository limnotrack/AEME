test_that("building DYRESM works", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- yaml_to_aeme(path = path, file = "aeme.yaml")
  # aeme <- yaml_to_aeme(path = "inst/extdata/lake", "aeme.yaml")
  # config <- yaml::read_yaml("inst/extdata/lake/aeme.yaml")
  model_controls <- get_model_controls()
  inf_factor = c("dy_cd" = 1)
  outf_factor = c("dy_cd" = 1)
  model <- c("dy_cd")
  build_aeme(path = path, aeme = aeme, model = model,
             model_controls = model_controls, inf_factor = inf_factor, ext_elev = 5,
             use_bgc = FALSE)
  lke <- lake(aeme)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "dyresm3p1.par"))
  testthat::expect_true(file_chk)

  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "wainamu.wdr"))
  testthat::expect_true(file_chk)

  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "wainamu.met"))
  testthat::expect_true(file_chk)

  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "wainamu.inf"))
  testthat::expect_true(file_chk)

})

test_that("building DYRESM-CAEDYM works", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  list.files(tmpdir, full.names = TRUE, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- get_model_controls(use_bgc = TRUE)
  inf_factor = c("dy_cd" = 1)
  outf_factor = c("dy_cd" = 1)
  model <- c("dy_cd")
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, inf_factor = inf_factor, ext_elev = 5,
                     use_bgc = TRUE)
  lke <- lake(aeme)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "dyresm3p1.par"))
  testthat::expect_true(file_chk)

  model_controls2 <- get_model_controls(aeme = aeme)

  # test that model controls are equal
  testthat::expect_equal(model_controls, model_controls2)

})

test_that("building GLM works", {
  library(AEME)
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
                     model_controls = model_controls, inf_factor = inf_factor,
                     ext_elev = 5, use_bgc = FALSE)
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
  inf_factor = c("glm_aed" = 1)
  outf_factor = c("glm_aed" = 1)
  model <- c("glm_aed")
  build_aeme(path = path, aeme = aeme, model = model,
             model_controls = model_controls, inf_factor = inf_factor, ext_elev = 5,
             use_bgc = TRUE)
  lke <- lake(aeme)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "aed2", "aed2.nml"))
  testthat::expect_true(file_chk)
})

test_that("building GOTM works", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  list.files(tmpdir, full.names = TRUE, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- yaml_to_aeme(path = path, file = "aeme.yaml")
  model_controls <- get_model_controls()
  inf_factor = c("gotm_wet" = 1)
  outf_factor = c("gotm_wet" = 1)
  model <- c("gotm_wet")
  build_aeme(path = path, aeme = aeme, model = model,
             model_controls = model_controls, inf_factor = inf_factor, ext_elev = 5,
             use_bgc = FALSE)
  lke <- lake(aeme)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "gotm.yaml"))
  testthat::expect_true(file_chk)
})

test_that("building GOTM-WET works", {
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
  wb <- water_balance(aeme)
  wb$method <- 3
  water_balance(aeme) <- wb
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, inf_factor = inf_factor,
                     ext_elev = 5, use_bgc = TRUE)
  lke <- lake(aeme)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "fabm.yaml"))
  testthat::expect_true(file_chk)

  # Check inflow files are generated
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "inputs", "inf_flow_wbal_in.dat"))
  testthat::expect_true(file_chk)

  # Check outflow files are generated
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "inputs", "outf_outflow.dat"))
  testthat::expect_true(file_chk)

  # Check met file is generated
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "inputs", "meteo.dat"))
  testthat::expect_true(file_chk)

})

test_that("building all models with same initial depth", {
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
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, inf_factor = inf_factor,
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
  build_aeme(path = path, aeme = aeme, model = model,
             model_controls = model_controls, inf_factor = inf_factor, ext_elev = 5,
             use_bgc = TRUE)
  aeme <- load_configuration(model = model, aeme = aeme,
                             model_controls = model_controls, path = path,
                             use_bgc = TRUE)
  cfg <- configuration(aeme)
  chk <- all(sapply(cfg, is.list)) & (is.vector(cfg$dy_cd$ecosystem)) &
    all(sapply(cfg[2:3],\(x) is.list(x[["ecosystem"]])))

  testthat::expect_true(chk)
})

test_that("can build all models and write to new directory", {
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
  build_aeme(path = path, aeme = aeme, model = model,
             model_controls = model_controls, inf_factor = inf_factor, ext_elev = 5,
             use_bgc = TRUE)
  aeme <- load_configuration(model = model, aeme = aeme,
                             path = path, use_bgc = TRUE)

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
                                    "glm_aed", "aed2", "aed2.nml"))
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

  model_controls <- get_model_controls(use_bgc = TRUE)
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("dy_cd", "glm_aed", "gotm_wet")
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, inf_factor = inf_factor,
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
                     model_controls = model_controls, inf_factor = inf_factor,
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

test_that("derived variables are in aeme object", {
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
                     model_controls = model_controls, inf_factor = inf_factor,
                     ext_elev = 5, use_bgc = FALSE)

  obs <- observations(aeme)
  thmcln1 <- obs$lake |>
    dplyr::filter(var_aeme == "HYD_thmcln")
  testthat::expect_true(all(!is.na(thmcln1$value)))

  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, inf_factor = inf_factor,
                     ext_elev = 5, use_bgc = FALSE)

  obs <- observations(aeme)
  thmcln2 <- obs$lake |>
    dplyr::filter(var_aeme == "HYD_thmcln")
  testthat::expect_true(nrow(thmcln2) == nrow(thmcln1))
})
