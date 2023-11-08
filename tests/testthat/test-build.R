test_that("building DYRESM works", {
  library(AEME)
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  list.files(tmpdir, full.names = TRUE, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme_data <- yaml_to_aeme(path = path, file = "aeme.yaml")
  # aeme_data <- yaml_to_aeme(path = "inst/extdata/lake", "aeme.yaml")
  # config <- yaml::read_yaml("inst/extdata/lake/aeme.yaml")
  mod_ctrls <- read.csv(file.path(path, "model_controls.csv"))
  inf_factor = c("dy_cd" = 1)
  outf_factor = c("dy_cd" = 1)
  model <- c("dy_cd")
  build_ensemble(path = path, aeme_data = aeme_data, model = model,
                 mod_ctrls = mod_ctrls, inf_factor = inf_factor, ext_elev = 5,
                 use_bgc = FALSE)
  lke <- lake(aeme_data)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "dyresm3p1.par"))
  testthat::expect_true(file_chk)
})

test_that("building DYRESM-CAEDYM works", {
  library(AEME)
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  list.files(tmpdir, full.names = TRUE, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme_data <- yaml_to_aeme(path = path, "aeme.yaml")
  mod_ctrls <- read.csv(file.path(path, "model_controls.csv"))
  inf_factor = c("dy_cd" = 1)
  outf_factor = c("dy_cd" = 1)
  model <- c("dy_cd")
  build_ensemble(path = path, aeme_data = aeme_data, model = model,
                 mod_ctrls = mod_ctrls, inf_factor = inf_factor, ext_elev = 5,
                 use_bgc = TRUE)
  lke <- lake(aeme_data)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "dyresm3p1.par"))
  testthat::expect_true(file_chk)
})

test_that("building GLM works", {
  library(AEME)
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  list.files(tmpdir, full.names = TRUE, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme_data <- yaml_to_aeme(path = path, "aeme.yaml")
  mod_ctrls <- read.csv(file.path(path, "model_controls.csv"))
  inf_factor = c("glm_aed" = 1)
  outf_factor = c("glm_aed" = 1)
  model <- c("glm_aed")
  build_ensemble(path = path, aeme_data = aeme_data, model = model,
                 mod_ctrls = mod_ctrls, inf_factor = inf_factor, ext_elev = 5,
                 use_bgc = FALSE)
  lke <- lake(aeme_data)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "glm3.nml"))
  testthat::expect_true(file_chk)
})

test_that("building GLM-AED works", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  list.files(tmpdir, full.names = TRUE, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme_data <- yaml_to_aeme(path = path, "aeme.yaml")
  mod_ctrls <- read.csv(file.path(path, "model_controls.csv"))
  inf_factor = c("glm_aed" = 1)
  outf_factor = c("glm_aed" = 1)
  model <- c("glm_aed")
  build_ensemble(path = path, aeme_data = aeme_data, model = model,
                 mod_ctrls = mod_ctrls, inf_factor = inf_factor, ext_elev = 5,
                 use_bgc = TRUE)
  lke <- lake(aeme_data)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "aed2", "aed2.nml"))
  testthat::expect_true(file_chk)
})

test_that("building GOTM works", {
  library(AEME)
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  list.files(tmpdir, full.names = TRUE, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme_data <- yaml_to_aeme(path = path, file = "aeme.yaml")
  mod_ctrls <- read.csv(file.path(path, "model_controls.csv"))
  inf_factor = c("gotm_wet" = 1)
  outf_factor = c("gotm_wet" = 1)
  model <- c("gotm_wet")
  build_ensemble(path = path, aeme_data = aeme_data, model = model,
                 mod_ctrls = mod_ctrls, inf_factor = inf_factor, ext_elev = 5,
                 use_bgc = FALSE)
  lke <- lake(aeme_data)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "gotm.yaml"))
  testthat::expect_true(file_chk)
})

test_that("building GOTM-WET works", {
  library(AEME)
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  list.files(tmpdir, full.names = TRUE, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme_data <- yaml_to_aeme(path = path, "aeme.yaml")
  mod_ctrls <- read.csv(file.path(path, "model_controls.csv"))
  inf_factor = c("gotm_wet" = 1)
  outf_factor = c("gotm_wet" = 1)
  model <- c("gotm_wet")
  build_ensemble(path = path, aeme_data = aeme_data, model = model,
                 mod_ctrls = mod_ctrls, inf_factor = inf_factor, ext_elev = 5,
                 use_bgc = TRUE)
  lke <- lake(aeme_data)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "fabm.yaml"))
  testthat::expect_true(file_chk)
})

test_that("building all models and loading to aeme works", {
  library(AEME)
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
  build_ensemble(path = path, aeme_data = aeme_data, model = model,
                 mod_ctrls = mod_ctrls, inf_factor = inf_factor, ext_elev = 5,
                 use_bgc = TRUE)
  aeme_data <- load_configuration(model = model, aeme_data = aeme_data,
                                  path = path, use_bgc = TRUE)
  cfg <- configuration(aeme_data)
  chk <- all(sapply(cfg$physical, is.list)) & (is.vector(cfg$bgc$dy_cd)) &
    all(sapply(cfg$bgc[2:3], is.list))

  testthat::expect_true(chk)
})
