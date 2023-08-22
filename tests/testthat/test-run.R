test_that("running DYRESM works", {
  library(AEME)
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  dir <- file.path(tmpdir, "lake")
  config <- configr::read.config(file.path(dir, "aeme.yaml"))
  mod_ctrls <- read.csv(file.path(dir, "model_controls.csv"))
  inf_factor = c("dy_cd" = 1)
  outf_factor = c("dy_cd" = 1)
  model <- c("dy_cd")
  build_ensemble(dir = dir, config = config, model = model,
                 mod_ctrls = mod_ctrls, inf_factor = inf_factor, ext_elev = 5,
                 use_bgc = FALSE, use_lw = TRUE)
  run_aeme(config = config, model = model, verbose = TRUE, dir = dir)

  file_chk <- file.exists(file.path(dir, paste0(config$location$lake_id,"_",
                                                tolower(config$location$name)),
                                    model, "DYsim.nc"))
  testthat::expect_true(file_chk)
})

test_that("running GLM works", {
  library(AEME)
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  dir <- file.path(tmpdir, "lake")
  config <- configr::read.config(file.path(dir, "aeme.yaml"))
  mod_ctrls <- read.csv(file.path(dir, "model_controls.csv"))
  inf_factor = c("glm_aed" = 1)
  outf_factor = c("glm_aed" = 1)
  model <- c("glm_aed")
  build_ensemble(dir = dir, config = config, model = model,
                 mod_ctrls = mod_ctrls, inf_factor = inf_factor, ext_elev = 5,
                 use_bgc = FALSE, use_lw = TRUE)
  run_aeme(config = config, model = model, verbose = TRUE, dir = dir)

  file_chk <- file.exists(file.path(dir, paste0(config$location$lake_id,"_",
                                                tolower(config$location$name)),
                                    model, "output", "output.nc"))
  testthat::expect_true(file_chk)
})

test_that("running GOTM works", {
  library(AEME)
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  dir <- file.path(tmpdir, "lake")
  config <- configr::read.config(file.path(dir, "aeme.yaml"))
  mod_ctrls <- read.csv(file.path(dir, "model_controls.csv"))
  inf_factor = c("gotm_wet" = 1)
  outf_factor = c("gotm_wet" = 1)
  model <- c("gotm_wet")
  build_ensemble(dir = dir, config = config, model = model,
                 mod_ctrls = mod_ctrls, inf_factor = inf_factor, ext_elev = 5,
                 use_bgc = FALSE, use_lw = TRUE)
  run_aeme(config = config, model = model, verbose = TRUE, dir = dir)

  file_chk <- file.exists(file.path(dir, paste0(config$location$lake_id,"_",
                                                tolower(config$location$name)),
                                    model, "output", "output.nc"))
  testthat::expect_true(file_chk)
})

test_that("running DYRESM-CAEDYM works", {
  library(AEME)
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  dir <- file.path(tmpdir, "lake")
  config <- configr::read.config(file.path(dir, "aeme.yaml"))
  mod_ctrls <- read.csv(file.path(dir, "model_controls.csv"))
  inf_factor = c("dy_cd" = 1)
  outf_factor = c("dy_cd" = 1)
  model <- c("dy_cd")
  build_ensemble(dir = dir, config = config, model = model,
                 mod_ctrls = mod_ctrls, inf_factor = inf_factor, ext_elev = 5,
                 use_bgc = TRUE, use_lw = TRUE)
  run_aeme(config = config, model = model, verbose = TRUE, dir = dir)

  file_chk <- file.exists(file.path(dir, paste0(config$location$lake_id,"_",
                                                tolower(config$location$name)),
                                    model, "DYsim.nc"))
  testthat::expect_true(file_chk)
})

test_that("running GLM-AED works", {
  library(AEME)
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  dir <- file.path(tmpdir, "lake")
  config <- configr::read.config(file.path(dir, "aeme.yaml"))
  mod_ctrls <- read.csv(file.path(dir, "model_controls.csv"))
  inf_factor = c("glm_aed" = 1)
  outf_factor = c("glm_aed" = 1)
  model <- c("glm_aed")
  build_ensemble(dir = dir, config = config, model = model,
                 mod_ctrls = mod_ctrls, inf_factor = inf_factor, ext_elev = 5,
                 use_bgc = TRUE, use_lw = TRUE)
  run_aeme(config = config, model = model, verbose = TRUE, dir = dir)

  file_chk <- file.exists(file.path(dir, paste0(config$location$lake_id,"_",
                                                tolower(config$location$name)),
                                    model, "output", "output.nc"))
  testthat::expect_true(file_chk)
})

test_that("running GOTM-WET works", {
  library(AEME)
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  dir <- file.path(tmpdir, "lake")
  config <- configr::read.config(file.path(dir, "aeme.yaml"))
  mod_ctrls <- read.csv(file.path(dir, "model_controls.csv"))
  inf_factor = c("gotm_wet" = 1)
  outf_factor = c("gotm_wet" = 1)
  model <- c("gotm_wet")
  build_ensemble(dir = dir, config = config, model = model,
                 mod_ctrls = mod_ctrls, inf_factor = inf_factor, ext_elev = 5,
                 use_bgc = FALSE, use_lw = TRUE)
  run_aeme(config = config, model = model, verbose = TRUE, dir = dir)

  file_chk <- file.exists(file.path(dir, paste0(config$location$lake_id,"_",
                                                tolower(config$location$name)),
                                    model, "output", "output.nc"))
  testthat::expect_true(file_chk)
})
