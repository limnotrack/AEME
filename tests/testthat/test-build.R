test_that("building DYRESM-CAEDYM works", {
  library(AEME)
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  list.files(tmpdir, full.names = TRUE, recursive = TRUE)
  dir <- file.path(tmpdir, "lake")
  config <- configr::read.config(file.path(dir, "aeme.yaml"))
  mod_ctrls <- read.csv(file.path(dir, "model_controls.csv"))
  inf_factor = c("dy_cd" = 1)
  outf_factor = c("dy_cd" = 1)
  model <- c("dy_cd")
  build_ensemble(dir = dir, config = config, model = model,
                 mod_ctrls = mod_ctrls, inf_factor = inf_factor, ext_elev = 5,
                 use_bgc = FALSE, use_lw = TRUE)
  file_chk <- file.exists(file.path(dir, paste0(config$location$lake_id,"_",
                                                tolower(config$location$name)),
                                    model, "dyresm3p1.par"))
  testthat::expect_true(file_chk)
})

test_that("building GLM-AED works", {
  library(AEME)
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  list.files(tmpdir, full.names = TRUE, recursive = TRUE)
  dir <- file.path(tmpdir, "lake")
  config <- configr::read.config(file.path(dir, "aeme.yaml"))
  mod_ctrls <- read.csv(file.path(dir, "model_controls.csv"))
  inf_factor = c("glm_aed" = 1)
  outf_factor = c("glm_aed" = 1)
  model <- c("glm_aed")
  build_ensemble(dir = dir, config = config, model = model,
                 mod_ctrls = mod_ctrls, inf_factor = inf_factor, ext_elev = 5,
                 use_bgc = FALSE, use_lw = TRUE)
  file_chk <- file.exists(file.path(dir, paste0(config$location$lake_id,"_",
                                                tolower(config$location$name)),
                                    model, "glm3.nml"))
  testthat::expect_true(file_chk)
})
