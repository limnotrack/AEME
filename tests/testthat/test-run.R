test_that("running DYRESM works", {
  library(AEME)
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
  build_ensemble(path = path, aeme_data = aeme_data, model = model,
                 mod_ctrls = mod_ctrls, inf_factor = inf_factor, ext_elev = 5,
                 use_bgc = FALSE, use_lw = TRUE)
  run_aeme(aeme_data = aeme_data, model = model, verbose = TRUE, path = path)

  file_chk <- file.exists(file.path(path, paste0(aeme_data@lake$id,"_",
                                                tolower(aeme_data@lake$name)),
                                    model, "DYsim.nc"))
  testthat::expect_true(file_chk)
})

test_that("running GLM works", {
  library(AEME)
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
  build_ensemble(path = path, aeme_data = aeme_data, model = model,
                 mod_ctrls = mod_ctrls, inf_factor = inf_factor, ext_elev = 5,
                 use_bgc = FALSE, use_lw = TRUE)
  run_aeme(aeme_data = aeme_data, model = model, verbose = TRUE, path = path)

  file_chk <- file.exists(file.path(path, paste0(aeme_data@lake$id,"_",
                                                tolower(aeme_data@lake$name)),
                                    model, "output", "output.nc"))
  testthat::expect_true(file_chk)
})

test_that("running GOTM works", {
  library(AEME)
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
  build_ensemble(path = path, aeme_data = aeme_data, model = model,
                 mod_ctrls = mod_ctrls, inf_factor = inf_factor, ext_elev = 5,
                 use_bgc = FALSE, use_lw = TRUE)
  run_aeme(aeme_data = aeme_data, model = model, verbose = TRUE, path = path)

  file_chk <- file.exists(file.path(path, paste0(aeme_data@lake$id,"_",
                                                tolower(aeme_data@lake$name)),
                                    model, "output", "output.nc"))
  testthat::expect_true(file_chk)
})

test_that("running DYRESM-CAEDYM works", {
  library(AEME)
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
  build_ensemble(path = path, aeme_data = aeme_data, model = model,
                 mod_ctrls = mod_ctrls, inf_factor = inf_factor, ext_elev = 5,
                 use_bgc = TRUE, use_lw = TRUE)
  run_aeme(aeme_data = aeme_data, model = model, verbose = TRUE, path = path)

  file_chk <- file.exists(file.path(path, paste0(aeme_data@lake$id,"_",
                                                tolower(aeme_data@lake$name)),
                                    model, "DYsim.nc"))
  testthat::expect_true(file_chk)
})

test_that("running GLM-AED works", {
  library(AEME)
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
  build_ensemble(path = path, aeme_data = aeme_data, model = model,
                 mod_ctrls = mod_ctrls, inf_factor = inf_factor, ext_elev = 5,
                 use_bgc = TRUE, use_lw = TRUE)
  run_aeme(aeme_data = aeme_data, model = model, verbose = TRUE, path = path)

  file_chk <- file.exists(file.path(path, paste0(aeme_data@lake$id,"_",
                                                tolower(aeme_data@lake$name)),
                                    model, "output", "output.nc"))
  testthat::expect_true(file_chk)
})

test_that("running GOTM-WET works", {
  library(AEME)
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
  build_ensemble(path = path, aeme_data = aeme_data, model = model,
                 mod_ctrls = mod_ctrls, inf_factor = inf_factor, ext_elev = 5,
                 use_bgc = FALSE, use_lw = TRUE)
  run_aeme(aeme_data = aeme_data, model = model, verbose = TRUE, path = path)

  file_chk <- file.exists(file.path(path, paste0(aeme_data@lake$id,"_",
                                                tolower(aeme_data@lake$name)),
                                    model, "output", "output.nc"))
  testthat::expect_true(file_chk)
})

test_that("running models in parallel works", {
  library(AEME)
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme_data <- yaml_to_aeme(path = path, "aeme.yaml")
  mod_ctrls <- read.csv(file.path(path, "model_controls.csv"))
  inf_factor = c("gotm_wet" = 1)
  outf_factor = c("gotm_wet" = 1)
  model <- c("glm_aed", "gotm_wet")
  build_ensemble(path = path, aeme_data = aeme_data, model = model,
                 mod_ctrls = mod_ctrls, inf_factor = inf_factor, ext_elev = 5,
                 use_bgc = TRUE, use_lw = TRUE)
  run_aeme(aeme_data = aeme_data, model = model, verbose = TRUE, path = path,
           parallel = TRUE)

  file_chk <- all(file.exists(file.path(path, paste0(aeme_data@lake$id,"_",
                                                    tolower(aeme_data@lake$name)),
                                        model, "output", "output.nc")))
  testthat::expect_true(file_chk)
})

test_that("getting model output works", {
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
  model <- c("glm_aed", "gotm_wet")
  build_ensemble(path = path, aeme_data = aeme_data, model = model,
                 mod_ctrls = mod_ctrls, inf_factor = inf_factor, ext_elev = 5,
                 use_bgc = TRUE, use_lw = TRUE)
  run_aeme(aeme_data = aeme_data, model = model, verbose = TRUE, path = path,
           parallel = TRUE)

  aeme_data <- load_output(model = model, aeme_data = aeme_data, path = path,
                          mod_ctrls = mod_ctrls, parallel = FALSE)

  output_chk <- !all(is.null(unlist(aeme_data@output)))
  testthat::expect_true(output_chk)
})

test_that("getting model output in parallel works", {
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
  model <- c("glm_aed", "gotm_wet")
  build_ensemble(path = path, aeme_data = aeme_data, model = model,
                 mod_ctrls = mod_ctrls, inf_factor = inf_factor, ext_elev = 5,
                 use_bgc = TRUE, use_lw = TRUE)
  run_aeme(aeme_data = aeme_data, model = model, verbose = TRUE, path = path,
           parallel = TRUE)

  aeme_data <- load_output(model = model, aeme_data = aeme_data, path = path,
                          mod_ctrls = mod_ctrls, parallel = TRUE)

  output_chk <- !all(is.null(unlist(aeme_data@output)))
  testthat::expect_true(output_chk)
})
