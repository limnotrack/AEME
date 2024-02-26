test_that("building DYRESM works", {
  library(AEME)
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  list.files(tmpdir, full.names = TRUE, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- yaml_to_aeme(path = path, file = "aeme.yaml")
  # aeme <- yaml_to_aeme(path = "inst/extdata/lake", "aeme.yaml")
  # config <- yaml::read_yaml("inst/extdata/lake/aeme.yaml")
  model_controls <- get_model_controls()
  inf_factor = c("dy_cd" = 1)
  outf_factor = c("dy_cd" = 1)
  model <- c("dy_cd")
  build_ensemble(path = path, aeme = aeme, model = model,
                 model_controls = model_controls, inf_factor = inf_factor, ext_elev = 5,
                 use_bgc = FALSE)
  lke <- lake(aeme)
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
  aeme <- yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- get_model_controls(use_bgc = TRUE)
  inf_factor = c("dy_cd" = 1)
  outf_factor = c("dy_cd" = 1)
  model <- c("dy_cd")
  build_ensemble(path = path, aeme = aeme, model = model,
                 model_controls = model_controls, inf_factor = inf_factor, ext_elev = 5,
                 use_bgc = TRUE)
  lke <- lake(aeme)
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
  aeme <- yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- get_model_controls()
  inf_factor = c("glm_aed" = 1)
  outf_factor = c("glm_aed" = 1)
  model <- c("glm_aed")
  build_ensemble(path = path, aeme = aeme, model = model,
                 model_controls = model_controls, inf_factor = inf_factor, ext_elev = 5,
                 use_bgc = FALSE)
  lke <- lake(aeme)
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
  aeme <- yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- get_model_controls(use_bgc = TRUE)
  inf_factor = c("glm_aed" = 1)
  outf_factor = c("glm_aed" = 1)
  model <- c("glm_aed")
  build_ensemble(path = path, aeme = aeme, model = model,
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
  build_ensemble(path = path, aeme = aeme, model = model,
                 model_controls = model_controls, inf_factor = inf_factor, ext_elev = 5,
                 use_bgc = FALSE)
  lke <- lake(aeme)
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
  aeme <- yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- get_model_controls(use_bgc = TRUE)
  inf_factor = c("gotm_wet" = 1)
  outf_factor = c("gotm_wet" = 1)
  model <- c("gotm_wet")
  build_ensemble(path = path, aeme = aeme, model = model,
                 model_controls = model_controls, inf_factor = inf_factor, ext_elev = 5,
                 use_bgc = TRUE)
  lke <- lake(aeme)
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
  aeme <- yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- get_model_controls(use_bgc = TRUE)
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("dy_cd", "glm_aed", "gotm_wet")
  build_ensemble(path = path, aeme = aeme, model = model,
                 model_controls = model_controls, inf_factor = inf_factor, ext_elev = 5,
                 use_bgc = TRUE)
  aeme <- load_configuration(model = model, aeme = aeme,
                                  path = path, use_bgc = TRUE)
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
  build_ensemble(path = path, aeme = aeme, model = model,
                 model_controls = model_controls, inf_factor = inf_factor, ext_elev = 5,
                 use_bgc = TRUE)
  aeme <- load_configuration(model = model, aeme = aeme,
                                  path = path, use_bgc = TRUE)

  path2 <- file.path(tmpdir, "lake-rewrite")
  aeme <- write_configuration(model = model, aeme = aeme,
                                   path = path2)

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
