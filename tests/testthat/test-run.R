test_that("running DYRESM works", {
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
  aeme_data <- build_ensemble(path = path, aeme_data = aeme_data, model = model,
                              mod_ctrls = mod_ctrls, inf_factor = inf_factor,
                              ext_elev = 5, use_bgc = FALSE, use_lw = TRUE)
  aeme_data <- run_aeme(aeme_data = aeme_data, model = model, verbose = TRUE,
                        mod_ctrls = mod_ctrls, path = path)
  lke <- lake(aeme_data)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "DYsim.nc"))
  testthat::expect_true(file_chk)
})

test_that("running GLM works", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme_data <- yaml_to_aeme(path = path, "aeme.yaml")
  mod_ctrls <- read.csv(file.path(path, "model_controls.csv"))
  inf_factor <- c("glm_aed" = 1)
  outf_factor <- c("glm_aed" = 1)
  model <- c("glm_aed")
  aeme_data <- build_ensemble(path = path, aeme_data = aeme_data, model = model,
                              mod_ctrls = mod_ctrls, inf_factor = inf_factor,
                              ext_elev = 5, use_bgc = FALSE, use_lw = TRUE)
  aeme_data <- run_aeme(aeme_data = aeme_data, model = model, verbose = TRUE,
                        mod_ctrls = mod_ctrls, path = path)
  lke <- lake(aeme_data)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "output", "output.nc"))
  testthat::expect_true(file_chk)
})

test_that("running GOTM works", {
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
  aeme_data <- build_ensemble(path = path, aeme_data = aeme_data,
                              model = model, mod_ctrls = mod_ctrls,
                              inf_factor = inf_factor, ext_elev = 5,
                              use_bgc = FALSE, use_lw = TRUE)
  aeme_data <- run_aeme(aeme_data = aeme_data, model = model, verbose = TRUE,
                        mod_ctrls = mod_ctrls, path = path)
  lke <- lake(aeme_data)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "output", "output.nc"))
  testthat::expect_true(file_chk)
})

test_that("running DYRESM-CAEDYM works", {
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
  aeme_data <- build_ensemble(path = path, aeme_data = aeme_data, model = model,
                              mod_ctrls = mod_ctrls, inf_factor = inf_factor,
                              ext_elev = 5, use_bgc = TRUE, use_lw = TRUE)
  aeme_data <- run_aeme(aeme_data = aeme_data, model = model, verbose = TRUE,
                        mod_ctrls = mod_ctrls, path = path)
  lke <- lake(aeme_data)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "DYsim.nc"))
  testthat::expect_true(file_chk)
})

test_that("running GLM-AED works", {
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
  aeme_data <- build_ensemble(path = path, aeme_data = aeme_data, model = model,
                              mod_ctrls = mod_ctrls, inf_factor = inf_factor,
                              ext_elev = 5, use_bgc = TRUE, use_lw = TRUE)
  aeme_data <- run_aeme(aeme_data = aeme_data, model = model, verbose = TRUE,
                        mod_ctrls = mod_ctrls, path = path)
  lke <- lake(aeme_data)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "output", "output.nc"))
  testthat::expect_true(file_chk)
})

test_that("running GOTM-WET works", {
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
  aeme_data <- build_ensemble(path = path, aeme_data = aeme_data, model = model,
                              mod_ctrls = mod_ctrls, inf_factor = inf_factor,
                              ext_elev = 5, use_bgc = TRUE, use_lw = TRUE)
  aeme_data <- run_aeme(aeme_data = aeme_data, model = model, verbose = TRUE,
                        mod_ctrls = mod_ctrls, path = path)
  lke <- lake(aeme_data)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "output", "output.nc"))
  testthat::expect_true(file_chk)
})

test_that("running models in parallel works", {
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
  aeme_data <- run_aeme(aeme_data = aeme_data, model = model, verbose = TRUE,
                        mod_ctrls = mod_ctrls, path = path, parallel = TRUE)
  lke <- lake(aeme_data)
  file_chk <- all(file.exists(file.path(path, paste0(lke$id, "_",
                                                     tolower(lke$name)),
                                        model, "output", "output.nc")))
  testthat::expect_true(file_chk)
})

test_that("getting model output works", {
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
           parallel = TRUE, return = FALSE)

  aeme_data <- load_output(model = model, aeme_data = aeme_data, path = path,
                           mod_ctrls = mod_ctrls, parallel = FALSE)

  outp <- output(aeme_data)
  output_chk <- !all(is.null(unlist(outp)))
  testthat::expect_true(output_chk)
})

test_that("getting model output in parallel works", {
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
  aeme_data <- run_aeme(aeme_data = aeme_data, model = model, verbose = FALSE,
                        mod_ctrls = mod_ctrls, path = path, parallel = TRUE)

  outp <- output(aeme_data)
  output_chk <- !all(is.null(unlist(outp)))
  testthat::expect_true(output_chk)
})

test_that("plotting model output works", {
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
  aeme_data <- build_ensemble(path = path, aeme_data = aeme_data, model = model,
                              mod_ctrls = mod_ctrls, inf_factor = inf_factor,
                              ext_elev = 5, use_bgc = FALSE, use_lw = TRUE)
  aeme_data <- run_aeme(aeme_data = aeme_data, model = model, verbose = FALSE,
                        path = path, mod_ctrls = mod_ctrls, parallel = TRUE)

  p1 <- plot_output(aeme_data = aeme_data, model = model, var_sim = "HYD_temp",
                    level = TRUE, label = TRUE, print_plots = FALSE,
                    var_lims = c(0, 30), ylim = c(0, 16))
  testthat::expect_true(all(ggplot2::is.ggplot(p1[[1]]),
                            ggplot2::is.ggplot(p1[[2]])))

  p2 <- plot_output(aeme_data = aeme_data, model = model, var_sim = "HYD_evap",
                    print_plots = TRUE, ylim = c(0, 0.02))
  testthat::expect_true(ggplot2::is.ggplot(p2))

  p3 <- plot_output(aeme_data = aeme_data, model = model, var_sim = "HYD_wlev",
                    print_plots = TRUE)
  testthat::expect_true(ggplot2::is.ggplot(p2))
})
