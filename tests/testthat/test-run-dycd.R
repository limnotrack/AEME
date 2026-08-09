test_that("running DYRESM works", {
  skip_if_models_unavailable(c("dy_cd"))
  aeme_yaml <- system.file("extdata/lake/aeme.yaml", package = "AEME")
  aeme <- yaml_to_aeme(file = aeme_yaml)
  model_controls <- get_model_controls(use_bgc = F)
  model <- c("dy_cd")
  path <- tempdir()
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls,
                     ext_elev = 5, use_bgc = FALSE) |>
    run_aeme()
  file_chk <- check_all_model_outfiles(aeme)
  testthat::expect_true(file_chk)
  outp <- output(aeme)
  testthat::expect_true(!is.null(outp$ens_001$dy_cd))

  lake_dir <- get_lake_dir(aeme = aeme, path = path)
  outfile <- get_model_outfile(aeme = aeme, model = model)

  vars_sim <- "HYD_temp"
  out <- read_dy_output(file = outfile$dy_cd, vars_sim = vars_sim)
  testthat::expect_true(nrow(out$HYD_temp) > 2)
  out2 <- read_dy_output(file = outfile$dy_cd, vars_sim = "HYD_temp",
                         depths = c(0, 11))
  testthat::expect_true(nrow(out2$HYD_temp) == 2)
  testthat::expect_true(all(out2$HYD_temp[1, ] >= out2$HYD_temp[2, ]))
  out3 <- read_dy_output(file = outfile$dy_cd, vars_sim = "HYD_temp",
                         depths = c(0, 11), dates = c("2020-09-01", "2020-12-02"))
  testthat::expect_true(ncol(out3$HYD_temp) == 2)

})

test_that("running DYRESM-CAEDYM works", {
  aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
  aeme <- readRDS(aeme_file)
  path <- tempdir()
  model_controls <- get_model_controls(use_bgc = TRUE)
  model <- c("dy_cd")
  skip_if_models_unavailable(model)
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls,
                     ext_elev = 5, use_bgc = TRUE)
  aeme <- run_aeme(aeme = aeme, verbose = FALSE)

  outfile <- get_model_outfile(aeme = aeme)
  file_chk <- file.exists(outfile[["dy_cd"]])
  testthat::expect_true(file_chk)

  outp <- output(aeme)
  testthat::expect_true(!is.null(outp$ens_001$dy_cd))
})

test_that("running DYRESM with a spinup works", {
  skip_if_models_unavailable(c("dy_cd"))
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- get_model_controls()
  inf_factor <- c("dy_cd" = 1)
  outf_factor <- c("dy_cd" = 1)
  model <- c("dy_cd")

  # Add spin up time
  tim <- time(aeme)
  tim[["spin_up"]][[model]] <- 100
  time(aeme) <- tim

  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, inf_factor = inf_factor,
                     ext_elev = 5, use_bgc = FALSE)
  aeme <- run_aeme(aeme = aeme, model = model,
                   model_controls = model_controls, path = path)
  lke <- lake(aeme)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "DYsim.nc"))
  testthat::expect_true(file_chk)
})
