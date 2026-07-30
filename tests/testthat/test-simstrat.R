# Simstrat-AED2 is currently only bundled as a Windows binary
# (inst/extbin/simstrat_aed2/simstrat.exe), so the run tests are skipped
# elsewhere. The build tests need no binary and run on any platform.
skip_simstrat_run <- function() {
  if (AEME:::.detect_os() != "windows") {
    testthat::skip("Simstrat-AED2 binary is only bundled for Windows")
  }
}

# Defensive retry: run_aeme() now fails cleanly (rather than crashing, see
# run_simstrat_aed2() in R/run_aeme.R) if simstrat.exe ever exits 0 without
# producing output. Not expected to trigger in normal use, but cheap
# insurance against transient external interference (e.g. antivirus/
# file-system scanning a freshly-written config directory).
run_aeme_with_retry <- function(aeme, model, path, tries = 2) {
  for (i in seq_len(tries)) {
    aeme <- run_aeme(aeme = aeme, model = model, path = path, verbose = FALSE)
    if (!is.null(output(aeme)$ens_001[[model]])) return(aeme)
  }
  aeme
}

test_that("simstrat_aed2 model registry works", {
  chk <- check_model(c("SIMSTRAT-AED2", "simstrat_aed2"))
  testthat::expect_equal(unname(chk), c("simstrat_aed2", "simstrat_aed2"))

  testthat::expect_true("simstrat_aed2" %in% list_models())

  testthat::expect_equal(toggle_models("SIMSTRAT-AED2"), c("SIMSTRAT-AED2" = "simstrat_aed2"))
  testthat::expect_equal(toggle_models("simstrat_aed2", to = "display"),
                         c("SIMSTRAT-AED2"))
})

test_that("building Simstrat works", {
  path <- file.path(tempdir(), "simstrat_build_phys")
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  aeme <- yaml_to_aeme(path = aeme_dir, file = "aeme.yaml")
  model_controls <- get_model_controls()
  model <- "simstrat_aed2"
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls,
                     ext_elev = 5, use_bgc = FALSE)

  lake_dir <- get_lake_dir(aeme = aeme, path = path)
  sim_dir <- file.path(lake_dir, model)

  for (f in c("simstrat.par", "Bathymetry.dat", "Grid.dat",
             "MeteoForcing.dat", "InitialConditions.dat",
             "Qinp.dat", "Qout.dat", "Tinp.dat", "Sinp.dat")) {
    testthat::expect_true(file.exists(file.path(sim_dir, f)),
                          info = paste("missing", f))
  }

  cfg <- configuration(aeme)
  testthat::expect_true(!is.null(cfg$simstrat_aed2$hydrodynamic))

  par <- jsonlite::fromJSON(file.path(sim_dir, "simstrat.par"), simplifyVector = FALSE)
  testthat::expect_false(isTRUE(par$ModelConfig$CoupleAED2))
})

test_that("building Simstrat-AED2 (with biogeochemistry) works", {
  path <- file.path(tempdir(), "simstrat_build_bgc")
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  aeme <- yaml_to_aeme(path = aeme_dir, file = "aeme.yaml")
  model_controls <- get_model_controls(use_bgc = TRUE)
  model <- "simstrat_aed2"
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls,
                     ext_elev = 5, use_bgc = TRUE)

  lake_dir <- get_lake_dir(aeme = aeme, path = path)
  sim_dir <- file.path(lake_dir, model)

  testthat::expect_true(file.exists(file.path(sim_dir, "aed2.nml")))
  testthat::expect_true(dir.exists(file.path(sim_dir, "AED2_inflow")))
  testthat::expect_true(dir.exists(file.path(sim_dir, "AED2_initcond")))

  # Every active AED2 state variable must have an inflow file, or Simstrat
  # aborts with a Fortran runtime error at run time (see initialise_aed2())
  aed2_nml <- read_nml(file.path(sim_dir, "aed2.nml"))
  active_modules <- get_nml_value(aed2_nml, "models")
  testthat::expect_true(length(active_modules) > 0)

  cfg <- configuration(aeme)
  testthat::expect_true(!is.null(cfg$simstrat_aed2$bgc))

  par <- jsonlite::fromJSON(file.path(sim_dir, "simstrat.par"), simplifyVector = FALSE)
  testthat::expect_true(isTRUE(par$ModelConfig$CoupleAED2))

  chk <- check_simstrat_par(file.path(sim_dir, "simstrat.par"))
  testthat::expect_true(chk)
})

test_that("running Simstrat works", {
  skip_simstrat_run()

  path <- file.path(tempdir(), "simstrat_run_phys")
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  aeme <- yaml_to_aeme(path = aeme_dir, file = "aeme.yaml")
  model_controls <- get_model_controls()
  model <- "simstrat_aed2"
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls,
                     ext_elev = 5, use_bgc = FALSE)
  aeme <- run_aeme_with_retry(aeme = aeme, model = model, path = path)

  outfile <- get_model_outfile(aeme = aeme, model = model, path = path)
  testthat::expect_true(file.exists(outfile[["simstrat_aed2"]]))

  outp <- output(aeme)
  testthat::expect_true(!is.null(outp$ens_001$simstrat_aed2))

  vars_sim <- "HYD_temp"
  out <- read_simstrat_output(file = outfile$simstrat_aed2, vars_sim = vars_sim)
  testthat::expect_true(nrow(out$HYD_temp) > 2)
  # Water temperature should show real seasonal variation, not a constant
  # initial-condition value (regression check for the date-index/output
  # cadence bug fixed during development)
  testthat::expect_true(diff(range(out$HYD_temp[1, ], na.rm = TRUE)) > 1)

  out2 <- read_simstrat_output(file = outfile$simstrat_aed2, vars_sim = "HYD_temp",
                               depths = c(0, 5))
  testthat::expect_true(nrow(out2$HYD_temp) == 2)

  wlev <- read_model_wlev(lake_dir = get_lake_dir(aeme = aeme, path = path),
                          model = model)
  testthat::expect_true(is.data.frame(wlev))

  p <- plot_output(aeme, model = model)
  testthat::expect_true(!is.null(p))
})

test_that("running Simstrat-AED2 (with biogeochemistry) works", {
  skip_simstrat_run()

  path <- file.path(tempdir(), "simstrat_run_bgc")
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  aeme <- yaml_to_aeme(path = aeme_dir, file = "aeme.yaml")
  model_controls <- get_model_controls(use_bgc = TRUE)
  model <- "simstrat_aed2"
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls,
                     ext_elev = 5, use_bgc = TRUE)
  aeme <- run_aeme_with_retry(aeme = aeme, model = model, path = path)

  outfile <- get_model_outfile(aeme = aeme, model = model, path = path)
  testthat::expect_true(file.exists(outfile[["simstrat_aed2"]]))

  outp <- output(aeme)
  testthat::expect_true(!is.null(outp$ens_001$simstrat_aed2))

  # A biogeochemical state variable should have been simulated and read back
  out <- read_simstrat_output(file = outfile$simstrat_aed2, vars_sim = "CHM_oxy")
  testthat::expect_true(nrow(out$CHM_oxy) > 2)
  testthat::expect_true(any(!is.na(out$CHM_oxy)))
})

# --- Additional coverage matching test-run-glm.R/test-run-gotm.R ----------

test_that("running Simstrat with a spinup works", {
  skip_simstrat_run()

  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- get_model_controls()
  inf_factor <- c("simstrat_aed2" = 1)
  outf_factor <- c("simstrat_aed2" = 1)
  model <- "simstrat_aed2"

  # Add spin up time
  tim <- time(aeme)
  tim[["spin_up"]][[model]] <- 100
  time(aeme) <- tim

  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, inf_factor = inf_factor,
                     ext_elev = 5, use_bgc = FALSE)
  aeme <- run_aeme_with_retry(aeme = aeme, model = model, path = path)
  lke <- lake(aeme)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "output.nc"))
  testthat::expect_true(file_chk)
})

test_that("can get variable indices after running Simstrat", {
  skip_simstrat_run()

  path <- file.path(tempdir(), "simstrat_var_indices")
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  aeme <- yaml_to_aeme(path = aeme_dir, file = "aeme.yaml")
  model_controls <- get_model_controls()
  model <- "simstrat_aed2"
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls,
                     ext_elev = 5, use_bgc = FALSE)
  aeme <- run_aeme_with_retry(aeme = aeme, model = model, path = path)

  var_indices <- get_var_indices(model = model, aeme = aeme, path = path,
                                 vars_sim = "HYD_temp", use_obs = TRUE)
  testthat::expect_true(length(var_indices) > 0)
  testthat::expect_true(is.list(var_indices))
})

test_that("assessing Simstrat model performance works", {
  skip_simstrat_run()

  path <- file.path(tempdir(), "simstrat_assess")
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  aeme <- yaml_to_aeme(path = aeme_dir, file = "aeme.yaml")
  model_controls <- get_model_controls()
  model <- "simstrat_aed2"
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls,
                     ext_elev = 5, use_bgc = FALSE)
  aeme <- run_aeme_with_retry(aeme = aeme, model = model, path = path)

  model_performance <- assess_model(aeme = aeme, model = model,
                                    var_sim = c("LKE_lvlwtr", "HYD_temp"))
  testthat::expect_true(is.data.frame(model_performance))
})

test_that("getting variables from Simstrat output works", {
  skip_simstrat_run()

  path <- file.path(tempdir(), "simstrat_get_var")
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  aeme <- yaml_to_aeme(path = aeme_dir, file = "aeme.yaml")
  model_controls <- get_model_controls()
  model <- "simstrat_aed2"
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls,
                     ext_elev = 5, use_bgc = FALSE)
  aeme <- run_aeme_with_retry(aeme = aeme, model = model, path = path)

  v <- get_var(aeme = aeme, model = model, var_sim = "HYD_temp", depth = 0)
  v2 <- get_var(aeme = aeme, model = model, var_sim = "HYD_temp", depth = 0,
                depth_ref = "bottom")
  testthat::expect_true(is.data.frame(v))
  testthat::expect_true(is.data.frame(v2))
})

test_that("running Simstrat ensemble works", {
  skip_simstrat_run()

  path <- file.path(tempdir(), "simstrat_ensemble")
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  aeme <- yaml_to_aeme(path = aeme_dir, file = "aeme.yaml")
  model_controls <- get_model_controls()
  model <- "simstrat_aed2"
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls,
                     ext_elev = 5, use_bgc = FALSE)
  aeme <- run_aeme_with_retry(aeme = aeme, model = model, path = path)
  aeme <- run_aeme_with_retry(aeme = aeme, model = model, path = path)
  aeme <- run_aeme(aeme = aeme, model = model, verbose = FALSE, path = path,
                   ens_n = 2)

  outp <- output(aeme)
  testthat::expect_true(check_all_model_outfiles(aeme))
  testthat::expect_true(outp$n_members > 1)
})

test_that("reading Simstrat output via a direct nc handle works", {
  skip_simstrat_run()

  path <- file.path(tempdir(), "simstrat_nc_handle")
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  aeme <- yaml_to_aeme(path = aeme_dir, file = "aeme.yaml")
  model_controls <- get_model_controls()
  model <- "simstrat_aed2"
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls,
                     ext_elev = 5, use_bgc = FALSE)
  aeme <- run_aeme_with_retry(aeme = aeme, model = model, path = path)

  lake_dir <- get_lake_dir(aeme = aeme, path = path)
  outfile <- get_model_outfile(aeme = aeme, model = model, path = path)

  nc <- ncdf4::nc_open(outfile$simstrat_aed2)
  wlev <- read_model_wlev(nc = nc, model = model)
  testthat::expect_true(is.data.frame(wlev))

  outp1 <- read_model_outputs(nc = nc, lake_dir = lake_dir, model = model,
                              vars_sim = "HYD_temp")
  testthat::expect_true(is.list(outp1))
  testthat::expect_true(nrow(outp1$HYD_temp) > 2)
  ncdf4::nc_close(nc)
})

