# Simstrat-AED (not AED2) couples Simstrat to the same AED module as
# GLM-AED, via a feature-branch Simstrat build that isn't bundled with the
# package (see install_simstrat_aed()) -- unlike Simstrat-AED2's bundled
# Windows binary. Run tests therefore skip unless a Simstrat-AED executable
# is actually installed locally. The build tests need no binary and run on
# any platform.
skip_simstrat_aed_run <- function() {
  chk <- tryCatch(AEME:::.resolve_simstrat_aed_exec(), error = function(e) NULL)
  if (is.null(chk)) {
    testthat::skip("No Simstrat-AED binary installed (see install_simstrat_aed())")
  }
}

# Local copy of test-simstrat.R's run_aeme_with_retry() -- kept self-contained
# rather than shared, since testthat sources "test-simstrat-aed.R" before
# "test-simstrat.R" (alphabetically, "-" < ".") so the helper wouldn't yet
# exist when this file's tests run.
run_aeme_with_retry_aed <- function(aeme, model, path, tries = 2) {
  for (i in seq_len(tries)) {
    aeme <- run_aeme(aeme = aeme, model = model, path = path, verbose = FALSE)
    if (!is.null(output(aeme)$ens_001[[model]])) return(aeme)
  }
  aeme
}

test_that("simstrat_aed model registry works", {
  chk <- check_model(c("SIMSTRAT-AED", "simstrat_aed"))
  testthat::expect_equal(unname(chk), c("simstrat_aed", "simstrat_aed"))

  testthat::expect_true("simstrat_aed" %in% list_models())

  testthat::expect_equal(toggle_models("SIMSTRAT-AED"), c("SIMSTRAT-AED" = "simstrat_aed"))
  testthat::expect_equal(toggle_models("simstrat_aed", to = "display"),
                         c("SIMSTRAT-AED"))
})

test_that("building Simstrat-AED works", {
  path <- file.path(tempdir(), "simstrat_aed_build_phys")
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  aeme <- yaml_to_aeme(path = aeme_dir, file = "aeme.yaml")
  model_controls <- get_model_controls()
  model <- "simstrat_aed"
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
  testthat::expect_true(!is.null(cfg$simstrat_aed$hydrodynamic))

  par <- jsonlite::fromJSON(file.path(sim_dir, "simstrat.par"), simplifyVector = FALSE)
  testthat::expect_false(isTRUE(par$ModelConfig$CoupleAED))
})

test_that("building Simstrat-AED (with biogeochemistry) works", {
  path <- file.path(tempdir(), "simstrat_aed_build_bgc")
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  aeme <- yaml_to_aeme(path = aeme_dir, file = "aeme.yaml")
  model_controls <- get_model_controls(use_bgc = TRUE)
  model <- "simstrat_aed"
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls,
                     ext_elev = 5, use_bgc = TRUE)

  lake_dir <- get_lake_dir(aeme = aeme, path = path)
  sim_dir <- file.path(lake_dir, model)

  # bgc_lib = "aed" (not "aed2") -- distinct nml file and inflow/initcond dirs
  testthat::expect_true(file.exists(file.path(sim_dir, "aed", "aed.nml")))
  testthat::expect_false(file.exists(file.path(sim_dir, "aed", "aed2.nml")))
  testthat::expect_true(dir.exists(file.path(sim_dir, "aed", "AED_inflow")))
  testthat::expect_true(dir.exists(file.path(sim_dir, "aed", "AED_initcond")))

  aed_nml <- read_nml(file.path(sim_dir, "aed", "aed.nml"))
  active_modules <- get_nml_value(aed_nml, "models")
  testthat::expect_true(length(active_modules) > 0)

  cfg <- configuration(aeme)
  testthat::expect_true(!is.null(cfg$simstrat_aed$bgc))

  par <- jsonlite::fromJSON(file.path(sim_dir, "simstrat.par"), simplifyVector = FALSE)
  testthat::expect_true(isTRUE(par$ModelConfig$CoupleAED))
  testthat::expect_equal(par$AEDConfig$NZones, 2)
  testthat::expect_equal(length(par$AEDConfig$ZoneHeights), 2)
  
  chk <- check_simstrat_par(file.path(sim_dir, "simstrat.par"))
  testthat::expect_true(chk)
})

test_that("running Simstrat-AED works", {
  skip_simstrat_aed_run()

  path <- file.path(tempdir(), "simstrat_aed_run_phys")
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  aeme <- yaml_to_aeme(path = aeme_dir, file = "aeme.yaml")
  model_controls <- get_model_controls()
  model <- "simstrat_aed"
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls,
                     ext_elev = 5, use_bgc = FALSE)
  aeme <- run_aeme(aeme = aeme, model = model, path = path)

  outfile <- get_model_outfile(aeme = aeme)
  testthat::expect_true(file.exists(outfile[["simstrat_aed"]]))

  outp <- output(aeme)
  testthat::expect_true(!is.null(outp$ens_001$simstrat_aed))

  out <- read_simstrat_output(file = outfile$simstrat_aed, vars_sim = "HYD_temp", model = "simstrat_aed")
  testthat::expect_true(nrow(out$HYD_temp) > 2)
  testthat::expect_true(diff(range(out$HYD_temp[1, ], na.rm = TRUE)) > 1)

  p <- plot_output(aeme, model = model)
  testthat::expect_true(!is.null(p))
})

test_that("running Simstrat-AED (with biogeochemistry) works", {
  skip_simstrat_aed_run()

  path <- file.path(tempdir(), "simstrat_aed_run_bgc")
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  aeme <- yaml_to_aeme(path = aeme_dir, file = "aeme.yaml")
  model_controls <- get_model_controls(use_bgc = TRUE)
  model <- "simstrat_aed"
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls,
                     ext_elev = 5, use_bgc = TRUE)
  aeme <- run_aeme(aeme = aeme, model = model, path = path)
  plot_output(aeme) /
  plot_output(aeme, "oxy")
  
  outfile <- get_model_outfile(aeme = aeme)
  testthat::expect_true(file.exists(outfile[["simstrat_aed"]]))
  run_simstrat_aed(sim_folder = file.path(get_lake_dir(aeme = aeme, path = path), "simstrat_aed"))
  out <- read_simstrat_output(file = outfile$simstrat_aed, load_all = TRUE,
                              raw_output = T)
  plot_model_output(out, var_sim = "nuh")
  plot_model_output(out, var_sim = "PHY_cyano")
  plot_model_output(out, var_sim = "PHY_diatom")
  plot_model_output(out, var_sim = "T") /
  plot_model_output(out, var_sim = "PHY_green") /
  plot_model_output(out, var_sim = "OXY_oxy")
  plot_model_output(out, var_sim = "Eseiche")
  
  outp <- output(aeme)
  testthat::expect_true(!is.null(outp$ens_001$simstrat_aed))

  # Uses the glm_aed-identical key_naming column (OXY_oxy), not AED2's
  out <- read_simstrat_output(file = outfile$simstrat_aed, vars_sim = "CHM_oxy", model = "simstrat_aed")
  testthat::expect_true(nrow(out$CHM_oxy) > 2)
  testthat::expect_true(any(!is.na(out$CHM_oxy)))
})

test_that("editing and running Simstrat-AED via the thin path-based wrapper works", {
  skip_simstrat_aed_run()

  path <- file.path(tempdir(), "simstrat_aed_thin_wrapper")
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  aeme <- yaml_to_aeme(path = aeme_dir, file = "aeme.yaml")
  model_controls <- get_model_controls()
  model <- "simstrat_aed"
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls,
                     ext_elev = 5, use_bgc = FALSE)

  lake_dir <- get_lake_dir(aeme = aeme, path = path)
  path_simstrat <- file.path(lake_dir, "simstrat_aed")

  old_f_wind <- get_simstrat_param(path_simstrat, "ModelParameters.f_wind")
  set_simstrat_param(path_simstrat, `ModelParameters.f_wind` = 0.5)
  testthat::expect_equal(
    get_simstrat_param(path_simstrat, "ModelParameters.f_wind"),
    0.5
  )

  # -- init --
  ic_file <- file.path(path_simstrat, "InitialConditions.dat")
  n_depths <- length(readLines(ic_file)) - 1
  new_temp <- seq(20, 10, length.out = n_depths)
  new_salt <- rep(1, n_depths)
  set_simstrat_init(path_simstrat, temp = new_temp, salt = new_salt,
                    wq_init = list(NIT_amm = 0.5))
  ic <- read.table(ic_file, skip = 1,
                   col.names = c("depth", "U", "V", "temperature", "salt",
                                 "k", "eps"))
  testthat::expect_equal(ic$temperature, new_temp)
  testthat::expect_equal(ic$salt, new_salt)
  wq_file <- file.path(path_simstrat, "aed", "AED_initcond", "NIT_amm_ini.dat")
  testthat::expect_true(file.exists(wq_file))
  wq_prof <- read.table(wq_file, skip = 1, col.names = c("depth", "value"))
  testthat::expect_true(all(wq_prof$value == 0.5))

  run_simstrat_aed(sim_folder = path_simstrat)
  outfile <- file.path(path_simstrat, "output", "output.nc")
  testthat::expect_true(file.exists(outfile))
  out <- read_simstrat_output(file = outfile, vars_sim = "HYD_temp", model = "simstrat_aed")
  plot_model_output(out, var_sim = "HYD_temp")
  testthat::expect_true(nrow(out$HYD_temp) > 0)
  testthat::expect_true(is_aeme_output(out))
  testthat::expect_equal(attr(out, "model"), "simstrat_aed")
})
