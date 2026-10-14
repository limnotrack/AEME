# Tests for set_output_vars() and its build_aeme(output_vars=) wiring.
#
# set_output_vars() rewrites the output section of a model's configuration so
# only the calibration variables of interest (plus the internals AEME's
# readers always need) are written. Each model handler has a different
# mechanism and a different failure mode if it over-trims, so every model is
# exercised end-to-end: trim -> write_configuration() -> run_aeme() -> the
# result is still readable and finite.

# Local skip for the bundled Simstrat-AED2 Windows binary. Kept inline rather
# than reused from test-simstrat-aed2.R, which testthat sources *after* this
# file (alphabetically), so its helper isn't defined yet when these run.
skip_simstrat_aed2_run <- function() {
  if (AEME:::.detect_os() != "windows") {
    testthat::skip("Simstrat-AED2 binary is only bundled for Windows")
  }
}

build_phys_aeme <- function(model, path) {
  aeme <- readRDS(system.file("extdata/aeme.rds", package = "AEME"))
  unlink(list.files(path, recursive = TRUE, full.names = TRUE))
  build_aeme(path = path, aeme = aeme, model = model,
             model_controls = get_model_controls(), ext_elev = 5,
             use_bgc = FALSE)
}

test_that("set_output_vars() keeps lake.csv for GLM but drops the point CSVs", {
  skip_if_models_unavailable("glm_aed")
  model <- "glm_aed"
  path <- file.path(tempdir(), "sov_glm")
  aeme <- build_phys_aeme(model, path)

  hd0 <- configuration(aeme)[[model]][["hydrodynamic"]]
  # sanity: the untrimmed build has both the lake CSV and the point CSV keys,
  # plus a &mass_balance block
  expect_false(is.null(hd0[["output"]][["csv_lake_fname"]]))
  expect_false(is.null(hd0[["output"]][["csv_point_nlevs"]]))
  expect_false(is.null(hd0[["mass_balance"]]))

  aeme <- set_output_vars(aeme, model, "HYD_temp", mass_balance = FALSE)

  hd1 <- configuration(aeme)[[model]][["hydrodynamic"]]
  # csv_lake_fname is deliberately retained: in GLM 4.x the netCDF diagnostic
  # scalars (lake_level, ...) are only written while lake.csv is open, and
  # AEME needs lake_level to read a GLM result back.
  expect_identical(hd1[["output"]][["csv_lake_fname"]],
                   hd0[["output"]][["csv_lake_fname"]])
  # the fixed-depth point CSV keys are gone
  for (k in c("csv_point_nlevs", "csv_point_fname", "csv_point_at",
              "csv_point_nvars", "csv_point_vars")) {
    expect_null(hd1[["output"]][[k]])
  }
  # mass_balance = FALSE drops the whole block
  expect_null(hd1[["mass_balance"]])

  write_configuration(aeme, model = model, path = path)
  lake_dir <- get_lake_dir(aeme, path = path)
  out_dir <- file.path(lake_dir, model, "output")

  aeme <- run_aeme(aeme, model = model, path = path, verbose = FALSE)

  produced <- list.files(out_dir)
  expect_true("output.nc" %in% produced)
  expect_true("lake.csv" %in% produced)
  expect_false(any(grepl("^WQ_", produced)))
  expect_false("mass_balance.csv" %in% produced)

  o <- output(aeme)[["ens_001"]][[model]]
  expect_true(isTRUE(o[["ok"]]))
  expect_equal(nrow(o[["HYD_temp"]]), 42)
  expect_equal(mean(is.finite(o[["HYD_temp"]])), 1)
  expect_equal(mean(is.finite(o[["LKE_lvlwtr"]])), 1)
})

test_that("build_aeme(output_vars=) writes an already-trimmed GLM config", {
  skip_if_models_unavailable("glm_aed")
  model <- "glm_aed"
  path <- file.path(tempdir(), "sov_glm_build")
  aeme <- readRDS(system.file("extdata/aeme.rds", package = "AEME"))
  unlink(list.files(path, recursive = TRUE, full.names = TRUE))

  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = get_model_controls(), ext_elev = 5,
                     use_bgc = FALSE, output_vars = "HYD_temp",
                     mass_balance = FALSE)

  # in-memory configuration is trimmed
  hd <- configuration(aeme)[[model]][["hydrodynamic"]]
  expect_false(is.null(hd[["output"]][["csv_lake_fname"]]))
  expect_null(hd[["output"]][["csv_point_nlevs"]])
  expect_null(hd[["mass_balance"]])

  # ...and so is the config on disk (build_aeme re-wrote it)
  lake_dir <- get_lake_dir(aeme, path = path)
  nml <- read_nml(find_glm_nml(file.path(lake_dir, model)))
  expect_null(nml[["output"]][["csv_point_nlevs"]])
  expect_false("mass_balance" %in% names(nml))

  aeme <- run_aeme(aeme, model = model, path = path, verbose = FALSE)
  o <- output(aeme)[["ens_001"]][[model]]
  expect_true(isTRUE(o[["ok"]]))
  expect_equal(mean(is.finite(o[["HYD_temp"]])), 1)
})

test_that("set_output_vars() cuts Simstrat's per-variable *_out.dat files", {
  skip_simstrat_aed2_run()
  model <- "simstrat_aed2"
  path <- file.path(tempdir(), "sov_simstrat")
  aeme <- build_phys_aeme(model, path)

  aeme <- set_output_vars(aeme, model, "HYD_temp", mass_balance = FALSE)
  hd <- configuration(aeme)[[model]][["hydrodynamic"]]
  expect_false(isTRUE(hd[["Output"]][["All"]]))
  # the always-keep grid/level set is present
  expect_true(all(c("T", "WaterH") %in% unlist(hd[["Output"]][["Variables"]])))

  write_configuration(aeme, model = model, path = path)
  lake_dir <- get_lake_dir(aeme, path = path)
  out_dir <- file.path(lake_dir, model, "output")

  aeme <- run_aeme(aeme, model = model, path = path, verbose = FALSE)

  dat <- list.files(out_dir, pattern = "_out\\.dat$")
  # untrimmed Simstrat writes ~25 *_out.dat; trimmed keeps only the handful
  # in the always-keep list
  expect_lt(length(dat), 15)
  expect_true(file.exists(file.path(out_dir, "output.nc")))

  o <- output(aeme)[["ens_001"]][[model]]
  expect_true(isTRUE(o[["ok"]]))
  expect_equal(mean(is.finite(o[["HYD_temp"]])), 1)
})

test_that("set_output_vars() shrinks the GOTM netCDF but keeps it readable", {
  skip_if_models_unavailable("gotm_wet")
  model <- "gotm_wet"
  path <- file.path(tempdir(), "sov_gotm")
  aeme <- build_phys_aeme(model, path)

  aeme <- set_output_vars(aeme, model, "HYD_temp", mass_balance = FALSE)
  hd <- configuration(aeme)[[model]][["hydrodynamic"]]
  srcs <- vapply(hd[["output"]][[1]][["variables"]], `[[`, character(1), "source")
  # sst and the surface-flux vars the reader needs unconditionally must be
  # in the explicit list, not just temp/salt/grid
  expect_true(all(c("temp", "sst", "qe", "airt") %in% srcs))
  expect_false("/*" %in% srcs)

  write_configuration(aeme, model = model, path = path)
  lake_dir <- get_lake_dir(aeme, path = path)
  ncf <- file.path(lake_dir, model, "output", "output.nc")

  aeme <- run_aeme(aeme, model = model, path = path, verbose = FALSE)
  expect_true(file.exists(ncf))

  nc <- ncdf4::nc_open(ncf)
  nc_vars <- names(nc$var)
  ncdf4::nc_close(nc)
  # a full GOTM-WET run writes 100+ variables; trimmed is a small fraction
  expect_lt(length(nc_vars), 30)

  o <- output(aeme)[["ens_001"]][[model]]
  expect_true(isTRUE(o[["ok"]]))
  expect_equal(mean(is.finite(o[["HYD_temp"]])), 1)
  expect_equal(mean(is.finite(o[["LKE_lvlwtr"]])), 1)
})

test_that("set_output_vars() warns on variables with no mapping for the model", {
  # Build-only (no run), so no binary needed. Routed through a model whose
  # handler actually consults the variable list (glm_aed ignores it, since
  # GLM's netCDF is always full).
  model <- "simstrat_aed2"
  path <- file.path(tempdir(), "sov_warn")
  aeme <- build_phys_aeme(model, path)

  expect_warning(
    set_output_vars(aeme, model, c("HYD_temp", "NOT_a_real_var")),
    "output name"
  )
})
