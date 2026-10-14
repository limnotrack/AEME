test_that("aed_options() has defaults that leave every step on", {
  o <- aed_options()
  testthat::expect_s3_class(o, "aed_options")
  testthat::expect_null(o$modules)
  testthat::expect_true(all(unlist(o[c("resolve_deps", "initialise",
                                       "sed_zones", "totals")])))
})

test_that("aed_options() validates its arguments", {
  testthat::expect_error(aed_options(modules = "aed_not_a_module"),
                         "Unknown AED module")
  testthat::expect_error(aed_options(modules = 1), "character vector")
  testthat::expect_error(aed_options(totals = NA), "TRUE")
  testthat::expect_error(aed_options(sed_zones = "yes"), "TRUE")
  testthat::expect_error(aed_options(initialise = c(TRUE, FALSE)), "TRUE")
})

test_that("aed_options() prints", {
  testthat::expect_no_error(
    suppressMessages(print(aed_options(modules = "aed_oxygen"))))
})

test_that("check_aed_options() accepts NULL, a list or an aed_options object", {
  testthat::expect_identical(check_aed_options(NULL), aed_options())
  testthat::expect_identical(check_aed_options(list(totals = FALSE)),
                             aed_options(totals = FALSE))
  o <- aed_options(sed_zones = FALSE)
  testthat::expect_identical(check_aed_options(o), o)
  testthat::expect_error(check_aed_options("nope"), "aed_options")
})

test_that("aed_options_modules() resolves dependencies unless told not to", {
  testthat::expect_null(aed_options_modules(aed_options()))
  res <- aed_options_modules(aed_options(modules = "aed_nitrogen"))
  testthat::expect_true(all(c("aed_nitrogen", "aed_oxygen", "aed_sedflux") %in%
                              res))
  exact <- aed_options_modules(
    aed_options(modules = "aed_nitrogen", resolve_deps = FALSE))
  testthat::expect_equal(exact, "aed_nitrogen")
})

test_that("set_aed_models_nml() sets the module list on an nml", {
  nml <- list(aed_models = list(models = c("aed_oxygen", "aed_nitrogen")))
  class(nml) <- "nml"
  res <- suppressMessages(set_aed_models_nml(nml, "aed_oxygen"))
  testthat::expect_equal(res$aed_models$models, "aed_oxygen")
  testthat::expect_equal(nml$aed_models$models,
                         c("aed_oxygen", "aed_nitrogen"))
  testthat::expect_error(set_aed_models_nml(nml, 1), "character vector")
  testthat::expect_error(set_aed_models_nml(structure(list(), class = "nml"),
                                            "aed_oxygen"),
                         "aed_models")
})

test_that("derive_aed_sed_const2d() sets zones and leaves pinned fluxes", {
  scd <- list(n_zones = 1L, active_zones = 1L, fsed_oxy = -1, fsed_amm = 1,
              fsed_nit = 1, fsed_frp = 1)
  cfg <- list(bgc = list(aed = structure(list(aed_sed_const2d = scd),
                                         class = "nml")))
  fluxes <- list(fsed_oxy = c(-10, -20), fsed_amm = c(2, 3),
                 fsed_nit = c(0.1, 0.2), fsed_frp = c(0.01, 0.02))
  res <- derive_aed_sed_const2d(cfg, n_zones = 2, fluxes = fluxes,
                                pinned = "fsed_amm")
  out <- res$bgc$aed$aed_sed_const2d
  testthat::expect_equal(out$n_zones, 2)
  testthat::expect_equal(out$active_zones, 1:2)
  testthat::expect_equal(out$fsed_oxy, c(-10, -20))
  testthat::expect_equal(out$fsed_amm, 1)
  testthat::expect_equal(cfg$bgc$aed$aed_sed_const2d$n_zones, 1L)
})

test_that("build_aeme() runs only the AED steps asked for", {
  mk <- function(dir, ...) {
    path <- file.path(tempdir(), dir)
    aeme <- suppressWarnings(suppressMessages(
      yaml_to_aeme(path = system.file("extdata/lake/", package = "AEME"),
                   "aeme.yaml")))
    aeme <- suppressWarnings(suppressMessages(
      build_aeme(path = path, aeme = aeme, model = "glm_aed", ext_elev = 3,
                 model_controls = get_model_controls(use_bgc = TRUE),
                 use_bgc = TRUE, ...)))
    read_model_config("glm_aed", get_lake_dir(aeme, path))
  }
  full <- mk("aedopt_full")
  trimmed <- mk("aedopt_trim",
                aed = aed_options(modules = c("aed_sedflux", "aed_oxygen"),
                                  sed_zones = FALSE, totals = FALSE))

  # Modules: exactly the requested ones (they have no prerequisites outside
  # the list)
  testthat::expect_setequal(trimmed$bgc$aed$aed_models$models,
                            c("aed_sedflux", "aed_oxygen"))
  testthat::expect_gt(length(full$bgc$aed$aed_models$models), 2)

  # Sediment fluxes are not re-estimated from the bathymetry, so they differ
  # from the full build's estimates but still have one value per zone
  n_zones <- trimmed$hydrodynamic$sediment$n_zones
  testthat::expect_length(trimmed$bgc$aed$aed_sed_const2d$fsed_oxy, n_zones)
  testthat::expect_false(isTRUE(all.equal(
    trimmed$bgc$aed$aed_sed_const2d$fsed_oxy,
    full$bgc$aed$aed_sed_const2d$fsed_oxy)))
})
