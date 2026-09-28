#' Build a minimal, otherwise-valid nml list from the package's own GLM
#' template, with file-reference blocks stripped so check_glm_nml()'s file-
#' existence checks don't interfere with testing the sediment checks in
#' isolation. The template's &sediment block already has n_zones = 3 with
#' sed_heat_Ksoil/sed_temp_depth at length 1 (a pre-existing mismatch that
#' AEME's own build pipeline normally overwrites before check_glm_nml() ever
#' sees it) -- a convenient natural fixture for this test.
#' @noRd
.glm_nml_fixture <- function() {
  nml <- read_nml(system.file("extdata/glm_aed/glm3.nml", package = "AEME"))
  nml$inflow <- NULL
  nml$outflow <- NULL
  nml$wq_setup <- NULL
  nml$meteorology$meteo_fl <- NULL
  # The shipped template has latitude/longitude swapped (a pre-existing,
  # unrelated issue) -- correct it here so it doesn't interfere with
  # testing the sediment checks in isolation
  nml$morphometry$latitude <- -36.8897994896407
  nml$morphometry$longitude <- 174.468977283869
  nml
}

test_that("check_glm_nml() enforces sed_heat_Ksoil/sed_temp_depth zone length when sed_heat_model is absent or 1", {
  nml <- .glm_nml_fixture()
  testthat::expect_equal(as.numeric(nml$sediment$n_zones), 3)
  testthat::expect_length(nml$sediment$sed_heat_Ksoil, 1)

  tmp <- tempfile(fileext = ".nml")

  # sed_heat_model absent entirely (as in the current template) -- treated
  # as implicitly enabled, for backwards compatibility with GLM3 nmls
  write_nml(nml, tmp)
  testthat::expect_error(check_glm_nml(tmp), class = "aeme_error_glm_nml")

  # sed_heat_model explicitly 1
  nml$sediment$sed_heat_model <- 1
  write_nml(nml, tmp)
  testthat::expect_error(check_glm_nml(tmp), class = "aeme_error_glm_nml")
})

test_that("check_glm_nml() skips sed_heat_Ksoil/sed_temp_depth zone length when sed_heat_model != 1", {
  nml <- .glm_nml_fixture()
  nml$sediment$sed_heat_model <- 2
  # sed_heat_model = 2 is supplied by the WQ library, so a coupled WQ module
  # must be present for it to be valid -- otherwise a separate check fires and
  # masks what this test is exercising.
  nml$wq_setup$wq_lib <- "aed"
  tmp <- tempfile(fileext = ".nml")
  write_nml(nml, tmp)

  # No longer flagged: sed_heat_Ksoil/sed_temp_depth are unused when
  # sed_heat_model != 1
  testthat::expect_true(check_glm_nml(tmp))
})

test_that("check_glm_nml() still enforces other zone-length params regardless of sed_heat_model", {
  nml <- .glm_nml_fixture()
  nml$sediment$sed_heat_model <- 2
  nml$sediment$sed_temp_mean <- 10  # break a param NOT gated by sed_heat_model
  tmp <- tempfile(fileext = ".nml")
  write_nml(nml, tmp)

  testthat::expect_error(check_glm_nml(tmp), class = "aeme_error_glm_nml")
})

test_that("check_glm_nml() catches aed_sed_const2d fsed_* vectors shorter than n_zones", {
  # Reproduces a real build artefact: aed_sed_const2d/n_zones bumped (e.g. to
  # match a new GLM sediment/n_zones) without resizing the per-zone flux
  # vectors -- AED aborts at runtime on this, so it must be a hard failure.
  nml <- .glm_nml_fixture()
  nml$sediment$sed_heat_model <- 2
  nml$wq_setup <- list(wq_lib = "aed", wq_nml_file = "aed.nml")

  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  glm_file <- file.path(tmp_dir, "glm3.nml")
  aed_file <- file.path(tmp_dir, "aed.nml")
  write_nml(nml, glm_file)

  aed_nml <- list(
    aed_models = list(models = "aed_sedflux"),
    aed_sedflux = list(sedflux_model = "Constant2d"),
    aed_sed_const2d = list(
      n_zones = 4,
      active_zones = c(1, 2, 3, 4),
      fsed_oxy = c(-25, -25, -25),
      fsed_amm = c(3.231, 2, 0.66),
      fsed_nit = c(-0.4, -0.4, 0.1),
      fsed_frp = c(0.15, 0.05, 0.05)
    )
  )
  class(aed_nml) <- "nml"
  write_nml(aed_nml, aed_file)

  testthat::expect_error(check_glm_nml(glm_file), class = "aeme_error_glm_nml")
  err <- tryCatch(check_glm_nml(glm_file), error = function(e) e)
  testthat::expect_match(paste(conditionMessage(err), collapse = "\n"),
                         "fsed_oxy has 3 values, but n_zones = 4")
})

test_that("check_glm_nml() skips aed_sed_const2d validation when aed_sedflux is not active", {
  nml <- .glm_nml_fixture()
  nml$sediment$sed_heat_model <- 2
  nml$wq_setup <- list(wq_lib = "aed", wq_nml_file = "aed.nml")

  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  glm_file <- file.path(tmp_dir, "glm3.nml")
  aed_file <- file.path(tmp_dir, "aed.nml")
  write_nml(nml, glm_file)

  # aed_sedflux not listed in aed_models -- aed_sed_const2d is inert leftover
  # config and should not be validated.
  aed_nml <- list(
    aed_models = list(models = "aed_oxygen"),
    aed_sed_const2d = list(
      n_zones = 4,
      active_zones = c(1, 2, 3, 4),
      fsed_oxy = c(-25, -25, -25),
      fsed_amm = c(3.231, 2, 0.66),
      fsed_nit = c(-0.4, -0.4, 0.1),
      fsed_frp = c(0.15, 0.05, 0.05)
    )
  )
  class(aed_nml) <- "nml"
  write_nml(aed_nml, aed_file)

  testthat::expect_true(check_glm_nml(glm_file))
})

test_that("check_glm_nml() skips aed_sed_const2d validation when sedflux_model is not Constant2d", {
  nml <- .glm_nml_fixture()
  nml$sediment$sed_heat_model <- 2
  nml$wq_setup <- list(wq_lib = "aed", wq_nml_file = "aed.nml")

  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  glm_file <- file.path(tmp_dir, "glm3.nml")
  aed_file <- file.path(tmp_dir, "aed.nml")
  write_nml(nml, glm_file)

  # aed_sedflux active but using a different flux model -- aed_sed_const2d is
  # not the block AED actually reads for fluxes.
  aed_nml <- list(
    aed_models = list(models = "aed_sedflux"),
    aed_sedflux = list(sedflux_model = "Dynamic2d"),
    aed_sed_const2d = list(
      n_zones = 4,
      active_zones = c(1, 2, 3, 4),
      fsed_oxy = c(-25, -25, -25),
      fsed_amm = c(3.231, 2, 0.66),
      fsed_nit = c(-0.4, -0.4, 0.1),
      fsed_frp = c(0.15, 0.05, 0.05)
    )
  )
  class(aed_nml) <- "nml"
  write_nml(aed_nml, aed_file)

  testthat::expect_true(check_glm_nml(glm_file))
})
