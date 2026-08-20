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
