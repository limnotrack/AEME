test_that("weir parameters convert to and from water-balance C / h_inv", {
  w <- list(crest_elev = 10, crest_factor = 0.6, crest_width = 6)
  p <- glm_weir_to_wbal(w)
  expect_equal(p[["h_inv"]], 10)
  expect_equal(p[["C"]] * 1.5^1.5, glm_weir_outflow(11.5, 0.6, 10, 6))
  back <- wbal_to_glm_weir(p, crest_width = 6)
  expect_equal(back$crest_factor, 0.6)
  expect_equal(back$crest_elev, 10)
})

test_that("est_glm_weir_params recovers a known weir", {
  lv <- seq(9.5, 11, length.out = 100)
  fl <- glm_weir_outflow(lv, 0.6, 10, 6)
  fit <- est_glm_weir_params(lv, fl, crest_width = 6)
  expect_equal(fit$crest_elev, 10, tolerance = 1e-3)
  expect_equal(fit$crest_factor, 0.6, tolerance = 1e-2)
  # too few pairs / no flow -> no fit
  expect_null(est_glm_weir_params(lv[1:5], fl[1:5]))
  expect_null(est_glm_weir_params(lv, rep(0, 100)))
})

test_that("resolve_glm_weir fits from observed outflow and respects FALSE", {
  d <- seq(as.Date("2020-01-01"), by = "day", length.out = 100)
  lv <- seq(9.5, 11, length.out = 100)
  outf <- list(outflow = data.frame(
    Date = d, HYD_flow = glm_weir_outflow(lv, 0.6, 10, 6) * 86400))
  level <- data.frame(Date = d, value = lv)
  w <- resolve_glm_weir(TRUE, outf, level)
  expect_equal(w$crest_elev, 10, tolerance = 1e-3)
  expect_equal(w$source, "obs")
  expect_null(resolve_glm_weir(FALSE, outf, level))
  expect_null(resolve_glm_weir(TRUE, list(), level))
})

test_that("set_wbal_family_params keeps other families", {
  v <- c(C = 1, h_inv = 2)
  expect_equal(set_wbal_family_params(NULL, "glm_aed", v), list(glm_aed = v))
  flat <- c(C = 0.5, h_inv = 3)
  out <- set_wbal_family_params(flat, "glm_aed", v)
  expect_equal(out$glm_aed, v)
  expect_equal(out$gotm_wet, flat)
})

test_that("set_glm_weir_nml writes the weir and clamps below the start level", {
  nml <- list(morphometry = list(crest_elev = 20), outflow = NULL)
  weir <- list(crest_elev = 9, crest_factor = 0.5, crest_width = 6)
  out <- set_glm_weir_nml(nml, weir, min_crest = 10)
  expect_equal(out$morphometry$crest_elev, 10)
  expect_equal(out$outflow$crest_factor, 0.5)
  expect_equal(out$outflow$num_outlet, 0L)
})

test_that("build_aeme uses the GLM weir instead of the wbal outflow", {
  skip_if_no_glm()
  path <- withr::local_tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  aeme <- yaml_to_aeme(path = aeme_dir, file = "aeme.yaml")
  nml_of <- function(a) {
    read_nml(file.path(get_lake_dir(a, path), "glm_aed", "glm4.nml"))
  }
  on <- build_aeme(aeme = aeme, model = "glm_aed", path = path, ext_elev = 5,
                   use_bgc = FALSE)
  expect_false("bcs/outflow_wbal.csv" %in% nml_of(on)$outflow$outflow_fl)
  expect_equal(nml_of(on)$outflow$crest_width, 6)

  off <- build_aeme(aeme = aeme, model = "glm_aed", path = path, ext_elev = 5,
                    use_bgc = FALSE, glm_weir = FALSE)
  expect_true("bcs/outflow_wbal.csv" %in% nml_of(off)$outflow$outflow_fl)
})
