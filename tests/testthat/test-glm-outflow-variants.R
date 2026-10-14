# GLM-AED outflow variants. Each is built (or reconfigured) and actually run
# through GLM:
#   1. the default floating offtake (elevation sentinel -1)
#   2. an explicitly placed fixed outlet (a real elevation on the hypsograph)
#   3. the granular set_glm_outflow_config() writer

build_glm_lake <- function() {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  list(path = path, aeme = yaml_to_aeme(path = path, "aeme.yaml"))
}

test_that("GLM runs with the default floating offtake", {
  skip_if_models_unavailable("glm_aed")
  lk <- build_glm_lake()
  model <- "glm_aed"

  aeme <- build_aeme(path = lk$path, aeme = lk$aeme, model = model,
                     model_controls = get_model_controls(),
                     ext_elev = 5, use_bgc = FALSE)

  nml <- read_nml(glm_nml_path(get_lake_dir(aeme, path = lk$path)))
  outf <- nml$outflow
  H <- get_nml_value(nml, "H")
  lake_depth <- max(H) - min(H)

  # every outlet defaulted to a floating offtake ...
  testthat::expect_true(all(as.logical(outf$flt_off_sw)))
  testthat::expect_true(all(outf$outlet_type == 2))
  # ... with outl_elvs written as a depth below the surface in [0, lake depth]
  testthat::expect_true(all(is.finite(outf$outl_elvs)))
  testthat::expect_true(all(outf$outl_elvs >= 0 & outf$outl_elvs <= lake_depth))
  # ... and finite basin geometry at each outlet (the bug wrote NA here)
  testthat::expect_true(all(is.finite(outf$bsn_len_outl) & outf$bsn_len_outl > 0))
  testthat::expect_true(all(is.finite(outf$bsn_wid_outl) & outf$bsn_wid_outl > 0))

  aeme <- run_aeme(aeme = aeme, model = model, path = lk$path)
  testthat::expect_true(file.exists(
    get_model_outfile(aeme = aeme, model = model)$glm_aed))
})

test_that("GLM runs with an explicitly placed fixed outlet", {
  skip_if_models_unavailable("glm_aed")
  lk <- build_glm_lake()
  aeme <- lk$aeme
  model <- "glm_aed"

  # pin the "outflow" outlet to a fixed absolute elevation inside the
  # hypsograph; leave "wbal" on the -1 sentinel (=> still floating)
  hyps_rng <- range(input(aeme)$hypsograph$elev)
  fixed_elev <- round(mean(hyps_rng), 2)
  outf <- outflows(aeme)
  outf$elevation$outflow <- fixed_elev
  outflows(aeme) <- outf

  aeme <- build_aeme(path = lk$path, aeme = aeme, model = model,
                     model_controls = get_model_controls(),
                     ext_elev = 5, use_bgc = FALSE)

  outf_nml <- read_nml(glm_nml_path(get_lake_dir(aeme, path = lk$path)))$outflow
  fl <- as.logical(outf_nml$flt_off_sw)

  # exactly one fixed outlet, at the elevation we asked for
  testthat::expect_equal(sum(!fl), 1L)
  i_fixed <- which(!fl)
  testthat::expect_equal(outf_nml$outlet_type[i_fixed], 1)
  testthat::expect_equal(outf_nml$outl_elvs[i_fixed], fixed_elev,
                         tolerance = 1e-3)
  # the other outlet is still a floating offtake, geometry finite for both
  testthat::expect_true(any(fl))
  testthat::expect_true(all(is.finite(outf_nml$bsn_len_outl) &
                              outf_nml$bsn_len_outl > 0))

  aeme <- run_aeme(aeme = aeme, model = model, path = lk$path)
  testthat::expect_true(file.exists(
    get_model_outfile(aeme = aeme, model = model)$glm_aed))
})

test_that("set_glm_outflow_config() reconfigures the block and GLM still runs", {
  skip_if_models_unavailable("glm_aed")
  lk <- build_glm_lake()
  model <- "glm_aed"
  aeme <- build_aeme(path = lk$path, aeme = lk$aeme, model = model,
                     model_controls = get_model_controls(),
                     ext_elev = 5, use_bgc = FALSE)
  path_glm <- file.path(get_lake_dir(aeme, path = lk$path), "glm_aed")
  glm_file <- find_glm_nml(path_glm)

  nml0 <- read_nml(glm_file)
  H <- get_nml_value(nml0, "H")
  base_elev  <- min(H)
  crest_elev <- max(H)
  surface_elev <- base_elev + get_nml_value(nml0, "lake_depth")

  # reuse the two flow CSVs build_aeme() already wrote
  files <- file.path("bcs", c("outflow_outflow.csv", "outflow_wbal.csv"))
  testthat::expect_true(all(file.exists(file.path(path_glm, files))))

  # outlet 1: floating, 1.5 m below the surface; outlet 2: fixed near the bed
  set_glm_outflow_config(
    path_glm,
    outlets = data.frame(
      name = c("spillway", "gate"),
      type = c(2L, 1L),
      elev = c(surface_elev - 1.5, base_elev + 1),
      file = files
    ),
    seepage = TRUE, seepage_rate = 0.001
  )

  outf <- read_nml(glm_file)$outflow
  testthat::expect_equal(outf$num_outlet, 2)
  testthat::expect_equal(as.integer(outf$outlet_type), c(2L, 1L))
  testthat::expect_equal(as.logical(outf$flt_off_sw), c(TRUE, FALSE))
  # floating stored as depth below surface; fixed as absolute elevation
  testthat::expect_equal(outf$outl_elvs[1], 1.5, tolerance = 1e-3)
  testthat::expect_equal(outf$outl_elvs[2], base_elev + 1, tolerance = 1e-3)
  testthat::expect_true(isTRUE(as.logical(outf$seepage)))
  testthat::expect_equal(outf$seepage_rate, 0.001, tolerance = 1e-9)
  testthat::expect_true(all(is.finite(outf$bsn_len_outl) &
                              outf$bsn_len_outl > 0))

  run_glm_aed(sim_folder = path_glm)
  testthat::expect_true(file.exists(file.path(path_glm, "output", "output.nc")))

  # a floating outlet above the surface fails GLM's own range check
  testthat::expect_error(
    set_glm_outflow_config(
      path_glm,
      outlets = data.frame(type = 2L, elev = crest_elev + 5, file = files[1])
    ),
    "outside"
  )
})
