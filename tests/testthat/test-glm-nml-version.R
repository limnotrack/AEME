#' Build a small GLM-AED lake, then rename glm3.nml -> glm4.nml to simulate a
#' lake built with a newer GLM release that writes a differently-named
#' hydrodynamic nml file. Used to test that nothing in the package assumes
#' the literal filename "glm3.nml".
#' @noRd
.build_glm4_fixture <- function(tmpdir) {
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")

  aeme <- yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- get_model_controls(use_bgc = TRUE)
  aeme <- build_aeme(path = path, aeme = aeme, model = "glm_aed",
                     model_controls = model_controls, ext_elev = 5,
                     use_bgc = TRUE)

  glm_dir <- file.path(get_lake_dir(aeme, path), "glm_aed")
  file.rename(file.path(glm_dir, "glm3.nml"), file.path(glm_dir, "glm4.nml"))

  list(aeme = aeme, path = path, model_controls = model_controls,
       glm_dir = glm_dir)
}

test_that("find_glm_nml() and find_glm_nml_key() detect glm3/glm4 correctly", {
  tmpdir <- tempfile("glm_nml_version_")
  dir.create(tmpdir)
  fx <- .build_glm4_fixture(tmpdir)

  testthat::expect_equal(basename(find_glm_nml(fx$glm_dir)), "glm4.nml")
  testthat::expect_equal(find_glm_nml_key(c("glm4", "aed", "aed_phyto_pars")),
                         "glm4")

  # No match
  testthat::expect_error(find_glm_nml_key(c("aed", "aed_phyto_pars")))
  testthat::expect_true(is.na(find_glm_nml_key(c("aed"), must_exist = FALSE)))

  # Ambiguous match (both glm3.nml and glm4.nml present) is resolved via a
  # preference hierarchy rather than erroring
  file.copy(system.file("extdata/glm_aed/glm3.nml", package = "AEME"),
            file.path(fx$glm_dir, "glm3.nml"))

  # 1. An explicitly pinned GLM version wins, whichever it is
  withr::local_options(AEME.glm_version = "3.9.108")
  testthat::expect_equal(basename(find_glm_nml(fx$glm_dir)), "glm3.nml")
  testthat::expect_equal(find_glm_nml_key(c("glm3", "glm4")), "glm3")

  withr::local_options(AEME.glm_version = "4.0.0")
  testthat::expect_equal(basename(find_glm_nml(fx$glm_dir)), "glm4.nml")
  testthat::expect_equal(find_glm_nml_key(c("glm3", "glm4")), "glm4")

  # 2. With no pinned/installed version determinable at all, the highest
  # version number present wins (glm4 preferred over glm3)
  withr::local_options(AEME.glm_version = NULL)
  testthat::local_mocked_bindings(.preferred_glm_major_version = function() NULL)
  testthat::expect_equal(basename(find_glm_nml(fx$glm_dir)), "glm4.nml")
  testthat::expect_equal(find_glm_nml_key(c("glm3", "glm4")), "glm4")
})

test_that("get_model_config_files()/read_model_config() classify glm4.nml as hydrodynamic, not bgc", {
  tmpdir <- tempfile("glm_nml_version_")
  dir.create(tmpdir)
  fx <- .build_glm4_fixture(tmpdir)

  cfg_files <- get_model_config_files(path = dirname(fx$glm_dir), model = "glm_aed")
  testthat::expect_true("glm4" %in% names(cfg_files$glm_aed))
  testthat::expect_false("glm3" %in% names(cfg_files$glm_aed))

  model_cfg <- read_model_config(model = "glm_aed", lake_dir = fx$glm_dir)
  testthat::expect_false(is.null(model_cfg$hydrodynamic))
  testthat::expect_equal(model_cfg$hydrodynamic_file, "glm4.nml")
  # The GLM hydrodynamic block must not leak into bgc under an unclassified
  # "glm4" key (this was the originally-reported bug:
  # cfg$glm_aed$bgc$glm4)
  testthat::expect_false("glm4" %in% names(model_cfg$bgc))
  testthat::expect_true(is.list(model_cfg$hydrodynamic$morphometry))
})

test_that("get_model_outfile() resolves output files for a glm4.nml lake", {
  tmpdir <- tempfile("glm_nml_version_")
  dir.create(tmpdir)
  fx <- .build_glm4_fixture(tmpdir)

  # get_model_outfile() only needs the config files to resolve without
  # error -- it doesn't require GLM to have actually been run
  testthat::expect_no_error(
    get_model_outfile(model = "glm_aed", path = dirname(fx$glm_dir))
  )
})

test_that("glm_config_to_aeme() and build_aeme(use_aeme = TRUE) round-trip a glm4.nml lake", {
  tmpdir <- tempfile("glm_nml_version_")
  dir.create(tmpdir)
  fx <- .build_glm4_fixture(tmpdir)

  nml_file <- file.path(fx$glm_dir, "glm4.nml")
  loaded <- glm_config_to_aeme(nml_file, model_controls = fx$model_controls)
  lke <- lake(loaded)
  testthat::expect_equal(lke$name, "wainamu")

  path2 <- file.path(tmpdir, "lake-rebuilt")
  loaded <- build_aeme(aeme = loaded, path = path2, model = "glm_aed",
                       model_controls = fx$model_controls, use_aeme = TRUE)
  lake_dir2 <- get_lake_dir(loaded, path2)

  # Must be written back out as glm4.nml, not glm3.nml
  testthat::expect_true(file.exists(file.path(lake_dir2, "glm_aed", "glm4.nml")))
  testthat::expect_false(file.exists(file.path(lake_dir2, "glm_aed", "glm3.nml")))
  testthat::expect_identical(readLines(nml_file),
                             readLines(file.path(lake_dir2, "glm_aed", "glm4.nml")))

  # BGC files must round-trip verbatim too, not get re-derived
  aed_dir  <- file.path(fx$glm_dir, "aed")
  aed_dir2 <- file.path(lake_dir2, "glm_aed", "aed")
  aed_files <- list.files(aed_dir)
  testthat::expect_setequal(aed_files, list.files(aed_dir2))
  for (f in aed_files) {
    testthat::expect_identical(readLines(file.path(aed_dir, f)),
                               readLines(file.path(aed_dir2, f)),
                               info = f)
  }
})

test_that("write_configuration() preserves glm4.nml when rewriting to a new directory", {
  tmpdir <- tempfile("glm_nml_version_")
  dir.create(tmpdir)
  fx <- .build_glm4_fixture(tmpdir)

  aeme <- load_configuration(model = "glm_aed", aeme = fx$aeme,
                             model_controls = fx$model_controls, path = fx$path)

  path2 <- file.path(tmpdir, "lake-rewrite")
  write_configuration(aeme = aeme, model = "glm_aed", path = path2)
  lake_dir2 <- get_lake_dir(aeme, path2)
  testthat::expect_true(file.exists(file.path(lake_dir2, "glm_aed", "glm4.nml")))
  testthat::expect_false(file.exists(file.path(lake_dir2, "glm_aed", "glm3.nml")))
})
