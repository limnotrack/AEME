#' Compare two numeric vectors up to a relative tolerance, reported via
#' testthat with a fixed, well-defined ("mean relative difference") meaning
#' regardless of testthat's own default comparison backend.
#' @noRd
expect_close <- function(actual, expected, tolerance = 1e-3, info = NULL) {
  testthat::expect_true(
    isTRUE(all.equal(actual, expected, tolerance = tolerance)),
    info = info
  )
}

test_that("glm_config_to_aeme() recovers the lake, time, and input slots", {
  tmpdir <- tempfile("glm_config_to_aeme_")
  dir.create(tmpdir)
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")

  aeme <- yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- get_model_controls(use_bgc = TRUE)
  aeme <- build_aeme(path = path, aeme = aeme, model = "glm_aed",
                     model_controls = model_controls, ext_elev = 5,
                     use_bgc = TRUE)

  nml_file <- file.path(get_lake_dir(aeme, path), "glm_aed", "glm3.nml")
  testthat::expect_true(file.exists(nml_file))

  loaded <- glm_config_to_aeme(nml_file, model_controls = model_controls)

  lke <- lake(aeme)
  lke2 <- lake(loaded)
  testthat::expect_equal(lke2$name, tolower(lke$name))
  # glm3.nml stores lat/lon with limited text precision, so allow a small
  # tolerance rather than requiring exact equality
  expect_close(lke2$latitude, lke$latitude, tolerance = 1e-4)
  expect_close(lke2$longitude, lke$longitude, tolerance = 1e-4)

  inp <- input(aeme)
  inp2 <- input(loaded)
  testthat::expect_equal(inp2$Kw, inp$Kw)
  testthat::expect_equal(inp2$init_depth, inp$init_depth)
  expect_close(inp2$hypsograph$elev, inp$hypsograph$elev)
  expect_close(inp2$hypsograph$area, inp$hypsograph$area)

  inf <- inflows(aeme)
  inf2 <- inflows(loaded)
  testthat::expect_setequal(names(inf2$data), names(inf$data))
  for (nm in names(inf$data)) {
    # The AED mass-unit round trip through the .csv (rounded to 5 dp on
    # write) introduces a small amount of floating-point noise
    expect_close(inf2$data[[nm]]$HYD_flow, inf$data[[nm]]$HYD_flow, info = nm)
    expect_close(inf2$data[[nm]]$HYD_temp, inf$data[[nm]]$HYD_temp, info = nm)
  }

  outf <- outflows(aeme)
  outf2 <- outflows(loaded)
  testthat::expect_setequal(names(outf2$data), names(outf$data))
})

test_that("glm_config_to_aeme() disables recompute of water balance/lake level", {
  tmpdir <- tempfile("glm_config_to_aeme_")
  dir.create(tmpdir)
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")

  aeme <- yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- get_model_controls(use_bgc = TRUE)
  aeme <- build_aeme(path = path, aeme = aeme, model = "glm_aed",
                     model_controls = model_controls, ext_elev = 5,
                     use_bgc = TRUE)
  nml_file <- file.path(get_lake_dir(aeme, path), "glm_aed", "glm3.nml")

  loaded <- glm_config_to_aeme(nml_file, model_controls = model_controls)
  cfg <- configuration(loaded)
  testthat::expect_false(cfg$calc_wbal)
  testthat::expect_false(cfg$calc_wlev)
  testthat::expect_equal(cfg$ext_elev, 0)
})

test_that("build_aeme(use_aeme = TRUE) on a loaded object reproduces the original build", {
  tmpdir <- tempfile("glm_config_to_aeme_")
  dir.create(tmpdir)
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")

  # The example lake uses water_balance method = 2 with observed level data,
  # so the original build produces an auto-computed "wbal" outflow -- this
  # is the trickiest case to round-trip, since "wbal" is a reserved name
  # that several writer functions treat specially.
  aeme <- yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- get_model_controls(use_bgc = TRUE)
  aeme <- build_aeme(path = path, aeme = aeme, model = "glm_aed",
                     model_controls = model_controls, ext_elev = 5,
                     use_bgc = TRUE)
  outf <- outflows(aeme)
  testthat::expect_true("wbal" %in% names(outf$data))

  lake_dir <- get_lake_dir(aeme, path)
  nml_file <- file.path(lake_dir, "glm_aed", "glm3.nml")
  loaded <- glm_config_to_aeme(nml_file, model_controls = model_controls)

  # Rebuild from the loaded object into a fresh directory, trusting it as-is
  path2 <- file.path(tmpdir, "lake-rebuilt")
  loaded <- build_aeme(aeme = loaded, path = path2, model = "glm_aed",
                       model_controls = model_controls, use_aeme = TRUE)
  lake_dir2 <- get_lake_dir(loaded, path2)
  nml_file2 <- file.path(lake_dir2, "glm_aed", "glm3.nml")
  testthat::expect_true(file.exists(nml_file2))

  # glm3.nml must be written back verbatim from the cached configuration
  testthat::expect_identical(readLines(nml_file), readLines(nml_file2))

  # Boundary-condition files must not gain, lose, or silently recompute
  # entries (e.g. a dropped/altered "wbal" outflow) relative to the original
  bcs_dir  <- file.path(lake_dir, "glm_aed", "bcs")
  bcs_dir2 <- file.path(lake_dir2, "glm_aed", "bcs")
  bcs1 <- list.files(bcs_dir)
  bcs2 <- list.files(bcs_dir2)
  testthat::expect_setequal(bcs1, bcs2)

  for (f in bcs1) {
    df1 <- read.csv(file.path(bcs_dir, f))
    df2 <- read.csv(file.path(bcs_dir2, f))
    testthat::expect_equal(names(df1), names(df2), info = f)
    testthat::expect_equal(nrow(df1), nrow(df2), info = f)
    num_cols <- names(df1)[vapply(df1, is.numeric, logical(1))]
    for (col in num_cols) {
      expect_close(df1[[col]], df2[[col]], info = paste(f, col))
    }
  }

  # The returned Aeme object itself must not have gained/lost outflows
  # (this is what silently happened when calc_wbal/calc_wlev were left on,
  # and what silently dropped "wbal" before the build_aeme.R fix)
  outf2 <- outflows(loaded)
  testthat::expect_setequal(names(outf2$data), names(outf$data))
  testthat::expect_true("wbal" %in% names(outf2$data))

  # BGC files must be written back verbatim too -- initialise_aed(),
  # set_aed_sed_const2d(), and set_aed_totals() all re-derive aed.nml
  # (and the aed_*_pars.csv files) from generic aeme state and must not
  # run when use_aeme = TRUE trusts the cached configuration instead
  aed_dir  <- file.path(lake_dir, "glm_aed", "aed")
  aed_dir2 <- file.path(lake_dir2, "glm_aed", "aed")
  aed_files <- list.files(aed_dir)
  testthat::expect_setequal(aed_files, list.files(aed_dir2))
  for (f in aed_files) {
    testthat::expect_identical(readLines(file.path(aed_dir, f)),
                               readLines(file.path(aed_dir2, f)),
                               info = f)
  }
})

test_that("write_configuration() alone reproduces a full GLM-AED lake directory, with zero recomputation", {
  tmpdir <- tempfile("glm_config_to_aeme_")
  dir.create(tmpdir)
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")

  aeme <- yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- get_model_controls(use_bgc = TRUE)
  aeme <- build_aeme(path = path, aeme = aeme, model = "glm_aed",
                     model_controls = model_controls, ext_elev = 5,
                     use_bgc = TRUE)

  lake_dir <- get_lake_dir(aeme, path)
  nml_file <- file.path(lake_dir, "glm_aed", "glm3.nml")
  loaded <- glm_config_to_aeme(nml_file, model_controls = model_controls)

  # write_configuration() alone -- not build_aeme() -- into a fresh
  # directory. This must be enough on its own to reproduce every glm_aed
  # file (nml, bcs boundary files, and aed bgc files) exactly, with no
  # build_aeme() pipeline involved at all.
  path2 <- file.path(tmpdir, "lake-write-configuration")
  write_configuration(aeme = loaded, model = "glm_aed", path = path2)
  lake_dir2 <- get_lake_dir(loaded, path2)

  glm_dir  <- file.path(lake_dir, "glm_aed")
  glm_dir2 <- file.path(lake_dir2, "glm_aed")

  testthat::expect_identical(readLines(nml_file),
                             readLines(file.path(glm_dir2, "glm3.nml")))

  for (sub in c("bcs", "aed")) {
    dir1 <- file.path(glm_dir, sub)
    dir2 <- file.path(glm_dir2, sub)
    files1 <- list.files(dir1)
    testthat::expect_setequal(files1, list.files(dir2))
    for (f in files1) {
      # bcs csvs carry a lossless-but-not-byte-identical unit round trip
      # (rounded to 5 dp on write); aed files should be exactly identical
      if (sub == "aed") {
        testthat::expect_identical(readLines(file.path(dir1, f)),
                                   readLines(file.path(dir2, f)), info = f)
      } else {
        df1 <- read.csv(file.path(dir1, f))
        df2 <- read.csv(file.path(dir2, f))
        testthat::expect_equal(names(df1), names(df2), info = f)
        num_cols <- names(df1)[vapply(df1, is.numeric, logical(1))]
        for (col in num_cols) {
          expect_close(df1[[col]], df2[[col]], info = paste(f, col))
        }
      }
    }
  }
})
