# test-aed_noncohesive.R
#
# Tests for the bundled aed_noncohesive (suspended-sediment) defaults and the
# module wiring that activates it. Run with:
#   testthat::test_file("tests/testthat/test-aed_noncohesive.R")

# ── Bundled template ─────────────────────────────────────────────────────────

get_default_aed_nml <- function() {
  f <- system.file("extdata/aed/aed.nml", package = "AEME")
  testthat::expect_true(file.exists(f))
  read_nml(f)
}

test_that("default aed.nml carries an aed_noncohesive block with 2 groups", {
  nml <- get_default_aed_nml()

  expect_true("aed_noncohesive" %in% names(nml))
  ncs <- nml[["aed_noncohesive"]]

  expect_equal(ncs[["num_ss"]], 2)
  # per-group vectors are all length num_ss
  for (p in c("ss_initial", "Ke_ss", "w_ss", "rho_ss", "d_ss",
              "tau_0", "fs", "Fsed", "decay")) {
    expect_length(ncs[[p]], 2L)
  }
  expect_equal(ncs[["ss_initial"]], c(3.0, 1.0))
  expect_equal(ncs[["w_ss"]], c(0.5, 0.03))
  expect_equal(ncs[["settling"]], 1)
  expect_equal(ncs[["resuspension"]], 1)
  expect_identical(ncs[["simSedimentMass"]], FALSE)
})

test_that("default aed.nml lists aed_noncohesive right after aed_sedflux", {
  nml <- get_default_aed_nml()
  models <- nml[["aed_models"]][["models"]]

  expect_true("aed_noncohesive" %in% models)
  expect_equal(which(models == "aed_noncohesive"),
               which(models == "aed_sedflux") + 1L)
})

test_that("default aed.nml places the &aed_noncohesive block after &aed_sed_const2d", {
  # libaed reads the module namelists in a single forward pass without
  # rewinding between aed_sedflux (which also consumes &aed_sed_const2d) and
  # aed_noncohesive. A &aed_noncohesive block earlier in the file is never
  # found -> GLM aborts with "ERROR reading namelist aed_noncohesive".
  f <- system.file("extdata/aed/aed.nml", package = "AEME")
  blocks <- grep("^&", readLines(f), value = TRUE)
  expect_lt(which(blocks == "&aed_sed_const2d"),
            which(blocks == "&aed_noncohesive"))
})

test_that("default aed_totals counts both non-cohesive groups as TSS", {
  nml <- get_default_aed_nml()
  totals <- nml[["aed_totals"]]

  expect_equal(totals[["TSS_vars"]], c("NCS_ss1", "NCS_ss2"))
  expect_length(totals[["TSS_varscale"]], 2L)
  expect_equal(totals[["TSS_varscale"]], c(1.0, 1.0))
})

# ── Module resolution wiring ─────────────────────────────────────────────────

test_that("NCS prefix maps to aed_noncohesive", {
  expect_identical(unname(.aed_module_map[["NCS"]]), "aed_noncohesive")
  expect_identical(aed_prefixes_to_modules("NCS"), "aed_noncohesive")
})

test_that("aed_noncohesive sits between aed_sedflux and aed_oxygen in the order", {
  expect_true("aed_noncohesive" %in% .aed_module_order)
  expect_equal(which(.aed_module_order == "aed_noncohesive"),
               which(.aed_module_order == "aed_sedflux") + 1L)
  expect_lt(which(.aed_module_order == "aed_noncohesive"),
            which(.aed_module_order == "aed_oxygen"))
})

test_that("resolve_aed_active_modules keeps aed_noncohesive and orders it", {
  res <- resolve_aed_active_modules(c("aed_oxygen", "aed_noncohesive"))
  expect_true("aed_noncohesive" %in% res)
  expect_lt(which(res == "aed_noncohesive"), which(res == "aed_oxygen"))
})

test_that("aed_noncohesive pulls in no forced dependencies", {
  expect_null(.aed_module_deps[["aed_noncohesive"]])
  expect_setequal(resolve_aed_active_modules("aed_noncohesive"),
                  "aed_noncohesive")
})

# ── Build integration ───────────────────────────────────────────────────────

test_that("building GLM-AED activates aed_noncohesive and TSS totals", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- get_model_controls(use_bgc = TRUE)
  model <- "glm_aed"
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls,
                     ext_elev = 5, use_bgc = TRUE)
  lke <- lake(aeme)
  aed_file <- file.path(path, paste0(lke$id, "_", tolower(lke$name)),
                        model, "aed", "aed.nml")
  expect_true(file.exists(aed_file))

  nml <- read_nml(aed_file)
  expect_true("aed_noncohesive" %in% nml[["aed_models"]][["models"]])
  expect_true("aed_noncohesive" %in% names(nml))

  # The build must preserve the block ordering the template ships with.
  built_blocks <- grep("^&", readLines(aed_file), value = TRUE)
  expect_lt(which(built_blocks == "&aed_sed_const2d"),
            which(built_blocks == "&aed_noncohesive"))

  # set_aed_totals() re-derives aed_totals during the build; TSS_vars must
  # survive with one NCS_ss<i> per non-cohesive group.
  num_ss <- nml[["aed_noncohesive"]][["num_ss"]]
  expect_equal(nml[["aed_totals"]][["TSS_vars"]],
               paste0("NCS_ss", seq_len(num_ss)))
  expect_length(nml[["aed_totals"]][["TSS_varscale"]], num_ss)

  # GLM-AED must actually run to completion with aed_noncohesive active.
  aeme <- run_aeme(aeme)
  out_file <- get_model_outfile(aeme = aeme)
  expect_true(file.exists(out_file$glm_aed))
})
