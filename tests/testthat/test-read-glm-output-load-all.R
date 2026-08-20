#' Build and run a small BGC-enabled GLM-AED lake, for testing that
#' read_glm_output()'s "load everything" behaviour (load_all = TRUE,
#' the default) picks up variables beyond the declared vars_sim set --
#' including ones with dimensions other than (time)/(z, time), e.g. the
#' AED sediment zone flux variables (nzones, time).
#' @noRd
.build_glm_bgc_run <- function(tmpdir) {
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")

  aeme <- yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- get_model_controls(use_bgc = TRUE)
  aeme <- build_aeme(path = path, aeme = aeme, model = "glm_aed",
                     model_controls = model_controls, ext_elev = 5,
                     use_bgc = TRUE)
  aeme <- run_aeme(aeme = aeme, model = "glm_aed", verbose = FALSE,
                   model_controls = model_controls, path = path)
  list(aeme = aeme, path = path, model_controls = model_controls)
}

test_that("read_glm_output() loads variables beyond the declared vars_sim set", {
  tmpdir <- tempfile("load_all_output_")
  dir.create(tmpdir)
  fx <- .build_glm_bgc_run(tmpdir)

  out_vars <- get_output_vars(fx$aeme, model = "glm_aed")
  # The declared/simulated set for this model_controls is a small (~15-25
  # variable) whitelist -- if load_all is working, actual GLM/AED output
  # variables far outnumber it (this lake's real output.nc has ~250)
  testthat::expect_gt(length(out_vars), 100)

  # An ordinary (z, time)-shaped variable that was never declared via
  # vars_sim/model_controls$simulate should still have been picked up
  # automatically, and be usable exactly like any declared variable
  testthat::expect_true("OXY_sat" %in% out_vars)
  outp <- output(fx$aeme)
  ens_lab <- format_ens_label(ens_n = 1)
  oxy_sat <- outp[[ens_lab]]$glm_aed$OXY_sat
  testthat::expect_true(is.matrix(oxy_sat))

  df <- get_var(aeme = fx$aeme, model = "glm_aed", var_sim = "OXY_sat")
  testthat::expect_true(nrow(df) > 0)
  testthat::expect_true(all(c("Date", "depth", "value") %in% names(df)))
})

test_that("variables with non-standard dimensions load as aeme_grouped_var, not misinterpreted as depth x time", {
  tmpdir <- tempfile("load_all_output_")
  dir.create(tmpdir)
  fx <- .build_glm_bgc_run(tmpdir)

  outp <- output(fx$aeme)
  ens_lab <- format_ens_label(ens_n = 1)
  glm <- outp[[ens_lab]]$glm_aed

  # AED's sediment-zone flux variables are shaped (nzones, time) in the
  # underlying netCDF -- this is the case find_glm_nml-adjacent work was
  # aiming to eventually support; here they must be present, correctly
  # classed, and *not* silently run through the depth x time interpolation
  # path (which would misinterpret zone index as depth)
  testthat::expect_true("SDF_Fsed_oxy_Z" %in% names(glm))
  gv <- glm[["SDF_Fsed_oxy_Z"]]
  testthat::expect_s3_class(gv, "aeme_grouped_var")
  testthat::expect_setequal(gv$dim_names, c("nzones", "time"))
  testthat::expect_length(gv$dim_values$time, ncol(glm$LKE_depths))
  testthat::expect_equal(dim(gv$value), c(length(gv$dim_values$nzones),
                                          length(gv$dim_values$time)))

  # print() method works without error
  testthat::expect_output(print(gv), "aeme_grouped_var")
})

test_that("as.data.frame.aeme_grouped_var() produces a correct long-format frame", {
  tmpdir <- tempfile("load_all_output_")
  dir.create(tmpdir)
  fx <- .build_glm_bgc_run(tmpdir)

  outp <- output(fx$aeme)
  ens_lab <- format_ens_label(ens_n = 1)
  gv <- outp[[ens_lab]]$glm_aed[["zarea"]]
  testthat::expect_s3_class(gv, "aeme_grouped_var")

  df <- as.data.frame(gv)
  testthat::expect_setequal(names(df), c("nzones", "Date", "value"))
  testthat::expect_equal(nrow(df), length(gv$dim_values$nzones) *
                           length(gv$dim_values$time))
  # Column order should match the array's own linear (column-major) order
  testthat::expect_equal(df$value, as.vector(gv$value))
})

test_that("get_var() returns a usable long-format data frame for a grouped variable", {
  tmpdir <- tempfile("load_all_output_")
  dir.create(tmpdir)
  fx <- .build_glm_bgc_run(tmpdir)

  df <- get_var(aeme = fx$aeme, model = "glm_aed", var_sim = "zarea")
  testthat::expect_true(nrow(df) > 0)
  testthat::expect_true(all(c("Date", "nzones", "value", "var_sim", "Model") %in%
                           names(df)))
})

test_that("plot_output() gives a clear error for a grouped variable instead of crashing", {
  tmpdir <- tempfile("load_all_output_")
  dir.create(tmpdir)
  fx <- .build_glm_bgc_run(tmpdir)

  testthat::expect_error(
    plot_output(aeme = fx$aeme, model = "glm_aed", var_sim = "zarea",
               print_plots = FALSE),
    class = "aeme_error_grouped_var_plot"
  )
})

test_that("check_aeme_vars() accepts names present in aeme's loaded output without a key_naming entry", {
  tmpdir <- tempfile("load_all_output_")
  dir.create(tmpdir)
  fx <- .build_glm_bgc_run(tmpdir)

  # OXY_sat has no key_naming var_aeme row, but is present in the loaded
  # output once load_all picked it up
  testthat::expect_silent(check_aeme_vars("OXY_sat", aeme = fx$aeme))
  testthat::expect_equal(check_aeme_vars("OXY_sat", aeme = fx$aeme), "OXY_sat")

  # A genuinely unknown/typo'd name must still error, aeme or not
  testthat::expect_error(check_aeme_vars("not_a_real_variable_xyz"))
  testthat::expect_error(check_aeme_vars("not_a_real_variable_xyz", aeme = fx$aeme))

  # Existing declared-variable behaviour is unaffected
  testthat::expect_equal(check_aeme_vars("HYD_temp"), "HYD_temp")
})

test_that("read_model_outputs(load_all = FALSE) restricts loading to the declared set as before", {
  tmpdir <- tempfile("load_all_output_")
  dir.create(tmpdir)
  fx <- .build_glm_bgc_run(tmpdir)

  lake_dir <- get_lake_dir(fx$aeme, fx$path)
  vars_sim <- get_vars_sim(model_controls = fx$model_controls)

  out_all <- read_model_outputs(lake_dir = lake_dir, model = "glm_aed",
                                vars_sim = vars_sim, load_all = TRUE)
  out_declared <- read_model_outputs(lake_dir = lake_dir, model = "glm_aed",
                                     vars_sim = vars_sim, load_all = FALSE)

  testthat::expect_gt(length(out_all), length(out_declared))
  testthat::expect_false("OXY_sat" %in% names(out_declared))
  testthat::expect_true("OXY_sat" %in% names(out_all))
})
