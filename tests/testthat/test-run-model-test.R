# Local copies of the Simstrat skip helpers defined in test-simstrat.R /
# test-simstrat-aed.R -- kept self-contained rather than relying on cross-file
# load order (testthat sources files alphabetically; "-model-" sorts before
# "-simstrat").
skip_simstrat_run_local <- function() {
  if (AEME:::.detect_os() != "windows") {
    testthat::skip("Simstrat-AED2 binary is only bundled for Windows")
  }
}
skip_simstrat_aed_run_local <- function() {
  chk <- tryCatch(AEME:::.resolve_simstrat_aed_exec(), error = function(e) NULL)
  if (is.null(chk)) {
    testthat::skip("No Simstrat-AED binary installed (see install_simstrat_aed())")
  }
}

test_that("run_model_test() rejects an unsupported model", {
  testthat::expect_error(
    run_model_test("not_a_model", path = tempdir()),
    "Unsupported 'model'"
  )
})

test_that("run_model_test() works end-to-end for GLM-AED", {
  skip_if_models_unavailable(c("glm_aed"))

  path <- file.path(tempdir(), "run_model_test_glm")
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  aeme <- yaml_to_aeme(path = aeme_dir, file = "aeme.yaml")
  model_controls <- get_model_controls()
  model <- "glm_aed"
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, ext_elev = 5,
                     use_bgc = FALSE)

  lake_dir <- get_lake_dir(aeme = aeme, path = path)
  path_glm <- file.path(lake_dir, "glm_aed")

  glm_file <- find_glm_nml(path_glm)
  nml0 <- read_nml(glm_file)
  n_depths <- nml0$init_profiles$num_depths
  new_temp <- seq(20, 10, length.out = n_depths)

  inf_df <- read.csv(file.path(path_glm, "bcs", "inflow_FWMT.csv"))
  inf_df <- data.frame(Date = as.Date(inf_df$time),
                       HYD_flow = inf_df$flow * 86400,
                       HYD_temp = inf_df$temp,
                       CHM_salt = inf_df$salt)
  outf_df <- read.csv(file.path(path_glm, "bcs", "outflow_outflow.csv"))
  outf_df <- data.frame(Date = as.Date(outf_df$time),
                        HYD_flow = outf_df$flow * 0.99)

  out <- run_model_test(
    "glm_aed", path_glm,
    param_overrides = list(Kw = 1.5),
    init = list(temp = new_temp),
    inflow_args = list(data = list(FWMT = inf_df), mass = FALSE),
    outflow_args = list(data = list(outflow = outf_df),
                        heights_wdr = c(outflow = 12.07)),
    tgt_vars = "HYD_temp"
  )
  plot_model_output(out, "HYD_temp")
  

  testthat::expect_true(is.list(out))
  testthat::expect_true(nrow(out$HYD_temp) > 0)
  testthat::expect_true(is_aeme_output(out))
  testthat::expect_equal(get_glm_param(path_glm, "Kw"), 0.8)
  nml1 <- read_nml(glm_file)
  testthat::expect_equal(nml1$init_profiles$the_temps, new_temp)
})

test_that("run_model_test() works end-to-end for GOTM-WET", {
  skip_if_models_unavailable(c("gotm_wet"))

  path <- file.path(tempdir(), "run_model_test_gotm")
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  aeme <- yaml_to_aeme(path = aeme_dir, file = "aeme.yaml")
  model_controls <- get_model_controls()
  model <- "gotm_wet"
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, ext_elev = 5,
                     use_bgc = FALSE)

  lake_dir <- get_lake_dir(aeme = aeme, path = path)
  path_gotm <- file.path(lake_dir, "gotm_wet")

  t_prof_file <- file.path(path_gotm, "inputs", "t_prof_file.dat")
  n_depths <- length(readLines(t_prof_file)) - 1
  new_temp <- seq(20, 10, length.out = n_depths)

  inf_data <- inflows(aeme)[["data"]]
  outf_data <- outflows(aeme)[["data"]]

  out <- run_model_test(
    "gotm_wet", path_gotm,
    param_overrides = list(`time.dt` = 1800),
    init = list(temp = new_temp),
    inflow_args = list(data = inf_data),
    outflow_args = list(data = outf_data),
    tgt_vars = "HYD_temp"
  )
  plot_model_output(out, "HYD_temp")

  testthat::expect_true(is.list(out))
  testthat::expect_true(nrow(out$HYD_temp) > 0)
  testthat::expect_equal(get_gotm_param(path_gotm, "time.dt"), 1800)
  t_prof <- read.table(t_prof_file, skip = 1, col.names = c("depth", "value"))
  testthat::expect_equal(t_prof$value, new_temp)
  testthat::expect_true(file.exists(file.path(path_gotm, "inputs",
                                              "inf_flow_FWMT.dat")))
})

test_that("run_model_test() works end-to-end for Simstrat-AED2", {
  skip_simstrat_run_local()

  path <- file.path(tempdir(), "run_model_test_simstrat_aed2")
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  aeme <- yaml_to_aeme(path = aeme_dir, file = "aeme.yaml")
  model_controls <- get_model_controls()
  model <- "simstrat_aed2"
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls,
                     ext_elev = 5, use_bgc = FALSE)

  lake_dir <- get_lake_dir(aeme = aeme, path = path)
  path_simstrat <- file.path(lake_dir, "simstrat_aed2")

  ic_file <- file.path(path_simstrat, "InitialConditions.dat")
  n_depths <- length(readLines(ic_file)) - 1
  new_temp <- seq(20, 10, length.out = n_depths)

  hyps <- input(aeme)$hypsograph
  init_depth <- input(aeme)$init_depth
  surface_elev <- min(hyps$elev) + init_depth
  inf_data <- inflows(aeme)[["data"]]
  outf_data <- outflows(aeme)[["data"]]

  out <- run_model_test(
    "simstrat_aed2", path_simstrat,
    param_overrides = list(`ModelParameters.f_wind` = 0.9),
    init = list(temp = new_temp),
    inflow_args = list(data = inf_data),
    outflow_args = list(data = outf_data,
                        heights_wdr = c(outflow = surface_elev - 3,
                                        wbal = surface_elev - 1),
                        surface_elev = surface_elev),
    tgt_vars = "HYD_temp"
  )

  testthat::expect_true(is.list(out))
  testthat::expect_true(nrow(out$HYD_temp) > 0)
  testthat::expect_equal(attr(out, "model"), "simstrat_aed2")
  testthat::expect_equal(
    get_simstrat_param(path_simstrat, "ModelParameters.f_wind"), 0.9
  )
  testthat::expect_true(file.exists(file.path(path_simstrat, "Qinp.dat")))
  testthat::expect_true(file.exists(file.path(path_simstrat, "Qout.dat")))
})

test_that("run_model_test() works end-to-end for Simstrat-AED", {
  skip_simstrat_aed_run_local()

  path <- file.path(tempdir(), "run_model_test_simstrat_aed")
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  aeme <- yaml_to_aeme(path = aeme_dir, file = "aeme.yaml")
  model_controls <- get_model_controls()
  model <- "simstrat_aed"
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls,
                     ext_elev = 5, use_bgc = FALSE)

  lake_dir <- get_lake_dir(aeme = aeme, path = path)
  path_simstrat <- file.path(lake_dir, "simstrat_aed")

  ic_file <- file.path(path_simstrat, "InitialConditions.dat")
  n_depths <- length(readLines(ic_file)) - 1
  new_temp <- seq(20, 10, length.out = n_depths)

  out <- run_model_test(
    "simstrat_aed", path_simstrat,
    init = list(temp = new_temp),
    param_overrides = list(`ModelParameters.f_wind` = 2),
    tgt_vars = "HYD_temp"
  )
  plot_model_output(out, "HYD_temp")
  
  testthat::expect_true(is.list(out))
  testthat::expect_true(nrow(out$HYD_temp) > 0)
  testthat::expect_equal(attr(out, "model"), "simstrat_aed")
})

test_that("run_model_test() traps errors when safe = TRUE and propagates when safe = FALSE", {
  skip_if_models_unavailable(c("glm_aed"))

  path <- file.path(tempdir(), "run_model_test_glm_error")
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  aeme <- yaml_to_aeme(path = aeme_dir, file = "aeme.yaml")
  model_controls <- get_model_controls()
  model <- "glm_aed"
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, ext_elev = 5,
                     use_bgc = FALSE)
  lake_dir <- get_lake_dir(aeme = aeme, path = path)
  path_glm <- file.path(lake_dir, "glm_aed")

  # Not-a-real-parameter name should error inside set_glm_param()
  out <- run_model_test("glm_aed", path_glm,
                        param_overrides = list(not_a_real_param = 1),
                        safe = TRUE)
  testthat::expect_null(out)

  testthat::expect_error(
    run_model_test("glm_aed", path_glm,
                   param_overrides = list(not_a_real_param = 1),
                   safe = FALSE)
  )
})
