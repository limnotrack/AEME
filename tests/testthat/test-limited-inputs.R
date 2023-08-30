test_that("it errors when met data is nor present", {
  library(AEME)
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  yaml <- yaml::read_yaml(file.path(path, "aeme.yaml"))

  # Remove all info
  yaml$lake$shape <- NULL
  yaml$catchment$shape <- NULL
  yaml$observations$lake <- NULL
  yaml$observations$level <- NULL
  yaml$input$hypsograph <- NULL
  yaml$input$meteo <- NULL
  yaml$inflows$data <- NULL
  yaml$outflows$data <- NULL

  write_yaml(yaml, file.path(path, "aeme_simple.yaml"))

  aeme_data <- yaml_to_aeme(path = path, "aeme_simple.yaml")
  mod_ctrls <- read.csv(file.path(path, "model_controls.csv"))
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("glm_aed", "gotm_wet")

  # Without any met data
  testthat::expect_error({
    build_ensemble(path = path, aeme_data = aeme_data, model = model,
                   mod_ctrls = mod_ctrls, inf_factor = inf_factor, ext_elev = 5,
                   use_bgc = TRUE, use_lw = TRUE)
    })
})

testthat::test_that("can run AEME with simple set of inputs works", {

  library(AEME)
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")

  # Keep meteo
  yaml <- yaml::read_yaml(file.path(path, "aeme.yaml"))

  # Remove all info
  yaml$lake$shape <- NULL
  yaml$catchment$shape <- NULL
  yaml$observations$lake <- NULL
  yaml$observations$level <- NULL
  yaml$input$hypsograph <- NULL
  yaml$inflows$data <- NULL
  yaml$outflows$data <- NULL

  write_yaml(yaml, file.path(path, "aeme_simple.yaml"))

  aeme_data <- yaml_to_aeme(path = path, "aeme_simple.yaml")

  aeme_data <- build_ensemble(path = path, aeme_data = aeme_data, model = model,
                              mod_ctrls = mod_ctrls, inf_factor = inf_factor,
                              ext_elev = 5, use_bgc = FALSE, use_lw = TRUE)
  inp <- input(aeme_data)

  testthat::expect_true(is.data.frame(inp$hypsograph))

  aeme_data <- run_aeme(aeme_data = aeme_data, model = model, verbose = TRUE,
                        path = path)

  lke <- lake(aeme_data)
  file_chk <- all(file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "output", "output.nc")))
  testthat::expect_true(file_chk)
})
