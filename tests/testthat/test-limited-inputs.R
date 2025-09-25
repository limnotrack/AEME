test_that("it errors when met data is not present", {
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
  
  aeme <- yaml_to_aeme(path = path, "aeme_simple.yaml")
  model_controls <- get_model_controls(use_bgc = TRUE)
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("glm_aed", "gotm_wet")
  
  # Without any met data
  testthat::expect_error({
    build_aeme(path = path, aeme = aeme, model = model,
               model_controls = model_controls, inf_factor = inf_factor, 
               ext_elev = 5, use_bgc = TRUE)
  })
})

testthat::test_that("can build AEME with simple set of inputs", {
  
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  
  hyps <- read.csv(file.path(path, "data", "hypsograph.csv"))
  met <- read.csv(file.path(path, "data", "meteo.csv"))
  
  # Keep meteo
  aeme_input <- list(
    lake = list(
      name = "Wainamu",
      id = 45819,
      latitude = -36.89,
      longitude = 174.47,
      elevation = 23.64,
      depth = 13.07,
      area = 152343
    ),
    time = list(
      start = "2020-08-01 00:00:00",
      stop = "2021-06-30 00:00:00",
      time_step = 3600
    ),
    input = list(
      init_depth = 13.0,
      hypsograph = hyps,
      meteo = met,
      Kw = 0.98
    )
  )
  
  aeme <- aeme_constructor(lake = aeme_input$lake, time = aeme_input$time,
                           input = aeme_input$input)
  
  obs_vars <- list_obs_vars(aeme)
  testthat::expect_true(is.null(obs_vars))
  
  model_controls <- get_model_controls()
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("dy_cd", "glm_aed", "gotm_wet")
  sys_OS <- AEME:::get_os()
  if (sys_OS == "osx") {
    testthat::skip("Skip testing on macOS")
  }
  
  
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, inf_factor = inf_factor,
                     ext_elev = 5, use_bgc = FALSE)
  inp <- input(aeme)
  
  testthat::expect_true(is.data.frame(inp$hypsograph))
})

testthat::test_that("can run AEME with simple set of inputs works", {
  
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
  
  aeme <- yaml_to_aeme(path = path, "aeme_simple.yaml")
  model_controls <- get_model_controls()
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("dy_cd", "glm_aed", "gotm_wet")
  sys_OS <- AEME:::get_os()
  if (sys_OS == "osx") {
    model <- "glm_aed"
  }
  
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, inf_factor = inf_factor,
                     ext_elev = 5, use_bgc = FALSE)
  inp <- input(aeme)
  
  testthat::expect_true(is.data.frame(inp$hypsograph))
  
  aeme <- run_aeme(aeme = aeme, model = model, verbose = TRUE,
                   model_controls = model_controls, path = path)
  
  tgt_vars <- list_mod_obs_vars(aeme, model = model)
  testthat::expect_true(is.null(tgt_vars))
  
  p1 <- plot_output(aeme, model = model)
  p2 <- plot_output(aeme, model = model, var_sim = "LKE_lvlwtr", facet = FALSE)
  testthat::expect_true(ggplot2::is_ggplot(p1))
  testthat::expect_true(ggplot2::is_ggplot(p2))
  
  lke <- lake(aeme)
  file_chk <- all(file.exists(file.path(path, paste0(lke$id, "_",
                                                     tolower(lke$name)),
                                        model[1], "DYsim.nc")),
                  file.exists(file.path(path, paste0(lke$id, "_",
                                                     tolower(lke$name)),
                                        model[2:3], "output", "output.nc")))
  testthat::expect_true(file_chk)
})
