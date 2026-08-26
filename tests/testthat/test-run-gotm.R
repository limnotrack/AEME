test_that("running GOTM works", {
  skip_if_models_unavailable(c("gotm_wet"))
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- get_model_controls()
  model_controls <- set_vars_sim(model_controls = model_controls,
                                 vars_sim = c("CHM_oxynal", "CHM_oxymom",
                                              "LKE_tli4", "LKE_tli3"))
  inf_factor = c("gotm_wet" = 1)
  outf_factor = c("gotm_wet" = 1)
  model <- c("gotm_wet")
  aeme <- build_aeme(path = path, aeme = aeme,
                     model = model, model_controls = model_controls,
                     inf_factor = inf_factor, ext_elev = 5,
                     use_bgc = F)
  aeme <- run_aeme(aeme = aeme, model = model, path = path, verbose = F)
  plot_output(aeme)
  plot_output(aeme, var_sim = "LKE_evpflx")
  lake_dir <- get_lake_dir(aeme = aeme, path = path)
  wlev <- read_model_wlev(lake_dir = lake_dir, model = model)
  # p1 <- plot_output(aeme, var_sim = "CHM_oxynal")
  # testthat::expect_true(ggplot2::is.ggplot(p1))
  # p2 <- plot_output(aeme, var_sim = "LKE_tli4")
  # testthat::expect_true(ggplot2::is.ggplot(p2))
  lake_dir <- get_lake_dir(aeme = aeme, path = path)
  outfile <- get_model_outfile(aeme = aeme, model = model)

  vars_sim <- "HYD_temp"
  out <- read_gotm_output(file = outfile$gotm_wet["output"],
                          vars_sim = vars_sim)
  testthat::expect_true(nrow(out$HYD_temp) > 2)
  out2 <- read_gotm_output(file = outfile$gotm_wet["output"],
                           vars_sim = "HYD_temp", depths = c(0, 11))
  testthat::expect_true(nrow(out2$HYD_temp) == 2)
  testthat::expect_true(all(out2$HYD_temp[1, ] >= out2$HYD_temp[2, ]))
  out3 <- read_gotm_output(file = outfile$gotm_wet["output"],
                           vars_sim = "HYD_temp", depths = c(0, 11),
                           dates = c("2020-09-01", "2020-12-02"))
  testthat::expect_true(ncol(out3$HYD_temp) == 2)

  lke <- lake(aeme)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "output", "output.nc"))
  testthat::expect_true(file_chk)

  outp1 <- output(aeme)
  testthat::expect_true(outp1$n_members > 0)
  aeme <- remove_output(aeme)
  outp2 <- output(aeme)
  testthat::expect_true(outp2$n_members == 0)
})

test_that("running GOTM-WET works", {
  skip_if_models_unavailable(c("gotm_wet"))
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- get_model_controls(use_bgc = TRUE)
  model_controls <- model_controls |>
    dplyr::mutate(simulate = dplyr::case_when(
      var_aeme == "ZOO_zoo1" ~ TRUE,
      .default = simulate
    ))
  inf_factor = c("gotm_wet" = 1)
  outf_factor = c("gotm_wet" = 1)
  model <- c("gotm_wet")
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, inf_factor = inf_factor,
                     ext_elev = 5, use_bgc = TRUE)
  aeme <- run_aeme(aeme = aeme, model = model, verbose = FALSE,
                   model_controls = model_controls, path = path)
  plot_output(aeme = aeme, model = model)
  # plot_output(aeme = aeme, model = model, var_sim = "CHM_oxy")
  lke <- lake(aeme)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "output", "output.nc"))
  testthat::expect_true(file_chk)
})

test_that("running GOTM with a spinup works", {
  skip_if_models_unavailable(c("gotm_wet"))
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- get_model_controls()
  inf_factor <- c("gotm_wet" = 1)
  outf_factor <- c("gotm_wet" = 1)
  model <- c("gotm_wet")


  tim <- time(aeme)
  tim[["spin_up"]][[model]] <- 200
  time(aeme) <- tim

  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, inf_factor = inf_factor,
                     ext_elev = 5, use_bgc = FALSE)
  aeme <- run_aeme(aeme = aeme, model = model, verbose = FALSE,
                   model_controls = model_controls, path = path)
  lke <- lake(aeme)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "output", "output.nc"))

  plot_output(aeme = aeme, model = model, var_sim = "LKE_outflow",
              level = TRUE, print_plots = FALSE,
              var_lims = c(0, 30))

  p1 <- plot_output(aeme = aeme, model = model, var_sim = "HYD_temp",
                    level = TRUE, print_plots = FALSE,
                    var_lims = c(0, 30), ylim = c(0, 16))
  testthat::expect_true(all(ggplot2::is_ggplot(p1)))

  testthat::expect_true(file_chk)
})

test_that("editing and running GOTM-WET via the thin path-based wrapper works", {
  skip_if_models_unavailable(c("gotm_wet"))
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- get_model_controls()
  model <- "gotm_wet"
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, ext_elev = 5,
                     use_bgc = FALSE)

  # From here on, only `path_gotm` is used -- no `aeme` object required,
  # mirroring a GOTM-WET-only user's workflow of editing an existing
  # configuration directory, running it, and loading the output.
  lake_dir <- get_lake_dir(aeme = aeme, path = path)
  path_gotm <- file.path(lake_dir, "gotm_wet")

  # -- parameters --
  old_dt <- get_gotm_param(path_gotm, "time.dt")
  set_gotm_param(path_gotm, `time.dt` = 1800)
  testthat::expect_equal(get_gotm_param(path_gotm, "time.dt"), 1800)

  # -- init --
  t_prof_file <- file.path(path_gotm, "inputs", "t_prof_file.dat")
  n_depths <- length(readLines(t_prof_file)) - 1
  new_temp <- seq(20, 10, length.out = n_depths)
  new_salt <- rep(1, n_depths)
  set_gotm_init(path_gotm, temp = new_temp, salt = new_salt)
  t_prof <- read.table(t_prof_file, skip = 1,
                       col.names = c("depth", "value"))
  s_prof <- read.table(file.path(path_gotm, "inputs", "s_prof_file.dat"),
                       skip = 1, col.names = c("depth", "value"))
  # Row order (and depths) in each .dat file is left untouched -- only the
  # value column is overwritten positionally. The first row is depth 0 (the
  # surface), so its new value also becomes gotm.yaml's
  # surface$sst$constant_value seed.
  testthat::expect_equal(t_prof$value, new_temp)
  testthat::expect_equal(s_prof$value, new_salt)
  gotm <- yaml::read_yaml(file.path(path_gotm, "gotm.yaml"))
  testthat::expect_equal(gotm$surface$sst$constant_value, new_temp[1])

  # -- inflows --
  inf_data <- inflows(aeme)[["data"]]
  set_gotm_inflows(path_gotm, inf_list = inf_data)
  testthat::expect_true(file.exists(file.path(path_gotm, "inputs",
                                              "inf_flow_FWMT.dat")))

  # -- outflows --
  outf_data <- outflows(aeme)[["data"]]
  set_gotm_outflows(path_gotm, outf = outf_data)
  testthat::expect_true(file.exists(file.path(path_gotm, "inputs",
                                              "outf_outflow.dat")))

  # -- run and load output, all from the path alone --
  run_gotm_wet(sim_folder = path_gotm)
  outfile <- file.path(path_gotm, "output", "output.nc")
  testthat::expect_true(file.exists(outfile))
  out <- read_gotm_output(file = outfile, vars_sim = "HYD_temp")
  testthat::expect_true(nrow(out$HYD_temp) > 0)

  out_raw <- read_gotm_output(file = outfile, raw_output = TRUE)
  testthat::expect_true(nrow(out_raw$temp) > 0)
  testthat::expect_null(out_raw$HYD_temp)
  testthat::expect_true(is_aeme_output_raw(out_raw))
  # "Date" is structural, not a variable -- must survive the raw-mode
  # rename sweep unchanged (key_naming has a real Date -> "date" entry for
  # gotm_wet that previously renamed it out from under every consumer)
  testthat::expect_true("Date" %in% names(out_raw))
  testthat::expect_s3_class(out_raw$Date, "Date")
  testthat::expect_error(
    read_gotm_output(file = outfile, vars_sim = "HYD_temp",
                     raw_output = TRUE, depths = c(0, 5))
  )

  out_min <- read_gotm_output(file = outfile, vars_sim = "HYD_temp",
                              load_all = FALSE)
  testthat::expect_true(length(out_min) < length(out))
})

test_that("can get variable indices after running the model", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- get_model_controls()
  inf_factor <- c("gotm_wet" = 1)
  outf_factor <- c("gotm_wet" = 1)
  model <- c("gotm_wet")
  skip_if_models_unavailable(model)
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls,
                     ext_elev = 5, use_bgc = FALSE)
  aeme <- run_aeme(aeme = aeme, model = model, verbose = FALSE,
                   model_controls = model_controls, path = path)
  var_indices <- get_var_indices(model = model, aeme = aeme, path = path,
                                 vars_sim = "HYD_temp", use_obs = TRUE)
  testthat::expect_true(length(var_indices) > 0)
  testthat::expect_true(is.list(var_indices))
  testthat::expect_true(length(var_indices$HYD_temp$date_index) == 10)

})
