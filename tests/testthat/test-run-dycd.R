test_that("running DYRESM works", {
  skip_if_models_unavailable(c("dy_cd"))
  aeme_yaml <- system.file("extdata/lake/aeme.yaml", package = "AEME")
  aeme <- yaml_to_aeme(file = aeme_yaml)
  model_controls <- get_model_controls(use_bgc = F)
  model <- c("dy_cd")
  path <- tempdir()
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls,
                     ext_elev = 5, use_bgc = FALSE) |>
    run_aeme()
  file_chk <- check_all_model_outfiles(aeme)
  testthat::expect_true(file_chk)
  outp <- output(aeme)
  testthat::expect_true(!is.null(outp$ens_001$dy_cd))

  lake_dir <- get_lake_dir(aeme = aeme, path = path)
  outfile <- get_model_outfile(aeme = aeme, model = model)

  vars_sim <- "HYD_temp"
  out <- read_dy_output(file = outfile$dy_cd, vars_sim = vars_sim)
  testthat::expect_true(nrow(out$HYD_temp) > 2)
  out2 <- read_dy_output(file = outfile$dy_cd, vars_sim = "HYD_temp",
                         depths = c(0, 11))
  testthat::expect_true(nrow(out2$HYD_temp) == 2)
  testthat::expect_true(all(out2$HYD_temp[1, ] >= out2$HYD_temp[2, ]))
  out3 <- read_dy_output(file = outfile$dy_cd, vars_sim = "HYD_temp",
                         depths = c(0, 11), dates = c("2020-09-01", "2020-12-02"))
  testthat::expect_true(ncol(out3$HYD_temp) == 2)

})

test_that("running DYRESM-CAEDYM works", {
  aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
  aeme <- readRDS(aeme_file)
  path <- tempdir()
  model_controls <- get_model_controls(use_bgc = TRUE)
  model <- c("dy_cd")
  skip_if_models_unavailable(model)
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls,
                     ext_elev = 5, use_bgc = TRUE)
  aeme <- run_aeme(aeme = aeme, verbose = FALSE)

  outfile <- get_model_outfile(aeme = aeme)
  file_chk <- file.exists(outfile[["dy_cd"]])
  testthat::expect_true(file_chk)

  outp <- output(aeme)
  testthat::expect_true(!is.null(outp$ens_001$dy_cd))
})

test_that("editing and running DYRESM-CAEDYM via the thin path-based wrapper works", {
  skip_if_models_unavailable(c("dy_cd"))
  aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
  aeme <- readRDS(aeme_file)
  path <- tempdir()
  unlink(list.files(path, recursive = TRUE, full.names = TRUE))
  model_controls <- get_model_controls(use_bgc = TRUE)
  model <- c("dy_cd")
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, ext_elev = 5,
                     use_bgc = TRUE)

  # From here on, only `path_dy` is used -- no `aeme` object required,
  # mirroring a DYRESM-CAEDYM-only user's workflow of editing an existing
  # configuration directory, running it, and loading the output.
  lake_dir <- get_lake_dir(aeme = aeme, path = path)
  path_dy <- file.path(lake_dir, "dy_cd")

  # -- config file discovery --
  cfg_file <- find_dy_cd_cfg(path_dy)
  testthat::expect_true(file.exists(cfg_file))

  # -- parameters (.cfg and .par) --
  old_kw <- get_dy_cd_param(path_dy, "Kw")
  set_dy_cd_param(path_dy, Kw = old_kw * 1.5, max_layer_thickness = 2,
                  eta_S = 0.42)
  testthat::expect_equal(get_dy_cd_param(path_dy, "Kw"), old_kw * 1.5)
  testthat::expect_equal(get_dy_cd_param(path_dy, "max_layer_thickness"), 2)
  testthat::expect_equal(get_dy_cd_param(path_dy, "eta_S"), 0.42)

  # -- init profile --
  prefix <- sub("\\.cfg$", "", basename(cfg_file))
  pro_file <- file.path(path_dy, paste0(prefix, ".pro"))
  n_rows <- nrow(AEME:::.read_dy_pro(pro_file))
  new_temp <- seq(20, 10, length.out = n_rows)
  set_dy_cd_init(path_dy, temp = new_temp, salt = 0.5)
  prof <- AEME:::.read_dy_pro(pro_file)
  testthat::expect_equal(prof$temp, new_temp)
  testthat::expect_true(all(prof$salt == 0.5))

  # -- inflows --
  # span exactly the window the built .cfg covers, so re-running stays
  # within the time common to every input data set
  cfg_par <- get_dy_cd_param(path_dy, c("start_date", "sim_days"))
  start_doy <- as.integer(substr(cfg_par$start_date, 5, 7))
  start_date <- as.Date(paste0(substr(cfg_par$start_date, 1, 4), "-01-01")) +
    (start_doy - 1)
  dr <- seq(start_date, by = 1, length.out = as.integer(cfg_par$sim_days))
  new_inf <- data.frame(Date = dr, HYD_flow = 500, HYD_temp = 12,
                        CHM_salt = 0.05)
  set_dy_cd_inflows(path_dy, list_inf = list(FWMT = new_inf))
  stg <- AEME:::read_dy_stg(file.path(path_dy, paste0(prefix, ".stg")))
  testthat::expect_equal(stg$inflows$name, "FWMT")

  # -- outflows --
  outf_df <- data.frame(Date = dr, HYD_flow = 400)
  set_dy_cd_outflows(path_dy, outf = list(outflow = outf_df),
                     heights_wdr = c(outflow = stg$base_elev + 0.5))
  testthat::expect_true(file.exists(file.path(path_dy,
                                              paste0(prefix, ".wdr"))))

  # -- run from the path alone; DYsim.nc is what read_dy_output() consumes
  # (its own reading is covered by "running DYRESM works" above) --
  run_dy_cd(sim_folder = path_dy)
  testthat::expect_true(file.exists(file.path(path_dy, "DYsim.nc")))
})

test_that("running DYRESM with a spinup works", {
  skip_if_models_unavailable(c("dy_cd"))
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- get_model_controls()
  inf_factor <- c("dy_cd" = 1)
  outf_factor <- c("dy_cd" = 1)
  model <- c("dy_cd")

  # Add spin up time
  tim <- time(aeme)
  tim[["spin_up"]][[model]] <- 100
  time(aeme) <- tim

  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls, inf_factor = inf_factor,
                     ext_elev = 5, use_bgc = FALSE)
  aeme <- run_aeme(aeme = aeme, model = model,
                   model_controls = model_controls, path = path)
  lke <- lake(aeme)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "DYsim.nc"))
  testthat::expect_true(file_chk)
})
