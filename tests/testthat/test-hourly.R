test_that("output_time_step defaults to daily and round-trips", {
  aeme <- suppressMessages(new_aeme())
  expect_equal(time(aeme)$time_step, 3600)
  expect_equal(time(aeme)$output_time_step, 86400)

  aeme <- suppressMessages(set_time(aeme, output_time_step = 3600))
  expect_equal(time(aeme)$output_time_step, 3600)

  # write_aeme_to_files() carries the new field in time.csv
  td <- withr::local_tempdir()
  ae <- readRDS(system.file("extdata/aeme.rds", package = "AEME"))
  ae <- suppressMessages(set_time(ae, output_time_step = 3600))
  suppressMessages(write_aeme_to_files(ae, path = td))
  tf <- list.files(td, pattern = "time.csv", recursive = TRUE, full.names = TRUE)
  time_csv <- read.csv(tf)
  expect_true("output_time_step" %in% names(time_csv))
  expect_equal(time_csv$output_time_step, 3600)

  # the yaml loader honours output_time_step, defaulting to daily when absent
  ae_yaml <- suppressMessages(
    yaml_to_aeme(path = system.file("extdata/lake/", package = "AEME"),
                 file = "aeme.yaml")
  )
  expect_equal(time(ae_yaml)$output_time_step, 86400)
})

test_that("set_time() rejects output_time_step < time_step", {
  aeme <- suppressMessages(new_aeme())
  expect_error(
    suppressMessages(set_time(aeme, time_step = 7200, output_time_step = 3600)),
    class = "aeme_error_output_time_step"
  )
})

test_that("is_subdaily() and .as_forcing_datetime() classify by spacing", {
  daily <- seq(as.POSIXct("2020-01-01", tz = "UTC"), by = "day", length.out = 10)
  hourly <- seq(as.POSIXct("2020-01-01", tz = "UTC"), by = "hour", length.out = 48)

  expect_false(is_subdaily(as.Date(daily)))
  expect_false(is_subdaily(daily))
  expect_true(is_subdaily(hourly))

  expect_s3_class(.as_forcing_datetime(format(daily, "%Y-%m-%d")), "Date")
  expect_s3_class(.as_forcing_datetime(format(hourly, "%Y-%m-%d %H:%M:%S")),
                  "POSIXct")
})

test_that(".collapse_output_date keeps Date for daily, POSIXct for sub-daily", {
  midnights <- seq(as.POSIXct("2020-01-01", tz = "UTC"), by = "day",
                   length.out = 5)
  hourly <- seq(as.POSIXct("2020-01-01", tz = "UTC"), by = "hour",
                length.out = 30)
  expect_s3_class(.collapse_output_date(midnights), "Date")
  expect_s3_class(.collapse_output_date(hourly), "POSIXct")
  expect_identical(.collapse_output_date(as.Date(midnights)), as.Date(midnights))
})

test_that("aeme_time_axis() reduces to the historical daily sequence", {
  aeme_time <- list(
    start = as.POSIXct("2020-02-01", tz = "UTC"),
    stop  = as.POSIXct("2020-03-01", tz = "UTC"),
    time_step = 3600, output_time_step = 86400,
    spin_up = list(glm_aed = 3, gotm_wet = 3)
  )
  ax_gotm <- aeme_time_axis(aeme_time, "gotm_wet")
  # daily grid from (start - 3 days) to stop, spin-up rows dropped
  ref <- seq.Date(as.Date("2020-02-01") - 3, as.Date("2020-03-01"), by = "day")
  expect_equal(length(ax_gotm$axis), length(ref))
  expect_equal(ax_gotm$index, which(seq_along(ref) > 3))

  # hourly output step -> 24x more rows, spin-up still 3 days
  aeme_time$output_time_step <- 3600
  ax_hr <- aeme_time_axis(aeme_time, "gotm_wet")
  expect_equal(length(ax_hr$axis), (length(ref) - 1) * 24 + 1)
  expect_equal(min(ax_hr$index), 3 * 24 + 1)
})

test_that("build_glm() propagates output_time_step to nsave/subdaily", {
  skip_if_models_unavailable("glm_aed")
  aeme <- suppressMessages(
    yaml_to_aeme(path = system.file("extdata/lake/", package = "AEME"),
                 file = "aeme.yaml")
  )
  path <- withr::local_tempdir()
  mc <- get_model_controls()

  # daily (default): shipped template values preserved
  a_daily <- suppressWarnings(suppressMessages(
    build_aeme(aeme = aeme, path = path, model = "glm_aed",
               model_controls = mc, ext_elev = 5)
  ))
  nml_file <- find_glm_nml(file.path(get_lake_dir(a_daily, path), "glm_aed"))
  nml_d <- read_nml(nml_file)
  expect_equal(nml_d$output$nsave, 24)
  expect_false(isTRUE(nml_d$meteorology$subdaily))
})

test_that("read_glm_output() tolerates a date_index that overshoots the file", {
  skip_if_models_unavailable("glm_aed")
  aeme <- suppressMessages(
    yaml_to_aeme(path = system.file("extdata/lake/", package = "AEME"),
                 file = "aeme.yaml")
  )
  path <- withr::local_tempdir()
  mc <- get_model_controls()
  aeme <- suppressWarnings(suppressMessages(
    build_aeme(aeme = aeme, path = path, model = "glm_aed",
               model_controls = mc, ext_elev = 5)
  ))
  aeme <- suppressWarnings(suppressMessages(
    run_aeme(aeme = aeme, model = "glm_aed", path = path,
             model_controls = mc, verbose = FALSE)
  ))

  outfile <- get_model_outfile(aeme, model = "glm_aed", path = path)[["glm_aed"]]
  nc <- ncdf4::nc_open(outfile)
  on.exit(ncdf4::nc_close(nc))
  n_rec <- length(ncdf4::ncvar_get(nc, "time"))

  # An index ~3x longer than the file (what an hourly output_time_step would
  # reconstruct against a still-daily run) must not empty the output.
  out <- suppressWarnings(
    read_glm_output(nc = nc, vars_sim = "HYD_temp", date_index = seq_len(3 * n_rec))
  )
  expect_false(is_model_error(out))
  expect_equal(length(out[["Date"]]), n_rec)
  expect_equal(ncol(out[["HYD_temp"]]), n_rec)

  # A non-overlapping index still bails cleanly.
  bail <- suppressWarnings(
    read_glm_output(nc = nc, vars_sim = "HYD_temp",
                    date_index = seq(n_rec + 10L, n_rec + 20L))
  )
  expect_true(is_model_error(bail))
})

test_that(".glm_fill_daily() carries once-per-day values across a sub-daily axis", {
  hourly <- seq(as.POSIXct("2020-07-30 01:00", tz = "UTC"), by = "hour",
                length.out = 72)
  x <- rep(NA_real_, 72)
  x[which(format(hourly, "%H") == "00")] <- c(10, 20, 30)  # GLM's daily writes

  filled <- .glm_fill_daily(x, hourly)
  expect_false(anyNA(filled))
  # steps at 01:00..23:00 of 07-30 plus 07-31 00:00 all take the 07-31 write
  expect_equal(unique(filled[1:24]), 10)
  expect_equal(unique(filled[25:48]), 20)
  expect_equal(unique(filled[49:72]), 30)

  # daily series (no NA) is returned untouched
  daily <- as.Date("2020-07-30") + 0:9
  y <- runif(10)
  expect_identical(.glm_fill_daily(y, daily), y)
})

test_that("hourly GLM output loads without all-NA daily-cadence variables", {
  skip_if_models_unavailable("glm_aed")
  path <- withr::local_tempdir()
  file.copy(system.file("extdata/lake", package = "AEME"), path, recursive = TRUE)
  lake_path <- file.path(path, "lake")
  aeme <- suppressMessages(yaml_to_aeme(path = lake_path, "aeme.yaml"))

  met <- read.csv(file.path(lake_path, "data/meteo_era5_hr.csv.gz"))
  met$Date <- as.POSIXct(met$Date, tz = "UTC")
  met <- met[met$Date >= as.POSIXct("2020-07-25", tz = "UTC") &
             met$Date <= as.POSIXct("2021-07-01", tz = "UTC"), ]
  inp <- input(aeme); inp$meteo <- met; input(aeme) <- inp
  aeme <- suppressMessages(set_time(aeme, output_time_step = 3600))
  aeme <- set_time(aeme, time_step = 3600, start = as.POSIXct("2020-08-01", tz = "UTC"),
                  stop = as.POSIXct("2020-08-15", tz = "UTC"), spin_up = 1)

  mc <- get_model_controls()
  aeme <- suppressWarnings(suppressMessages(
    build_aeme(aeme = aeme, path = lake_path, model = "glm_aed",
               model_controls = mc, ext_elev = 5, use_bgc = FALSE)
  ))
  aeme <- suppressWarnings(suppressMessages(
    run_aeme(aeme = aeme, verbose = FALSE)
  ))
  file <- get_model_outfile(aeme, model = "glm_aed", path = lake_path)[["glm_aed"]]
  raw <- read_glm_output(file = file, raw_output = TRUE)

  g <- output(aeme)[[format_ens_label(1)]]$glm_aed
  expect_false(is_model_error(g))
  expect_s3_class(g$Date, "POSIXct")
  # ~14 days of hourly output (minus the 1-day spin-up), sub-daily spacing
  expect_gt(length(g$Date), 24 * 10)
  expect_lt(as.numeric(stats::median(diff(g$Date)), units = "hours"), 2)

  # daily-cadence GLM diagnostics used to come back ~96% NA on a sub-daily run
  for (v in c("HYD_surft", "LKE_Qe", "LKE_Qh", "LKE_A0", "LKE_evprte")) {
    expect_lt(mean(is.na(g[[v]])), 0.05, label = v)
  }
  # and are piecewise-constant within each GLM day (filled, not interpolated)
  day <- as.Date(g$Date - 1)
  runs <- tapply(g$LKE_Qe, day, function(z) length(unique(round(z, 8))))
  expect_true(all(runs <= 1, na.rm = TRUE))
})

test_that("plotting functions work on sub-daily (POSIXct) output", {
  skip_if_models_unavailable("glm_aed")
  path <- withr::local_tempdir()
  file.copy(system.file("extdata/lake", package = "AEME"), path, recursive = TRUE)
  lake_path <- file.path(path, "lake")
  aeme <- suppressMessages(yaml_to_aeme(path = lake_path, "aeme.yaml"))

  met <- read.csv(file.path(lake_path, "data/meteo_era5_hr.csv.gz"))
  met$Date <- as.POSIXct(met$Date, tz = "UTC")
  met <- met[met$Date >= as.POSIXct("2021-01-10", tz = "UTC") &
             met$Date <= as.POSIXct("2021-02-10", tz = "UTC"), ]
  inp <- input(aeme); inp$meteo <- met; input(aeme) <- inp
  aeme <- suppressMessages(set_time(aeme, output_time_step = 3600))
  aeme <- suppressMessages(set_time(
    aeme, time_step = 3600, start = as.POSIXct("2021-01-13", tz = "UTC"),
    stop = as.POSIXct("2021-01-31", tz = "UTC"), spin_up = 1))

  mc <- get_model_controls()
  aeme <- suppressWarnings(suppressMessages(
    build_aeme(aeme = aeme, path = lake_path, model = "glm_aed",
               model_controls = mc, ext_elev = 5, use_bgc = FALSE)))
  aeme <- suppressWarnings(suppressMessages(run_aeme(aeme = aeme, verbose = FALSE)))

  file <- get_model_outfile(aeme, model = "glm_aed", path = lake_path)[["glm_aed"]]
  std <- read_glm_output(file = file)
  raw <- read_glm_output(file = file, raw_output = TRUE)
  plot_model_output(raw, "temp")

  # ggplot backend: heatmap tiles must span the real output step, not 1 s
  p <- plot_output(aeme, "HYD_temp")
  tile_w <- ggplot2::ggplot_build(p)$data[[1]]
  tile_w <- stats::median(tile_w$xmax - tile_w$xmin, na.rm = TRUE)
  expect_equal(tile_w, 3600)

  expect_s3_class(plot_output(aeme, "LKE_evpvol"), "gg")            # 1-D series
  expect_no_error(plot_output(aeme, "HYD_temp", backend = "base")) # image() x-axis
  expect_s3_class(plot_model_output(std, "HYD_temp"), "gg")
  expect_s3_class(plot_glm_output(raw, "temp"), "gg")

  # observation overlay: daily obs must still match a POSIXct model axis
  ad <- align_depth_data(aeme, model = "glm_aed", var_sim = "HYD_temp")
  expect_gt(nrow(ad$lake_adj), 0)
  expect_no_error(plot_output(aeme, "HYD_temp", add_obs = TRUE))
})
