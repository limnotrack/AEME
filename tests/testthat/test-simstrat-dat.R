# read_simstrat_dat() reads Simstrat's raw <var>_out.dat text output, the
# same output write_simstrat_nc() consolidates into output.nc. Reading the
# text needs the Simstrat binary to have produced it, so -- as in
# test-simstrat.R -- the run-dependent tests only run where the bundled
# Windows binary exists.
skip_simstrat_dat_run <- function() {
  if (AEME:::.detect_os() != "windows") {
    testthat::skip("Simstrat-AED2 binary is only bundled for Windows")
  }
}

# The netCDF written by write_simstrat_nc() stores values at ncdf4's default
# `float` precision, so its values are the *less* precise of the two: reading
# the .dat text keeps full double precision. Standardised output is rounded
# to 4 dp by interp_static_grid(), which is therefore the granularity the two
# paths can differ by.
expect_out_equal <- function(a, b, tol = 1e-4) {
  testthat::expect_lt(max(abs(a - b), na.rm = TRUE), tol * 1.001)
}

test_that("read_simstrat_dat_file() reads a Simstrat .dat file", {
  skip_simstrat_dat_run()

  path <- file.path(tempdir(), "simstrat_dat")
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  aeme <- yaml_to_aeme(path = aeme_dir, file = "aeme.yaml")
  aeme <- build_aeme(path = path, aeme = aeme, model = "simstrat_aed2",
                     model_controls = get_model_controls(), ext_elev = 5,
                     use_bgc = FALSE)
  aeme <- run_aeme(aeme = aeme, model = "simstrat_aed2", path = path,
                   verbose = FALSE)
  sim_folder <- file.path(get_lake_dir(aeme = aeme, path = path),
                          "simstrat_aed2")
  out_dir <- file.path(sim_folder, "output")
  testthat::expect_true(dir.exists(out_dir))

  # Depth profile: one column per output depth, plus the day-number column
  d <- read_simstrat_dat_file(file.path(out_dir, "T_out.dat"))
  testthat::expect_true(length(d$depths) > 1)
  testthat::expect_equal(ncol(d$values), length(d$depths))
  testthat::expect_equal(nrow(d$values), length(d$day))
  testthat::expect_true(all(d$depths <= 0))  # negative-down from the surface
  testthat::expect_true(diff(range(d$values, na.rm = TRUE)) > 1)

  # Surface variable: a single value column
  w <- read_simstrat_dat_file(file.path(out_dir, "WaterH_out.dat"))
  testthat::expect_equal(ncol(w$values), 1)
  testthat::expect_equal(w$day, d$day)

  # Row windowing must return exactly the rows it says it does
  win <- read_simstrat_dat_file(file.path(out_dir, "T_out.dat"),
                                skip_rows = 10, n_rows = 5)
  testthat::expect_equal(nrow(win$values), 5)
  testthat::expect_equal(win$offset, 10L)
  testthat::expect_equal(win$values, d$values[11:15, ])
  testthat::expect_equal(win$day, d$day[11:15])

  testthat::expect_error(read_simstrat_dat_file(file.path(out_dir, "nope.dat")))
})

test_that("read_simstrat_dat() matches read_simstrat_output()", {
  skip_simstrat_dat_run()

  path <- file.path(tempdir(), "simstrat_dat_bgc")
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  aeme <- yaml_to_aeme(path = aeme_dir, file = "aeme.yaml")
  aeme <- build_aeme(path = path, aeme = aeme, model = "simstrat_aed2",
                     model_controls = get_model_controls(use_bgc = TRUE),
                     ext_elev = 5, use_bgc = TRUE)
  aeme <- run_aeme(aeme = aeme, model = "simstrat_aed2", path = path,
                   verbose = FALSE)
  sim_folder <- file.path(get_lake_dir(aeme = aeme, path = path),
                          "simstrat_aed2")
  outfile <- get_model_outfile(aeme = aeme)[["simstrat_aed2"]]
  testthat::expect_true(file.exists(outfile))

  vars_sim <- c("HYD_temp", "CHM_oxy")

  # Same keys, whichever way the output is read
  onc <- read_simstrat_output(file = outfile, vars_sim = vars_sim)
  odat <- read_simstrat_dat(sim_folder = sim_folder, vars_sim = vars_sim,
                            load_all = TRUE)
  testthat::expect_equal(sort(names(onc)), sort(names(odat)))
  testthat::expect_equal(as.character(onc$Date), as.character(odat$Date))
  testthat::expect_s3_class(odat, "aeme_output")

  # ... and the same values. Compared on an explicitly supplied depth grid:
  # left to derive its own, the netCDF path's float lake level rounds a
  # handful of standardised depths to a different centimetre.
  deps <- c(0, 1, 2, 5, 10)
  a <- read_simstrat_output(file = outfile, vars_sim = vars_sim, depths = deps)
  b <- read_simstrat_dat(sim_folder = sim_folder, vars_sim = vars_sim,
                         depths = deps)
  for (v in vars_sim) {
    testthat::expect_equal(dim(b[[v]]), c(length(deps), length(b$Date)))
    expect_out_equal(a[[v]], b[[v]])
  }
  expect_out_equal(a$LKE_lvlwtr, b$LKE_lvlwtr, tol = 1e-3)
  expect_out_equal(a$HYD_surft, b$HYD_surft, tol = 1e-3)
  expect_out_equal(a$LKE_Qh, b$LKE_Qh, tol = 1e-2)

  # date_index / dates subsetting, and the row-skipping that goes with it
  di <- 20:120
  full <- read_simstrat_dat(sim_folder = sim_folder, vars_sim = "HYD_temp",
                            depths = deps, incl_fluxes = FALSE)
  sub <- read_simstrat_dat(sim_folder = sim_folder, vars_sim = "HYD_temp",
                           depths = deps, date_index = di,
                           incl_fluxes = FALSE)
  testthat::expect_equal(sub$HYD_temp, full$HYD_temp[, di])
  testthat::expect_equal(as.character(sub$Date), as.character(full$Date[di]))
  testthat::expect_false(any(c("LKE_Qh", "HYD_surft") %in% names(sub)))

  tail_i <- (length(full$Date) - 20):length(full$Date)
  tl <- read_simstrat_dat(sim_folder = sim_folder, vars_sim = "HYD_temp",
                          depths = deps, date_index = tail_i,
                          incl_fluxes = FALSE)
  testthat::expect_equal(tl$HYD_temp, full$HYD_temp[, tail_i])

  dts <- full$Date[c(3, 40, 90)]
  bd <- read_simstrat_dat(sim_folder = sim_folder, vars_sim = "HYD_temp",
                          depths = deps, dates = dts, incl_fluxes = FALSE)
  testthat::expect_equal(as.character(bd$Date), as.character(dts))

  # raw output: native names, native depths, no unit conversion
  r1 <- read_simstrat_output(file = outfile, vars_sim = "HYD_temp",
                             dates = dts, raw_output = TRUE)
  r2 <- read_simstrat_dat(sim_folder = sim_folder, vars_sim = "HYD_temp",
                          dates = dts, raw_output = TRUE, load_all = TRUE)
  testthat::expect_s3_class(r2, "aeme_output_raw")
  testthat::expect_equal(sort(names(r1)), sort(names(r2)))
  testthat::expect_equal(r1$LKE_depths, r2$LKE_depths)
  expect_out_equal(r1$T, r2$T, tol = 1e-3)
  testthat::expect_equal(unname(attr(r2, "var_units")[["T"]]),
                         unname(attr(r1, "var_units")[["T"]]))
  testthat::expect_error(
    read_simstrat_dat(sim_folder = sim_folder, depths = 1, raw_output = TRUE)
  )

  # water level
  w1 <- read_model_wlev(lake_dir = get_lake_dir(aeme = aeme, path = path),
                        model = "simstrat_aed2")
  w2 <- read_simstrat_dat_wlev(sim_folder = sim_folder)
  testthat::expect_equal(as.character(w1$Date), as.character(w2$Date))
  expect_out_equal(w1$LKE_lvlwtr, w2$LKE_lvlwtr, tol = 1e-3)
})

test_that("read_model_outputs(use_dat =) reads the same output either way", {
  skip_simstrat_dat_run()

  path <- file.path(tempdir(), "simstrat_dat_bgc")
  aeme <- yaml_to_aeme(path = system.file("extdata/lake/", package = "AEME"),
                       file = "aeme.yaml")
  lake_dir <- get_lake_dir(aeme = aeme, path = path)
  model <- "simstrat_aed2"
  testthat::skip_if_not(dir.exists(file.path(lake_dir, model, "output")))
  outfile <- file.path(lake_dir, model, "output", "output.nc")

  deps <- c(0, 2, 5, 10)
  di <- 20:120
  args <- list(lake_dir = lake_dir, model = model, vars_sim = "HYD_temp",
               depths = deps, date_index = di, incl_fluxes = FALSE)

  a <- do.call(read_model_outputs, c(args, list(use_dat = FALSE)))
  b <- do.call(read_model_outputs, c(args, list(use_dat = TRUE)))
  testthat::expect_equal(sort(names(a)), sort(names(b)))
  testthat::expect_equal(as.character(a$Date), as.character(b$Date))
  testthat::expect_equal(dim(b$HYD_temp), c(length(deps), length(di)))
  expect_out_equal(a$HYD_temp, b$HYD_temp)
  testthat::expect_equal(class(a), class(b))
  testthat::expect_equal(attr(a, "model"), attr(b, "model"))
  testthat::expect_null(dim(b$LKE_lvlwtr))

  # dates=, rather than date_index=
  d1 <- read_model_outputs(lake_dir = lake_dir, model = model,
                           vars_sim = "HYD_temp", depths = deps,
                           dates = a$Date[1:5], use_dat = FALSE)
  d2 <- read_model_outputs(lake_dir = lake_dir, model = model,
                           vars_sim = "HYD_temp", depths = deps,
                           dates = a$Date[1:5], use_dat = TRUE)
  testthat::expect_equal(as.character(d1$Date), as.character(d2$Date))
  expect_out_equal(d1$HYD_temp, d2$HYD_temp)

  # derived variables are added on the text path too
  v <- read_model_outputs(lake_dir = lake_dir, model = model,
                          vars_sim = "HYD_thmcln", use_dat = TRUE)
  testthat::expect_true("HYD_thmcln" %in% names(v))
  testthat::expect_null(dim(v$HYD_thmcln))

  # load_all is passed through
  testthat::expect_gt(
    length(read_model_outputs(lake_dir = lake_dir, model = model,
                              vars_sim = "HYD_temp", use_dat = TRUE,
                              load_all = TRUE)),
    length(read_model_outputs(lake_dir = lake_dir, model = model,
                              vars_sim = "HYD_temp", use_dat = TRUE,
                              load_all = FALSE))
  )

  # use_dat = NULL: the netCDF is used when there is one ...
  auto <- do.call(read_model_outputs, args)
  testthat::expect_identical(auto$HYD_temp, a$HYD_temp)

  # ... and the text output when there is not
  hidden <- paste0(outfile, ".bak")
  file.rename(outfile, hidden)
  on.exit(if (file.exists(hidden)) file.rename(hidden, outfile), add = TRUE)
  auto2 <- do.call(read_model_outputs, args)
  testthat::expect_identical(auto2$HYD_temp, b$HYD_temp)
  # ... but only as a fall-back: use_dat = FALSE still demands the netCDF
  testthat::expect_error(
    do.call(read_model_outputs, c(args, list(use_dat = FALSE)))
  )
  file.rename(hidden, outfile)

  testthat::expect_error(
    read_model_outputs(lake_dir = lake_dir, model = "glm_aed",
                       vars_sim = "HYD_temp", use_dat = TRUE),
    "use_dat"
  )
})

test_that("read_simstrat_dat() handles missing or out-of-range output", {
  skip_simstrat_dat_run()

  path <- file.path(tempdir(), "simstrat_dat_bgc")
  aeme <- yaml_to_aeme(path = system.file("extdata/lake/", package = "AEME"),
                       file = "aeme.yaml")
  sim_folder <- file.path(get_lake_dir(aeme = aeme, path = path),
                          "simstrat_aed2")
  testthat::skip_if_not(dir.exists(file.path(sim_folder, "output")))

  # No output files at all
  empty <- file.path(tempdir(), "simstrat_dat_empty")
  dir.create(empty, showWarnings = FALSE)
  e1 <- read_simstrat_dat(out_dir = empty, ref_year = 2020)
  testthat::expect_true(is_model_error(e1))

  # date_index past the end of the simulation
  e2 <- read_simstrat_dat(sim_folder = sim_folder, date_index = 1:1e5)
  testthat::expect_true(is_model_error(e2))

  # dates that were never simulated
  testthat::expect_error(
    read_simstrat_dat(sim_folder = sim_folder, dates = as.Date("1900-01-01"))
  )

  # no simstrat.par to resolve the output directory from
  testthat::expect_error(read_simstrat_dat(sim_folder = tempdir()))

  # a variable the model didn't output comes back empty, not an error --
  # exactly as read_simstrat_output() reports it
  out <- read_simstrat_dat(sim_folder = sim_folder,
                           vars_sim = c("HYD_temp", "PHY_notavar"),
                           incl_fluxes = FALSE)
  testthat::expect_null(out[["PHY_notavar"]])
  testthat::expect_true(nrow(out[["HYD_temp"]]) > 2)
})

test_that("read_simstrat_dat() reads AED sediment-zone output as a grouped var", {
  skip_simstrat_dat_run()

  # The bundled Simstrat-AED2 configuration has no benthic zones, so the
  # <var>_zone_out.dat family is written here directly: one column per zone,
  # headed by that zone's height, deliberately out of order to check the
  # columns are sorted onto the zone axis rather than taken as written.
  path <- file.path(tempdir(), "simstrat_dat_bgc")
  aeme <- yaml_to_aeme(path = system.file("extdata/lake/", package = "AEME"),
                       file = "aeme.yaml") |> 
    build_aeme(model = "simstrat_aed2", use_bgc = TRUE, ext_elev = 3, path = path) |> 
    run_aeme()
  src <- file.path(get_lake_dir(aeme = aeme, path = path), "simstrat_aed2")
  testthat::skip_if_not(dir.exists(file.path(src, "output")))

  sim_folder <- file.path(tempdir(), "simstrat_dat_zone")
  unlink(sim_folder, recursive = TRUE)
  dir.create(sim_folder, recursive = TRUE)
  file.copy(list.files(src, full.names = TRUE), sim_folder, recursive = TRUE)
  out_dir <- file.path(sim_folder, "output")

  tdat <- read_simstrat_dat_file(file.path(out_dir, "T_out.dat"))
  zone_h <- c(7.5, 0.5, 11, 3.25)
  set.seed(42)
  vals <- matrix(round(runif(length(tdat$day) * length(zone_h), 0, 50), 3),
                 ncol = length(zone_h))
  writeLines(
    c(paste(c("Datetime", sprintf("%12.3f", zone_h)), collapse = ","),
      apply(cbind(tdat$day, vals), 1,
            \(r) paste(sprintf("%18.8E", r), collapse = ","))),
    file.path(out_dir, "OXY_oxy_dsf_zone_out.dat")
  )

  out <- read_simstrat_dat(sim_folder = sim_folder, load_all = TRUE)
  g <- out[["OXY_oxy_dsf_zone"]]
  testthat::expect_s3_class(g, "aeme_grouped_var")
  testthat::expect_equal(g$dim_names, c("zone", "time"))
  testthat::expect_equal(g$dim_values$zone, sort(zone_h))
  testthat::expect_equal(g$value, t(vals[, order(zone_h)]))
  testthat::expect_equal(dim(g$value), c(length(zone_h), length(out$Date)))

  # The zone axis must not leak into the water-column depth grid
  plain <- read_simstrat_dat(sim_folder = src, load_all = TRUE)
  testthat::expect_equal(out$LKE_depths, plain$LKE_depths)

  # ... and it survives a date window
  di <- 10:30
  sub <- read_simstrat_dat(sim_folder = sim_folder, load_all = TRUE,
                           date_index = di)
  testthat::expect_equal(sub[["OXY_oxy_dsf_zone"]]$value, g$value[, di])
})
