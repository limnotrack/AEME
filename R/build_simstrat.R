#' Build a Simstrat-AED2 model from generic inputs
#'
#' @inheritParams build_dycd
#' @inheritParams build_aeme
#' @param lake_shape shapefile
#' @param heights_wdr numeric vector; height of outflow(s), one per named
#' outflow (as used by \code{\link{make_wdr_simstrat}}).
#' @param overwrite_par logical, overwrite the `simstrat.par` file. Default is
#' TRUE
#'
#' @return Directory with Simstrat-AED2 configuration
#' @noRd
#'
build_simstrat <- function(lakename, model_controls, date_range,
                           lake_shape, lat, lon, hyps,
                           lvl, inf, outf, heights_wdr, met,
                           lake_dir, init_prof, init_depth,
                           inf_factor = 1, outf_factor = 1,
                           Kw, use_bgc, overwrite_par = TRUE) {

  cli_safe(paste0("Building Simstrat-AED2 for lake ", lakename), FUN = cli::cli_h2)

  path_simstrat <- file.path(lake_dir, "simstrat_aed2")
  dir.create(path_simstrat, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(path_simstrat, "AED2_inflow"), showWarnings = FALSE,
             recursive = TRUE)
  dir.create(file.path(path_simstrat, "AED2_initcond"), showWarnings = FALSE,
             recursive = TRUE)
  dir.create(file.path(path_simstrat, "Results"), showWarnings = FALSE,
             recursive = TRUE)

  par_file <- file.path(path_simstrat, "simstrat.par")
  if (!file.exists(par_file)) {
    par_file <- system.file("extdata/simstrat_aed2/simstrat.par", package = "AEME")
    file.copy(par_file, file.path(path_simstrat, "simstrat.par"))
    overwrite_par <- TRUE
    cli_inform_safe(c("i" = "Copied in Simstrat par file"))
  }
  aed2_file <- file.path(path_simstrat, "aed2.nml")
  if (!file.exists(aed2_file)) {
    aed2_files <- list.files(system.file("extdata/simstrat_aed2/", package = "AEME"),
                             full.names = TRUE, pattern = "^aed2")
    file.copy(aed2_files, path_simstrat)
    cli_inform_safe(c("i" = "Copied in AED2 nml files and supporting files"))
  }

  # Remove previous output files
  list.files(file.path(path_simstrat, "Results"), full.names = TRUE) |>
    unlink()

  # Reference year for the Simstrat day-number time convention (day 1 = 00:00
  # on 1 Jan of this year; see date_to_simstrat_day())
  ref_year <- as.integer(format(as.Date(date_range[1]), "%Y"))
  start_day <- date_to_simstrat_day(date_range[1], ref_year)
  end_day <- date_to_simstrat_day(date_range[2], ref_year)

  # Current lake surface elevation -- the zero-point for all Simstrat depth
  # coordinates (Bathymetry.dat, InitialConditions.dat, inflow/outflow depths)
  surface_elev <- min(hyps$elev) + init_depth

  par <- jsonlite::fromJSON(file.path(path_simstrat, "simstrat.par"),
                            simplifyVector = FALSE)

  par[["Input"]] <- list(
    "Initial conditions" = "InitialConditions.dat",
    "Grid"               = "Grid.dat",
    "Morphology"         = "Bathymetry.dat",
    "Forcing"            = "MeteoForcing.dat",
    "Absorption"         = "Absorption.dat",
    "Inflow"             = "Qinp.dat",
    "Outflow"            = "Qout.dat",
    "Inflow temperature" = "Tinp.dat",
    "Inflow salinity"    = "Sinp.dat"
  )
  par[["Output"]][["Path"]] <- "Results/"
  # AEME's date-index machinery (get_date_index()) assumes exactly one
  # output row per calendar day, matching GLM-AED/GOTM-WET's convention --
  # it indexes model output positionally (1, 2, 3, ...) rather than by
  # matching actual dates. Output.Times must therefore always be set so
  # thinning_interval * Timestep = 1 day, regardless of the configured
  # timestep.
  par[["Output"]][["Times"]] <- 86400 / par[["Simulation"]][["Timestep s"]]

  par[["AED2Config"]][["AED2ConfigFile"]] <- "aed2.nml"
  par[["AED2Config"]][["PathAED2initial"]] <- "AED2_initcond/"
  par[["AED2Config"]][["PathAED2inflow"]] <- "AED2_inflow/"
  par[["ModelConfig"]][["CoupleAED2"]] <- isTRUE(use_bgc)

  par[["Simulation"]][["Reference year"]] <- ref_year
  par[["Simulation"]][["Start d"]] <- start_day
  par[["Simulation"]][["End d"]] <- end_day

  par[["ModelParameters"]][["lat"]] <- lat
  if ("MET_prsttn" %in% names(met) && any(!is.na(met$MET_prsttn))) {
    # Pa -> hPa
    par[["ModelParameters"]][["p_air"]] <- round(mean(met$MET_prsttn, na.rm = TRUE) / 100, 1)
  }

  make_stg_simstrat(hyps = hyps, path_simstrat = path_simstrat,
                    surface_elev = surface_elev)

  make_met_simstrat(met = met, path_simstrat = path_simstrat, ref_year = ref_year)

  initialise_simstrat(init_prof = init_prof, path_simstrat = path_simstrat,
                      surface_elev = surface_elev)

  make_inf_simstrat(inf = inf, path_simstrat = path_simstrat,
                    surface_elev = surface_elev, inf_factor = inf_factor,
                    model_controls = model_controls, use_bgc = use_bgc,
                    ref_year = ref_year)

  make_wdr_simstrat(outf = outf, heights_wdr = heights_wdr,
                    path_simstrat = path_simstrat, surface_elev = surface_elev,
                    outf_factor = outf_factor, ref_year = ref_year)

  # Light extinction as a constant time series (single depth is valid here --
  # Absorption.dat uses a different, non-integrated read path, see
  # .write_simstrat_absorption_file())
  .write_simstrat_absorption_file(
    df = data.frame(Date = date_range, value = c(Kw, Kw)),
    file = file.path(path_simstrat, "Absorption.dat"),
    comment = "Time [d] (1.col)    z [m] (1.row)    Absorption [m-1] (rest)",
    depth = 0, ref_year = ref_year
  )

  if (use_bgc) {
    max_depth <- surface_elev - min(hyps$elev)
    initialise_aed2(model_controls = model_controls, path_aed2 = path_simstrat,
                    max_depth = max_depth, date_range = date_range,
                    ref_year = ref_year)
  }

  if (overwrite_par) {
    jsonlite::write_json(par, file.path(path_simstrat, "simstrat.par"),
                         pretty = TRUE, auto_unbox = TRUE, null = "null")
  }

  return(invisible())
}

#' Convert a Date/POSIXct to Simstrat's day-number time convention
#'
#' Simstrat represents time as a continuous day count relative to 1 Jan
#' 00:00 of `Simulation.Reference year`, where day 1.0 corresponds to that
#' instant (confirmed from `utilities.f90::init_calendar()` in the Simstrat
#' source: day 1 spans 1 Jan, i.e. `date = ref_year-01-01 + (day - 1)` days).
#'
#' @param date Date or POSIXct vector.
#' @param ref_year integer; Simstrat `Simulation.Reference year`.
#'
#' @return numeric vector of Simstrat day numbers.
#' @noRd
date_to_simstrat_day <- function(date, ref_year) {
  origin <- as.POSIXct(paste0(ref_year, "-01-01 00:00:00"), tz = "UTC")
  as.numeric(difftime(as.POSIXct(date, tz = "UTC"), origin, units = "days")) + 1
}

#' Convert a Simstrat day-number back to a POSIXct date-time
#' @inheritParams date_to_simstrat_day
#' @param day numeric vector of Simstrat day numbers.
#' @return POSIXct vector.
#' @noRd
simstrat_day_to_date <- function(day, ref_year) {
  origin <- as.POSIXct(paste0(ref_year, "-01-01 00:00:00"), tz = "UTC")
  origin + (day - 1) * 86400
}

#' Write a Simstrat "time-depth grid" input file
#'
#' Shared writer for the custom format used by Simstrat's `Inflow`,
#' `Outflow`, `Inflow temperature`, `Inflow salinity`, and AED2 inflow files
#' (confirmed from `strat_lateral.f90`): a comment line, a `<nval_deep>
#' <nval_surface>` line, a `-1 <z1> <z2> ...` depth-header row (depths
#' relative to the lake surface when `nval_deep = 0`), then `<day> <val1>
#' <val2> ...` data rows.
#'
#' Simstrat integrates these depth-value pairs with the trapezoidal rule
#' (`utilities.f90::Integrate()`) to get the actual flux applied to the
#' model -- with only a single depth point the loop that does this
#' (`do i = 2, num`) never executes, so the integrated flux is always
#' exactly zero regardless of the value written (verified empirically: a
#' single-point file produces no lake-level response at all, even for an
#' extreme constant outflow). When `integrate = TRUE`, this writer emits
#' **two** depth points exactly 1 m apart with the *same* value at both --
#' for two equal y-values, the trapezoidal rule gives
#' `0.5 * dx * (y + y) = dx * y`, so `dx = 1` makes the integrated flux
#' exactly equal to `value`, correctly representing a single point
#' source/sink at `depth`.
#'
#' @section Known limitation -- inflow temperature/salinity left inert:
#' Applying the two-point fix to `Qinp.dat`/`Qout.dat` (volume flux) is
#' confirmed stable and gives physically sensible lake-level variation
#' (verified against the water-balance target trajectory). Applying the
#' *same* fix to `Tinp.dat`/`Sinp.dat` (advected inflow temperature/
#' salinity) causes a severe, growing surface-temperature instability
#' (observed: blows up to roughly -350 degC over part of a one-year run),
#' even though the written values themselves are unremarkable -- isolated
#' by reverting each file individually. The exact Simstrat-side mechanism
#' (suspected: a heat/salt flux getting divided by a near-zero volume flux
#' at some layer, given AEME's simplification of combining every named
#' inflow into one flow-weighted-mean series at one representative depth)
#' has not been root-caused yet. Until it is, `make_inf_simstrat()` calls
#' this with `integrate = FALSE` for `Tinp.dat`/`Sinp.dat` and AED2 inflow
#' files, deliberately keeping them at their historical (pre-fix, zero
#' thermal/salinity forcing from inflow) behavior -- a real simplification,
#' but not a regression, and not silently broken.
#'
#' Not used for `Absorption.dat`, which is read by a different Simstrat
#' module (`strat_absorption.f90`) with a distinct, simpler file format (a
#' single `nval` header, not `nval_deep`/`nval_surface`, and no
#' trapezoidal-rule integration) -- see \code{\link{.write_simstrat_absorption_file}}.
#'
#' @param df data.frame with columns `Date` and `value`.
#' @param file destination file path.
#' @param comment character; header comment line.
#' @param depth numeric; depth (m, negative down, relative to the lake
#' surface) at which the value applies.
#' @param ref_year integer; Simstrat `Simulation.Reference year`.
#' @param integrate logical; if `TRUE` (default), write two depth points 1 m
#' apart so Simstrat's trapezoidal-rule integration gives a real, non-zero
#' flux (see Details). If `FALSE`, write the single-point format, which
#' Simstrat reads successfully but always integrates to zero -- use this
#' for quantities not yet confirmed stable at full effect (see the "Known
#' limitation" section).
#'
#' @return Invisibly returns `NULL`.
#' @noRd
.write_simstrat_grid_file <- function(df, file, comment, depth, ref_year,
                                      integrate = TRUE) {

  day <- date_to_simstrat_day(df$Date, ref_year)

  if (integrate) {
    # Depth header must be ascending (Simstrat's Integrate() computes
    # x(i) - x(i-1) with no reordering of the file's own depth values -- see
    # strat_lateral.f90's z_Inp read and utilities.f90::Integrate()). Writing
    # depth points in descending order flips the sign of dx, and therefore
    # the sign of every integrated flux (inflow, outflow, temperature,
    # salinity, AED2 inflow concentrations) that goes through this writer.
    lines <- c(
      comment,
      "0 2",
      paste("-1", format(depth - 1, nsmall = 2), format(depth, nsmall = 2)),
      paste(format(day, nsmall = 4), format(df$value, nsmall = 4),
            format(df$value, nsmall = 4))
    )
  } else {
    lines <- c(
      comment,
      "0 1",
      paste("-1", format(depth, nsmall = 2)),
      paste(format(day, nsmall = 4), format(df$value, nsmall = 4))
    )
  }
  writeLines(lines, file)
  invisible()
}

#' Write a Simstrat `Absorption.dat` light-extinction input file
#'
#' Unlike \code{\link{.write_simstrat_grid_file}}'s format, `Absorption.dat`
#' is read by `strat_absorption.f90` with a single `nval` header line (not
#' `nval_deep`/`nval_surface`) and depths as plain positive values (`abs()`
#' is applied on read), interpolated directly onto the grid rather than
#' integrated -- so a single depth point is valid here.
#'
#' @inheritParams .write_simstrat_grid_file
#' @return Invisibly returns `NULL`.
#' @noRd
.write_simstrat_absorption_file <- function(df, file, comment, depth, ref_year) {

  day <- date_to_simstrat_day(df$Date, ref_year)

  lines <- c(
    comment,
    "1",
    paste("-1", format(abs(depth), nsmall = 2)),
    paste(format(day, nsmall = 4), format(df$value, nsmall = 4))
  )
  writeLines(lines, file)
  invisible()
}
