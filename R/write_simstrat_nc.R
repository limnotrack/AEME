#' Consolidate Simstrat-AED2 text output into a single netCDF file
#'
#' Simstrat writes one plain-text `.dat` file per output variable (see
#' `strat_outputfile.f90` in the Simstrat source), unlike GLM-AED and
#' GOTM-WET which write netCDF directly. This function reads every
#' `<var>_out.dat` file in the simulation's output directory and writes them
#' into a single compressed `output.nc`, so that Simstrat-AED2 output can be
#' read with the same netCDF-based tooling
#' (\code{\link{read_model_outputs}}, \code{\link{get_model_outfile}}, ...)
#' used for the other models, and so the on-disk output is much smaller than
#' the raw text files.
#'
#' @param sim_folder character; path to the `simstrat_aed2` simulation
#' directory (containing `simstrat.par` and the output directory referenced
#' by its `Output.Path`).
#' @param remove_dat logical; delete the source `<var>_out.dat` files after
#' they have been written to `output.nc`. Default `TRUE` (this is the actual
#' disk-space saving -- keeping both would use more space, not less).
#'
#' @return Invisibly returns the path to the written `output.nc` file, or
#' `NULL` if no output files were found.
#'
#' @importFrom ncdf4 ncdim_def ncvar_def nc_create ncvar_put nc_close
#' @export
write_simstrat_nc <- function(sim_folder, remove_dat = TRUE) {

  par <- jsonlite::fromJSON(file.path(sim_folder, "simstrat.par"),
                           simplifyVector = FALSE)
  ref_year <- as.integer(par[["Simulation"]][["Reference year"]])
  out_dir <- file.path(sim_folder, par[["Output"]][["Path"]])

  dat_files <- list.files(out_dir, pattern = "_out\\.dat$", full.names = TRUE)
  if (length(dat_files) == 0) {
    cli::cli_warn(c("!" = "No Simstrat output {.file *_out.dat} files found in
                    {.path {out_dir}}."))
    return(invisible(NULL))
  }
  var_names <- gsub("_out\\.dat$", "", basename(dat_files))

  read_one <- function(f) {
    header <- strsplit(readLines(f, n = 1), ",")[[1]]
    header <- gsub('"', "", header)
    depths <- suppressWarnings(as.numeric(header[-1]))
    body <- utils::read.csv(f, skip = 1, header = FALSE, na.strings = "NaN")
    list(day = body[[1]], depths = depths,
        values = as.matrix(body[, -1, drop = FALSE]))
  }
  data_list <- stats::setNames(lapply(dat_files, read_one), var_names)

  # All output files share the same simulation output times
  dt <- simstrat_day_to_date(data_list[[1]]$day, ref_year)
  time_sec <- as.numeric(dt)

  # z grid (positive-down depth, ascending from surface) shared by every
  # volume/face-grid variable; Simstrat stores these as negative-down offsets
  # from the surface (OutputDepthReference = "surface")
  z_source <- Find(function(x) length(x$depths) > 1, data_list)
  has_z <- !is.null(z_source)
  z <- if (has_z) sort(-z_source$depths) else 0

  dim_time <- ncdf4::ncdim_def("time", units = "seconds since 1970-01-01 00:00:00",
                               vals = time_sec, unlim = TRUE)
  dim_z <- ncdf4::ncdim_def("z", units = "m", vals = z)

  nc_vars <- lapply(var_names, function(v) {
    d <- data_list[[v]]
    if (length(d$depths) > 1) {
      ncdf4::ncvar_def(v, units = "", dim = list(dim_z, dim_time),
                       missval = NaN, compression = 9)
    } else {
      ncdf4::ncvar_def(v, units = "", dim = list(dim_time),
                       missval = NaN, compression = 9)
    }
  })
  names(nc_vars) <- var_names

  nc_file <- file.path(sim_folder, "output.nc")
  nc <- ncdf4::nc_create(nc_file, nc_vars)
  on.exit(ncdf4::nc_close(nc), add = TRUE)

  for (v in var_names) {
    d <- data_list[[v]]
    if (length(d$depths) > 1) {
      # Reorder columns so they match `z`'s ascending (surface -> bottom) order
      ord <- order(-d$depths)
      mat <- t(d$values[, ord, drop = FALSE])
      ncdf4::ncvar_put(nc, v, mat)
    } else {
      ncdf4::ncvar_put(nc, v, as.vector(d$values))
    }
  }

  if (remove_dat) unlink(dat_files)

  return(invisible(nc_file))
}
