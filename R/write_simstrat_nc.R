#' Consolidate Simstrat-AED2/AED text output into a single netCDF file
#'
#' Simstrat writes one plain-text `.dat` file per output variable (see
#' `strat_outputfile.f90` in the Simstrat source), unlike GLM-AED and
#' GOTM-WET which write netCDF directly. This function reads every
#' `<var>_out.dat` file in the simulation's output directory and writes them
#' into a single compressed `output.nc`, so that Simstrat output can be read
#' with the same netCDF-based tooling (\code{\link{read_model_outputs}},
#' \code{\link{get_model_outfile}}, ...) used for the other models, and so
#' the on-disk output is much smaller than the raw text files.
#'
#' AED's sediment-zone output (Simstrat-AED only, not AED2) is written as a
#' second, separate family of files, `<var>_zone_out.dat` -- one column per
#' benthic zone (labelled by that zone's reference depth), alongside the
#' regular `<var>_out.dat` for the same variable (a single lake-bottom or
#' whole-lake value). Because both exist side by side for the same variable
#' name and `_zone_out.dat` itself ends in `_out.dat`, these can't share the
#' water-column `z` dimension/grid the way regular depth-profile variables
#' do -- doing so silently wrote zone values against the wrong depths (or
#' failed outright on a column-count mismatch) whenever a zone file's column
#' count didn't happen to match the shared `z` grid's length. They get their
#' own `zone` netCDF dimension instead, coordinate-valued by each zone's
#' reference depth (m). Variable names keep their `_zone` suffix (stripping
#' only the trailing `_out.dat`, as for every other file), so e.g.
#' `NIT_amm_dsf_out.dat` and `NIT_amm_dsf_zone_out.dat` become the distinct
#' netCDF variables `NIT_amm_dsf` (dims `time`) and `NIT_amm_dsf_zone` (dims
#' `zone, time`) -- never colliding. Reading them back needs no extra code:
#' \code{\link{read_simstrat_output}}'s `load_all` sweep already routes any
#' variable whose dimensions aren't `(time)` or `(z, time)` through
#' \code{\link{.read_glm_grouped_var}} into an \code{\link{new_grouped_var}}
#' object, the same generic path GLM-AED's own `nzones`-dimensioned output
#' uses.
#'
#' @param sim_folder character; path to the `simstrat_aed2`/`simstrat_aed`
#' simulation directory (containing `simstrat.par` and the output directory
#' referenced by its `Output.Path`).
#' @param remove_dat logical; delete the source `<var>_out.dat` files after
#' they have been written to `output.nc`. Default `FALSE` -- deleting them is
#' the actual disk-space saving (keeping both uses more space, not less), but
#' it also removes what \code{\link{read_simstrat_dat}} reads.
#'
#' Each netCDF variable's `units` and `long_name` attributes are looked up
#' from the package's `key_naming` table (matched on the variable's native
#' Simstrat-AED2/AED name, sediment-zone variables under their base name with
#' the `_zone` suffix stripped). A variable not present in `key_naming` is
#' written with empty `units` and `long_name` equal to its netCDF variable
#' name.
#'
#' @return Invisibly returns the path to the written `output.nc` file, or
#' `NULL` if no output files were found.
#'
#' @importFrom ncdf4 ncdim_def ncvar_def nc_create ncvar_put nc_close
#' @export
write_simstrat_nc <- function(sim_folder, config_file = "simstrat.par",
                              remove_dat = FALSE) {

  par_info <- .simstrat_par_paths(sim_folder = sim_folder,
                                  config_file = config_file)
  ref_year <- par_info$ref_year
  out_dir <- par_info$out_dir

  dat_files <- list.files(out_dir, pattern = "_out\\.dat$", full.names = TRUE)
  if (length(dat_files) == 0) {
    cli::cli_warn(c("!" = "No Simstrat output {.file *_out.dat} files found in
                    {.path {out_dir}}."))
    return(invisible(NULL))
  }
  var_names <- gsub("_out\\.dat$", "", basename(dat_files))
  is_zone <- grepl("_zone$", var_names)

  data_list <- stats::setNames(lapply(dat_files, read_simstrat_dat_file),
                               var_names)

  # All output files share the same simulation output times
  dt <- simstrat_day_to_date(data_list[[1]]$day, ref_year)
  time_sec <- as.numeric(dt)

  # z grid (positive-down depth, ascending from surface) shared by every
  # volume/face-grid variable; Simstrat stores these as negative-down offsets
  # from the surface (OutputDepthReference = "surface"). Sediment-zone
  # variables (is_zone) are excluded here -- they get their own `zone`
  # dimension below, not this one (see @details).
  z_source <- Find(function(x) length(x$depths) > 1,
                   data_list[!is_zone])
  has_z <- !is.null(z_source)
  z <- if (has_z) sort(-z_source$depths) else 0

  dim_time <- ncdf4::ncdim_def("time", units = "seconds since 1970-01-01 00:00:00",
                               vals = time_sec, unlim = TRUE)
  dim_z <- ncdf4::ncdim_def("z", units = "m", vals = z)

  # Sediment-zone dimension (AED's benthic zones), coordinate-valued by each
  # zone's reference depth -- shared by every `_zone`-suffixed variable, the
  # same way dim_z is shared by every regular depth-profile variable.
  has_zone <- any(is_zone)
  if (has_zone) {
    zone_source <- data_list[is_zone][[1]]
    zone_depths <- sort(zone_source$depths)
    dim_zone <- ncdf4::ncdim_def("zone", units = "m", vals = zone_depths)
  }

  # Look up units/long_name for each variable's native Simstrat-AED2/AED name
  # from `key_naming` (the same var_aeme<->model-name table used to translate
  # Simstrat output elsewhere, e.g. read_simstrat_output()). Zone variables
  # are looked up under their base name (the `_zone` suffix stripped) since
  # that's what's recorded in key_naming.
  meta <- .simstrat_var_meta()
  units_map <- meta$units
  long_map <- meta$long_name

  base_names <- ifelse(is_zone, sub("_zone$", "", var_names), var_names)
  var_units <- unname(units_map[base_names])
  var_units[is.na(var_units)] <- ""
  var_long <- unname(long_map[base_names])
  no_long <- is.na(var_long) | !nzchar(var_long)
  var_long[no_long] <- var_names[no_long]

  nc_vars <- Map(function(v, zone, units, longname) {
    d <- data_list[[v]]
    if (zone) {
      ncdf4::ncvar_def(v, units = units, dim = list(dim_zone, dim_time),
                       missval = NaN, longname = longname, compression = 9)
    } else if (length(d$depths) > 1) {
      ncdf4::ncvar_def(v, units = units, dim = list(dim_z, dim_time),
                       missval = NaN, longname = longname, compression = 9)
    } else {
      ncdf4::ncvar_def(v, units = units, dim = list(dim_time),
                       missval = NaN, longname = longname, compression = 9)
    }
  }, var_names, is_zone, var_units, var_long)
  names(nc_vars) <- var_names

  nc_file <- file.path(out_dir, "output.nc")
  nc <- ncdf4::nc_create(nc_file, nc_vars)
  on.exit(ncdf4::nc_close(nc), add = TRUE)

  for (i in seq_along(var_names)) {
    v <- var_names[i]
    d <- data_list[[v]]
    if (is_zone[i]) {
      # Reorder columns so they match `zone_depths`'s ascending order
      ord <- order(d$depths)
      mat <- t(d$values[, ord, drop = FALSE])
      ncdf4::ncvar_put(nc, v, mat)
    } else if (length(d$depths) > 1) {
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
