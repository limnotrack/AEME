#' Write bathymetry and grid files for a Simstrat simulation
#'
#' @param hyps data.frame; hypsograph with columns `elev` and `area`.
#' @param path_simstrat filepath; to the Simstrat directory.
#' @param surface_elev numeric; lake surface elevation (m) used as the
#' zero-point for Simstrat's depth coordinate. Must match the reference used
#' by \code{\link{initialise_simstrat}} and \code{\link{make_infSimstrat}}/
#' \code{\link{make_wdrSimstrat}} so that bathymetry, initial conditions, and
#' inflow/outflow depths are all expressed relative to the same surface.
#' @param thickness_factor numeric; factor to multiply the thickness of the
#' model grid layers. Default is 1.
#'
#' @return Writes `Bathymetry.dat` and `Grid.dat` to `path_simstrat`.
#' @noRd
make_stg_simstrat <- function(hyps, path_simstrat, surface_elev,
                              thickness_factor = 1) {

  bathy <- hyps |>
    dplyr::arrange(dplyr::desc(elev)) |>
    dplyr::mutate(depth = round(elev - surface_elev, 2),
                  area = round(area, 1))

  bathy_lines <- c(
    "Depth [m]    Area [m^2]",
    paste(format(bathy$depth, nsmall = 1, width = 8),
          format(bathy$area, nsmall = 1, width = 12))
  )
  writeLines(bathy_lines, file.path(path_simstrat, "Bathymetry.dat"))

  max_depth <- max(hyps$elev) - min(hyps$elev)
  sub_layers <- get_model_layers(depth = max_depth, thickness_factor = thickness_factor)
  nlev <- nrow(sub_layers)

  writeLines(c("Number of grid points", as.character(nlev)),
             file.path(path_simstrat, "Grid.dat"))

  invisible()
}
