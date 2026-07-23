#' Write initial conditions file for a Simstrat simulation
#'
#' @param init_prof data.frame; initial profile with columns `depth`
#' (positive-down, 0 at the surface), `temperature`, and `salt`.
#' @param path_simstrat filepath; to the Simstrat directory.
#' @param surface_elev numeric; lake surface elevation (m), see
#' \code{\link{make_stg_simstrat}}.
#'
#' @return Writes `InitialConditions.dat` to `path_simstrat`.
#' @noRd
initialise_simstrat <- function(init_prof, path_simstrat, surface_elev) {

  prof <- init_prof |>
    dplyr::arrange(depth) |>
    dplyr::mutate(depth = -depth,
                  U = 0, V = 0,
                  k = 3.0e-06, eps = 5.0e-10)

  lines <- c(
    "Depth [m]    U [m/s]    V [m/s]    T [°C]    S [‰]    k [J/kg]    eps [W/kg]",
    paste(
      format(prof$depth, nsmall = 2),
      format(prof$U, nsmall = 3),
      format(prof$V, nsmall = 3),
      format(prof$temperature, nsmall = 3),
      format(prof$salt, nsmall = 3),
      format(prof$k, scientific = TRUE),
      format(prof$eps, scientific = TRUE)
    )
  )
  writeLines(lines, file.path(path_simstrat, "InitialConditions.dat"))

  invisible()
}
