#' Write meteorological forcing file for use in Simstrat
#'
#' @param met data.frame of standardised AEME meteorological variables (as
#' produced by \code{\link{expand_met}}/\code{\link{standardise_met}}).
#' @param path_simstrat filepath; to the Simstrat directory.
#' @param ref_year integer; Simstrat `Simulation.Reference year`.
#'
#' @return Writes `MeteoForcing.dat` to `path_simstrat`.
#' @noRd
make_met_simstrat <- function(met, path_simstrat, ref_year) {

  day <- date_to_simstrat_day(met$Date, ref_year)

  met_out <- data.frame(
    day  = day,
    u    = met$MET_wnduvu,
    v    = met$MET_wnduvv,
    Tair = met$MET_tmpair,
    sol  = met$MET_radswd,
    vap  = met$MET_prvapr,
    cloud = met$MET_cldcvr,
    # mm/day -> m/hr
    rain = met$MET_pprain / 1000 / 24
  )

  met_out <- met_out |>
    dplyr::mutate(dplyr::across(dplyr::everything(), \(x) signif(x, 6)),
                  dplyr::across(dplyr::everything(), \(x) format(x, nsmall = 4,
                                                                 width = 10)))

  lines <- c(
    "  Time [d]     u [m/s]  v [m/s] Tair [degC] sol [W/m2] vap [mbar] cloud [-] rain [m/hr]",
    apply(met_out, 1, paste, collapse = " ")
  )
  writeLines(lines, file.path(path_simstrat, "MeteoForcing.dat"))

  invisible()
}
