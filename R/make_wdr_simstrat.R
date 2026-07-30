#' Write and configure outflows for a Simstrat-AED2 simulation
#'
#' @param outf list of outflow data.frames.
#' @param heights_wdr numeric vector; withdrawal elevation(s) (m, absolute),
#' one per named outflow in `outf`.
#' @param path_simstrat filepath; to the Simstrat directory.
#' @param surface_elev numeric; lake surface elevation (m), see
#' \code{\link{make_stg_simstrat}}.
#' @param outf_factor numeric; scaling factor to apply to outflows.
#' @param ref_year integer; Simstrat `Simulation.Reference year`.
#'
#' @details Simstrat's own outflow file format (`Qout.dat`) supports one
#' column per distinct withdrawal depth. AEME combines all named outflows
#' into a single summed series at a single representative withdrawal depth
#' (the mean of `heights_wdr`), matching the level of detail already applied
#' elsewhere in the water-balance calculation
#' (\code{\link{calc_water_balance}}) rather than modelling each outflow's
#' vertical position independently.
#'
#' @return Writes `Qout.dat` to `path_simstrat`.
#' @noRd
make_wdr_simstrat <- function(outf, heights_wdr, path_simstrat, surface_elev,
                              outf_factor = 1, ref_year) {

  if (length(outf) == 0) {
    writeLines(c("no outflow", "0 1", "-1 0.00", "0.0000 0.0000"),
               file.path(path_simstrat, "Qout.dat"))
    return(invisible())
  }

  for (n in names(outf)) {
    if ("model" %in% colnames(outf[[n]])) {
      outf[[n]] <- outf[[n]] |>
        dplyr::filter(model == "simstrat_aed2") |>
        dplyr::select(-model)
    }
    if ("HYD_flow" %in% colnames(outf[[n]])) {
      outf[[n]] <- outf[[n]] |>
        dplyr::rename(outflow = HYD_flow)
    }
  }

  combined <- dplyr::bind_rows(outf, .id = "outflow_name") |>
    dplyr::group_by(Date) |>
    dplyr::summarise(outflow = sum(outflow, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(Date)

  # A height of NA or <= 0 is AEME's sentinel for a floating/surface offtake
  # (see build_aeme.R's `outf$elevation$wbal <- -1` for the water-balance
  # correction term, and make_wdr_glm.R's `outlet_type <- ifelse(heights_wdr
  # < 0, 2, 1)`), NOT a literal elevation. Treating it as a literal negative
  # elevation placed the withdrawal below the lake bottom, which silently
  # broke Simstrat's lake-level response to the water balance (level stayed
  # frozen at its initial value for the whole simulation).
  heights_wdr <- unlist(heights_wdr)
  heights_wdr[is.na(heights_wdr) | heights_wdr <= 0] <- surface_elev
  depth <- round(mean(heights_wdr, na.rm = TRUE) - surface_elev, 2)

  q_df <- data.frame(
    Date = combined$Date,
    value = round(-(combined$outflow * outf_factor) / 86400, 5)
  )
  .write_simstrat_grid_file(
    df = q_df, file = file.path(path_simstrat, "Qout.dat"),
    comment = 't(1. "column)\tz_Inp"\t(1. "row)\tInp"\t(rest)',
    depth = depth, ref_year = ref_year
  )

  invisible()
}
