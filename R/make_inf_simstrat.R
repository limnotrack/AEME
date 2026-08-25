#' Write and configure inflows for a Simstrat-AED2 simulation
#'
#' @param inf list of inflow data.frames.
#' @param path_simstrat filepath; to the Simstrat directory.
#' @param surface_elev numeric; lake surface elevation (m), see
#' \code{\link{make_stg_simstrat}}.
#' @param inf_factor numeric; scaling factor to apply to inflows.
#' @param model_controls dataframe of loaded model controls.
#' @param use_bgc logical; write BGC inflow concentration files.
#' @param ref_year integer; Simstrat `Simulation.Reference year`.
#' @param model character; which Simstrat coupling this is for (`"simstrat_aed2"`
#' or `"simstrat_aed"`), used to select the matching rows of `inf`'s `model`
#' column (if present) and, when `use_bgc`, as the `rename_modelvars()`
#' `type_output` and the BGC inflow subdirectory name (`AED2_inflow` /
#' `AED_inflow`).
#'
#' @return Writes `Qinp.dat`, `Tinp.dat`, `Sinp.dat` (and, if `use_bgc`,
#' `<AED2|AED>_inflow/<var>_inflow.dat` files) to `path_simstrat`.
#' @noRd
make_inf_simstrat <- function(inf, path_simstrat, surface_elev, inf_factor = 1,
                              model_controls, use_bgc = FALSE, ref_year,
                              model = "simstrat_aed2") {

  if (length(inf) == 0) {
    # No inflows: Simstrat requires the files to exist but they can be empty
    for (f in c("Qinp.dat", "Tinp.dat", "Sinp.dat")) {
      writeLines(c("no inflow", "0 1", "-1 0.00", "0.0000 0.0000"),
                 file.path(path_simstrat, f))
    }
    return(invisible())
  }

  for (i in seq_along(inf)) {
    if ("model" %in% colnames(inf[[i]])) {
      inf[[i]] <- inf[[i]][inf[[i]]$model == model, , drop = FALSE]
      inf[[i]]$model <- NULL
    }
  }

  # Combine inflows by date, summing flow and taking a flow-weighted mean of
  # temperature/salinity (Simstrat only accepts a single combined inflow
  # series per state variable in this simple single-depth configuration)
  combined <- dplyr::bind_rows(inf, .id = "inflow_name") |>
    dplyr::group_by(Date) |>
    dplyr::summarise(
      # HYD_temp/CHM_salt must be computed *before* HYD_flow is collapsed to
      # its sum below -- dplyr::summarise() evaluates arguments
      # sequentially
      HYD_temp = stats::weighted.mean(HYD_temp, w = pmax(HYD_flow, 0), na.rm = TRUE),
      CHM_salt = stats::weighted.mean(CHM_salt, w = pmax(HYD_flow, 0), na.rm = TRUE),
      HYD_flow = sum(HYD_flow, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(Date)

  q_df <- data.frame(Date = combined$Date,
                     value = round((combined$HYD_flow * inf_factor) / 86400, 5))
  .write_simstrat_grid_file(
    df = q_df, file = file.path(path_simstrat, "Qinp.dat"),
    comment = 't(1. "column)\tz_Inp"\t(1. "row)\tInp"\t(rest)',
    depth = 0, ref_year = ref_year
  )

  # integrate = FALSE: see the "Known limitation" section of
  # .write_simstrat_grid_file() -- giving inflow temperature/salinity real
  # effect (via the same two-point fix used for Qinp/Qout) causes a severe
  # surface-temperature instability that hasn't been root-caused yet, so
  # these are deliberately left at their historical zero-effect behavior.
  t_df <- data.frame(Date = combined$Date, value = round(combined$HYD_temp, 3))
  .write_simstrat_grid_file(
    df = t_df, file = file.path(path_simstrat, "Tinp.dat"),
    comment = 't(1. "column)\tz_Inp"\t(1. "row)\tInp"\t(rest)',
    depth = 0, ref_year = ref_year, integrate = FALSE
  )

  s_df <- data.frame(Date = combined$Date, value = round(combined$CHM_salt, 3))
  .write_simstrat_grid_file(
    df = s_df, file = file.path(path_simstrat, "Sinp.dat"),
    comment = 't(1. "column)\tz_Inp"\t(1. "row)\tInp"\t(rest)',
    depth = 0, ref_year = ref_year, integrate = FALSE
  )

  if (use_bgc) {
    data("key_naming", package = "AEME", envir = environment())
    deriv_vars <- key_naming |>
      dplyr::filter(derived) |>
      dplyr::pull(var_aeme)
    bgc_vars <- model_controls |>
      dplyr::filter(simulate, !var_aeme %in% deriv_vars,
                    !var_aeme %in% c("DateTime", "HYD_flow", "HYD_temp",
                                     "HYD_dens", "LKE_lvlwtr", "RAD_par",
                                     "RAD_extc", "RAD_secchi", "CHM_salt")) |>
      dplyr::pull(var_aeme)

    all_inf <- dplyr::bind_rows(inf)
    inflow_dir <- if (model == "simstrat_aed") "AED_inflow" else "AED2_inflow"
    for (v in bgc_vars) {
      if (!v %in% names(all_inf)) next
      simstrat_name <- rename_modelvars(v, type_output = model,
                                        warn_unmatched = TRUE)
      if (is.na(simstrat_name) || simstrat_name == "") next

      var_df <- all_inf |>
        dplyr::select(Date, dplyr::all_of(v)) |>
        dplyr::group_by(Date) |>
        dplyr::summarise(value = sum(.data[[v]], na.rm = TRUE), .groups = "drop") |>
        dplyr::arrange(Date)

      # integrate = FALSE for the same reason as Tinp/Sinp above -- BGC
      # concentration advection shares the same at-risk code path in
      # Simstrat (strat_lateral.f90's surface-input handling) as
      # temperature/salinity, and hasn't been separately confirmed stable.
      .write_simstrat_grid_file(
        df = var_df,
        file = file.path(path_simstrat, inflow_dir,
                         paste0(simstrat_name, "_inflow.dat")),
        comment = "depth [m], conc. [mmol/m3 * m2/s]",
        depth = 0, ref_year = ref_year, integrate = FALSE
      )
    }
  }

  invisible()
}
