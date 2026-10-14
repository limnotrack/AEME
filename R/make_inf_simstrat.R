#' Write and configure inflows for a Simstrat-AED2 simulation
#'
#' @param inf list of inflow data.frames.
#' @param path_simstrat filepath; to the Simstrat directory.
#' @param bgc_dir filepath; to the BGC (AED/AED2) subdirectory, used (when
#' `use_bgc`) as the base for the `<AED2|AED>_inflow` subdirectory. Defaults
#' to `path_simstrat` for callers that keep BGC files unnested.
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
#' @details
#' ## Combining multiple inflows
#' Simstrat takes a single inflow series per state variable in the
#' single-depth configuration AEME builds. Multiple named inflows are
#' therefore merged per date: `HYD_flow` is **summed**, and every
#' concentration-like quantity (`HYD_temp`, `CHM_salt`, and each AED/AED2
#' inflow variable) is a **flow-weighted mean** across streams. Summing the
#' flows and flow-weighting the concentrations means the combined single
#' series carries the same total mass/heat load as GLM-AED's per-stream
#' inflows (`sum_i C_i * Q_i == (flow-weighted mean C) * sum_i Q_i`). On
#' days with no inflow the flow-weighted mean falls back to a plain mean.
#'
#' ## Units
#' AED/AED2 inflow concentrations are divided by `conversion_aed` (from
#' `model_controls`) before being written, so the files are in the model's
#' native units - the same conversion `make_inf_glm()` applies to GLM-AED's
#' inflow csvs.
#'
#' ## Inflow scalar load (temperature / salinity / BGC)
#' Whether these files actually force the model is controlled by
#' `getOption("AEME.simstrat_inflow_load", "none")`, which takes one of:
#' * `"none"` (default, also `FALSE`) - `Tinp.dat`/`Sinp.dat` and the
#'   AED/AED2 inflow files are written in the single-point (inert) form:
#'   present on disk so Simstrat can open them, but integrating to exactly
#'   zero flux. This is the pre-0.4.x behaviour.
#' * `"bgc"` - only the AED/AED2 inflow **concentration** files are written
#'   depth-integrated, so the inflow carries its nutrient / organic-matter
#'   load (the quantity that matters for a GLM-AED vs Simstrat-AED
#'   comparison). `Tinp.dat`/`Sinp.dat` stay inert.
#' * `"all"` (also `TRUE`) - `Tinp.dat`/`Sinp.dat` are made effective too.
#'   **Experimental:** applying advected inflow heat through this surface
#'   file mechanism currently produces an unphysical warm surface bias
#'   (>40 degC in summer on the test lake) and, historically, a growing
#'   cold instability - Simstrat's inflow scheme does not distribute a
#'   single surface point source the way GLM's plunging-inflow scheme does.
#'
#' In any effective mode the scalar value is forced to `0` on dates where
#' the combined inflow is negligible, so Simstrat never divides a scalar
#' flux by a near-zero volume flux. `Qinp.dat`/`Qout.dat` (volume flux) are
#' always written depth-integrated.
#'
#' @return Writes `Qinp.dat`, `Tinp.dat`, `Sinp.dat` to `path_simstrat` (and,
#' if `use_bgc`, `<AED2|AED>_inflow/<var>_inflow.dat` files to `bgc_dir`).
#' @noRd
make_inf_simstrat <- function(inf, path_simstrat, bgc_dir = path_simstrat,
                              surface_elev, inf_factor = 1,
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

  # Whether inflow temperature/salinity and/or BGC actually force the model.
  # "none"/FALSE -> all inert; "bgc" -> AED inflow effective, T/S inert;
  # "all"/TRUE   -> T/S effective too (experimental, see @details).
  load_mode <- .resolve_simstrat_inflow_load()
  ts_effective  <- load_mode == "all"
  bgc_effective <- load_mode %in% c("bgc", "all")

  # Flow-weighted mean that degrades to a plain mean when there is no flow to
  # weight by (all-zero weights -> stats::weighted.mean() returns NaN).
  fw_mean <- function(x, w) {
    w <- pmax(w, 0)
    w[is.na(w)] <- 0
    if (sum(w) == 0) return(mean(x, na.rm = TRUE))
    stats::weighted.mean(x, w, na.rm = TRUE)
  }

  # Combine inflows by date: sum the flow, flow-weight the scalars (see
  # @details -- this conserves total load across the merge).
  combined <- dplyr::bind_rows(inf, .id = "inflow_name") |>
    dplyr::group_by(Date) |>
    dplyr::summarise(
      # HYD_temp/CHM_salt must be computed *before* HYD_flow is collapsed to
      # its sum below -- dplyr::summarise() evaluates arguments sequentially.
      HYD_temp = fw_mean(HYD_temp, HYD_flow),
      CHM_salt = fw_mean(CHM_salt, HYD_flow),
      HYD_flow = sum(HYD_flow, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(Date)

  # Volume flux in m3/s -- always depth-integrated (confirmed stable).
  q_cumecs <- round((combined$HYD_flow * inf_factor) / 86400, 5)
  q_df <- data.frame(Date = combined$Date, value = q_cumecs)
  .write_simstrat_grid_file(
    df = q_df, file = file.path(path_simstrat, "Qinp.dat"),
    comment = 't(1. "column)\tz_Inp"\t(1. "row)\tInp"\t(rest)',
    depth = 0, ref_year = ref_year
  )

  # Dates with negligible inflow: zero the advected scalars so Simstrat never
  # divides a scalar flux by a ~0 volume flux (see @details).
  no_flow <- q_cumecs < 1e-6

  write_scalar <- function(value, file, comment) {
    if (ts_effective && any(no_flow)) value[no_flow] <- 0
    .write_simstrat_grid_file(
      df = data.frame(Date = combined$Date, value = value),
      file = file, comment = comment, depth = 0, ref_year = ref_year,
      integrate = ts_effective
    )
  }

  write_scalar(round(combined$HYD_temp, 3),
               file.path(path_simstrat, "Tinp.dat"),
               't(1. "column)\tz_Inp"\t(1. "row)\tInp"\t(rest)')
  write_scalar(round(combined$CHM_salt, 3),
               file.path(path_simstrat, "Sinp.dat"),
               't(1. "column)\tz_Inp"\t(1. "row)\tInp"\t(rest)')

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

    conv_of <- function(v) {
      i <- match(v, model_controls$var_aeme)
      cv <- if (is.na(i)) NA_real_ else model_controls$conversion_aed[i]
      if (is.na(cv) || cv == 0) 1 else cv
    }

    all_inf <- dplyr::bind_rows(inf)
    inflow_dir <- if (model == "simstrat_aed") "AED_inflow" else "AED2_inflow"
    for (v in bgc_vars) {
      if (!v %in% names(all_inf)) next
      simstrat_name <- rename_modelvars(v, type_output = model,
                                        warn_unmatched = TRUE)
      if (is.na(simstrat_name) || simstrat_name == "") next

      conv <- conv_of(v)
      var_df <- all_inf |>
        dplyr::select(Date, HYD_flow, .val = dplyr::all_of(v)) |>
        dplyr::group_by(Date) |>
        dplyr::summarise(value = fw_mean(.val, HYD_flow) / conv,
                         .groups = "drop") |>
        dplyr::arrange(Date)

      # Align the no-flow zeroing to this variable's own dates.
      val <- var_df$value
      if (bgc_effective) {
        zero_dates <- combined$Date[no_flow]
        val[var_df$Date %in% zero_dates] <- 0
      }

      .write_simstrat_grid_file(
        df = data.frame(Date = var_df$Date, value = val),
        file = file.path(bgc_dir, inflow_dir,
                         paste0(simstrat_name, "_inflow.dat")),
        comment = "depth [m], conc. [mmol/m3 * m2/s]",
        depth = 0, ref_year = ref_year, integrate = bgc_effective
      )
    }
  }

  invisible()
}
