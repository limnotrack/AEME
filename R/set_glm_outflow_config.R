#' Fine-grained control of the GLM-AED `&outflow` configuration
#'
#' Rewrites the `&outflow` block of a GLM nml file with per-outlet withdrawal
#' settings and block-level options, validated against GLM's own rules
#' (see `src/glm_init.c` in the GLM source). [set_glm_outflows()] is the thin
#' writer [build_aeme()] uses and only distinguishes fixed vs floating outlets;
#' this function additionally exposes adaptive (temperature-targeting) outlets,
#' submerged (type 6) outlets, per-outlet critical-withdrawal thresholds,
#' target-withdrawal-temperature forcing, bed seepage and weir geometry.
#'
#' Only the nml is updated - no outflow forcing CSVs are created or modified.
#' Point each outlet's `file` at a CSV you have already written (for example
#' with [set_glm_outflows()] or [build_aeme()]). Keys already present in the
#' `&outflow` block that you do not set here are left untouched.
#'
#' @section Outlet elevation convention:
#' `elev` is always given as an absolute elevation on the hypsography /
#' `&morphometry` `H` datum. For a fixed outlet it is written straight to
#' `outl_elvs` (GLM requires `base_elev <= elev <= crest_elev`). For a floating
#' offtake GLM instead wants a depth *below the moving surface*, so `elev` is
#' converted with `surface_elev - elev` and must satisfy
#' `0 <= surface_elev - elev <= (crest_elev - base_elev)`.
#'
#' @param path_glm GLM-AED directory. Used to locate the nml when `glm_file` is
#'   not given and to resolve relative `file` paths for existence checks.
#' @param outlets data.frame, one row per outlet. Recognised columns (all
#'   optional except where noted):
#'   \describe{
#'     \item{`name`}{outlet label, used only in messages.}
#'     \item{`type`}{GLM `outlet_type`: `1` fixed, `2` floating, `3`
#'       adaptive/target-temperature, `4`-`5` other withdrawal modes, `6`
#'       submerged. Default `2`.}
#'     \item{`float`}{logical `flt_off_sw`. Default `type == 2`. GLM forces a
#'       floating outlet to `type = 2`.}
#'     \item{`elev`}{outlet elevation (m, hypsography datum) - see
#'       *Outlet elevation convention*. Required for every outlet unless the
#'       existing block already has an `outl_elvs` entry for it.}
#'     \item{`bsn_len`, `bsn_wid`}{basin length / width at the outlet (m).
#'       Computed from the hypsography when omitted.}
#'     \item{`factor`}{`outflow_factor`, per-outlet flow multiplier. Default `1`.}
#'     \item{`file`}{`outflow_fl`, path to the outlet's flow-forcing CSV,
#'       relative to `path_glm`. Carried over from the existing block when
#'       omitted and the outlet count is unchanged.}
#'     \item{`target_temp`}{`target_temp` (degC) for `type == 3`.}
#'     \item{`crit`}{`outlet_crit`, per-outlet critical threshold (`Hcrit`).}
#'     \item{`subm_elev`}{`subm_elev_outflow` for `type == 6`: a fixed submerged
#'       elevation as height above the bed (m), `0 <= subm_elev <= max depth`.}
#'     \item{`elev_idx`}{`elev_idx_outflow` for `type == 6`: a dynamic layer
#'       index that overrides `subm_elev`. Use `NA` / `-1` for none.}
#'   }
#' @param surface_elev current lake surface elevation (m, hypsography datum),
#'   used to convert a floating outlet's `elev` to a depth below the surface.
#'   Default: `min(H) + lake_depth` read from the nml.
#' @param seepage,seepage_rate enable constant bed seepage and set its rate
#'   (m/day). `NULL` leaves the current value.
#' @param withdr_temp_file `withdrTemp_fl`: a single target-withdrawal-
#'   temperature forcing CSV (relative to `path_glm`) shared by the adaptive
#'   outlets. `NULL` leaves the current value; `NA` removes it.
#' @param adaptive named list of block-level adaptive-withdrawal controls, any
#'   of `crit_val`, `crit_dep`, `crit_days`, `crit_above`, `crit_varname`,
#'   `crit_idx`, `min_lake_temp`, `fac_range_upper`, `fac_range_lower`,
#'   `mix_withdraw`, `coupl_oxy_sw`. Only supplied names are written.
#' @param crest_width,crest_factor weir / overflow geometry for the surface
#'   outlet. `NULL` leaves the current value.
#' @param thick_limit `outflow_thick_limit`: minimum layer thickness (m) an
#'   outlet will draw from. `NULL` leaves the current value.
#' @param single_layer_draw logical `single_layer_draw`: force each outlet to
#'   draw from the single layer at its elevation. `NULL` leaves the current
#'   value.
#' @param extra named list of any further raw `&outflow` keys to set verbatim
#'   (e.g. `time_fmt`, `timezone`). Applied last, so it overrides the above.
#' @param bathy data.frame with `elev` / `area` columns (the hypsograph), used
#'   to size outlets and to validate elevations. Default: the `H` / `A` arrays
#'   in the nml's `&morphometry` block.
#' @param dims_lake length-2 numeric `c(basin_length, basin_width)` at the
#'   crest. Default: the nml's `bsn_len` / `bsn_wid`.
#' @param validate check every value against GLM's ranges before writing and
#'   abort on any violation. Default `TRUE`.
#' @param glm_file path to the GLM nml. Default: discovered under `path_glm`.
#'
#' @return invisibly, the updated nml list (also written to `glm_file`).
#' @export
#'
#' @seealso [set_glm_outflows()], [build_aeme()]
#'
#' @examples
#' \dontrun{
#' # A floating offtake 2 m below the surface plus a fixed bottom gate,
#' # with bed seepage switched on.
#' set_glm_outflow_config(
#'   path_glm,
#'   outlets = data.frame(
#'     name   = c("spillway", "bottom_gate"),
#'     type   = c(2L, 1L),
#'     elev   = c(surface_elev - 2, base_elev + 0.5),
#'     file   = c("bcs/outflow_spillway.csv", "bcs/outflow_bottom_gate.csv")
#'   ),
#'   seepage = TRUE, seepage_rate = 0.001
#' )
#'
#' # An adaptive outlet targeting 12 degC.
#' set_glm_outflow_config(
#'   path_glm,
#'   outlets = data.frame(type = 3L, elev = surface_elev - 5,
#'                        target_temp = 12,
#'                        file = "bcs/outflow_wbal.csv"),
#'   adaptive = list(min_lake_temp = 4, fac_range_upper = 1.2,
#'                   fac_range_lower = 0.8)
#' )
#' }
set_glm_outflow_config <- function(path_glm,
                                   outlets = NULL,
                                   surface_elev = NULL,
                                   seepage = NULL,
                                   seepage_rate = NULL,
                                   withdr_temp_file = NULL,
                                   adaptive = NULL,
                                   crest_width = NULL,
                                   crest_factor = NULL,
                                   thick_limit = NULL,
                                   single_layer_draw = NULL,
                                   extra = NULL,
                                   bathy = NULL,
                                   dims_lake = NULL,
                                   validate = TRUE,
                                   glm_file = find_glm_nml(path_glm,
                                                           must_exist = FALSE)) {

  if (is.na(glm_file) || !file.exists(glm_file)) {
    cli::cli_abort("Could not find a GLM nml file (looked in {.path {path_glm}}).")
  }
  glm_nml <- read_nml(glm_file)
  block <- glm_nml[["outflow"]]
  if (is.null(block)) block <- list()

  # --- lake geometry -------------------------------------------------------
  if (is.null(bathy)) {
    bathy <- data.frame(elev = get_nml_value(glm_nml, "H"),
                        area = get_nml_value(glm_nml, "A"))
  }
  if (!all(c("elev", "area") %in% names(bathy))) {
    cli::cli_abort("'bathy' must have 'elev' and 'area' columns.")
  }
  if (is.null(dims_lake)) {
    dims_lake <- c(get_nml_value(glm_nml, "bsn_len"),
                   get_nml_value(glm_nml, "bsn_wid"))
  }
  base_elev  <- min(bathy[["elev"]])
  crest_elev <- max(bathy[["elev"]])
  lake_depth <- crest_elev - base_elev
  if (is.null(surface_elev)) {
    ld <- tryCatch(get_nml_value(glm_nml, "lake_depth"), error = function(e) NA_real_)
    surface_elev <- if (is.finite(ld)) base_elev + ld else crest_elev
  }

  abort_bad <- function(msg, env = parent.frame()) {
    if (validate) cli::cli_abort(msg, .envir = env)
    else cli::cli_warn(msg, .envir = env)
  }

  # --- per-outlet settings ----------------------------------------------------
  if (!is.null(outlets)) {
    outlets <- as.data.frame(outlets, stringsAsFactors = FALSE)
    n <- nrow(outlets)
    if (n < 1) cli::cli_abort("'outlets' has no rows.")

    lbl <- if ("name" %in% names(outlets)) as.character(outlets[["name"]]) else
      paste0("outlet ", seq_len(n))

    type  <- if ("type" %in% names(outlets)) as.integer(outlets[["type"]]) else
      rep(2L, n)
    float <- if ("float" %in% names(outlets)) as.logical(outlets[["float"]]) else
      (type == 2L)
    # GLM: a floating outlet is forced to type 2
    type[float] <- 2L
    float[type == 2L] <- TRUE

    if (any(is.na(type)) || any(type < 1L | type > 6L)) {
      abort_bad(c("x" = "'type' must be an integer in 1:6 for every outlet."))
    }

    # elevation (absolute datum); fall back to existing outl_elvs if omitted
    if ("elev" %in% names(outlets)) {
      elev_abs <- as.numeric(outlets[["elev"]])
    } else {
      prev <- block[["outl_elvs"]]
      prev_flt <- block[["flt_off_sw"]]
      if (is.null(prev) || length(prev) != n) {
        cli::cli_abort(c("x" = "'outlets' has no 'elev' column and the existing
                         &outflow block has no matching 'outl_elvs' to reuse."))
      }
      # convert any previously-floating entries back to an absolute elevation
      prev_flt <- rep_len(if (is.null(prev_flt)) FALSE else as.logical(prev_flt), n)
      elev_abs <- ifelse(prev_flt, surface_elev - prev, prev)
    }
    if (any(!is.finite(elev_abs))) {
      cli::cli_abort("Every outlet needs a finite 'elev'.")
    }

    # value written to outl_elvs: depth-below-surface for floating, else abs
    outl_elvs <- ifelse(float, surface_elev - elev_abs, elev_abs)

    for (i in seq_len(n)) {
      if (float[i]) {
        if (outl_elvs[i] < 0 || outl_elvs[i] > lake_depth) {
          abort_bad(c("x" = "{lbl[i]}: floating outlet depth below surface
                      ({round(outl_elvs[i], 3)} m) is outside [0,
                      {round(lake_depth, 3)}]."))
        }
      } else {
        if (elev_abs[i] < base_elev || elev_abs[i] > crest_elev) {
          abort_bad(c("x" = "{lbl[i]}: fixed outlet elevation
                      ({round(elev_abs[i], 3)} m) is outside
                      [{round(base_elev, 3)}, {round(crest_elev, 3)}]."))
        }
      }
    }

    # basin geometry at each outlet (absolute elevation, clamped into bathy)
    er <- range(bathy[["elev"]], na.rm = TRUE)
    dim_elev <- pmin(pmax(elev_abs, er[1]), er[2])
    if ("bsn_len" %in% names(outlets) && "bsn_wid" %in% names(outlets)) {
      bsn_len <- as.numeric(outlets[["bsn_len"]])
      bsn_wid <- as.numeric(outlets[["bsn_wid"]])
    } else {
      d <- lapply(dim_elev, elipse_dims, bathy = bathy, dims_lake = dims_lake) |>
        dplyr::bind_rows()
      bsn_len <- d[["length"]]
      bsn_wid <- d[["width"]]
    }

    factor_out <- if ("factor" %in% names(outlets))
      as.numeric(outlets[["factor"]]) else rep(1, n)

    # forcing files
    if ("file" %in% names(outlets)) {
      files <- as.character(outlets[["file"]])
    } else if (!is.null(block[["outflow_fl"]]) &&
               length(block[["outflow_fl"]]) == n) {
      files <- as.character(block[["outflow_fl"]])
    } else {
      cli::cli_abort(c("x" = "'outlets' needs a 'file' column (one flow CSV per
                       outlet); none found and the existing block does not have
                       {n} 'outflow_fl' entr{?y/ies} to reuse."))
    }
    miss <- !file.exists(file.path(path_glm, files)) & !file.exists(files)
    if (any(miss)) {
      cli::cli_warn(c("!" = "Outflow forcing file{?s} not found under
                      {.path {path_glm}}: {.val {files[miss]}}. Write {?it/them}
                      before running GLM."))
    }

    block[["num_outlet"]]   <- n
    block[["outlet_type"]]  <- as.integer(type)
    block[["flt_off_sw"]]   <- as.logical(float)
    block[["outl_elvs"]]    <- round(as.numeric(outl_elvs), 4)
    block[["bsn_len_outl"]] <- round(as.numeric(bsn_len), 2)
    block[["bsn_wid_outl"]] <- round(as.numeric(bsn_wid), 2)
    block[["outflow_fl"]]   <- files
    block[["outflow_factor"]] <- as.numeric(factor_out)

    if ("target_temp" %in% names(outlets)) {
      block[["target_temp"]] <- as.numeric(outlets[["target_temp"]])
    }
    if ("crit" %in% names(outlets)) {
      block[["outlet_crit"]] <- as.numeric(outlets[["crit"]])
    }
    if (any(type == 6L)) {
      subm <- if ("subm_elev" %in% names(outlets))
        as.numeric(outlets[["subm_elev"]]) else rep(0, n)
      if (validate && any(type == 6L &
                          (subm < 0 | subm > lake_depth), na.rm = TRUE)) {
        cli::cli_abort(c("x" = "Submerged (type 6) 'subm_elev' must be in
                         [0, {round(lake_depth, 3)}]."))
      }
      block[["subm_elev_outflow"]] <- subm
      if ("elev_idx" %in% names(outlets)) {
        idx <- suppressWarnings(as.integer(outlets[["elev_idx"]]))
        idx[is.na(idx)] <- -1L
        block[["elev_idx_outflow"]] <- idx
      }
    }
  }

  # --- block-level options --------------------------------------------------
  if (!is.null(seepage))       block[["seepage"]]      <- isTRUE(seepage)
  if (!is.null(seepage_rate))  block[["seepage_rate"]] <- as.numeric(seepage_rate)
  if (!is.null(crest_width))   block[["crest_width"]]  <- as.numeric(crest_width)
  if (!is.null(crest_factor))  block[["crest_factor"]] <- as.numeric(crest_factor)
  if (!is.null(thick_limit))
    block[["outflow_thick_limit"]] <- as.numeric(thick_limit)
  if (!is.null(single_layer_draw))
    block[["single_layer_draw"]] <- isTRUE(single_layer_draw)

  if (!is.null(withdr_temp_file)) {
    if (length(withdr_temp_file) == 1 && is.na(withdr_temp_file)) {
      block[["withdrTemp_fl"]] <- NULL
    } else {
      block[["withdrTemp_fl"]] <- as.character(withdr_temp_file)
      if (!file.exists(file.path(path_glm, withdr_temp_file)) &&
          !file.exists(withdr_temp_file)) {
        cli::cli_warn(c("!" = "withdrTemp_fl {.val {withdr_temp_file}} not found
                        under {.path {path_glm}}."))
      }
    }
  }

  adaptive_keys <- c("crit_val", "crit_dep", "crit_days", "crit_above",
                     "crit_varname", "crit_idx", "min_lake_temp",
                     "fac_range_upper", "fac_range_lower", "mix_withdraw",
                     "coupl_oxy_sw")
  if (!is.null(adaptive)) {
    bad <- setdiff(names(adaptive), adaptive_keys)
    if (length(bad)) {
      cli::cli_abort(c("x" = "Unknown 'adaptive' entr{?y/ies}: {.val {bad}}.",
                       "i" = "Allowed: {.val {adaptive_keys}}."))
    }
    for (k in names(adaptive)) block[[k]] <- adaptive[[k]]
  }

  # GLM's own guard on the critical-withdrawal block (glm_init.c)
  if (validate && !is.null(block[["outlet_crit"]])) {
    cv <- block[["crit_val"]] %||% -1
    cd <- block[["crit_dep"]] %||% (base_elev - 1)
    cdays <- block[["crit_days"]] %||% 0
    if (cv < 0 || cd < base_elev || cdays < 1) {
      cli::cli_abort(c("x" = "'outlet_crit' is set, so GLM also needs",
                       "*" = "crit_val >= 0 (got {cv}),",
                       "*" = "crit_dep >= base elevation {round(base_elev, 3)} (got {cd}),",
                       "*" = "crit_days >= 1 (got {cdays})."))
    }
  }

  if (!is.null(extra)) {
    if (is.null(names(extra)) || any(names(extra) == "")) {
      cli::cli_abort("'extra' must be a fully named list.")
    }
    for (k in names(extra)) block[[k]] <- extra[[k]]
  }

  glm_nml[["outflow"]] <- block
  write_nml(glm_nml, file = glm_file)
  cli::cli_alert_success("Updated &outflow block in {.path {glm_file}}
                         ({block[['num_outlet']] %||% 0} outlet{?s}).")
  invisible(glm_nml)
}
