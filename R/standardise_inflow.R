#' Standardise inflow variable names and units for AEME
#'
#' Attempts to match column names in an inflow data frame to AEME standard
#' variable names using \code{guess_aeme_vars()}, then detects the likely input
#' units of each variable from its values and converts to the units expected by
#' the package (GLM-AED conventions).
#'
#' @section AEME standard inflow variables and units:
#' \tabular{llll}{
#'   \strong{Variable}          \tab \strong{Name}       \tab \strong{Unit}   \tab \strong{Required} \cr
#'   Flow                       \tab \code{flow}          \tab m³/day          \tab Yes \cr
#'   Water temperature          \tab \code{temp}          \tab °C              \tab Yes \cr
#'   Salinity                   \tab \code{salt}          \tab PSU             \tab Yes \cr
#'   Dissolved oxygen           \tab \code{OXY_oxy}       \tab mmol/m³         \tab No \cr
#'   Phosphate-P                \tab \code{PHS_frp}       \tab mmol/m³         \tab No \cr
#'   Dissolved organic P        \tab \code{OGM_dop}       \tab mmol/m³         \tab No \cr
#'   Particulate organic P      \tab \code{OGM_pop}       \tab mmol/m³         \tab No \cr
#'   Particulate inorganic P    \tab \code{PHS_frp_ads}   \tab mmol/m³         \tab No \cr
#'   Ammoniacal nitrogen        \tab \code{NIT_amm}       \tab mmol/m³         \tab No \cr
#'   Nitrate-N                  \tab \code{NIT_nit}       \tab mmol/m³         \tab No \cr
#'   Dissolved organic N        \tab \code{OGM_don}       \tab mmol/m³         \tab No \cr
#'   Particulate organic N      \tab \code{OGM_pon}       \tab mmol/m³         \tab No \cr
#'   Dissolved organic C        \tab \code{OGM_doc}       \tab mmol/m³         \tab No \cr
#'   Particulate organic C      \tab \code{OGM_poc}       \tab mmol/m³         \tab No \cr
#'   Dissolved inorganic C      \tab \code{CAR_dic}       \tab mmol/m³         \tab No \cr
#'   Silica                     \tab \code{SIL_rsi}       \tab mmol/m³         \tab No \cr
#'   Suspended solids 1         \tab \code{NCS_ss1}       \tab g/m³            \tab No \cr
#'   Suspended solids 2         \tab \code{NCS_ss2}       \tab g/m³            \tab No \cr
#'   pH                         \tab \code{CHM_ph}        \tab -               \tab No \cr
#' }
#'
#' @section Unit detection logic:
#' Nutrient concentrations (N, P, C fractions) are expected in mmol/m³.
#' Detection thresholds are based on comparison of observed value ranges
#' against the example inflow data distributed with AEME:
#'
#' \itemize{
#'   \item \strong{N fractions} (\code{NIT_amm}, \code{NIT_nit}, \code{OGM_don},
#'     \code{OGM_pon}): mmol/m³ typically 0-10. Median > 20 assumed to be
#'     mg/L and divided by the molar mass of N (14.007 g/mol).
#'   \item \strong{P fractions} (\code{PHS_frp}, \code{OGM_dop}, \code{OGM_pop},
#'     \code{PHS_frp_ads}): mmol/m³ typically 0-5. Median > 10 assumed to be
#'     mg/L and divided by the molar mass of P (30.974 g/mol).
#'   \item \strong{C fractions} (\code{OGM_doc}, \code{OGM_poc}): mmol/m³
#'     typically 0-100. Median > 100 assumed to be mg/L and divided by the
#'     molar mass of C (12.011 g/mol).
#'   \item \strong{Dissolved inorganic C} (\code{CAR_dic}): mmol/m³ typically
#'     500-1000. Median > 2000 assumed to be mg/L.
#'   \item \strong{Dissolved oxygen} (\code{OXY_oxy}): mmol/m³ typically
#'     200-400. Median < 50 assumed to be mg/L and multiplied by 1000/32
#'     (molar mass of O2).
#'   \item \strong{Silica} (\code{SIL_rsi}): mmol/m³ typically 1-50. Median
#'     > 100 assumed to be mg/L and divided by molar mass of SiO2 (60.084).
#'   \item \strong{Temperature} (\code{temp}): °C expected. Median > 100
#'     assumed to be Kelvin.
#' }
#'
#' @param inflow data.frame; inflow data with a \code{time} or \code{Date}
#'   column and one or more inflow variable columns.
#' @param model_controls data.frame; model controls table containing at minimum
#'   columns \code{var_aeme} and \code{inf_default}. Used to fill missing state
#'   variables with default values. If \code{NULL} (default), the missing-variable
#'   fill step is skipped.
#' @param inf_vars character; vector of required inflow state variable names
#'   (e.g. from \code{model_controls$var_aeme}). Variables in \code{inf_vars}
#'   that are absent from \code{inflow} will be filled with values from
#'   \code{model_controls$inf_default}. Ignored when \code{model_controls} is
#'   \code{NULL}.
#' @param aeme_time named list or object; passed directly to \code{check_time()}.
#'   If \code{NULL} (default), the time-coverage check is skipped.
#' @param inflow_name character(1); human-readable label for this inflow stream,
#'   used in \code{check_time()} messages (e.g. \code{"inflow-tributary_1"}).
#'   Ignored when \code{aeme_time} is \code{NULL}.
#' @param model character(1); model identifier passed to \code{check_time()}.
#'   Ignored when \code{aeme_time} is \code{NULL}.
#' @param pot_inf_vars character; column allowlist used for the final
#'   \code{dplyr::select()} step. Defaults to \code{NULL}, which retains all
#'   columns. Typical value: \code{c("time", "HYD_flow", inf_vars, "model")}.
#' @param verbose logical; if \code{TRUE} (default), emit \code{cli_inform}
#'   messages describing each detected unit conversion applied.
#'
#' @return The input data frame with column names remapped to AEME standard
#'   names, values converted to AEME standard units, missing state variables
#'   filled with defaults, time coverage validated, and columns trimmed to
#'   \code{pot_inf_vars} (when supplied). Columns that could not be matched are
#'   retained unchanged with a warning. A warning is also emitted if required
#'   variables (\code{flow}, \code{temp}, \code{salt}) are absent after
#'   renaming.
#'
#' @importFrom cli cli_abort cli_warn cli_inform
#' @importFrom dplyr select any_of
#' @export
standardise_inflow <- function(inflow,
                               model_controls = NULL,
                               inf_vars       = NULL,
                               aeme_time      = NULL,
                               inflow_name    = "inflow",
                               model          = NULL,
                               pot_inf_vars   = NULL,
                               verbose        = TRUE) {
  
  # ── Input validation ────────────────────────────────────────────────────────
  
  if (!is.data.frame(inflow)) {
    cli::cli_abort(
      c("{.arg inflow} must be a {.cls data.frame}.",
        "x" = "Got {.cls {class(inflow)}}."),
      class = "aeme_error_inflow_type"
    )
  }
  if (nrow(inflow) == 0) {
    cli::cli_abort(
      c("{.arg inflow} has no rows.",
        "i" = "Supply a non-empty inflow data frame."),
      class = "aeme_error_inflow_empty"
    )
  }
  
  if (!is.null(model_controls)) {
    if (!is.data.frame(model_controls)) {
      cli::cli_abort(
        c("{.arg model_controls} must be a {.cls data.frame} or {.code NULL}.",
          "x" = "Got {.cls {class(model_controls)}}."),
        class = "aeme_error_inflow_model_controls_type"
      )
    }
    required_cols <- c("var_aeme", "inf_default")
    missing_cols  <- setdiff(required_cols, names(model_controls))
    if (length(missing_cols) > 0) {
      cli::cli_abort(
        c("{.arg model_controls} is missing required column{?s}: {.val {missing_cols}}."),
        class = "aeme_error_inflow_model_controls_cols"
      )
    }
  }
  
  time_col <- intersect(c("time", "Date", "date"), names(inflow))
  if (length(time_col) == 0) {
    cli::cli_abort(
      c("{.arg inflow} must contain a {.code time} or {.code Date} column.",
        "x" = "Columns found: {.val {names(inflow)}}."),
      class = "aeme_error_inflow_no_time"
    )
  }

  # ── Step 1: remap column names ──────────────────────────────────────────────
  
  inflow <- .rename_inflow_columns(inflow, verbose = verbose)
  
  # ── Step 2: warn if required variables are missing ──────────────────────────
  
  required <- c("HYD_flow", "HYD_temp", "CHM_salt")
  missing_required <- setdiff(required, names(inflow))
  if (length(missing_required) > 0) {
    cli::cli_warn(
      c("!" = "Required variable{?s} {.val {missing_required}} {?is/are} absent after renaming.",
        "i" = "AEME inflow processing requires {.code HYD_flow}, {.code HYD_temp}, and {.code CHM_salt}."),
      class = "aeme_warn_inflow_missing_required"
    )
  }
  
  # ── Step 3: detect units and convert ────────────────────────────────────────
  
  inflow <- .convert_inflow_units(inflow, verbose = verbose)
  
  # ── Step 4: fill missing state variables with defaults ──────────────────────
  
  if (!is.null(model_controls) && !is.null(inf_vars)) {
    missing_state <- setdiff(inf_vars, names(inflow))
    if (length(missing_state) > 0) {
      cli::cli_warn(
        c("!" = "{length(missing_state)} missing state variable{?s} in {.code {inflow_name}}:",
          setNames(paste("{.code", missing_state, "}"), rep("x", length(missing_state)))),
        class = "aeme_warn_inflow_missing_state"
      )
      for (v in missing_state) {
        default_val <- model_controls$inf_default[match(v, model_controls$var_aeme)]
        inflow[[v]] <- default_val
      }
      cli::cli_inform(
        c("i" = "Filled {length(missing_state)} missing variable{?s} with default value{?s} from {.arg model_controls}."),
        class = "aeme_inform_inflow_defaults_applied"
      )
    }
  }
  
  # ── Step 5: check time coverage ─────────────────────────────────────────────
  
  if (!is.null(aeme_time)) {
    check_time(
      df        = inflow,
      model     = model,
      aeme_time = aeme_time,
      name      = inflow_name
    )
  }
  
  # ── Step 6: select allowlisted columns ──────────────────────────────────────
  
  if (!is.null(pot_inf_vars)) {
    inflow <- dplyr::select(inflow, dplyr::any_of(c(time_col, pot_inf_vars)))
  }
  
  inflow
}


# ── Internal: column renaming ─────────────────────────────────────────────────

#' @noRd
.rename_inflow_columns <- function(inflow, verbose) {
  
  # Columns that are always passed through without guessing
  passthrough_cols <- c("time", "Date", "date", "model")
  
  non_time <- setdiff(names(inflow), passthrough_cols)
  if (length(non_time) == 0) {
    cli::cli_warn(
      c("!" = "{.arg inflow} contains only a {.code time} column.",
        "i" = "No inflow variables to rename or convert."),
      class = "aeme_warn_inflow_no_vars"
    )
    return(inflow)
  }
  
  # Known AEME inflow variable names — drawn from key_naming$var_aeme
  env <- new.env(parent = emptyenv())
  data("key_naming", package = "AEME", envir = env)
  known_inflow_vars <- c("HYD_flow", "HYD_temp", "CHM_salt",
                         env$key_naming$var_aeme)
  
  if (all(non_time %in% known_inflow_vars)) {
    if (verbose) {
      # cli::cli_inform(
      #   c("i" = "All columns already match AEME standard inflow variable names, skipping name guessing."),
      #   class = "aeme_inform_inflow_already_standard"
      # )
    }
    return(inflow)
  }
  
  # Columns that still need guessing
  to_guess <- non_time[!non_time %in% known_inflow_vars]
  
  guessed <- tryCatch(
    guess_aeme_vars(to_guess),
    error = function(e) {
      cli::cli_abort(
        c("Failed to call {.fn guess_aeme_vars}.",
          "x" = conditionMessage(e)),
        class = "aeme_error_guess_vars",
        call = NULL
      )
    }
  )
  names(guessed) <- to_guess
  
  # Drop any guess that is identical to the input name (no-op rename)
  guessed[!is.na(guessed) & guessed == to_guess] <- NA
  
  matched   <- guessed[!is.na(guessed)]
  unmatched <- to_guess[is.na(guessed)]
  
  if (length(unmatched) > 0) {
    cli::cli_warn(
      c("!" = "{length(unmatched)} column{?s} could not be matched to a known AEME inflow variable and will be left unchanged.",
        "x" = "Unmatched: {.val {unmatched}}."),
      class = "aeme_warn_inflow_unmatched"
    )
  }
  
  if (length(matched) > 0 && verbose) {
    rename_bullets <- setNames(
      paste0(names(matched), " \u2192 ", matched),
      rep("*", length(matched))
    )
    cli::cli_inform(
      c("i" = "Renaming {length(matched)} column{?s} to AEME standard names:",
        rename_bullets),
      class = "aeme_inform_inflow_rename"
    )
  }
  
  idx <- match(names(matched), names(inflow))
  names(inflow)[idx] <- matched
  
  inflow
}

# ── Internal: unit detection and conversion ────────────────────────────────────
#
# Target units are taken directly from key_naming$units:
#   HYD_temp  : degC       — detect Kelvin (median > 100)
#   CHM_oxy   : mg/L       — detect mmol/m³ (median > 50 → divide by 31.25)
#   NIT_*     : g/m^3      — detect mmol/m³ (median < 5  → multiply by 0.014007)
#   PHS_*     : g/m^3      — detect mmol/m³ (median < 1  → multiply by 0.030974)
#   CAR_doc/poc/dic : g/m^3 — detect mmol/m³ (median < thresholds → multiply by 0.012011)
#   SIL_rsi   : g/m^3(?)  — detect mmol/m³ (median < 10 → multiply by 0.060084)
#   CAR_ch4   : g/m^3      — detect mmol/m³ (median < 10 → multiply by 0.016043)
#
# Thresholds are derived from the ex_inf reference data (known mmol/m³) vs
# the g/m^3 target values expected by key_naming.

.inflow_conversion_table <- list(
  
  # ── Temperature (degC expected; detect Kelvin) ────────────────────────────
  HYD_temp = list(
    detect  = function(x) .nz_median(x) > 100,
    convert = function(x) x - 273.15,
    from    = "Kelvin (K)",
    to      = "degC"
  ),
  
  # ── Dissolved oxygen (mg/L expected; detect mmol/m³ ~200-400) ────────────
  # mmol/m³ × 32/1000 = mg/L  (molar mass O₂ = 32 g/mol)
  # mg/L typical ~8-14; mmol/m³ typical ~200-400 → unambiguous above 50
  CHM_oxy = list(
    detect  = function(x) .nz_median(x) > 50,
    convert = function(x) x * 32 / 1000,
    from    = "mmol/m\u00b3",
    to      = "mg/L"
  ),
  
  # ── Nitrogen fractions (g/m³ expected; detect mmol/m³ → divide by molar mass N) ─
  # g/m³ typical: NIT_amm 0-2, NIT_nit 0-5, DON/PON 0-5
  # mmol/m³ would be ~70x higher (÷ 0.014007 g/mmol)
  # Threshold > 20: safely above any real g/m³ value
  NIT_amm = list(
    detect  = function(x) .nz_median(x) > 20,
    convert = function(x) x / 0.014007,
    from    = "mmol/m\u00b3",
    to      = "g/m^3"
  ),
  NIT_nit = list(
    detect  = function(x) .nz_median(x) > 20,
    convert = function(x) x / 0.014007,
    from    = "mmol/m\u00b3",
    to      = "g/m^3"
  ),
  NIT_don = list(
    detect  = function(x) .nz_median(x) > 20,
    convert = function(x) x / 0.014007,
    from    = "mmol/m\u00b3",
    to      = "g/m^3"
  ),
  NIT_donr = list(
    detect  = function(x) .nz_median(x) > 20,
    convert = function(x) x / 0.014007,
    from    = "mmol/m\u00b3",
    to      = "g/m^3"
  ),
  NIT_pon = list(
    detect  = function(x) .nz_median(x) > 20,
    convert = function(x) x / 0.014007,
    from    = "mmol/m\u00b3",
    to      = "g/m^3"
  ),
  NIT_ponr = list(
    detect  = function(x) .nz_median(x) > 20,
    convert = function(x) x / 0.014007,
    from    = "mmol/m\u00b3",
    to      = "g/m^3"
  ),
  
  # ── Phosphorus fractions (g/m³ expected; detect mmol/m³ → divide by molar mass P) ─
  # g/m³ typical: PHS_frp 0-5, others 0-1
  # mmol/m³ would be ~30x higher (÷ 0.030974 g/mmol)
  # Threshold > 10: safely above any real g/m³ P value
  PHS_frp = list(
    detect  = function(x) .nz_median(x) > 10,
    convert = function(x) x / 0.030974,
    from    = "mmol/m\u00b3",
    to      = "g/m^3"
  ),
  PHS_dop = list(
    detect  = function(x) .has_nonzero(x) && .nz_median(x) > 10,
    convert = function(x) x / 0.030974,
    from    = "mmol/m\u00b3",
    to      = "g/m^3"
  ),
  PHS_dopr = list(
    detect  = function(x) .has_nonzero(x) && .nz_median(x) > 10,
    convert = function(x) x / 0.030974,
    from    = "mmol/m\u00b3",
    to      = "g/m^3"
  ),
  PHS_pop = list(
    detect  = function(x) .has_nonzero(x) && .nz_median(x) > 10,
    convert = function(x) x / 0.030974,
    from    = "mmol/m\u00b3",
    to      = "g/m^3"
  ),
  PHS_popr = list(
    detect  = function(x) .has_nonzero(x) && .nz_median(x) > 10,
    convert = function(x) x / 0.030974,
    from    = "mmol/m\u00b3",
    to      = "g/m^3"
  ),
  PHS_pip = list(
    detect  = function(x) .has_nonzero(x) && .nz_median(x) > 10,
    convert = function(x) x / 0.030974,
    from    = "mmol/m\u00b3",
    to      = "g/m^3"
  ),
  
  # ── Carbon fractions (g/m³ expected; detect mmol/m³ → divide by molar mass C) ──
  # g/m³ typical: CAR_doc 0-100, CAR_poc 0-50
  # mmol/m³ would be ~80x higher (÷ 0.012011 g/mmol)
  # Threshold > 500: safely above any real g/m³ DOC value
  CAR_doc = list(
    detect  = function(x) .nz_median(x) > 500,
    convert = function(x) x / 0.012011,
    from    = "mmol/m\u00b3",
    to      = "g/m^3"
  ),
  CAR_docr = list(
    detect  = function(x) .nz_median(x) > 500,
    convert = function(x) x / 0.012011,
    from    = "mmol/m\u00b3",
    to      = "g/m^3"
  ),
  CAR_poc = list(
    detect  = function(x) .has_nonzero(x) && .nz_median(x) > 200,
    convert = function(x) x / 0.012011,
    from    = "mmol/m\u00b3",
    to      = "g/m^3"
  ),
  CAR_pocr = list(
    detect  = function(x) .has_nonzero(x) && .nz_median(x) > 200,
    convert = function(x) x / 0.012011,
    from    = "mmol/m\u00b3",
    to      = "g/m^3"
  ),
  # CAR_dic: g/m³ ~10; mmol/m³ ~832 → unambiguous above 100
  CAR_dic = list(
    detect  = function(x) .nz_median(x) > 100,
    convert = function(x) x / 0.012011,
    from    = "mmol/m\u00b3",
    to      = "g/m^3"
  ),
  # CAR_ch4: g/m³ typically trace; mmol/m³ > 10 is safely non-g/m³
  CAR_ch4 = list(
    detect  = function(x) .has_nonzero(x) && .nz_median(x) > 10,
    convert = function(x) x / 0.016043,
    from    = "mmol/m\u00b3",
    to      = "g/m^3"
  ),
  
  # ── Silica (g/m³ expected; detect mmol/m³ → divide by molar mass SiO₂) ──
  # g/m³ typical: 1-30; mmol/m³ typical: 1-50 (SiO₂ 60.084 g/mol)
  # These ranges genuinely overlap — use > 100 as a conservative threshold
  SIL_rsi = list(
    detect  = function(x) .nz_median(x) > 100,
    convert = function(x) x / 0.060084,
    from    = "mmol/m\u00b3",
    to      = "g/m^3"
  )
)


#' @noRd
.convert_inflow_units <- function(inflow, verbose) {
  
  for (var in names(.inflow_conversion_table)) {
    
    if (!var %in% names(inflow)) next
    
    spec <- .inflow_conversion_table[[var]]
    x    <- inflow[[var]]
    
    # Skip entirely if column is all NA
    if (all(is.na(x))) {
      if (verbose) {
        cli::cli_inform(
          c("i" = "{.code {var}}: all NA, skipping unit detection."),
          class = "aeme_inform_inflow_all_na"
        )
      }
      next
    }
    
    flag <- spec$detect(x)
    
    if (isFALSE(flag) || is.null(flag) || is.na(flag)) {
      if (verbose) {
        # cli::cli_inform(
        #   c("i" = "{.code {var}}: values appear to be in the expected units, no conversion applied."),
        #   class = "aeme_inform_inflow_no_conversion"
        # )
      }
      next
    }
    
    inflow[[var]] <- spec$convert(x)
    
    if (verbose) {
      cli::cli_inform(
        c("v" = "{.code {var}}: converted from {spec$from} to {spec$to}."),
        class = "aeme_inform_inflow_converted"
      )
    }
  }
  
  # ── Post-conversion sanity checks ──────────────────────────────────────────
  
  inflow <- .sanity_check_inflow(inflow)
  
  inflow
}


# ── Internal: post-conversion sanity checks ────────────────────────────────────

#' @noRd
.sanity_check_inflow <- function(inflow) {
  
  checks <- list(
    list(
      var   = "HYD_temp",
      test  = function(x) any(x < -5 | x > 40, na.rm = TRUE),
      msg   = "Water temperature values outside plausible range [-5, 40] \u00b0C.",
      class = "aeme_warn_inflow_sanity_temp"
    ),
    list(
      var   = "CHM_salt",
      test  = function(x) any(x < 0 | x > 45, na.rm = TRUE),
      msg   = "Salinity values outside plausible range [0, 45] PSU.",
      class = "aeme_warn_inflow_sanity_salt"
    ),
    list(
      var   = "HYD_flow",
      test  = function(x) any(x < 0, na.rm = TRUE),
      msg   = "Negative flow values detected.",
      class = "aeme_warn_inflow_sanity_flow_negative"
    ),
    list(
      var   = "CHM_oxy",
      test  = function(x) any(x < 0 | x > 25, na.rm = TRUE),
      msg   = "Dissolved oxygen outside plausible range [0, 25] mg/L.",
      class = "aeme_warn_inflow_sanity_oxy"
    ),
    list(
      var   = "CHM_ph",
      test  = function(x) any(x < 3 | x > 11, na.rm = TRUE),
      msg   = "pH values outside plausible range [3, 11].",
      class = "aeme_warn_inflow_sanity_ph"
    ),
    list(
      var   = "NIT_amm",
      test  = function(x) any(x < 0 | x > 10, na.rm = TRUE),
      msg   = "NIT_amm outside plausible range [0, 10] g/m^3.",
      class = "aeme_warn_inflow_sanity_amm"
    ),
    list(
      var   = "NIT_nit",
      test  = function(x) any(x < 0 | x > 10, na.rm = TRUE),
      msg   = "NIT_nit outside plausible range [0, 10] g/m^3.",
      class = "aeme_warn_inflow_sanity_nit"
    ),
    list(
      var   = "PHS_frp",
      test  = function(x) any(x < 0 | x > 5, na.rm = TRUE),
      msg   = "PHS_frp outside plausible range [0, 5] g/m^3.",
      class = "aeme_warn_inflow_sanity_frp"
    ),
    list(
      var   = "CAR_doc",
      test  = function(x) any(x < 0 | x > 100, na.rm = TRUE),
      msg   = "CAR_doc outside plausible range [0, 100] g/m^3.",
      class = "aeme_warn_inflow_sanity_doc"
    ),
    list(
      var   = "CAR_dic",
      test  = function(x) {
        vals <- x[!is.na(x)]
        length(vals) > 1 && var(vals) == 0
      },
      msg   = "CAR_dic is constant across all rows — this may be a hardcoded placeholder rather than real data.",
      class = "aeme_warn_inflow_sanity_dic_constant"
    ),
    list(
      var   = "SIL_rsi",
      test  = function(x) {
        vals <- x[!is.na(x)]
        length(vals) > 1 && var(vals) == 0
      },
      msg   = "SIL_rsi is constant across all rows — this may be a placeholder value.",
      class = "aeme_warn_inflow_sanity_sil_constant"
    )
  )
  
  for (chk in checks) {
    if (!chk$var %in% names(inflow)) next
    x <- inflow[[chk$var]]
    if (all(is.na(x))) next
    if (chk$test(x)) {
      cli::cli_warn(
        c("!" = "{.code {chk$var}}: {chk$msg}",
          "i" = "Check raw data or unit conversion for this variable."),
        class = chk$class
      )
    }
  }
  
  inflow
}
