#' Diagnose the three baseline budgets (water, heat, nutrient) at once
#'
#' Runs \code{\link{diag_water_balance}}, \code{\link{diag_heat_budget}} and,
#' when `use_bgc = TRUE`, \code{\link{diag_nutrient_budget}} against an
#' already built and run \code{\link{Aeme-class}} object, and reports a
#' one-line classification for each. Meant as the entry point for a baseline
#' (shipped-default) simulation, before committing to a staged calibration
#' design: which parameters go in which stage, and what bounds, depends on
#' which of these budgets is actually biased.
#'
#' `aeme` must already have current output for `model` (i.e. built with
#' \code{\link{build_aeme}} and run with \code{\link{run_aeme}}) - this
#' function does not build or run anything itself.
#'
#' @param aeme Aeme object; already built and run for `model`.
#' @param model character; single model code to diagnose (see
#' \code{\link{check_model}}).
#' @param use_bgc logical; also run \code{\link{diag_nutrient_budget}}. Set
#' to `FALSE` when `aeme` was run without the BGC module active. Automatically
#' derived from the `use_bgc` flag in the `aeme` object when omitted.
#'
#' @returns an `aeme_diag` object: list with `aeme`, `model`,
#' `water_balance`, `heat_budget` and `nutrient_budget` (`NULL` when
#' `use_bgc = FALSE`).
#'
#' @export
diag_aeme <- function(aeme, model, use_bgc, ...) {
  
  aeme <- check_aeme(aeme)
  if (missing(model)) {
    model <- list_models(aeme = aeme)
  }
  model <- check_model(model = model)
  if (missing(use_bgc)) {
    use_bgc <- get_config_value(aeme, "use_bgc")
  }
  if (length(model) > 1) {
    cli::cli_abort("{.arg model} must be a single model, not {length(model)}.")
  }
  
  extra_args <- list(...)
  # Pass only matching args to each diagnostic function to avoid surprising errors
  wb_args <- extra_args[names(extra_args) %in% names(formals(diag_water_balance))]
  hb_args <- extra_args[names(extra_args) %in% names(formals(diag_heat_budget))]
  nb_args <- extra_args[names(extra_args) %in% names(formals(diag_nutrient_budget))]
  
  wb <- do.call(diag_water_balance, c(list(aeme = aeme, model = model), wb_args))
  hb <- do.call(diag_heat_budget, c(list(aeme = aeme, model = model), hb_args))
  nb <- if (use_bgc) do.call(diag_nutrient_budget, c(list(aeme = aeme, model = model), nb_args)) else NULL
  
  structure(
    list(aeme = aeme, model = model, water_balance = wb, heat_budget = hb,
         nutrient_budget = nb),
    class = c("aeme_diag", "list"))
}

#' @export
print.aeme_diag <- function(x, ...) {
  cli::cli_h2("Baseline diagnostic: {x$model}")
  cli::cli_bullets(c(
    "*" = "water balance: {x$water_balance$classification} - {x$water_balance$detail}",
    "*" = "heat budget: {x$heat_budget$classification} - {x$heat_budget$detail}"
  ))
  nb <- x$nutrient_budget
  if (!is.null(nb) && nrow(nb) > 0) {
    cli::cli_h3("Nutrient budget")
    print(nb, row.names = FALSE)
  }
  invisible(x)
}

#' Turn a diagnosis into concrete calibration suggestions
#'
#' Rule-based and deliberately conservative: it names the parameters and the
#' reasoning, it does not build or launch a calibration run itself. Meant to
#' be read, not blindly executed - a stage design still has to reconcile
#' these suggestions against every other lake being calibrated in the same
#' run.
#'
#' @param diag an `aeme_diag` object from
#' \code{\link{diag_aeme}}.
#' @param param_table data frame with columns `name` (parameter name) and
#' `vars` (list-column; each element a character vector of the AEME
#' `var_sim` names that parameter is linked to), used to look up which
#' calibration parameters are linked to a biased nutrient-budget variable.
#' `NULL` (default) skips that lookup.
#'
#' @returns data frame with columns `issue`, `severity`
#' (`"low"`/`"medium"`/`"high"`), `evidence`, `recommendation`, `params`
#' (comma-joined parameter names, where known).
#'
#' @export
recommend_calib_plan <- function(diag, param_table = NULL) {
  if (!inherits(diag, "aeme_diag")) {
    cli::cli_abort(
      "{.arg diag} must be an {.cls aeme_diag} object from {.fn diag_aeme}.")
  }
  if (!is.null(param_table) &&
      !all(c("name", "vars") %in% names(param_table))) {
    cli::cli_abort("{.arg param_table} must have columns {.val name} and {.val vars}.")
  }
  
  out <- list()

  # ---- water balance --------------------------------------------------------
  wb <- diag$water_balance
  drift_str <- function(x) if (is.na(x)) "not estimated" else sprintf("%.2f m/yr", x)

  if (identical(wb$classification, "drift")) {
    out[["water_balance"]] <- data.frame(
      issue    = "water balance: drift",
      severity = if (abs(wb$drift_m_per_yr) > 0.3) "high" else "medium",
      evidence = sprintf(
        "LKE_lvlwtr residual (sim - obs) trends at %s over %.1f yr observed span (mean bias %.1f m, %d paired observations).",
        drift_str(wb$drift_m_per_yr), wb$span_yr, wb$mean_bias, wb$n),
      recommendation = paste(
        "The water balance is drifting, not merely offset.",
        "Add MET_wndspd as a met-scaling parameter",
        "(file = 'met' in param_table) to adjust evaporative forcing.",
        "For glm_aed, seepage_rate can absorb residual (seepage = .true.)",
        "groundwater losses. Resolve the water balance before thermal calibration:",
        "a drifting level corrupts layer depths and volumes used by all other",
        "diagnostic variables."),
      params = "MET_wndspd (met scaling); outflow/seepage_rate (glm_aed)",
      stringsAsFactors = FALSE)

  } else if (identical(wb$classification, "static_offset")) {
    out[["water_balance"]] <- data.frame(
      issue    = "water balance: static offset",
      severity = "low",
      evidence = sprintf(
        "LKE_lvlwtr mean bias %.3f m with no significant trend (%s) over %d obs (%.1f yr span).",
        wb$mean_bias, drift_str(wb$drift_m_per_yr), wb$n, wb$span_yr),
      recommendation = paste(
        "A stable volume offset, not a worsening trend.",
        "for glm_aed, a seepage_rate adjustment via set_glm_outflow_config()",
        "can absorb residual groundwater losses.",
        "No need to open met-scaling parameters (MET_wndspd, MET_radswd) for",
        "a static offset - those introduce free parameters that may overfit."),
      params = "outflow/seepage_rate (glm_aed)",
      stringsAsFactors = FALSE)

  } else if (identical(wb$classification, "sim_drift")) {
    out[["water_balance"]] <- data.frame(
      issue    = "water balance: model-only drift (no or insufficient observed level)",
      severity = if (abs(wb$sim_drift_m_per_yr) > 0.3) "high" else "medium",
      evidence = sprintf(
        "No reliable observed LKE_lvlwtr record; simulated series trends at %s",
        " (%.3f m total change over %.1f yr simulated).",
        drift_str(wb$sim_drift_m_per_yr), wb$sim_change_m, wb$sim_span_yr),
      recommendation = paste(
        "The model's internal water balance is drifting without observed level",
        "data to constrain it.",
        "Run calc_water_balance() targeting the simulated LKE_lvlwtr series and",
        "transfer the fitted C and h_inv with set_wbal_param().",
        "Add MET_wndspd as a met-scaling parameter if drift persists.",
        "For glm_aed, check seepage_rate via set_glm_outflow_config().",
        "Resolve before thermal calibration: layer depths depend on water level."),
      params = "C, h_inv (set_wbal_param); MET_wndspd (met scaling); seepage_rate (glm_aed)",
      stringsAsFactors = FALSE)
  }

  # ---- heat budget ----------------------------------------------------------
  hb <- diag$heat_budget
  if (identical(hb$classification, "deficit") ||
      identical(hb$classification, "surplus")) {
    dir_word <- if (hb$classification == "deficit") "more" else "less"
    max_band <- if (!is.null(hb$by_depth_band))
      max(abs(hb$by_depth_band$mean_bias_degC), na.rm = TRUE) else NA_real_
    out[["heat_budget"]] <- data.frame(
      issue    = paste0("heat budget: net ", hb$classification),
      severity = if (abs(hb$volume_weighted_bias_degC) > 1.5) "high" else "medium",
      evidence = sprintf(
        "Volume-weighted HYD_temp bias %.3f degC (%.2e J over %.3e m3 lake volume). Largest single depth-band bias: %s.",
        hb$volume_weighted_bias_degC, hb$total_bias_J, hb$lake_volume_m3,
        if (is.na(max_band)) "n/a" else sprintf("%.3f degC", max_band)),
      recommendation = paste0(
        "The lake holds ", dir_word, " heat than observed across the whole water column",
        " - this is a net energy error, not a redistribution between surface and bottom.",
        " Calibrate met-input scaling parameters: MET_radswd (shortwave radiation),",
        " MET_radlwd (longwave radiation), and MET_tmpair (air temperature), all applied",
        " via file = 'met' entries in param_table.",
        " Also check Kw: too low a light extinction coefficient allows shortwave to",
        " penetrate deeper, adding heat to the whole column.",
        " Do not use mixing coefficients (coef_mix_*, eta_S, f_wind) for a net",
        " deficit/surplus: mixing redistributes heat already in the lake but does",
        " not change the total amount the model receives from the atmosphere."),
      params = "MET_radswd, MET_radlwd, MET_tmpair (met scaling); Kw",
      stringsAsFactors = FALSE)

  } else if (identical(hb$classification, "redistribution")) {
    max_band <- if (!is.null(hb$by_depth_band))
      max(abs(hb$by_depth_band$mean_bias_degC), na.rm = TRUE) else NA_real_
    out[["heat_budget"]] <- data.frame(
      issue    = "heat budget: redistribution (stratification error, not net bias)",
      severity = "medium",
      evidence = sprintf(
        "Volume-weighted HYD_temp bias %.3f degC is small relative to largest single-band bias %s - surface and bottom errors are cancelling.",
        hb$volume_weighted_bias_degC,
        if (is.na(max_band)) "n/a" else sprintf("%.3f degC", max_band)),
      recommendation = paste(
        "Heat is misplaced between depth bands, not missing or excess overall -",
        "the model's stratification depth or strength is wrong.",
        "Calibrate mixing and turbulence parameters:",
        "glm_aed: coef_mix_hyp, coef_mix_shear, coef_mix_KH (set_glm_param());",
        "dy_cd: eta_S (wind stirring), eta_K, eta_P (set_dy_cd_param());",
        "simstrat_aed2: ModelParameters.f_wind (set_simstrat_param());",
        "gotm_wet: turbulence.* parameters (set_gotm_param()).",
        "Do not adjust met-scaling factors (MET_radswd, MET_tmpair) for a",
        "redistribution - those change total heat input and will not fix",
        "depth-dependent misplacement."),
      params = paste("coef_mix_hyp/shear/KH (glm_aed);",
                     "eta_S/K/P (dy_cd);",
                     "ModelParameters.f_wind (simstrat_aed2);",
                     "turbulence.* (gotm_wet)"),
      stringsAsFactors = FALSE)
  }

  # ---- nutrient budget --------------------------------------------------------
  nb <- diag$nutrient_budget
  if (!is.null(nb) && nrow(nb) > 0) {
    biased <- nb[nb$direction != "ok", , drop = FALSE]
    if (nrow(biased) > 0) {
      linkage <- NULL
      if (!is.null(param_table)) {
        linkage <- stats::setNames(lapply(biased$var, function(v) {
          unique(param_table$name[vapply(param_table$vars,
                                         function(x) v %in% x, logical(1))])
        }), biased$var)
      }
      for (i in seq_len(nrow(biased))) {
        v <- biased$var[i]
        p <- if (!is.null(linkage)) linkage[[v]] else character(0)
        out[[paste0("nutrient_", v)]] <- data.frame(
          issue    = paste0("nutrient budget: ", v, " (", biased$direction[i], ")"),
          severity = if (!is.na(biased$rel_bias[i]) && biased$rel_bias[i] > 0.75)
            "high" else "medium",
          evidence = sprintf(
            "%s: mean bias %+.3f (obs mean %.3f, relative bias %.0f%%, n = %d paired obs).",
            v, biased$mean_bias[i], biased$mean_obs[i],
            100 * biased$rel_bias[i], biased$n[i]),
          recommendation = if (length(p) > 0) {
            paste0(
              "Parameters in param_table linked to ", v, ": ",
              paste(p, collapse = ", "), ". ",
              "Confirm the direction of adjustment against each parameter's physical",
              " role before moving bounds - use get_aeme_parameters() to inspect",
              " descriptions. Add to a BGC calibration stage after water and heat",
              " budgets are resolved: thermal stratification drives oxygen and",
              " nutrient cycling, so an uncorrected heat bias will confound BGC fits.")
          } else {
            paste0(
              "No parameters in param_table are linked to ", v, ". ",
              "Run get_aeme_parameters(module = '<relevant_module>') to find AED",
              " parameters associated with this variable, then add them to",
              " param_table before running a BGC calibration stage.")
          },
          params = paste(p, collapse = ", "),
          stringsAsFactors = FALSE)
      }
    }
  }

  if (length(out) == 0) {
    return(data.frame(issue = "none", severity = "none",
                      evidence = "all three budgets are within tolerance",
                      recommendation = "no calibration changes indicated",
                      params = "", stringsAsFactors = FALSE))
  }

  dplyr::bind_rows(out)
}

#' Water level bias and drift, against observations and against the
#' model's own starting point
#'
#' Reports two independent drift signals:
#' \itemize{
#' \item{\code{drift_m_per_yr}}{ - obs-based: slope of the (sim - obs)
#' residual over time, at whatever observed dates exist. Needs real
#' observations, and enough of them to trust a slope.}
#' \item{\code{sim_drift_m_per_yr}}{ - model-only: slope of the raw
#' simulated `LKE_lvlwtr` series itself (\code{\link{get_var}} with
#' \code{use_obs = FALSE}, i.e. referenced to the hypsography's own zero,
#' not any observation-derived elevation offset) against time, over every
#' simulated day in the post-spin-up window. Available for every lake
#' regardless of observation coverage, so a lake that is silently draining
#' or filling under its own simulated water balance shows up here even with
#' zero level observations to compare against.}
#' }
#'
#' @param aeme Aeme object; already built and run for `model` (see
#' \code{\link{build_aeme}}/\code{\link{run_aeme}}).
#' @param model character; single model code to diagnose (see
#' \code{\link{check_model}}).
#' @param drift_ok_m_per_yr numeric; below this absolute slope, classified
#' `"static_offset"` (or `"ok"`) rather than `"drift"`.
#' @param bias_ok_m numeric or `NULL`; below this absolute mean bias (in
#' metres), classified `"ok"` regardless of drift. When `NULL` (default),
#' derived from the lake's maximum depth as `5%` of max depth, clamped to
#' \eqn{[0.1, 0.5]} m, so the threshold scales sensibly across shallow and
#' deep lakes without changing the absolute-unit semantics.
#' @param min_n integer; below this many observations, the
#' obs-based signal is not trusted (even if computed) and the model-only
#' signal decides the classification instead.
#'
#' @returns list with `mean_bias`, `drift_m_per_yr`, `n`, `span_yr`,
#' `sim_drift_m_per_yr`, `sim_change_m`, `sim_span_yr`, `classification`
#' (one of `"ok"`, `"static_offset"`, `"drift"`, `"ok_no_obs"`,
#' `"sim_drift"`, `"no_data"`), `low_confidence`, `detail`.
#'
#' @importFrom stats coef lm median
#' @importFrom utils head tail
#'
#' @export
diag_water_balance <- function(aeme, model, drift_ok_m_per_yr = 0.05,
                                bias_ok_m = NULL, min_n = 20) {

  aeme <- check_aeme(aeme)
  model <- check_model(model = model)
  if (length(model) > 1) {
    cli::cli_abort("{.arg model} must be a single model, not {length(model)}.")
  }

  # Derive bias threshold from hypsograph when not supplied explicitly.
  if (is.null(bias_ok_m)) {
    hyp <- tryCatch(input(aeme)$hypsograph, error = function(e) NULL)
    max_depth <- if (!is.null(hyp) && nrow(hyp) > 0) {
      max(hyp$elev, na.rm = TRUE) - min(hyp$elev, na.rm = TRUE)
    } else {
      NA_real_
    }
    bias_ok_m <- if (is.na(max_depth) || max_depth <= 0) {
      0.3  # safe fallback when hypsograph unavailable
    } else {
      max(0.1, min(0.5, max_depth * 0.05))
    }
  }

  # ---- model-only signal: always available, no observations required ----
  sim <- tryCatch(
    get_var(aeme, model = model, var_sim = "LKE_lvlwtr", use_obs = FALSE),
    error = function(e) NULL)
  sim_drift <- NA_real_; sim_change_m <- NA_real_; sim_span_yr <- NA_real_
  if (!is.null(sim) && nrow(sim) > 0) {
    sim <- sim[!is.na(sim$sim), ]
    sim <- sim[order(sim$Date), ]
    if (nrow(sim) >= 2) {
      sim_span_yr <- as.numeric(max(sim$Date) - min(sim$Date)) / 365.25
      # Endpoint values as the median of the first/last 30 days, not the
      # single first/last day, so one noisy day at either edge (e.g. right
      # after spin-up) doesn't set the whole estimate.
      k <- min(30, floor(nrow(sim) / 2))
      start_val <- stats::median(utils::head(sim$sim, k))
      end_val   <- stats::median(utils::tail(sim$sim, k))
      sim_change_m <- end_val - start_val
      t_num <- as.numeric(sim$Date)
      fit <- tryCatch(stats::lm(sim ~ t_num, data = sim), error = function(e) NULL)
      if (!is.null(fit)) sim_drift <- unname(stats::coef(fit)[2]) * 365.25
    }
  }
  sim_detail <- if (is.na(sim_drift)) {
    "model-only drift: unavailable"
  } else {
    sprintf(
      "model-only drift %.4f m/yr (%.3f m total change over %.1f yr simulated)",
      sim_drift, sim_change_m, sim_span_yr)
  }

  # ---- observation-based signal: only if real observations exist --------
  lvl <- tryCatch(
    get_var(aeme, model = model, var_sim = "LKE_lvlwtr", use_obs = TRUE),
    error = function(e) NULL)
  has_obs <- !is.null(lvl) && nrow(lvl) > 0
  if (has_obs) lvl <- lvl[!is.na(lvl$sim) & !is.na(lvl$obs), ]
  has_obs <- has_obs && nrow(lvl) > 0

  mean_bias <- NA_real_; drift <- NA_real_; span_yr <- NA_real_; n_obs <- 0L
  if (has_obs) {
    resid <- lvl$sim - lvl$obs
    mean_bias <- mean(resid, na.rm = TRUE)
    n_obs <- nrow(lvl)
    # Actual calendar span the observations cover, not nrow(lvl)/365.25,
    # which silently assumes ~daily sampling and badly understates span for
    # a sparse level record.
    span_yr <- as.numeric(max(lvl$Date) - min(lvl$Date)) / 365.25
    if (n_obs >= min_n) {
      t_num <- as.numeric(lvl$Date)
      fit <- tryCatch(stats::lm(resid ~ t_num), error = function(e) NULL)
      if (!is.null(fit)) drift <- unname(stats::coef(fit)[2]) * 365.25
    }
  }
  low_confidence <- !has_obs || n_obs < min_n

  # ---- classification: obs-based when trustworthy, model-only fallback --
  if (has_obs && !low_confidence) {
    classification <- if (is.na(drift)) {
      if (abs(mean_bias) < bias_ok_m) "ok" else "static_offset"
    } else if (abs(drift) >= drift_ok_m_per_yr &&
               abs(drift) * span_yr > abs(mean_bias) * 0.5) {
      "drift"
    } else if (abs(mean_bias) < bias_ok_m) {
      "ok"
    } else {
      "static_offset"
    }
    drift_str <- if (is.na(drift)) "NA" else sprintf("%.4f", drift)
    detail <- sprintf(
      "mean bias %.3f m, drift %s m/yr over %d obs (%.1f yr actual span); %s",
      mean_bias, drift_str, n_obs, span_yr, sim_detail)
  } else if (!is.na(sim_drift)) {
    # No observations, or too few to trust a residual-based slope: fall
    # back to the model-only signal entirely.
    classification <- if (abs(sim_drift) >= drift_ok_m_per_yr) "sim_drift" else "ok_no_obs"
    obs_note <- if (has_obs) {
      sprintf("(%d obs, below the %d needed to trust an obs-based slope)",
              n_obs, min_n)
    } else {
      "(no observations at all)"
    }
    detail <- sprintf("%s %s - classified from model-only signal", sim_detail, obs_note)
  } else {
    classification <- "no_data"
    detail <- "no LKE_lvlwtr observations and no usable simulated series"
  }

  list(mean_bias = mean_bias, drift_m_per_yr = drift, n = n_obs, span_yr = span_yr,
       sim_drift_m_per_yr = sim_drift, sim_change_m = sim_change_m,
       sim_span_yr = sim_span_yr, classification = classification,
       low_confidence = low_confidence, detail = detail)
}

#' Whole-lake heat budget vs observations: net deficit/surplus, or just
#' surface/bottom redistribution?
#'
#' Uses `HYD_temp` (not `HYD_nrgcnt` directly - there is no independent
#' observation of energy density to compare against, only temperature) for
#' the bias itself, volume-weighted via the lake's own hypsography, then
#' reports the implied total heat-content bias in Joules for an intuitive
#' magnitude (`rho * cw * volume-weighted-bias * lake_volume`).
#'
#' @param aeme Aeme object; already built and run for `model`.
#' @param model character; single model code to diagnose (see
#' \code{\link{check_model}}).
#' @param depth_breaks numeric vector; passed to \code{\link{cut}} for the
#' by-depth-band summary.
#' @param bias_ok_degC numeric; below this absolute volume-weighted bias,
#' classified `"ok"`.
#' @param redistribution_ratio numeric; if the volume-weighted bias's
#' magnitude is below this fraction of the largest single-band absolute
#' bias, the bands are judged to be substantially cancelling each other out
#' (heat misplaced, not missing/excess) rather than agreeing (a genuine net
#' deficit or surplus).
#'
#' @returns list with `volume_weighted_bias_degC`, `total_bias_J`,
#' `lake_volume_m3`, `by_depth_band` (data frame), `classification` (one of
#' `"deficit"`, `"surplus"`, `"redistribution"`, `"ok"`, `"no_data"`),
#' `detail`.
#'
#' @importFrom stats approx aggregate
#'
#' @export
diag_heat_budget <- function(aeme, model,
                              depth_breaks = c(-0.01, 2, 6, 10, 15, 20, 100),
                              bias_ok_degC = 0.5, redistribution_ratio = 0.5) {

  aeme <- check_aeme(aeme)
  model <- check_model(model = model)
  if (length(model) > 1) {
    cli::cli_abort("{.arg model} must be a single model, not {length(model)}.")
  }

  cmp <- tryCatch(
    get_var(aeme, model = model, var_sim = "HYD_temp", use_obs = TRUE),
    error = function(e) NULL)
  if (is.null(cmp) || nrow(cmp) == 0) {
    return(list(volume_weighted_bias_degC = NA_real_, total_bias_J = NA_real_,
                lake_volume_m3 = NA_real_, by_depth_band = NULL,
                classification = "no_data",
                detail = "no HYD_temp observations in the simulation window"))
  }

  cmp <- cmp[!is.na(cmp$depth) & !is.na(cmp$sim) & !is.na(cmp$obs), ]
  cmp$resid <- cmp$sim - cmp$obs

  inp <- input(aeme)
  hyp <- inp$hypsograph
  hyp <- hyp[order(hyp$elev), ]
  hyp$depth <- max(hyp$elev) - hyp$elev
  cmp$area <- stats::approx(hyp$depth, hyp$area,
                            xout = pmin(pmax(cmp$depth, min(hyp$depth)),
                                        max(hyp$depth)))$y

  vw_bias <- sum(cmp$resid * cmp$area) / sum(cmp$area)

  cmp$band <- cut(cmp$depth, breaks = depth_breaks, include.lowest = TRUE, right = FALSE)
  band_mean <- stats::aggregate(resid ~ band, data = cmp, FUN = mean)
  band_n <- stats::aggregate(resid ~ band, data = cmp, FUN = function(x) length(x))
  band_df <- merge(band_mean, band_n, by = "band", sort = FALSE)
  names(band_df) <- c("band", "mean_bias_degC", "n")
  # preserve factor ordering of bands
  band_df$band <- factor(band_df$band, levels = levels(cmp$band))

  # Total heat content bias in Joules: rho*cw at a representative
  # temperature (10 degC, mid-range for most temperate lakes) times lake
  # volume times the volume-weighted temperature bias. An approximation
  # (rho varies ~0.1% over 5-25 degC) good enough for an order-of-magnitude
  # figure, not a substitute for LKE_nrgtot's own per-day series.
  cw <- 4186
  rho <- rLakeAnalyzer::water.density(10)
  # Lake volume via trapezoidal integration of the hypsographic area-vs-depth
  # curve.
  o <- order(hyp$depth)
  hd <- hyp$depth[o]; ha <- hyp$area[o]
  vol <- sum(diff(hd) * (ha[-1] + ha[-length(ha)]) / 2)
  total_bias_J <- rho * cw * vw_bias * vol

  max_band_mag <- max(abs(band_df$mean_bias_degC), na.rm = TRUE)
  classification <- if (abs(vw_bias) < bias_ok_degC) {
    "ok"
  } else if (abs(vw_bias) < redistribution_ratio * max_band_mag) {
    "redistribution"
  } else if (vw_bias < 0) {
    "deficit"
  } else {
    "surplus"
  }

  list(volume_weighted_bias_degC = vw_bias, total_bias_J = total_bias_J,
       lake_volume_m3 = vol, by_depth_band = band_df,
       classification = classification,
       detail = sprintf(
         "volume-weighted bias %.3f degC (%.2e J, over %.3e m3), max single-band |bias| %.3f degC",
         vw_bias, total_bias_J, vol, max_band_mag))
}

#' Per-variable BGC bias vs observations
#'
#' Reports bias for whichever candidate variables this lake actually has
#' usable data for.
#'
#' @param aeme Aeme object; already built and run for `model`.
#' @param model character; single model code to diagnose (see
#' \code{\link{check_model}}).
#' @param candidates character vector; AEME variable names to check.
#' @param min_obs integer; below this many in-window observations, the
#' variable is skipped.
#' @param bias_ok_frac numeric; a variable is classified `"ok"` when the
#' absolute mean bias is below this fraction of the observed mean (a
#' variable-appropriate relative threshold, since oxygen/nitrate/
#' chlorophyll live on very different absolute scales).
#'
#' @returns data frame with columns `var`, `mean_bias`, `mean_obs`,
#' `rel_bias`, `n`, `direction` (`"model too high"`, `"model too low"` or
#' `"ok"`). Zero rows if no candidate variable has enough usable data.
#'
#' @export
diag_nutrient_budget <- function(aeme, model,
                                  candidates = c("CHM_oxy", "NIT_amm", "NIT_nit",
                                                "NIT_tn", "PHS_frp", "PHS_tp",
                                                "PHY_tchla"),
                                  min_obs = 8, bias_ok_frac = 0.25) {

  aeme <- check_aeme(aeme)
  model <- check_model(model = model)
  if (length(model) > 1) {
    cli::cli_abort("{.arg model} must be a single model, not {length(model)}.")
  }

  rows <- lapply(candidates, function(v) {
    cmp <- tryCatch(
      get_var(aeme, model = model, var_sim = v, use_obs = TRUE),
      error = function(e) NULL)
    if (is.null(cmp) || nrow(cmp) == 0) return(NULL)
    cmp <- cmp[!is.na(cmp$sim) & !is.na(cmp$obs), ]
    if (nrow(cmp) < min_obs) return(NULL)

    mean_bias <- mean(cmp$sim - cmp$obs, na.rm = TRUE)
    mean_obs  <- mean(cmp$obs, na.rm = TRUE)
    rel <- if (abs(mean_obs) > 1e-9) abs(mean_bias) / abs(mean_obs) else NA_real_
    direction <- if (!is.na(rel) && rel < bias_ok_frac) {
      "ok"
    } else if (mean_bias > 0) {
      "model too high"
    } else {
      "model too low"
    }

    data.frame(var = v, mean_bias = mean_bias, mean_obs = mean_obs,
              rel_bias = rel, n = nrow(cmp), direction = direction,
              stringsAsFactors = FALSE)
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) {
    return(data.frame(var = character(), mean_bias = numeric(),
                      mean_obs = numeric(), rel_bias = numeric(),
                      n = integer(), direction = character(),
                      stringsAsFactors = FALSE))
  }
  dplyr::bind_rows(rows)
}
