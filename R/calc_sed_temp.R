#' GLM sediment-temperature parameters from observed temperature profiles
#'
#' Fits an annual sediment-temperature cycle (`sed_temp_mean`,
#' `sed_temp_amplitude`, `sed_temp_peak_doy`) for each GLM sediment zone from
#' observed water-column temperature profiles. Used internally by
#' [build_aeme()] to populate the GLM `&sediment` block, and exported so the
#' same per-zone values can be generated as a calibration parameter table.
#'
#' Inputs can be supplied directly (`obs_temp`, `hypsograph`, `sed_zones`) or
#' pulled from an [Aeme-class] object via `aeme`: observations come from
#' [get_obs()] and the hypsograph from [input()]. `sed_zones` defaults to
#' [estimate_sed_zones()] on the hypsograph when not given.
#'
#' @param aeme `Aeme` object; when supplied, `obs_temp` and `hypsograph` are
#'   extracted from it unless passed explicitly. Default `NULL`.
#' @param obs_temp long df: Date, var_aeme, depth, value (degC), depths in m
#'   below surface. Optional when `aeme` is given.
#' @param hypsograph df with `depth` (m, +down) or `elev`, and `area` (m2), used
#'   to area-weight within a zone and (with `sed_zones = NULL`) to derive the
#'   zones. Optional when `aeme` is given. `NULL` -> uniform weighting and
#'   `sed_zones` / `max_depth` must be supplied.
#' @param sed_zones numeric vector of zone upper-boundary heights above the bed,
#'   ascending - exactly as passed to GLM `zone_heights`. Zone 1 is
#'   0 -> sed_zones\[1\] (the deepest slab); the last value may exceed max depth.
#'   `NULL` (default) -> [estimate_sed_zones()] on `hypsograph`.
#' @param max_depth max lake depth (m). NULL -> from hypsograph, else deepest obs (warns).
#' @param temp_var value of var_aeme to keep.
#' @param nml_file character; name of the GLM nml file the parameters belong to,
#'   used for the `file` column of the parameter table. Default `"glm4.nml"`.
#' @param output one of `"parameters"` (default) - a long AEME model-parameter
#'   table in [param_colnames()]`(incl_opt = FALSE)` order, ready for
#'   calibration; `"nml"` - a named list of the three per-zone vectors to splice
#'   into `glm_nml[["sediment"]]`; or `"summary"` - the per-zone diagnostic
#'   data.frame with `parameters` / `nml` / `zone_series` attributes.
#' @param depth_grid within-zone integration step (m).
#' @param depth_tol a cast is used for a zone only if its deepest sample is within
#'   depth_tol m of the top of that zone's depth band.
#' @param min_obs,min_months thresholds for a harmonic fit (else monthly-range).
#' @param default_mean,default_amplitude,default_peak_doy last-resort values.
#'   default_peak_doy = 46 is mid-Feb (S. hemisphere summer); use ~209 for N.
#' @param borrow_amp_factor,borrow_lag_days when an unfitted deep zone borrows a
#'   cycle from the nearest shallower fitted zone: damp amplitude by
#'   borrow_amp_factor^(zone steps) and lag peak_doy by borrow_lag_days per step.
#' @param verbose logical; print the assembled per-zone table, nml snippet and
#'   parameter table.
#'
#' @return Depends on `output`:
#'   * `"parameters"` - data.frame with columns `model`, `file`, `name`,
#'     `value`, `min`, `max`, `group`, `index`; one row per zone for each of
#'     `sediment/sed_temp_mean`, `sediment/sed_temp_amplitude` and
#'     `sediment/sed_temp_peak_doy`.
#'   * `"nml"` - named list `sed_temp_mean` / `sed_temp_amplitude` /
#'     `sed_temp_peak_doy`, each a per-zone numeric vector in GLM zone order.
#'   * `"summary"` - the per-zone diagnostic data.frame (zone 1 = deepest),
#'     carrying the `"parameters"` table, a ready-to-paste `"nml"` snippet and
#'     the per-zone `"zone_series"` as attributes.
#'
#' @importFrom dplyr filter mutate group_by summarise bind_rows
#' @export
calc_sed_temp <- function(aeme = NULL, obs_temp = NULL, hypsograph = NULL,
                          sed_zones = NULL, max_depth = NULL,
                          temp_var = "HYD_temp", nml_file = "glm4.nml",
                          output = c("parameters", "nml", "summary"),
                          depth_grid = 0.25, depth_tol = 2,
                          min_obs = 10, min_months = 6,
                          default_mean = 12, default_amplitude = 4,
                          default_peak_doy = 46,
                          borrow_amp_factor = 0.6, borrow_lag_days = 25,
                          verbose = TRUE) {

  output <- match.arg(output)

  ## 0. resolve inputs from an Aeme object -----------------------------------
  if (!is.null(aeme)) {
    if (is.null(obs_temp)) obs_temp <- get_obs(aeme, var_sim = temp_var)
    if (is.null(hypsograph)) hypsograph <- input(aeme)[["hypsograph"]]
  }
  if (is.null(obs_temp) || !nrow(obs_temp))
    stop("No observed temperature data: supply 'obs_temp' or an 'aeme' with ",
         "temperature observations.")
  if (is.null(sed_zones)) {
    if (is.null(hypsograph))
      stop("Supply 'sed_zones', or a 'hypsograph' / 'aeme' to derive them from.")
    sed_zones <- estimate_sed_zones(hypsograph = hypsograph)
  }

  stopifnot(is.numeric(sed_zones), length(sed_zones) >= 1)
  if (length(sed_zones) > 1 && any(diff(sed_zones) <= 0))
    stop("sed_zones must be strictly ascending heights above the bed.")

  ## 1. tidy observations ------------------------------------------------------
  d <- obs_temp |>
    dplyr::filter(var_aeme == temp_var)
  if (!nrow(d)) stop("No rows with var_aeme == '", temp_var, "'.")
  d <- d |>
    dplyr::mutate(
      Date  = as.Date(Date),
      depth = as.numeric(depth),
      value = as.numeric(value)
    ) |>
    dplyr::filter(is.finite(depth), is.finite(value), !is.na(Date))
  if (!nrow(d)) stop("No finite temperature observations after cleaning.")
  
  ## 2. hypsograph + max depth ---------------------------------------------
  # Work in positive-down depth. AEME hypsographs carry `elev` plus a `depth`
  # column that is <= 0 below the surface, so prefer `elev` and only fall back
  # to `depth` (flipping its sign when it is the AEME-style negative-down one).
  area_at <- NULL
  if (!is.null(hypsograph)) {
    hyp <- hypsograph
    if (!is.null(hyp$elev)) {
      hyp$depth <- max(hyp$elev, na.rm = TRUE) - hyp$elev
    } else if (!is.null(hyp$depth)) {
      if (all(hyp$depth <= 0, na.rm = TRUE)) hyp$depth <- -hyp$depth
    } else {
      stop("hypsograph needs a 'depth' or 'elev' column.")
    }
    hyp <- hyp[is.finite(hyp$depth) & is.finite(hyp$area), ]
    hyp <- hyp[order(hyp$depth), ]
    area_at <- stats::approxfun(hyp$depth, hyp$area, rule = 2)
    if (is.null(max_depth)) max_depth <- max(hyp$depth)
  }
  if (is.null(max_depth)) {
    max_depth <- max(d$depth)
    warning("max_depth not supplied; using deepest observation (",
            round(max_depth, 1), " m).")
  }
  
  ## 3. zone depth bands (m below surface); zone 1 = deepest --------------
  n_zone <- length(sed_zones)
  upr_h  <- sed_zones
  lwr_h  <- c(0, sed_zones[-n_zone])
  zone_tbl <- data.frame(
    zone      = seq_len(n_zone),
    depth_top = round(pmax(max_depth - upr_h, 0), 2),   # shallower edge
    depth_bot = round(pmin(max_depth - lwr_h, max_depth), 2))  # deeper edge
  
  ## 4. per-zone area-weighted bottom-water series -----------------------
  by_date <- split(d[, c("depth", "value")], d$Date)
  ddates  <- as.Date(names(by_date))
  zone_series <- vector("list", n_zone)
  
  for (i in seq_len(n_zone)) {
    zt <- zone_tbl$depth_top[i]; zb <- zone_tbl$depth_bot[i]
    if (zb - zt < 1e-6) { zone_series[[i]] <- data.frame(); next }
    grid <- seq(zt, zb, by = depth_grid); if (length(grid) < 2) grid <- c(zt, zb)
    gk <- (head(grid, -1) + tail(grid, -1)) / 2
    wk <- if (is.null(area_at)) rep(1, length(gk)) else
      pmax(-diff(pmax(area_at(grid), 0)), 0)   # benthic area per slice
    if (all(wk == 0)) wk <- rep(1, length(gk))
    
    recs <- lapply(seq_along(by_date), function(j) {
      p <- by_date[[j]]
      if (nrow(p) < 2) return(NULL)
      p <- p |>
        dplyr::group_by(depth) |>
        dplyr::summarise(value = mean(value), .groups = "drop")
      if (max(p$depth) < zt - depth_tol) return(NULL)   # cast misses the zone
      tk <- approx(p$depth, p$value, xout = gk, rule = 2)$y
      ok <- is.finite(tk)
      if (!any(ok)) return(NULL)
      data.frame(Date = ddates[j],
                 doy  = as.integer(format(ddates[j], "%j")),
                 temp = sum(tk[ok] * wk[ok]) / sum(wk[ok]))
    })
    zs <- do.call(rbind, recs)
    zone_series[[i]] <- if (is.null(zs)) data.frame() else zs
  }
  
  ## 5. fit annual harmonic, with fallbacks -----------------------------
  w <- 2 * pi / 365.25
  fit_zone <- function(ts) {
    if (is.null(ts) || !nrow(ts)) return(NULL)
    nm <- length(unique(as.integer(format(ts$Date, "%m"))))
    if (nrow(ts) >= min_obs && nm >= min_months) {
      b <- coef(lm(temp ~ cos(w * doy) + sin(w * doy), data = ts))
      list(mean = unname(b[1]), amp = unname(sqrt(b[2]^2 + b[3]^2)),
           peak = as.integer(round((atan2(b[3], b[2]) / w) %% 365.25)),
           n = nrow(ts), nm = nm, method = "harmonic")
    } else if (nrow(ts) >= 3) {
      mm   <- tapply(ts$temp, as.integer(format(ts$Date, "%m")), mean)
      warm <- as.integer(names(mm)[which.max(mm)])
      list(mean = mean(ts$temp), amp = (max(mm) - min(mm)) / 2,
           peak = as.integer(round((warm - 0.5) * 365.25 / 12)),
           n = nrow(ts), nm = nm, method = "monthly_range")
    } else {
      list(mean = mean(ts$temp), amp = NA_real_, peak = NA_integer_,
           n = nrow(ts), nm = nm, method = "mean_only")
    }
  }
  fits <- lapply(zone_series, fit_zone)
  
  has_cycle <- function(f) !is.null(f) && is.finite(f$amp) && !is.na(f$peak)
  good <- which(vapply(fits, has_cycle, logical(1)))
  
  for (i in seq_len(n_zone)) {
    if (has_cycle(fits[[i]])) next
    f <- fits[[i]]
    if (length(good)) {
      donor <- good[which.min(abs(good - i))]
      step  <- max(donor - i, 0)                     # >0 when donor is shallower
      g <- fits[[donor]]
      fits[[i]] <- list(
        mean = if (!is.null(f)) f$mean else g$mean,
        amp  = g$amp * borrow_amp_factor^step,
        peak = as.integer((g$peak + borrow_lag_days * step) %% 365.25),
        n = if (is.null(f)) 0L else f$n, nm = if (is.null(f)) 0L else f$nm,
        method = if (is.null(f)) "borrowed" else paste0(f$method, "+borrowed"))
    } else {
      fits[[i]] <- list(
        mean = if (!is.null(f)) f$mean else default_mean,
        amp = default_amplitude, peak = as.integer(default_peak_doy),
        n = if (is.null(f)) 0L else f$n, nm = if (is.null(f)) 0L else f$nm,
        method = if (is.null(f)) "default" else paste0(f$method, "+default"))
    }
  }
  
  ## 6. assemble ------------------------------------------------------
  pull_fit <- function(field, mode) vapply(fits, `[[`, mode, field)
  out <- zone_tbl |>
    dplyr::mutate(
      n_obs              = pull_fit("n", integer(1)),
      n_months           = pull_fit("nm", integer(1)),
      sed_temp_mean      = round(pull_fit("mean", numeric(1)), 3),
      sed_temp_amplitude = round(pull_fit("amp", numeric(1)), 3),
      sed_temp_peak_doy  = pull_fit("peak", integer(1)),
      method             = pull_fit("method", character(1))
    )
  
  fmt <- function(v) paste(formatC(v, format = "f", digits = 3), collapse = ", ")
  attr(out, "nml") <- c(
    paste0("   sed_temp_mean = ",      fmt(out$sed_temp_mean)),
    paste0("   sed_temp_amplitude = ", fmt(out$sed_temp_amplitude)),
    paste0("   sed_temp_peak_doy = ",  paste(out$sed_temp_peak_doy, collapse = ", ")))
  attr(out, "zone_series") <- setNames(zone_series, paste0("zone", seq_len(n_zone)))

  ## 7. AEME model-parameter table ------------------------------------
  # Layout matches glm_sed_params() and param_colnames(incl_opt = FALSE):
  # model, file, name, value, min, max, group, index. One row per zone
  # (index = GLM zone number, zone 1 = deepest) for each temperature param.
  as_param <- function(par, value) {
    peak <- identical(par, "sed_temp_peak_doy")
    data.frame(
      model = "glm_aed",
      file  = nml_file,
      name  = paste0("sediment/", par),
      value = value,
      min   = if (peak) pmax(1, floor(value * 0.5)) else value * 0.5,
      max   = if (peak) ceiling(value * 1.5) else value * 1.5,
      group = NA_character_,
      index = seq_along(value),
      stringsAsFactors = FALSE
    )
  }
  attr(out, "parameters") <- dplyr::bind_rows(
    as_param("sed_temp_mean",      out$sed_temp_mean),
    as_param("sed_temp_amplitude", out$sed_temp_amplitude),
    as_param("sed_temp_peak_doy",  out$sed_temp_peak_doy)
  )

  # Named list for splicing straight into glm_nml[["sediment"]].
  nml_list <- list(
    sed_temp_mean      = out$sed_temp_mean,
    sed_temp_amplitude = out$sed_temp_amplitude,
    sed_temp_peak_doy  = out$sed_temp_peak_doy
  )

  if (verbose) {
    message("max_depth = ", round(max_depth, 2), " m;  ", n_zone, " zone(s)")
    print(out, row.names = FALSE)
    message("\n", paste(attr(out, "nml"), collapse = "\n"))
    message("\nAEME parameters:")
    print(attr(out, "parameters"), row.names = FALSE)
  }

  switch(output,
         parameters = attr(out, "parameters"),
         nml        = nml_list,
         summary    = invisible(out))
}
