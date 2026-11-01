#' GLM weir outflow (m3/s)
#'
#' The broad-crested weir equation GLM uses internally for surface spill when
#' lake level exceeds `crest_elev`.
#'
#' @param level numeric; lake level (m, hypsograph datum).
#' @param crest_factor numeric; GLM `crest_factor`.
#' @param crest_height numeric; GLM `morphometry/crest_elev` (m).
#' @param crest_width numeric; GLM `outflow/crest_width` (m).
#'
#' @return numeric vector of outflow in m3/s.
#' @noRd
glm_weir_outflow <- function(level, crest_factor, crest_height, crest_width) {
  (2 / 3) * crest_factor * sqrt(2 * 9.81) * crest_width *
    pmax(level - crest_height, 0)^1.5
}

#' Estimate GLM weir parameters from observed outflow and level
#'
#' Least-squares fit of GLM's weir equation (`glm_weir_outflow()`) to paired
#' observed level and outflow. For a given `crest_elev` the best `crest_factor`
#' has a closed form, so only `crest_elev` is searched (1-D `optimize()`).
#'
#' @param level,flow numeric vectors of paired observed level (m) and outflow
#'   (m3/s).
#' @param crest_width numeric; weir crest width (m). Width and `crest_factor`
#'   are not separately identifiable, so this is fixed.
#' @param interval length-2 numeric; search range for `crest_elev`. Default
#'   spans one metre below the lowest observed level to the highest level with
#'   flow.
#'
#' @return list with `crest_elev`, `crest_factor`, `crest_width` and `n`
#'   (number of pairs used).
#' @noRd
est_glm_weir_params <- function(level, flow, crest_width = 6, interval = NULL) {
  ok <- is.finite(level) & is.finite(flow) & flow >= 0
  level <- level[ok]
  flow <- flow[ok]
  if (length(level) < 10 || !any(flow > 0)) {
    return(NULL)
  }
  glm_coefficient <- (2 / 3) * sqrt(2 * 9.81)

  fit_at_crest <- function(crest_m) {
    base_flow <- glm_coefficient * crest_width * pmax(level - crest_m, 0)^1.5
    denom <- sum(base_flow^2)
    if (denom == 0) {
      return(c(sse = sum(flow^2), crest_factor = 0))
    }
    crest_factor <- sum(base_flow * flow) / denom
    c(sse = sum((flow - crest_factor * base_flow)^2),
      crest_factor = crest_factor)
  }

  if (is.null(interval)) {
    interval <- c(min(level) - 1, max(level[flow > 0]))
  }
  fit <- stats::optimize(function(crest_m) fit_at_crest(crest_m)[["sse"]],
                         interval = interval)
  crest_elev <- fit$minimum
  crest_factor <- fit_at_crest(crest_elev)[["crest_factor"]]
  if (!is.finite(crest_factor) || crest_factor <= 0) {
    return(NULL)
  }
  list(crest_elev = crest_elev, crest_factor = crest_factor,
       crest_width = crest_width, n = length(level))
}

#' Convert between GLM weir parameters and water-balance `C` / `h_inv`
#'
#' The water balance weir is `Q = C * (h - h_inv)^1.5` (m3/s), the same
#' equation as `glm_weir_outflow()`, so `h_inv = crest_elev` and
#' `C = (2/3) * crest_factor * sqrt(2 * 9.81) * crest_width`.
#'
#' @param weir list with `crest_elev`, `crest_factor` and `crest_width`.
#' @return `c(C = , h_inv = )`.
#' @noRd
glm_weir_to_wbal <- function(weir) {
  c(C = (2 / 3) * weir$crest_factor * sqrt(2 * 9.81) * weir$crest_width,
    h_inv = weir$crest_elev)
}

#' Inverse of `glm_weir_to_wbal()`
#'
#' @param wbal_par `c(C = , h_inv = )` as stored in `water_balance()$params`.
#' @param crest_width numeric; GLM crest width (m).
#' @noRd
wbal_to_glm_weir <- function(wbal_par, crest_width = 6) {
  list(crest_elev = unname(wbal_par[["h_inv"]]),
       crest_factor = unname(wbal_par[["C"]]) /
         ((2 / 3) * sqrt(2 * 9.81) * crest_width),
       crest_width = crest_width, n = NA_integer_)
}

#' Set one evaporation family's `C` / `h_inv` in water-balance params
#'
#' `water_balance()$params` may be `NULL`, a legacy flat `c(C =, h_inv =)`
#' (applies to every family) or a family-keyed list; returns a family-keyed
#' list with `family` set to `value`.
#' @noRd
set_wbal_family_params <- function(params, family, value) {
  if (is.null(params)) {
    params <- list()
  } else if (!is.list(params)) {
    params <- stats::setNames(lapply(c("glm_aed", "gotm_wet", "simstrat_aed2"),
                                     \(f) params),
                              c("glm_aed", "gotm_wet", "simstrat_aed2"))
  }
  params[[family]] <- value
  params
}

#' Resolve the GLM weir settings for a build
#'
#' @param glm_weir `TRUE`, `FALSE`, or a list with optional `crest_width` and
#'   `interval`.
#' @param outf_data named list of observed outflow data frames (`Date`,
#'   `HYD_flow` in m3/day); the entry named `"outflow"` is used for the fit.
#' @param level data.frame with `Date` and `value` (observed or modelled level).
#' @return `NULL` (no weir), or the weir list with `source` = `"obs"`.
#' @noRd
resolve_glm_weir <- function(glm_weir, outf_data, level) {
  if (isFALSE(glm_weir) || is.null(glm_weir)) return(NULL)
  opts <- if (is.list(glm_weir)) glm_weir else list()
  obs_out <- outf_data[["outflow"]]
  if (is.null(obs_out) || is.null(level) || nrow(level) == 0) return(NULL)

  d <- dplyr::inner_join(
    data.frame(Date = as.Date(obs_out[["Date"]]),
               flow = obs_out[["HYD_flow"]] / 86400),
    data.frame(Date = as.Date(level[["Date"]]), level = level[["value"]]),
    by = "Date"
  )
  weir <- est_glm_weir_params(d$level, d$flow,
                              crest_width = opts[["crest_width"]] %||% 6,
                              interval = opts[["interval"]])
  if (!is.null(weir)) weir$source <- "obs"
  weir
}

#' Write the GLM weir into the nml
#'
#' Sets `morphometry/crest_elev` and `outflow/crest_width`, `crest_factor`,
#' leaving the outlets untouched (an `&outflow` block with `num_outlet = 0` is
#' created if none exists).
#'
#' @param glm_nml GLM nml list.
#' @param weir list from `resolve_glm_weir()` / `wbal_to_glm_weir()`.
#' @param min_crest numeric; lowest allowed crest (the starting surface
#'   elevation), as GLM requires `lake_depth <= crest_elev - base`.
#' @noRd
set_glm_weir_nml <- function(glm_nml, weir, min_crest = -Inf) {
  crest_elev <- max(weir$crest_elev, min_crest)
  if (crest_elev > weir$crest_elev) {
    cli_inform_safe(c("!" = "Fitted weir crest ({round(weir$crest_elev, 2)} m)
                      is below the initial lake level; using
                      {round(crest_elev, 2)} m."))
  }
  glm_nml[["morphometry"]][["crest_elev"]] <- round(crest_elev, 4)
  if (is.null(glm_nml[["outflow"]])) {
    glm_nml[["outflow"]] <- list(num_outlet = 0L)
  }
  glm_nml[["outflow"]][["crest_width"]] <- weir$crest_width
  glm_nml[["outflow"]][["crest_factor"]] <- round(weir$crest_factor, 5)
  glm_nml
}
