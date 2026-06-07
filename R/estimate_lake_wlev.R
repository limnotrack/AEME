#' Estimate Lake Water Levels with Nudging
#'
#' @description
#' This function estimates lake water levels using a hydrological model with nudging
#' to observed water levels. It optimizes parameters to minimize the error between
#' simulated and observed levels, applying nudging to guide the simulation towards
#' observed values.
#' @param data data frame with columns:
#'   - Date: Date of observation
#'   - HYD_flow: Inflow to the lake (m3/day)
#'   - MET_pprain: Precipitation on the lake surface (m/day)
#'   - evap_m3: Evaporation from the lake surface (m3/day)
#'   - lvl_obs: Observed lake water level (m)
#'   - is_obs_lvl: Logical indicating if lvl_obs is an observation (TRUE/FALSE)
#' @param hyps_df data frame with hypsograph data containing columns:
#'   - elev: Elevation (m)
#'   - area: Surface area at that elevation (m2)
#'   - volume: Volume at that elevation (m3)
#' @param model Character string indicating the evaporation model to use:
#'   - "dy_cd": DYRESM-CAEDYM dynamic evaporation
#'   - "glm_aed": GLM-AED dynamic evaporation
#'   - "gotm_wet": GOTM-WET dynamic evaporation
#' @param initial_guess Optional initial guess for optimization parameters:
#'   - C: Outflow coefficient
#'   - h_inv: Inversion height for outflow calculation
#' @param init_elev Numeric; initial lake elevation (m) to start the simulation. 
#' This should be a reasonable estimate based on the observed levels to ensure 
#' the optimization converges.
#' @param verbose Logical indicating whether to print optimization details
#'
#' @returns A data frame with original data and additional columns:
#'   - lvl_sim: Simulated lake water level (m)
#'   - HYD_outflow_sim: Simulated lake outflow (m3/day)
#' @export
#'

estimate_lake_wlev <- function(data, hyps_df, model, init_elev, params = NULL,
                               initial_guess = NULL, verbose = FALSE) {
  
  msg <- paste0("Estimating lake water levels for ", model)
  cli_safe(msg, indent = FALSE)
  # 1. Setup Initial Conditions
  # Find the first non-NA observation for the starting level
  # first_obs_idx <- which(!is.na(data$lvl_obs))[1]
  # if (is.na(first_obs_idx)) stop("No observed levels found in lvl_obs column.")
  
  start_lvl <- init_elev
  
  # Default initial guess if not provided
  if (is.null(initial_guess)) {
    initial_guess <- c(C = 0.5, h_inv = min(data$lvl_obs, na.rm = TRUE) - 0.5)
  }
  if (model == "gotm_wet") {
    gotm_met <- data |>
      dplyr::rename(u10 = MET_wnduvu, v10 = MET_wnduvv, airt = MET_tmpair,
                    hum = MET_humrel, airp = MET_prsttn, precip = MET_pprain) |>
      dplyr::mutate(precip = precip / 86400, airp = airp) |> 
      add_hum_vars(hum_method = 1)
  } else {
    gotm_met <- NULL
  }
  
  out <- simulate_lake_nudged(params = initial_guess, data = data, 
                              hyps_df = hyps_df,  start_lvl = start_lvl,
                              model = model, gotm_met = gotm_met)
  level_cost(params = initial_guess, data = data, hyps_df = hyps_df,
             start_lvl = start_lvl, model = model, gotm_met = gotm_met)
  # plot(out$h, type = "l")
  # points(data$lvl_obs, col = "red")
  
  # 2. Run Optimization
  # Uses the 'level_cost' function you defined previously
  if (is.null(params)) {
    cli_safe(c("i" = "Optimizing parameters for water balance"))
    best_fit <- optim(
      par = initial_guess,
      fn = level_cost,
      data = data,
      hyps_df = hyps_df,
      start_lvl = start_lvl,
      model = model,
      gotm_met = gotm_met,
      method = "L-BFGS-B",
      lower = c(0.001, min(hyps_df$elev)),
      upper = c(10, max(data$lvl_obs, na.rm = TRUE)),
      control = list(maxit = 2)
    )
    # if (verbose) {
    #   message(paste0("Optimization Complete:"))
    #   message(paste0("  Best C: ", round(best_fit$par[1], 4)))
    #   message(paste0("  Best h_inv: ", round(best_fit$par[2], 4)))
    #   message(paste0("  Final RMSE: ", round(best_fit$value, 4)))
    # }
    msg <- paste0("Optimization Complete: C = ", round(best_fit$par[1], 4),
                  ", h_inv = ", round(best_fit$par[2], 4), 
                  ", Final RMSE = ", round(best_fit$value, 4))
    cli_safe(c("v" = msg))
    params <- c(best_fit$par[1], best_fit$par[2])
  }

  
  # 3. Generate Final Time Series
  # Uses the standard simulate_lake (un-nudged) for the final result
  final_sim <- simulate_lake_nudged(params = params, data = data, 
                                    hyps_df = hyps_df,  start_lvl = start_lvl,
                                    model = model, gotm_met = gotm_met)
  # sum(final_sim$residual)
  
  # 4. Append results to dataframe and return
  A_t <- get_hyps_val(depth = final_sim$h, hyps = hyps_df)

  data$lvl_sim <- final_sim$h
  data$spill_outflow <- final_sim$O
  data$evap_m3 <- final_sim$evap
  data$evap_flux <- final_sim$evap / A_t
  data$C <- params["C"]
  data$h_inv <- params["h_inv"]
  
  data$net_balance <- data$HYD_flow + (data$MET_pprain * A_t) - 
                        data$evap_m3 - data$spill_outflow
    
  # plot(data$lvl_sim, type = "l")
  # points(data$lvl_obs, col = "red")
  return(data)
}

#' Calculate level from volume using hypsograph
#' @param V numeric; volume (m3)
#' @param hyps data frame; with hypsography data containing columns:
#'   - elev: Elevation (m)
#'   - volume: Volume at that elevation (m3)
#' @return numeric; lake level (m)
#' @noRd
level_from_volume <- function(V, hyps) {
  approx(hyps$volume, hyps$elev, V, rule = 2)$y
}

#' Calculate cost for lake level simulation
#' @noRd
level_cost <- function(params, data, hyps_df, start_lvl, model,
                       gotm_met = NULL) {
  # cat("Parameters: C =", round(params[1],4), ", h_inv =", round(params[2],4), "\n")
  # Penalize if h_inv is physically impossible (e.g., above max lake level)
  if(params[2] > max(hyps_df$elev)) return(1e10)
  
  res <- simulate_lake_nudged(params, data, hyps_df, start_lvl, model, gotm_met)
  
  # Compare only on days where we have an observation (is_obs_lvl == TRUE)
  obs_idx <- which(data$is_obs_lvl)
  rmse <- sqrt(mean((res$h[obs_idx] - data$lvl_obs[obs_idx])^2, na.rm = TRUE))
  
  if (is.infinite(rmse) | is.nan(rmse)) {
    return(1e10)
  }
  
  return(rmse)
}

#' Simulate lake levels with nudging to observations
#' @noRd
simulate_lake_nudged <- function(params, data, hyps_df, start_lvl, 
                                 model = "dy_cd", gotm_met = NULL) {
  C <- params[1]
  h_inv <- params[2]
  alpha <- 0
  lambda <- 1e-6  # Penalty weight for outflow magnitude
  
  n_days <- nrow(data)
  
  sim_h <- numeric(n_days)
  sim_V <- numeric(n_days)
  sim_O <- numeric(n_days)
  sim_evap <- numeric(n_days)
  residual <- numeric(n_days)   # C) mass-balance residual
  
  sim_h[1] <- start_lvl
  sim_V[1] <- calc_V(depth = sim_h[1], hyps = hyps_df)
  
  # current_nudge <- 0
  penalty_outflow <- 0
  obs_idx <- which(data$is_obs_lvl)

  
  for (t in 1:(n_days - 1)) {
    
    # 1. Current Geometry
    A_t <- get_hyps_val(depth = sim_h[t], hyps = hyps_df)
    
    # 2. Outflow
    sim_O[t] <- C * (pmax(sim_h[t] - h_inv, 0))^1.5 * 86400
    
    # Accumulate penalty (B)
    penalty_outflow <- penalty_outflow + lambda * sim_O[t]^2
    
    # 3. Dynamic Evaporation (Model Dependent)
    evap_m_day <- 0 # Default
    V_min <- min(hyps_df$volume)
    
    if (model %in% c("dy_cd", "glm_aed")) {
      # Physics for DYRESM-CAEDYM / GLM
      Ts_t <- data$sst[t]
      es_t <- exp(2.3026 * (((7.5 * Ts_t) / (Ts_t + 237.3) + 0.7858)))
      Qlh_t <- (0.622/981.9) * 0.0013 * 1.168 * 2453000 * data$MET_wndspd[t] * (data$MET_prvapr[t] - es_t)
      if(Qlh_t > 0) Qlh_t <- 0
      
      # Convert heat flux to depth (m/day)
      # formula: (mass loss / density) / area -> becomes depth
      evap_m_day <- ((Qlh_t) / 2258000) * (86400 / 1000)
      
    } else if (model == "gotm_wet") {
      # For GOTM, we call your existing calc_evap function
      # We pass the row of data for time t

      evap_m_day <- calc_evap(met = gotm_met[t, ], model = "gotm_wet",
                              method = "fairall") * 86400
    }
    
    evap_m_day <- abs(evap_m_day)
    
    evap_vol_t <- evap_m_day * A_t
    sim_evap[t] <- evap_vol_t
    
    # 4. Water Balance
    rain_vol_t <- data$MET_pprain[t] * A_t
    net_flux <- data$HYD_flow[t] + rain_vol_t - evap_vol_t - sim_O[t] - 
      data$HYD_outflow[t]
    
    V_pred <- sim_V[t] + net_flux
    
    # 5. A) Kalman-style nudging with gap-aware alpha
    if (any(obs_idx > t)) {
      
      next_obs <- obs_idx[obs_idx > t][1]
      gap_days <- next_obs - t
      
      alpha_eff <- alpha * min(1, gap_days / 30)
      
      if (data$is_obs_lvl[t + 1]) {
        V_obs <-volume_from_level(data$lvl_obs[t + 1], hyps_df)
        # V_obs <- calc_V(data$lvl_obs[t + 1], hyps_df)
        innovation <- V_obs - V_pred
        V_upd <- V_pred + alpha_eff * innovation
      } else {
        V_upd <- V_pred
      }
      
    } else {
      V_upd <- V_pred
    }
    
    
    # 6. Finalize state
    sim_V[t + 1] <- max(min(hyps_df$volume), V_upd)
    if (is.na(sim_V[t + 1])) {
      # message("NA volume at time ", t + 1)
      break
    }
    sim_h[t + 1] <- level_from_volume(sim_V[t + 1], hyps_df)
    
    # 7. C) Mass-balance residual
    residual[t] <- sim_V[t + 1] - sim_V[t] - net_flux
  }
  
  # Estimate outflow for the last day
  A_t <- get_hyps_val(depth = sim_h[n_days], hyps = hyps_df)
  sim_O[n_days] <- pmax(0, C * (sim_h[n_days] - h_inv)^1.5) * 86400
  
  # 1. Current Geometry
  t <- n_days
  A_t <- get_hyps_val(depth = sim_h[t], hyps = hyps_df)
  
  if (model %in% c("dy_cd", "glm_aed")) {
    # Physics for DYRESM-CAEDYM / GLM
    Ts_t <- data$sst[t]
    es_t <- exp(2.3026 * (((7.5 * Ts_t) / (Ts_t + 237.3) + 0.7858)))
    Qlh_t <- (0.622/981.9) * 0.0013 * 1.168 * 2453000 * data$MET_wndspd[t] * (data$MET_prvapr[t] - es_t)
    if(Qlh_t > 0) Qlh_t <- 0
    
    # Convert heat flux to depth (m/day)
    # formula: (mass loss / density) / area -> becomes depth
    evap_m_day <- ((Qlh_t) / 2258000) * (86400 / 1000)
    
  } else if (model == "gotm_wet") {
    # For GOTM, we call your existing calc_evap function
    # We pass the row of data for time t
    evap_m_day <- calc_evap(met = gotm_met[t, ], model = "gotm_wet",
                            method = "fairall") * 86400
  }
  evap_m_day <- abs(evap_m_day)
  
  sim_evap[t] <- evap_m_day * A_t
  
  return(list(
    h = sim_h,
    V = sim_V,
    O = sim_O,
    evap = sim_evap,
    residual = residual,
    penalty = penalty_outflow
  ))
}

