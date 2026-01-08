#' Estimate Surface Temperature Using Energy Balance Model
#'
#' @param df data frame with meteorological and observed surface temperature data.
#'  Must include columns:
#'  - \code{Date}: Date of observation
#'  - \code{MET_tmpair}: Air temperature (°C)
#'  - \code{MET_wndspd}: Wind speed (m/s)
#'  - \code{MET_radswd}: Downward shortwave radiation (W/m²)
#'  - \code{MET_radlwd}: Downward longwave radiation (W/m²)
#'  - \code{MET_prvapr}: Vapor pressure (mb)
#'  - \code{HYD_temp}: Observed surface temperature (°C),
#'  - \code{T5avg}: 5-day average air temperature (°C)
#' @param depth Lake depth (m)
#' @param init_temp Initial surface temperature (°C). If NULL, uses first
#'  available observation or air temperature.
#' @param f_mix Fraction of lake involved in surface mixing. Defaults to 0.2
#' @param rho_w Water density (kg/m³)
#' @param cp_w Heat capacity of water (J/kg/K)
#' @param dt Timestep (s)
#' @param alpha_sw Shortwave albedo. Defaults to 0.07
#' @param ch Bulk sensible heat coefficient. Defaults to 1.3e-3
#' @param ce Bulk latent heat coefficient. Defaults to 1.3e-3
#' @param relax_tau Relaxation timescale to observations (s)
#'
#' @returns Data frame with estimated surface temperature added as column
#'  \code{sst}
#' @export
#' @importFrom dplyr mutate filter summarise
#' @importFrom zoo rollmean
#'

estimate_surface_temperature <- function(
    df,
    depth,  
    init_temp = NULL,      # initial surface temperature (°C)
    f_mix = 0.2,        # fraction of lake involved in surface mixing
    rho_w = 1000,       # water density (kg/m3)
    cp_w = 4186,        # heat capacity of water (J/kg/K)
    dt = 86400,         # timestep (s), default = daily
    alpha_sw = 0.07,    # shortwave albedo
    ch = 1.3e-3,        # bulk sensible heat coeff
    ce = 1.3e-3,        # bulk latent heat coeff
    relax_tau = 3 * 86400 # relaxation timescale to obs (s)
) {
  
  if (depth <= 0) {
    cli::cli_abort("Lake depth {.val {depth}} must be positive.")
  } else {
    h_mix <- pmax(2, f_mix * depth)  # minimum 0.5 m for numerical stability
  }
  
  # t_range <- df |> 
  #   dplyr::filter(!is.na(HYD_temp)) |> 
  #   dplyr::summarise(min_time = min(Date), max_time = max(Date))
  # df <- df |> 
  #   dplyr::filter(Date >= t_range$min_time & Date <= t_range$max_time)
  
  # Add 5-day rolling average of air temperature
  df <- df |>
    dplyr::mutate(T5avg = zoo::rollmean(MET_tmpair, 5, na.pad = TRUE,
                                        align = c("right")))
  
  
  n <- nrow(df)
  Ts <- numeric(n)
  
  # --- Initial condition ---
  if (!is.null(init_temp)) {
    Ts[1] <- init_temp
  } else if (!is.na(df$HYD_temp[1])) {
    Ts[1] <- df$HYD_temp[1]
  } else if (!is.na(df$T5avg[1])) {
    Ts[1] <- df$T5avg[1]
  } else {
    Ts[1] <- df$MET_tmpair[1]
  }
  
  for (t in 1:(n - 1)) {
    
    Ta <- df$MET_tmpair[t]
    U  <- max(df$MET_wndspd[t], 0.1)  # avoid zero wind
    Ts_t <- Ts[t]
    
    # --- Net shortwave ---
    Qsw <- (1 - alpha_sw) * df$MET_radswd[t]
    
    # --- Net longwave ---
    sigma <- 5.67e-8
    Qlw <- df$MET_radlwd[t] - sigma * (Ts_t + 273.15)^4
    
    # --- Sensible heat flux ---
    rho_a <- 1.2
    cp_a <- 1005
    Qh <- rho_a * cp_a * ch * U * (Ta - Ts_t)
    
    # --- Latent heat flux (simple bulk) ---
    es <- 610.78 * exp(17.27 * Ts_t / (Ts_t + 237.3))
    ea <- df$MET_prvapr[t] * 100
    L <- 2.5e6
    Qe <- rho_a * L * ce * U * (ea - es) / 101325
    
    # --- Temperature tendency ---
    dTs <- (Qsw + Qlw + Qh + Qe) / (rho_w * cp_w * h_mix) * dt
    Ts_pred <- Ts_t + dTs
    
    # --- Assimilate observed surface temperature if available ---
    if (!is.na(df$HYD_temp[t + 1])) {
      Ts[t + 1] <- Ts_pred +
        (dt / relax_tau) * (df$HYD_temp[t + 1] - Ts_pred)
    } else {
      Ts[t + 1] <- Ts_pred
    }
  }
  # fit <- lm(HYD_temp ~ T5avg, data = evap)
  # coeffs <- coefficients(fit)
  # lm_vals <- coeffs[1] + coeffs[2] * df$T5avg
  # 
  # plot(Ts, type='l', col='blue', main='Estimated Surface Temperature', ylab='Temperature (°C)', xlab='Time Step')
  # points(df$HYD_temp, col='red', pch=16)
  # lines(lm_vals, col='green', lty=1)
  # 
  # tdiff <- diff(Ts)
  # plot(tdiff)
  
  df$sst <- Ts
  return(df)
}
