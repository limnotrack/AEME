#' Get water balance components from AEME object
#'
#' @inheritParams build_aeme
#' @inheritParams get_var
#'
#' @returns List with observed lake levels, AEME water balance, and model components
#' @export
#'
get_wbal_components <- function(
    aeme,
    model,
    remove_spin_up = TRUE,
    cumulative = FALSE
) {
  
  aeme <- check_aeme(aeme)
  if (missing(model)) {
    model <- list_models(aeme)
  } else {
    model <- check_model(model = model)
  }
  ## --- Time handling ---
  tme <- time(aeme)
  
  start_date <- if (remove_spin_up) {
    tme$start
  } else {
    tme$start - lubridate::ddays(tme$spin_up[[model[1]]])
  }
  
  ## --- Observations ---
  inp <- input(aeme)
  elev_offset <- min(inp$hypsograph$elev)
  
  obs <- observations(aeme)
  if (!is.null(obs$level)) {
    lake_level <- obs$level |>
      dplyr::filter(Date >= tme$start, Date <= tme$stop) |>
      dplyr::mutate(level = value - elev_offset)
  } else {
    lake_level <- NULL
  }
  
  ## --- AEME water balance ---
  wb <- water_balance(aeme)$data$wbal |>
    dplyr::filter(Date >= start_date, Date <= tme$stop) |>
    dplyr::mutate(level = value - elev_offset)
  
  if (cumulative) {
    wb <- wb |>
      dplyr::mutate(
        HYD_flow = cumsum(HYD_flow),
        outflow  = cumsum(outflow),
        rain     = cumsum(rain),
        evap_m3  = cumsum(evap_m3),
        evap_flux = cumsum(evap_flux)
      )
  }
  
  wb_sum <- wb |>
    dplyr::group_by(model) |> 
    dplyr::summarise(
      inflow  = sum(HYD_flow),
      outflow = sum(spill_outflow + HYD_outflow),
      rain    = sum(rain),
      evap_m3 = sum(evap_m3),
      .groups = "drop"
    ) |> 
    dplyr::mutate(Model = toggle_models(model))
  
  ## --- Helper to pull model vars ---
  get_mod <- function(var, cumulative = FALSE) {
    get_var(
      aeme = aeme,
      model = model,
      var = var,
      remove_spin_up = remove_spin_up,
      cumulative = cumulative
    ) |>
      dplyr::mutate(model = toggle_models(Model))
  }
  
  ## --- Model components ---
  mod <- list(
    level   = get_mod("LKE_lvlwtr"),
    inflow  = get_mod("LKE_inflow", cumulative),
    outflow = get_mod("LKE_outflow", cumulative),
    rain    = get_mod("LKE_pcpvol", cumulative),
    evap    = get_mod("LKE_evpvol", cumulative),
    ts      = get_mod("HYD_surft")
  )
  
  mod_sum <- lapply(
    mod[c("inflow", "outflow", "rain", "evap")],
    \(x) x |>
      dplyr::group_by(Model) |>
      dplyr::summarise(value = sum(value), .groups = "drop")
  )
  
  ## --- Return structured object ---
  list(
    meta = list(
      elev_offset = elev_offset,
      cumulative = cumulative
    ),
    obs = lake_level,
    wb = wb,
    wb_sum = wb_sum,
    mod = mod,
    mod_sum = mod_sum
  )
}
