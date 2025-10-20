#' Update initial conditions in AEME object based on lake observations.
#'
#' @inheritParams build_aeme
#'
#' @returns Aeme object with updated initial conditions
#' @export
#' 
#' @importFrom dplyr mutate filter group_by summarise arrange n 
#'

update_init <- function(aeme, model_controls = NULL) {
  
  obs <- observations(aeme)
  tme <- time(aeme)
  inp <- input(aeme)
  init_depth <- inp$init_depth
  init_month <- as.numeric(format(as.Date(tme$start), "%m"))
  month_range <- c(init_month - 1, init_month, init_month + 1)
  if (init_month == 1) {
    month_range <- c(12, 1, 2)
  } else if (init_month == 12) {
    month_range <- c(11, 12, 1)
  }
  if (is.null(model_controls)) {
    mod_ctrls <- get_model_controls(aeme = aeme)
    if (is.null(mod_ctrls)) {
      mod_ctrls <- get_model_controls()
    }
  } else {
    mod_ctrls <- model_controls
  }
  
  if (!is.null(obs$lake)) {
    lake_obs <- obs$lake
    # Initial variables
    init_vars <- c(
      "HYD_temp", 
      "CHM_oxy", "CHM_salt",
      "PHS_frp", "PHS_dop", "PHS_dopr", "PHS_pop", "PHS_popr", 
      "PHS_pip", "PHS_tp",
      "NIT_amm", "NIT_nit", "NIT_don", "NIT_donr", "NIT_pon", 
      "NIT_ponr", "NIT_pin", "NIT_tn"
    )
    summ <- lake_obs |> 
      dplyr::mutate(month = as.numeric(format(as.Date(Date), "%m")),
                    depth_mid = (depth_from + depth_to) / 2) |>
      dplyr::filter(var_aeme %in% init_vars,
                    month %in% month_range
      ) |> 
      dplyr::group_by(var_aeme) |>
      dplyr::summarise(mean = mean(value), median = round(median(value), 3),
                       min = min(value), max = max(value),
                       n = dplyr::n(),
                       n_depths = length(unique(depth_mid)),
                       min_depth = min(depth_mid),
                       max_depth = max(depth_mid)) 
    for (v in summ$var_aeme) {
      new_val <- summ$median[summ$var_aeme == v]
      old_val <- mod_ctrls$initial_wc[mod_ctrls$var_aeme == v]
      mod_ctrls$initial_wc[mod_ctrls$var_aeme == v] <- new_val
      message(paste0("Setting initial value for ", v, " from ", old_val,
                     " to ", new_val))
    }
    depths <- seq(from = 0, to = init_depth, length.out = 10) |> 
      round(digits = 2)
    init_vars <- c("HYD_temp", "CHM_salt")
    init_values <- lapply(init_vars, \(v) {
      if (v %in% summ$var_aeme) {
        temp_profile <- lake_obs |>
          dplyr::mutate(month = as.numeric(format(as.Date(Date), "%m")),
                        depth_mid = (depth_from + depth_to) / 2) |> 
          dplyr::filter(var_aeme == v,
                        month %in% month_range) |>
          dplyr::group_by(depth_mid) |>
          dplyr::summarise(median = median(value)) |>
          dplyr::arrange(depth_mid) |> 
          dplyr::filter(depth_mid <= init_depth)
        lm <- lm(median ~ depth_mid, data = temp_profile)
        new_vals <- predict(lm, newdata = data.frame(depth_mid = depths)) |> 
          round(digits = 2)
        # Ensure temperature is montonically decreasing with depth
        for (i in 2:length(new_vals)) {
          if (new_vals[i] > new_vals[i - 1]) {
            new_vals[i] <- new_vals[i - 1]
          }
        }
      } else {
        new_vals <- rep(mod_ctrls$initial_wc[mod_ctrls$var_aeme == v],
                        length(depths))
      }
      return(new_vals)
    })
    names(init_values) <- init_vars
    init_profile <- data.frame(
      depth = depths,
      temperature = init_values[["HYD_temp"]],
      salt = init_values[["CHM_salt"]]
    )
    message("Updating initial profile in AEME input.")
    inp$init_profile <- init_profile
    input(aeme) <- inp
  } else {
    warning("No lake observations found in AEME object.")
  }
  
  if (is.null(model_controls)) {
    aeme <- set_model_controls(aeme = aeme, model_controls = mod_ctrls)
  } 
  return(aeme)
}
