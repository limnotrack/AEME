#' Convert AEME model outputs to inflow format
#'
#' @inheritParams build_aeme
#'
#' @returns List of data frames with inflow variables for each model
#' @export
#'
#' @importFrom dplyr select mutate rename bind_rows
#' @importFrom tidyr pivot_wider
#'

aeme_to_inflow <- function(aeme) {
  aeme <- check_aeme(aeme)
  model <- list_models(aeme = aeme)
  tme <- time(aeme)
  spin_up <- lapply(model, \(m) {
    list(start = lubridate::as_date(tme$start - 
                                      lubridate::ddays(tme$spin_up[[m]])),
         end = lubridate::as_date(tme$stop)
         )
  })
  names(spin_up) <- model
  outflow_factor <- rep(1, length(model))
  names(outflow_factor) <- model
  
  model_parameters <- parameters(aeme)
  if (!is.null(model_parameters)) {
    # Get outflow factor if present
    outflow_fct <- model_parameters |> 
      dplyr::filter(name == "outflow", file == "wdr") |> 
      dplyr::select(model, value) 
    for (m in model) {
      if (m %in% outflow_fct$model) {
        outflow_factor[m] <- outflow_fct |> 
          dplyr::filter(model == m) |> 
          dplyr::pull(value)
      }
    }
  }
  
  inflow_vars <- c(
    "LKE_outftot",
    "HYD_temp", "CHM_salt", "CHM_oxy",
    "NIT_tn", "NIT_nit", "NIT_amm", #"NIT_din",
    "PHS_tp", "PHS_frp",# "NCS_tss", "NCS_ss1",
    "NIT_don", "NIT_pon", "PHS_dop", "PHS_pop", "PHS_pip",
    "CAR_doc", "CAR_poc"
  )
  var_check <- check_var_in_output(aeme = aeme, model = model, 
                                   var_sim = inflow_vars)
  # AED mass units (mmol/m3) -> AEME units, for the GLM mass balance
  model_controls <- get_model_controls(aeme)
  if (is.null(model_controls)) model_controls <- get_model_controls()
  conversion <- stats::setNames(model_controls$conversion_aed,
                                model_controls$var_aeme)
  
  out <- lapply(model, \(m) {
    # GLM-AED: the mass balance holds the mass that actually left the lake, so
    # it gives the flow-weighted outflow concentration (see below)
    mb <- NULL
    if (m == "glm_aed") {
      # same time axis as the netCDF outflow volume
      mb <- tryCatch(
        glm_mb_to_output_time(read_glm_mass_balance(aeme = aeme)),
        error = function(e) NULL)
    }
    df <- lapply(inflow_vars, \(var) {
      
      if (var_check[[m]][var] == FALSE) {
        return(NULL)
      }
      dat <- get_var(aeme = aeme, model = m, var_sim = var, depth = 0.25,
                     remove_spin_up = FALSE)
      mean_value <- dat |>
        dplyr::filter(Date >= lubridate::as_date(tme$start) &
                        Date < lubridate::as_date(tme$start) + 3) |>
        dplyr::pull(value) |> 
        mean(na.rm = TRUE)
      dat_missing <- data.frame(Date = (dat$Date[1] - lubridate::ddays(1)),
                                var_sim = var,
                                value = mean_value)
      dat <- dplyr::bind_rows(dat_missing, dat)
      if (var == "LKE_outftot") {
        dat <- dat |> 
          dplyr::mutate(value = value * outflow_factor[m])
      }
      if (all(is.na(dat$value)) | all(dat$value < 0)) {
        return(NULL)
      } else {
        return(dat)
      }
    }) |> 
      dplyr::bind_rows()
    if (nrow(df) == 0) {
      return(NULL)
    }
    if (!is.null(mb)) {
      flow <- df |>
        dplyr::filter(var_sim == "LKE_outftot") |>
        dplyr::mutate(flow = value / outflow_factor[m]) |>
        dplyr::select(Date, flow)
      mb_conc <- glm_mass_balance_outflow_conc(
        mb = mb, flow = flow, var_sim = setdiff(inflow_vars, "LKE_outftot"),
        path_glm = file.path(get_lake_dir(aeme), "glm_aed"),
        conversion = conversion)
      df <- replace_inflow_values(df, mb_conc)
    }
    df <- df |>
      dplyr::select(Date, var_sim, value)
    df_wid <- df |> 
      tidyr::pivot_wider(names_from = var_sim, values_from = value) |> 
      dplyr::rename(HYD_flow = LKE_outftot) |>
      dplyr::mutate(model = m)
    return(df_wid)
  }) |> 
    dplyr::bind_rows()
  return(out)
}
#' Flow-weighted outflow concentration from the GLM-AED mass balance
#'
#' The `Out` term of `mass_balance.csv` is the mass (mmol) that left the lake
#' over each save interval, in all outflows. Dividing the per-day mass by the
#' outflow volume per day (m3/d) gives the concentration (mmol/m3) of the water
#' leaving the lake, which is what an inflow built from this lake should
#' carry. State variables are matched by their GLM name; `NIT_tn` and `PHS_tp`
#' are the summed TN and TP totals. Variables not in the mass balance (see
#' the groups chosen with [set_glm_balance_groups()]) or days without outflow are
#' absent from the result.
#'
#' @param mb tibble from [read_glm_mass_balance()].
#' @param flow data frame with `Date` and `flow` (outflow, m3/d, before any
#'   outflow factor).
#' @param var_sim character; AEME variable names wanted.
#' @param path_glm GLM-AED model directory, used for the TN/TP components.
#' @param conversion named numeric (`conversion_aed`, by AEME variable name) to
#'   convert mmol/m3 to AEME units (`aeme = glm * conversion_aed`); variables
#'   without an entry are left in mmol/m3.
#' @return data frame with `Date`, `var_sim` and `value`.
#' @noRd
glm_mass_balance_outflow_conc <- function(mb, flow, var_sim, path_glm = NULL,
                                          conversion = NULL) {
  out <- mb[mb$term == "Out", ]
  if (nrow(out) == 0) return(NULL)

  # AEME names whose GLM counterpart is a state in the mass balance
  glm_names <- rename_modelvars(var_sim, type_output = "glm_aed",
                                passthrough = TRUE)
  direct <- tibble::tibble(var_sim = var_sim, var = glm_names) |>
    dplyr::filter(var %in% out$var)
  mass <- dplyr::inner_join(out, direct, by = "var",
                            relationship = "many-to-many") |>
    dplyr::select(datetime, var_sim, value)

  # TN / TP totals
  totals <- tryCatch(
    suppressWarnings(calc_aed_totals_balance(mb, path_glm = path_glm)),
    error = function(e) NULL)
  if (!is.null(totals)) {
    totals <- totals |>
      dplyr::filter(term == "Out",
                    total %in% c("TN", "TP")) |>
      dplyr::mutate(var_sim = c(TN = "NIT_tn", TP = "PHS_tp")[total]) |>
      dplyr::filter(var_sim %in% !!var_sim) |>
      dplyr::select(datetime, var_sim, value)
    mass <- dplyr::bind_rows(mass, totals)
  }
  if (nrow(mass) == 0) return(NULL)

  # per-day mass: rows are totals over the save interval
  step <- stats::median(as.numeric(diff(sort(unique(mass$datetime))),
                                   units = "days"))
  mass |>
    dplyr::arrange(var_sim, datetime) |>
    dplyr::group_by(var_sim) |>
    dplyr::mutate(days = dplyr::coalesce(
      as.numeric(datetime - dplyr::lag(datetime), units = "days"), step)) |>
    dplyr::ungroup() |>
    dplyr::mutate(Date = lubridate::as_date(datetime),
                  mass_d = value / days) |>
    dplyr::inner_join(flow, by = "Date") |>
    dplyr::filter(is.finite(flow), flow > 0, is.finite(mass_d)) |>
    dplyr::mutate(value = mass_d / flow) |>
    dplyr::mutate(value = value * dplyr::coalesce(
      unname(conversion[var_sim]), 1)) |>
    dplyr::select(Date, var_sim, value)
}

#' Replace or add values in a long inflow table
#'
#' @param df data frame with `Date`, `var_sim`, `value`.
#' @param new data frame with the same columns; its rows replace matching rows
#'   of `df` (same `Date` and `var_sim`) and are added when there is no match.
#' @return `df` with `new` merged in.
#' @noRd
replace_inflow_values <- function(df, new) {
  if (is.null(new) || nrow(new) == 0) return(df)
  keep <- dplyr::anti_join(df, new[, c("Date", "var_sim")],
                           by = c("Date", "var_sim"))
  dplyr::bind_rows(keep, new[, c("Date", "var_sim", "value")]) |>
    dplyr::arrange(var_sim, Date)
}

