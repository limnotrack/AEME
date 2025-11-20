#' Calculate the derivatives of the lake observations
#'
#' @param obs list of observations extracted from the AEME object using
#' \code{\link{observations}}
#'
#' @importFrom rLakeAnalyzer thermo.depth center.buoyancy meta.depths
#' schmidt.stability
#' @importFrom dplyr filter mutate bind_rows
#' @importFrom tidyr pivot_wider
#'
#' @return aeme object with the derivatives of the lake observations added
#' @noRd
#'

calc_lake_obs_deriv <- function(aeme) {
  
  # Load Rdata
  data("key_naming", package = "AEME", envir = environment())
  deriv_vars <- key_naming |>
    dplyr::filter(derived) |>
    dplyr::pull(name)
  
  vars <- c("HYD_temp", "CHM_oxy")
  
  out_list <- list()
  
  lke <- lake(aeme)
  inp <- input(aeme)
  bathy <- inp$hypsograph |>
    dplyr::filter(depth <= 0)
  bathy$depth <- max(bathy$elev) - bathy$elev
  max_dep <- max(bathy$depth)
  
  if (lke$depth < 10) {
    z_step <- 0.2
  } else {
    z_step <- 0.5
  }
  
  obs <- observations(aeme)
  deriv_chk <- data.frame(aeme_var = deriv_vars,
                          present = deriv_vars %in% obs$lake$var_aeme,
                          group = sapply(deriv_vars, \(x) strsplit(x, "_")[[1]][1]))
  
  # Temperature derivatives
  if ("HYD_temp" %in% obs$lake$var_aeme) {
    wtr <- obs$lake |>
      dplyr::filter(var_aeme == "HYD_temp") |>
      # Create depth_mid column with "wtr_" prefix and 3 digits before the decimal and one after with preleading zero
      dplyr::mutate(depth_mid = paste0("wtr_",
                                       formatC((depth_from + depth_to) / 2,
                                               width = 5, format = "f",
                                               digits = 1, flag = 0))) |>
      # dplyr::mutate(depth_mid = (depth_from + depth_to) / 2) |>
      #Create a depth label with 3
      tidyr::pivot_wider(names_from = depth_mid, values_from = value,
                         id_cols = Date)
    
    # Re-order cols in ascending order
    numeric_part <- as.numeric(gsub("wtr_", "", names(wtr)[-1]))
    ord_cols <- names(wtr)[c(1 + order(numeric_part))]
    depths <- numeric_part[order(numeric_part)]
    Date <- wtr[["Date"]]
    wtr <- wtr[ord_cols] |> as.matrix()
    
    
    fun_list <- list(HYD_thmcln = rLakeAnalyzer::thermo.depth,
                     HYD_strat = is_strat,
                     HYD_ctrbuy = rLakeAnalyzer::center.buoyancy,
                     HYD_epidep = rLakeAnalyzer::meta.depths,
                     HYD_hypdep = rLakeAnalyzer::meta.depths)
    
    wtr_list <- lapply(names(fun_list), \(f) {
      
      if (deriv_chk$present[deriv_chk$aeme_var == f]) return()
      
      idx <- ifelse(f == "HYD_hypdep", 2, 1)
      vec <- vapply(1:nrow(wtr), \(c) {
        idx2 <- which(!is.na(wtr[c, ]))
        if (length(idx2) <= 1) return(NA)
        v <- fun_list[[f]](wtr = wtr[c, idx2], depths = depths[idx2])
        v[is.nan(v)] <- NA
        v <- ifelse(f == "HYD_thmcln" & is.na(v), max_dep, v)
        v[idx]
      }, numeric(1))
      data.frame(Date = Date, var_aeme = f, value = vec)
    })
    names(wtr_list) <- names(fun_list)
    bthD <- rev(bathy$depth)
    bthA <- (bathy$area)
    
    if (!deriv_chk$present[deriv_chk$aeme_var == "HYD_schstb"]) {
      schstb <- vapply(1:nrow(wtr), \(c) {
        idx2 <- which(!is.na(wtr[c, ]))
        if (length(idx2) <= 1) return(NA)
        v <- rLakeAnalyzer::schmidt.stability(wtr = wtr[c, idx2],
                                              depths = depths[idx2],
                                              bthA = bthA, bthD = bthD)
        v[is.nan(v)] <- NA
        v
      }, numeric(1))
      wtr_list[["HYD_schstb"]] <- data.frame(Date = Date,
                                             var_aeme = "HYD_schstb",
                                             value = schstb)
    }
    
    for (n in names(wtr_list)) {
      out_list[[n]] <- wtr_list[[n]]
    }
  }
  
  # Oxygen derivatives
  if ("CHM_oxysat" %in% obs$lake$var_aeme & "HYD_temp" %in% obs$lake$var_aeme &
      any(!deriv_chk$present[deriv_chk$group == "CHM"])) {
    
    # Calculate CHM_oxy from sat if not present
    oxy_sat <- obs$lake |>
      dplyr::filter(var_aeme == "CHM_oxysat") |> 
      dplyr::mutate(depth_mid = format_depth_label(depth_from = depth_from,
                                                   depth_to = depth_to))
    if ("HYD_temp" %in% obs$lake$var_aeme) {
      inp <- input(aeme)
      met <- inp$meteo
      baro <- met |> 
        dplyr::select(Date, MET_prsttn) |> 
        dplyr::rename(baro = MET_prsttn) |> 
        dplyr::mutate(baro = baro / 100) # Convert from Pa to mbar
      temp <- obs$lake |>
        dplyr::filter(var_aeme == "HYD_temp") |> 
        dplyr::mutate(depth_mid = format_depth_label(depth_from = depth_from,
                                                     depth_to = depth_to)) |> 
        dplyr::select(Date, depth_mid, value) |> 
        dplyr::rename(temp = value)
      
      oxy_mgl <- oxy_sat |>
        dplyr::left_join(temp, by = c("Date", "depth_mid")) |>
        dplyr::left_join(baro, by = "Date") |> 
        dplyr::mutate(
          depth = (depth_from + depth_to) / 2,
          do_sat = convert_do(value = value, temp = temp, depth = depth, 
                              baro = baro, direction = "to_mgL"),
          var_aeme = "CHM_oxy"
        ) |> 
        dplyr::select(Date, var_aeme, value = do_sat, depth_from, depth_to,
                      depth_mid)
      if ("CHM_oxy" %in% obs$lake$var_aeme) {
        # Check if already exists and only add new values
        existing_oxy <- obs$lake |>
          dplyr::filter(var_aeme == "CHM_oxy") |> 
          dplyr::mutate(depth_mid = format_depth_label(depth_from = depth_from,
                                                       depth_to = depth_to))
        
        oxy_mgl <- oxy_mgl |>
          dplyr::anti_join(existing_oxy, by = c("Date", "depth_mid"))
      }
      out_list[["CHM_oxy"]] <- oxy_mgl
    }
  }
  if ("CHM_oxy" %in% obs$lake$var_aeme & "HYD_temp" %in% obs$lake$var_aeme &
      any(!deriv_chk$present[deriv_chk$group == "CHM"])) {
    
    oxy <- obs$lake |>
      dplyr::filter(var_aeme == "CHM_oxy") |> 
      dplyr::mutate(depth_mid = format_depth_label(depth_from = depth_from,
                                                   depth_to = depth_to)) 
    
    if ("HYD_temp" %in% obs$lake$var_aeme) {
      inp <- input(aeme)
      met <- inp$meteo
      baro <- met |> 
        dplyr::select(Date, MET_prsttn) |> 
        dplyr::rename(baro = MET_prsttn) |> 
        dplyr::mutate(baro = baro / 100) # Convert from Pa to mbar
      temp <- obs$lake |>
        dplyr::filter(var_aeme == "HYD_temp") |> 
        dplyr::mutate(depth_mid = format_depth_label(depth_from = depth_from,
                                                     depth_to = depth_to)) |> 
        dplyr::select(Date, depth_mid, value) |> 
        dplyr::rename(temp = value)
      
      oxy_sat <- oxy |>
        dplyr::left_join(temp, by = c("Date", "depth_mid")) |>
        dplyr::left_join(baro, by = "Date") |> 
        dplyr::mutate(
          depth = (depth_from + depth_to) / 2,
          do_sat = convert_do(value = value, temp = temp, depth = depth, 
                              baro = baro, direction = "to_percent"),
          var_aeme = "CHM_oxysat"
        ) |> 
        dplyr::select(Date, var_aeme, value = do_sat, depth_from, depth_to)
      # head(oxy_sat)
      out_list[["CHM_oxysat"]] <- oxy_sat
      
      
    }
    
    oxy_wid <- oxy |>
      tidyr::pivot_wider(names_from = depth_mid, values_from = value,
                         id_cols = Date)
    
    # Re-order cols in ascending order
    numeric_part <- as.numeric(gsub("wtr_", "", names(oxy_wid)[-1]))
    ord_cols <- names(oxy_wid)[c(1 + order(numeric_part))]
    depths <- numeric_part[order(numeric_part)]
    Date <- oxy_wid[["Date"]]
    oxy_wid <- oxy_wid[ord_cols] |> as.matrix()
    
    # Oxycline ----
    oxy_cline <- vapply(1:nrow(oxy_wid), \(c) {
      idx2 <- which(!is.na(oxy_wid[c, ]))
      v <- cline_depth(wtr = oxy_wid[c, idx2], depths = depths[idx2],
                       water = FALSE)
      v
    }, numeric(1))
    # Epilimnion oxygen ----
    epi_oxy <- vapply(1:nrow(oxy_wid), \(c) {
      idx <- which(wtr_list$HYD_epidep$Date %in% Date[c])
      if (length(idx) != 1) return(NA)
      idx2 <- which(depths <= wtr_list$HYD_epidep$value[idx])
      v <- mean(oxy_wid[c, idx2])
      v[is.nan(v)] <- NA
      v
    }, numeric(1))
    
    # Hypolimnetic oxygen ----
    hyp_oxy <- vapply(1:nrow(oxy_wid), \(c) {
      idx <- which(wtr_list$HYD_hypdep$Date %in% Date[c])
      if (length(idx) != 1) return(NA)
      idx2 <- which(depths >= wtr_list$HYD_hypdep$value[idx])
      v <- mean(oxy_wid[c, idx2], na.rm = TRUE)
      v[is.nan(v)] <- NA
      v
    }, numeric(1))
    
    # Metalimnion oxygen ----
    meta_oxy <- vapply(1:nrow(oxy_wid), \(c) {
      idx <- which(wtr_list$HYD_hypdep$Date %in% Date[c])
      if (length(idx) != 1) return(NA)
      idx2 <- which(depths > wtr_list$HYD_epidep$value[idx] &
                      depths < wtr_list$HYD_hypdep$value[idx])
      v <- mean(oxy_wid[c, idx2], na.rm = TRUE)
      v[is.nan(v)] <- NA
      v
    }, numeric(1))
    
    exp_oxy <- (epi_oxy + hyp_oxy) / 2
    oxymom <- meta_oxy - exp_oxy
    
    oxynal <- vapply(1:nrow(oxy_wid), \(c) {
      idx2 <- which(!is.na(oxy_wid[c, ]))
      if (length(idx2) <= 1) return(NA)
      
      oxy_layers <- approx(y = oxy_wid[c, ], x = depths,
                           xout = seq(0, lke$depth, by = z_step), rule = 2)$y
      sum(oxy_layers < 1)
    }, numeric(1))
    
    oxy_list <- list(
      CHM_oxycln = data.frame(Date = Date, var_aeme = "CHM_oxycln",
                              value = oxy_cline),
      CHM_oxyepi = data.frame(Date = Date, var_aeme = "CHM_oxyepi",
                              value = epi_oxy),
      CHM_oxyepi = data.frame(Date = Date, var_aeme = "CHM_oxyhyp",
                              value = hyp_oxy),
      CHM_oxymet = data.frame(Date = Date, var_aeme = "CHM_oxymet",
                              value = meta_oxy),
      CHM_oxymom = data.frame(Date = Date, var_aeme = "CHM_oxymom",
                              value = oxymom),
      CHM_oxynal = data.frame(Date = Date, var_aeme = "CHM_oxynal",
                              value = oxynal)
    )
    
    for (n in names(oxy_list)) {
      out_list[[n]] <- oxy_list[[n]]
    }
  }
  
  # TLI calculation
  tli_vars <- c("PHS_tp", "NIT_tn", "PHY_tchla", "RAD_secchi")
  if (all(tli_vars %in% obs$lake$var_aeme) &
      !any(deriv_chk$present[deriv_chk$group == "LKE"])) {
    
    # GEt epi depths
    if ("HYD_epidep" %in% names(out_list)) {
      epi_dep <- out_list[["HYD_epidep"]] 
    } else {
      epi_dep <- obs$lake |>
        dplyr::filter(var_aeme == "HYD_epidep") 
    }
    epi_dep <- epi_dep |> 
      dplyr::select(Date, value) |>
      dplyr::rename(epi_dep = value) |> 
      dplyr::mutate(epi_dep = ifelse(is.na(epi_dep) | epi_dep > lke$depth,
                                     lke$depth, epi_dep))
    
    # Subset obs to where all variables are present on the same Date
    tli <- obs$lake |>
      # dplyr::filter(!var_aeme %in% c("RAD_secchi")) |>
      dplyr::filter(var_aeme %in% tli_vars) |>
      dplyr::mutate(depth_mid = (depth_from + depth_to) / 2) |> 
      dplyr::left_join(epi_dep, by = "Date") |>
      dplyr::filter(depth_mid <= epi_dep) |>
      dplyr::group_by(Date, var_aeme) |>
      dplyr::summarise(value = mean(value), .groups = "drop") |>
      tidyr::pivot_wider(names_from = var_aeme, values_from = value,
                         id_cols = Date)
    
    if (all(c("PHS_tp", "NIT_tn", "PHY_tchla") %in% colnames(tli))) {
      tli <- tli |>
        dplyr::filter(!is.na(PHS_tp) & !is.na(NIT_tn) & !is.na(PHY_tchla)) |>
        dplyr::mutate(
          LKE_tli3 = calc_tli3(chla = PHY_tchla,
                               tn = NIT_tn,
                               tp = PHS_tp),
          LKE_tli4 = calc_tli4(chla = PHY_tchla,
                               tn = NIT_tn,
                               tp = PHS_tp,
                               secchi = RAD_secchi)
        ) |>
        tidyr::pivot_longer(cols = c(LKE_tli3, LKE_tli4), names_to = "var_aeme",
          values_to = "value") |>
        dplyr::filter(!is.na(value)) |>
        dplyr::select(Date, var_aeme, value) |>
        as.data.frame()
    }
    tli_list <- list(tli_vars = tli)
    
    for (n in names(tli_list)) {
      out_list[[n]] <- tli_list[[n]]
    }
  }
  
  
  if (length(out_list) > 0) {
    out_df <- out_list |>
      dplyr::bind_rows() |>
      dplyr::filter(!is.na(value)) |>
      # dplyr::mutate(lake = lke$name, lake_id = lke$name_id) |> 
      dplyr::select(Date, var_aeme, depth_from, depth_to, value)
    
    obs$lake <- obs$lake |>
      dplyr::bind_rows(out_df)
    
  }
  
  observations(aeme) <- obs
  
  
  return(aeme)
}

#' Helper function to format depth labels
#' @noRd
format_depth_label <- function(stem = "wtr", depth_from, depth_to) {
  paste0(stem, "_",
         formatC((depth_from + depth_to) / 2,
                 width = 5, format = "f",
                 digits = 1, flag = 0))
}
