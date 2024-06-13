#' Convert netCDF output to a standardised list
#'
#' @param nc file connection; to a netCDF file
#' @inheritParams delangrangify
#' @inheritParams build_aeme
#' @param model character; model name
#' @param vars_sim vector; of variables to extract
#' @param nlev numeric; number of vertical levels
#' @param remove_spin_up logical; whether to remove spin-up period. Default is
#'  \code{TRUE}
#'
#' @return list of data.frames for each model containing the variables
#' specified in \code{vars_sim}
#' @noRd
#'
#' @importFrom ncdf4 nc_open nc_close ncvar_get ncatt_get
#' @importFrom utils data
#' @importFrom dplyr filter mutate
#' @importFrom withr local_locale local_timezone
#' @importFrom lubridate hour
#' @importFrom rLakeAnalyzer thermo.depth center.buoyancy meta.depths
#'

nc_listify <- function(nc, model, vars_sim, nlev, aeme,
                       remove_spin_up = TRUE, output_hour, path,
                       lake_analyzer = TRUE) {

  # Load Rdata
  utils::data("key_naming", package = "AEME", envir = environment())

  # Set timezone temporarily to UTC
  withr::local_locale(c("LC_TIME" = "C"))
  withr::local_timezone("UTC")

  # reduce the key to the simvars
  key_naming <- key_naming |>
    dplyr::filter(name %in% vars_sim) |>
    dplyr::mutate(conversion_aed = as.numeric(conversion_aed))

  aeme_time <- time(aeme)


  # find the simvars for this model
  if (model == "gotm_wet") {
    ## dates.. gotm seems to output the intial profiles, then every tstep
    out.steps <- ncdf4::ncvar_get(nc, "time")
    if (length(out.steps) == 0) {
      return(NULL)
    }
    date.start <- ncdf4::ncatt_get(nc, "time", "units")$value |>
      gsub("seconds since ", "", x = _) |>
      as.POSIXct()
    time_vec <- ncdf4::ncvar_get(nc, "time")
    dates <- as.POSIXct(time_vec + date.start)
    idx <- which(lubridate::hour(dates) == output_hour)
    if (length(idx) == 0) stop("No output for GOTM at ", output_hour, " hour")
    dates <- dates[idx] |> as.Date()

    # dates <- seq.Date(date.start, by = 1, length.out = length(out.steps))
    lyr_h <- ncdf4::ncvar_get(nc, "h")[, idx] # lyrs
    zeta <- ncdf4::ncvar_get(nc, "zeta")[idx]
    zi <- ncdf4::ncvar_get(nc, "zi")[, idx]
    z <- ncdf4::ncvar_get(nc, "z")[, idx]

    # Light
    rad <- ncdf4::ncvar_get(nc, "rad")[, idx]
    efold <- sapply(seq_len(ncol(rad)), \(t) {
      if (t == 1) return(0) # Day 1 is always 0
      if (sum(!is.na(rad[, t])) < 2) return(NA)
      zeta[t] - approx(rad[, t], zi[, t], xout = (1/exp(1) * rad[nrow(rad), t]))$y
    })
    euphotic <- sapply(seq_len(ncol(rad)), \(t) {
      if (t == 1) return(0) # Day 1 is always 0
      if (sum(!is.na(rad[, t])) < 2) return(NA)
      zeta[t] - approx(rad[, t], zi[, t], xout = (0.01 * rad[nrow(rad), t]))$y
    })

    sst <- ncdf4::ncvar_get(nc, "sst")[idx]
    if (sum(sst == 0) > 1 | sum(is.na(sst)) > 0) {
      # Run-length encoding of the vector
      sst[is.na(sst)] <- -999
      rle_result <- rle(as.vector(sst))
      start_index <- which(rle_result$lengths > 1)[1]
      if (is.na(start_index)) {
        warning(strwrap(paste0("There are ", sum(sst == 0),
                               " SST values of 0 in the GOTM output. Not
                               removing any output.")))
      } else {
        # vals <- rle_result$values[start_index]
        z[, start_index:ncol(z)] <- NA
        zi[, start_index:ncol(zi)] <- NA
        lyr_h[, start_index:ncol(lyr_h)] <- NA
      }
    }
    depth <- zi[nrow(zi), ] - zi[1, ]
    depth[depth <= 0] <- 0
    zeta[depth <= 0] <- NA
    # depth <- data.frame(ncdf4::ncvar_get(nc, "z"))
    lyrs <- z # lyrs
    lyrs[1, ] <- lyrs[1, ] - (lyr_h[1, ] / 2)
    lyrs[nrow(lyrs), ] <- lyrs[nrow(lyrs), ] + (lyr_h[nrow(lyr_h), ] / 2)
    lyrs <- apply(lyrs, 2, \(x) x + abs(min(x)))
    vars_sim.model <- key_naming[[model]]

    # GOTM - Daily averaged variables ----
    lke <- lake(aeme)
    lake_dir <- file.path(path, paste0(lke$id, "_", tolower(lke$name)))
    out_file <- file.path(lake_dir, model, "output", "output_daily.nc")

    if (!file.exists(out_file)) {
      message("No ", out_file, " present.")
      return(NULL)
    }

    nc_daily <- ncdf4::nc_open(out_file, return_on_error = TRUE)
    on.exit(ncdf4::nc_close(nc_daily))
    if (nc_daily$error) stop("Could not open netCDF file: ", out_file)

    V <- ncdf4::ncvar_get(nc_daily, "int_water_balance")
    Qe <- ncdf4::ncvar_get(nc_daily, "qe")
    Qh <- ncdf4::ncvar_get(nc_daily, "qh")
    Qlw <- ncdf4::ncvar_get(nc_daily, "ql")
    Qsw <- ncdf4::ncvar_get(nc_daily, "I_0")
    evap_flux <- abs(ncdf4::ncvar_get(nc_daily, "evap"))
    EVAP <- evap_flux * 86400 # m/s -> m/day
    A0 <- ncdf4::ncvar_get(nc_daily, "Af") |>
      apply(2, max)
    evap_vol <- EVAP * A0

    flow_vars <- names(nc_daily$var)[grepl("Q_", names(nc_daily$var))]
    inflow_vars <- flow_vars[!grepl("outflow|wbal", flow_vars)]
    outflow_vars <- flow_vars[grepl("outflow|wbal", flow_vars)]
    if (length(inflow_vars) >= 1) {
      inflow <- sapply(seq_along(inflow_vars), \(x) {
        (ncdf4::ncvar_get(nc_daily, inflow_vars[x]) * 86400) # / A0
      }) |>
        apply(1, sum)
    } else {
      inflow <- A0 * 0
    }
    if (length(outflow_vars) >= 1) {
      outflow <- sapply(seq_along(outflow_vars), \(x) {
        -1 * (ncdf4::ncvar_get(nc_daily, outflow_vars[x]) * 86400) # / A0
      }) |>
        apply(1, sum)
    } else {
      outflow <- A0 * 0
    }
    precip <- ncdf4::ncvar_get(nc_daily, "precip") * 86400
    precip_vol <- precip * A0
    Ts <- ncdf4::ncvar_get(nc_daily, "temp")
    Ts <- Ts[nrow(Ts), ]
    MET_tmpair <- ncdf4::ncvar_get(nc_daily, "airt")

  } else if (model == 'glm_aed') {

    # glm DOES NOT output initial profiles
    hours.since  <- ncdf4::ncvar_get(nc, "time")
    if (length(hours.since) == 0) {
      return(NULL)
    }
    date.start <- as.POSIXct(gsub("hours since ", "",
                                  ncdf4::ncatt_get(nc,'time','units')$value))
    dates <- as.POSIXct(hours.since * 3600 + date.start) #|>
    # as.Date()
    idx <- which(lubridate::hour(dates) == output_hour)
    if (length(idx) == 0) stop("No output for GLM at ", output_hour, " hour")
    dates <- dates[idx] |> as.Date()

    # Get air temperature for GLM
    inp <- input(aeme)
    MET_tmpair <- inp$meteo |>
      dplyr::mutate(Date = Date + 1) |> # GLM is 1 day behind
      dplyr::filter(Date %in% dates) |>
      dplyr::select(Date, MET_tmpair) |>
      dplyr::pull(MET_tmpair)

    vars_sim.model <- key_naming[["glm_aed"]]

    mod_layers <- ncdf4::ncvar_get(nc, "z")[, idx]
    mod_layers[mod_layers > 1000000] <- NA

    Qe <- ncdf4::ncvar_get(nc, "daily_qe")[idx]
    Qh <- ncdf4::ncvar_get(nc, "daily_qh")[idx]
    Qlw <- ncdf4::ncvar_get(nc, "daily_qlw")[idx]
    Qsw <- ncdf4::ncvar_get(nc, "daily_qsw")[idx]
    V <- ncdf4::ncvar_get(nc, "lake_volume")[idx]
    depth <- ncdf4::ncvar_get(nc, "lake_level")[idx]
    evap_vol <- -ncdf4::ncvar_get(nc, "evaporation")[idx]
    evap_flux <- -ncdf4::ncvar_get(nc, "evap_mass_flux")[idx]
    A0 <- ncdf4::ncvar_get(nc, "surface_area")[idx]
    EVAP <- abs(evap_vol / A0)
    inflow <- ncdf4::ncvar_get(nc, "tot_inflow_vol")[idx] # / A0
    outflow <- (ncdf4::ncvar_get(nc, "tot_outflow_vol")[idx] +
                  ncdf4::ncvar_get(nc, "overflow_vol")[idx]) # / A0
    precip <- ncdf4::ncvar_get(nc, "precipitation")[idx]
    precip_vol <- precip * A0
    Ts <- ncdf4::ncvar_get(nc, "surface_temp")[idx]

} else if (model == 'dy_cd') {
  # dyresm outputs initial profiles as first col
  if (!("dyresmTime" %in% names(nc$var))) {
    return(NULL)
  }
  dates <- as.POSIXct((ncdf4::ncvar_get(nc, "dyresmTime") - 2415018.5) *
                        86400,
                      origin = "1899-12-30")
  idx <- which(lubridate::hour(dates) == output_hour)
  if (length(idx) == 0) stop("No output for DYRESM at ", output_hour, " hour")
  dates <- dates[idx] |> as.Date()

  # mod_layers are elevation from bottom, last row is bottom
  mod_layers <- ncdf4::ncvar_get(nc, "dyresmLAYER_HTS_Var")[, idx]
  depth <- apply(mod_layers, 2, \(x) max(x, na.rm = TRUE))
  vars_sim.model <- paste0('dyresm', key_naming[["dy_cd"]],'_Var')

  H <- ncdf4::ncvar_get(nc, "morph_HEIGHT")
  A <- ncdf4::ncvar_get(nc, "morph_AREA")
  elev <- ncdf4::ncvar_get(nc, "morph_ELEV")
  init_H <- ncdf4::ncvar_get(nc, "initprofHeight")[2] + min(H)

  MET_wndspd <- ncdf4::ncvar_get(nc, "met_Uwind")[idx]
  MET_prvapr <- ncdf4::ncvar_get(nc, "met_Pvapour")[idx]
  MET_tmpair <- ncdf4::ncvar_get(nc, "met_Tair")[idx]
  Ts <- ncdf4::ncvar_get(nc, "dyresmTEMPTURE_Var")[, idx] |>
    apply(2, \(x) {
      x[!is.na(x)][1]
    })

  # Saturated vapour pressure - Magnus-Tetens formula (TVA 1972, eqn 4.1)
  es <- exp(2.3026 * (((7.5 * Ts) / (Ts + 237.3) + 0.7858)))
  #evaporative heat flux
  Qe <- ((0.622 / 981.9) *         #constant/mean station pressure
           0.0013 *               #latent heat transfer coefficient
           1.168 *                #density of air
           2453000 *              #latent heat of evaporation of water
           MET_wndspd *           #wind speed in m/s
           (MET_prvapr - es))
  Qe[Qe > 0] <- 0 # evaporation can't be negative

  # Conductive/sensible heat gain only affects the top layer.
  # Q_sensibleheat = -CH * rho_air * cp_air * WindSp * (Lake[surfLayer].Temp - MetData.AirTemp);
  # rho_air <- atm_density(MET_tmpair, 101325)
  Q_lw_in <- ncdf4::ncvar_get(nc, "met_LW_related")[idx]
  if (all(Q_lw_in <= 1)) {
    CloudCover <- Q_lw_in
    # eps_star <- (1.0 + 0.275 * CloudCover) * (1.0 - 0.261 * exp(-0.000777 * MET_tmpair^2.0))
    # Q_lw_in <- (1 - 0.03) * eps_star *
    #   5.678e-8  * # Stefan_Boltzman constant
    #   (273.15 + Ts)^4.0 # water surface temperature in Kelvin

    # DYRESM Science Manual
    Q_lw_in <- (1 - 0.03) * # albedo for long wave radiation, constant = 0.03 (Henderson-Sellers 1986).
      (1 + 0.17 * CloudCover^2) *
      (9.37e-6 * (MET_tmpair + 273.15)^2) * # Swinbank (1963)
      5.6697e-8 * # Stefan-Boltzmann constant
      (MET_tmpair + 273.15)^4

  }
  Qh <- 1.3e-3 * # sensible heat transfer coefficient for wind speed at 10 m reference height above the water surface
    1.168 * # density of air
    1003.0 *    # cp_air Specific heat of air
    MET_wndspd *
    (MET_tmpair - Ts)
  Q_lw_out <- -5.678e-8  * # Stefan_Boltzman constant
    0.985 * # emissivity of water
    (273.15 + Ts)^4.0 # water surface temperature in Kelvin
  Qlw <- Q_lw_out + Q_lw_in

  # Qlw <- NA
  Qsw <- ncdf4::ncvar_get(nc, "met_SW")[idx]
  EVAP <- ncdf4::ncvar_get(nc, "dyresmEVAP_DAILY_Var")[idx]

  inflow_vars <- names(nc$var)[grepl("stream", names(nc$var)) &
                                 grepl("VOL", names(nc$var)) ]
  if (length(inflow_vars) >= 1) {
    inflow <- sapply(seq_along(inflow_vars), \(x) {
      ncdf4::ncvar_get(nc, inflow_vars[x])[idx]
    }) |>
      apply(1, sum)
  } else {
    inflow <- Ts * 0
  }
  outflow_vars <- names(nc$var)[grepl("withdrawal", names(nc$var)) &
                                  grepl("VOL", names(nc$var))]
  if (length(outflow_vars) >= 1) {
    outflow <- sapply(seq_along(outflow_vars), \(x) {
      ncdf4::ncvar_get(nc, outflow_vars[x])[idx]
    }) |>
      apply(1, sum)
  } else {
    outflow <- Ts * 0
  }

  outflow <- outflow +
    ncdf4::ncvar_get(nc, "overflow_VOL_Var")[idx]
  precip <- ncdf4::ncvar_get(nc, "met_RAIN")[idx]

  A0 <- sapply(1:length(depth), function(d) approx((H - min(H)), A,
                                                   xout = depth[d])$y)
  dz <- 0.01
  V <- sapply(1:length(depth), function(d) {
    if(is.na(depth[d]) | is.infinite(depth[d])) return(NA)
    layerD <- seq(dz, (depth[d] - dz), dz)
    layerA <- stats::approx(H, A, layerD)$y
    sum((layerA) * dz)
  })
  # inflow <- inflow #/ A0
  # outflow <- outflow # / A0
  dates <- seq.Date(from = dates[1], by = 1, length.out = length(dates))
  evap_flux <- EVAP / 86400
  evap_vol <- EVAP * A0
  precip_vol <- precip * A0

  # Light
  efold <- rep(NA, length(idx))
  euphotic <- rep(NA, length(idx))

}

  # format the mod_layers and add as first list item (common to all three models)
  # h.surf <- map_df(mod_layers, max, na.rm = T) |> as.numeric()
  h.surf <- depth # |> as.numeric()
  h.surf[is.infinite(h.surf)] <- NA
  h.surf[depth == 0] <- 0
  each.layer <- h.surf / nlev
  layers <- mapply(each.layer, FUN = \(x) rep(x, times = nlev)) |>
    apply(2, FUN = cumsum)

  depths <- apply(layers, 2, function(x) x - max(x, na.rm = TRUE)) |>
    abs()

  if (model == "glm_aed") {
    # Light
    rad <- ncdf4::ncvar_get(nc, "radn")[, idx]
    rad <- regularise_model_output(depth = depth, depths = mod_layers,
                                   var = rad, nlev = nlev)
    suppressWarnings({
      efold <- sapply(seq_len(ncol(rad)), \(t) {
        if (sum(!is.na(rad[, t])) < 2 | length(unique(depths[, t])) <= 1 |
            length(unique(rad[, t])) <= 1) {
          return(NA)
        }
        approx(rad[, t], depths[, t], xout = (1/exp(1) * rad[nrow(rad), t]))$y
      })
      euphotic <- sapply(seq_len(ncol(rad)), \(t) {
        if (sum(!is.na(rad[, t])) < 2 | length(unique(depths[, t])) <= 1 |
            length(unique(rad[, t])) <= 1) {
          return(NA)
        }
        approx(rad[, t], depths[, t], xout = (0.01 * rad[nrow(rad), t]))$y
      })
    })
  }

  dV <- c(0, diff(V))
  # net <- inflow + precip - outflow - EVAP
  # plot(inflow, type = "l", ylim = c(-0.2, 0.2))
  # lines(outflow, col = 2)
  # lines(precip, col = 3)
  # lines(EVAP, col = 4)
  # lines(net, col = 5)
  #
  # plot(cumsum(net))

  # Net fluxes ----
  Qnet <- Qe + Qh + Qlw + Qsw

  # Net water balance ----
  net_wb <- inflow + precip_vol - outflow - evap_vol

  # Correct light for plotting over contour plots
  efold_h <- depth - abs(efold)
  euphotic_h <- depth - abs(euphotic)


  nc_list <- list(Date = dates,
                  LKE_lvlwtr = as.vector(depth),
                  LKE_V = as.vector(V),
                  LKE_dV = as.vector(dV),
                  LKE_A0 = as.vector(A0),
                  LKE_evprte = as.vector(EVAP),
                  LKE_evpflx = as.vector(evap_flux),
                  LKE_Qe = as.vector(Qe),
                  LKE_Qh = as.vector(Qh),
                  LKE_Qlw = as.vector(Qlw),
                  LKE_Qsw = as.vector(Qsw),
                  LKE_Qnet = as.vector(Qnet),
                  LKE_evpvol = as.vector(evap_vol),
                  LKE_precip = as.vector(precip),
                  LKE_pcpvol = as.vector(precip_vol),
                  LKE_inflow = as.vector(inflow),
                  LKE_outflow = as.vector(outflow),
                  LKE_netwbl = as.vector(net_wb),
                  LKE_layers = as.matrix(layers),
                  LKE_depths = as.matrix(depths),
                  LKE_efold = as.vector(abs(efold)),
                  LKE_efoldh = as.vector(abs(efold_h)),
                  LKE_photic = as.vector(abs(euphotic)),
                  LKE_photich = as.vector(abs(euphotic_h)),
                  HYD_Ts = as.vector(Ts),
                  MET_tmpair = as.vector(MET_tmpair),
                  LKE_Tdiff = as.vector(Ts - MET_tmpair)
  )

  if (model %in% c("dy_cd", "glm_aed")) {
    depths <- mod_layers
  }
  if (model == "gotm_wet") {
    depths <- vapply(seq_len(ncol(z)), \(i) {
      max(zi[, i]) - z[, i]
    }, FUN.VALUE = numeric(nlev))
  }

  ### Loop through the netcdf and make a list of the outputs
  vars_chk <- data.frame(vars = vars_sim.model, present = NA)
  for(v in seq_len(nrow(vars_chk))) {
    vars_chk[["present"]][v] <- vars_sim.model[v] %in% names(nc$var)
  }

  vars_list <- lapply(1:length(vars_sim.model), \(i) {

    if (is.na(vars_sim.model[i]) | !vars_chk[["present"]][i]) {

      this.var <- vars_sim[i]
      out <- nc_list[["LKE_layers"]]
      out[,1:ncol(out)] <- -99

    } else {
      message(paste0("Retrieving and formatting ", vars_sim.model[i],
                     " for model ", model))
      # get the table from the nc file
      this.var <- ncdf4::ncvar_get(nc, vars_sim.model[i])[, idx]

      # Reverse GOTM vars because depth is negative in GOTM
      if (model == "gotm_wet") {
        this.var <- this.var |>
          apply(2, rev)
      }

      if (all(is.na(as.vector(this.var)))) {
        out <- nc_list[["LKE_layers"]]
        out[, 1:ncol(out)] <- -99
      } else {
        # regularise the grid (interpolate)
        out <- regularise_model_output(depth = depth, depths = depths,
                                       var = this.var, nlev = nlev)
      }


      # process the table formats if necessary
      if (model == "dy_cd") {

        # regularise the grid (interpolate)
        # out <- delagrangify(depth = depth, elevs = lyrs, values = this.var,
        #                         nlev = nlev)

      } else if (model == "glm_aed") {
        # this.var <- apply(this.var, 2, rev)
        # out <- delagrangify(elevs = lyrs, values = this.var, nlev = nlev)
        ## unit conversions


        # out <- regularise_model_output(depth = dep)

        conv.fact <- key_naming[key_naming$name == vars_sim[i],
                                "conversion_aed"]

        if (!is.na(conv.fact)) {
          out <- out * conv.fact
        }

      } else if (model == "gotm_wet") {

        # out <- sapply(1:ncol(this.var), \(c) {
        #   if (is.na(zeta[c])) {
        #     as.data.frame(rep(NA, nlev))
        #   } else {
        #     if(all(is.na(this.var[, c])) | length(unique(z[, c])) <= 1) {
        #       as.data.frame(rep(NA, nlev))
        #     } else {
        #       deps <- seq(zi[1, c], zeta[c], len = nlev)
        #       approx(y = this.var[, c], x = z[, c], xout = deps, rule = 2)$y
        #     }
        #   }
        # }) |>
        #   as.data.frame()
      }
    }

    # colnames(out) <- dates
    out
  })

  if (model == "dy_cd") {
    var_names <- gsub("dyresm|_.*", "", vars_sim.model)
  } else {
    var_names <- vars_sim.model
  }
  names(vars_list) <- key_naming$name[key_naming[[model]] %in% var_names]
  nc_list <- c(nc_list, vars_list)

  # Calculate lakeAnalyzer values
  if (lake_analyzer) {


    if (mean(depth, na.rm = TRUE) < 10) {
      z_step <- 0.2
    } else {
      z_step <- 0.5
    }

    inp <- input(aeme)
    bathy <- inp$hypsograph
    bathy$depth <- max(bathy$elev) - bathy$elev
    wtr <- nc_list[["HYD_temp"]]
    wtr <- wtr[nrow(wtr):1, ]
    depths <- nc_list[["LKE_layers"]]

    fun_list <- list(HYD_thmcln = rLakeAnalyzer::thermo.depth,
                     HYD_ctrbuy = rLakeAnalyzer::center.buoyancy,
                     HYD_epidep = rLakeAnalyzer::meta.depths,
                     HYD_hypdep = rLakeAnalyzer::meta.depths)

    laz_list <- lapply(names(fun_list), \(f) {
      idx <- ifelse(f == "HYD_hypdep", 2, 1)
      vapply(1:ncol(wtr), \(c) {
        if (all(is.na(wtr[, c]))) {
          return(NA)
        }
        v <- fun_list[[f]](wtr = wtr[, c], depths = depths[, c])
        v[is.nan(v)] <- NA
        v[idx]
      }, numeric(1))
    })
    names(laz_list) <- names(fun_list)

    laz_list[["HYD_schstb"]] <- vapply(1:ncol(wtr), \(c) {
      bthD <- c(0, depths[, c])
      bthA <- approx(x = bathy$depth, y = bathy$area, xout = bthD, rule = 2)$y
      if (any(is.na(bthA)) | length(unique(bthA)) <= 1 |
          sum(!is.na(wtr[, c])) <= 1) {
        return(NA)
      }
      # plot(bthA, bthD, type = "l")
      v <- rLakeAnalyzer::schmidt.stability(wtr = wtr[, c],
                                            depths = depths[, c],
                                            bthA = bthA, bthD = bthD)
      v[is.nan(v)] <- NA
      v
    }, numeric(1))


    for (n in names(laz_list)) {
      nc_list[[n]] <- laz_list[[n]]
    }

    # Derived oxygen values
    oxy <- nc_list[["CHM_oxy"]]
    oxy <- oxy[nrow(oxy):1, ]

    oxy_cline <- vapply(1:ncol(oxy), \(c) {
      v <- cline_depth(wtr = oxy[, c], depths = depths[, c], water = FALSE)
      v[is.nan(v)] <- NA
      v
    }, numeric(1))


    epi_oxy <- vapply(1:ncol(wtr), \(c) {
      idx <- which(depths[, c] <= laz_list$HYD_epidep[c])
      mean(oxy[idx, c])
    }, numeric(1))
    hyp_oxy <- vapply(1:ncol(wtr), \(c) {
      idx <- which(depths[, c] >= laz_list$HYD_hypdep[c])
      mean(oxy[idx, c])
    }, numeric(1))
    meta_oxy <- vapply(1:ncol(wtr), \(c) {
      idx <- which(depths[, c] >= laz_list$HYD_epidep[c] &
                     depths[, c] < laz_list$HYD_hypdep[c])
      mean(oxy[idx, c])
    }, numeric(1))
    exp_oxy <- (epi_oxy + hyp_oxy) / 2
    # plot(epi_oxy, type = "l", ylim = c(0, 12))
    # lines(hyp_oxy, col = "red")
    # lines(meta_oxy, col = "blue")
    # lines(exp_oxy, col = "green")

    nc_list$CHM_oxycln <- oxy_cline
    nc_list$CHM_oxyepi <- epi_oxy
    nc_list$CHM_oxyhyp <- hyp_oxy
    nc_list$CHM_oxymet <- meta_oxy
    nc_list$CHM_oxymom <- meta_oxy - exp_oxy
    nc_list$CHM_oxynal <- vapply(1:ncol(wtr), \(c) {
      if (all(is.na(oxy[, c])) | length(unique(depths[, c])) <= 1) {
        return(NA)
      }
      oxy_layers <- approx(y = oxy[, c], x = depths[, c],
                           xout = seq(0, depth[c], by = z_step), rule = 2)$y
      sum(oxy_layers < 1)
    }, numeric(1))


  }


  # Trim off the spin up period ----
  # if (remove_spin_up) {
  #
  #   idx2 <- which(nc_list$Date >= aeme_time$start &
  #                   nc_list$Date <= aeme_time$stop)
  #
  #   for (l in seq_along(nc_list)) {
  #     if (length(dim(nc_list[[l]])) == 1 | is.null(dim(nc_list[[l]]))) {
  #       nc_list[[l]] <- nc_list[[l]][idx2]
  #     } else if (length(dim(nc_list[[l]])) == 2) {
  #       nc_list[[l]] <- nc_list[[l]][, idx2]
  #     }
  #   }
  # }

  return(nc_list)
  }


#' Regularise model output
#'
#' @param depth vector; of lake depths
#' @param depths matrix; of model layer depths
#' @param var matrix; of model output
#' @param nlev integer; number of layers to interpolate to
#'
#' @return matrix; of regularised model output
#' @noRd
#'
#' @keywords internal

regularise_model_output <- function(depth, depths, var, nlev) {

  # Loop through each column and interpolate to output depths
  vapply(1:ncol(var), \(c) {
    # print(c)
    if (is.na(depth[c])) {
      return(rep(NA, nlev))
    } else {
      if (length(unique(depths[!is.na(depths[, c]), c])) == 1) {
        rep((var[!is.na(depths[, c]), c][1]), nlev)
      } else if(all(is.na(var[, c])) | length(unique(depths[!is.na(depths[, c]), c])) <= 1) {
        rep(NA, nlev)
      } else {
        deps <- seq(0, depth[c], len = nlev)
        non_na <- !is.na(var[, c])
        if (sum(non_na) <= 0) return(rep(NA, nlev))
        approx(y = var[non_na, c], x = depths[non_na, c], xout = deps,
               rule = 2)$y
      }
    }
  }, numeric(nlev))
}

