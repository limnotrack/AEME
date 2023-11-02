### funtion to process nc files from dyresm, glm, and gotm
### results in a list of data frames with a common, regular structure
### interpolates results for glm and dyresm to make a regular layer structure (per glm tools)



nc_listify <- function(nc, model, vars_sim, nlev, spin_up) {

  # Load Rdata
  utils::data("key_naming", package = "AEME", envir = environment())

  # reduce the key to the simvars
  key_naming <- key_naming |>
    dplyr::filter(name %in% vars_sim) |>
    dplyr::mutate(conversion_aed = as.numeric(conversion_aed))

  # find the simvars for this model
  if (model == "gotm_wet") {
    ## dates.. gotm seems to output the intial profiles, then every tstep
    out.steps <- ncdf4::ncvar_get(nc, "time")
    if (length(out.steps) == 0) {
      return(NULL)
    }
    date.start <- ncdf4::ncatt_get(nc,'time','units')$value |>
      gsub("seconds since ", "", x = _) |>
      as.POSIXct() |>
      as.Date()
    dates <- seq.Date(date.start, by = 1, length.out = length(out.steps))
    lyr_h <- ncdf4::ncvar_get(nc, "h") # lyrs
    zeta <- ncdf4::ncvar_get(nc, "zeta")
    zi <- ncdf4::ncvar_get(nc, "zi")
    z <- ncdf4::ncvar_get(nc, "z")
    sst <- ncdf4::ncvar_get(nc, "sst")
    if (sum(sst == 0) > 1 | sum(is.na(sst)) > 0) {
      # Run-length encoding of the vector
      sst[is.na(sst)] <- -999
      rle_result <- rle(as.vector(sst))
      start_index <- which(rle_result$lengths > 1)[1]
      # vals <- rle_result$values[start_index]
      z[, start_index:ncol(z)] <- NA
      zi[, start_index:ncol(zi)] <- NA
      lyr_h[, start_index:ncol(lyr_h)] <- NA
    }
    DEPTH <- zi[nrow(zi), ] - zi[1, ]
    DEPTH[DEPTH <= 0] <- 0
    zeta[DEPTH <= 0] <- NA
    # DEPTH <- data.frame(ncdf4::ncvar_get(nc, "z"))
    lyrs <- z # lyrs
    lyrs[1, ] <- lyrs[1, ] - (lyr_h[1, ] / 2)
    lyrs[nrow(lyrs), ] <- lyrs[nrow(lyrs), ] + (lyr_h[nrow(lyr_h), ] / 2)
    lyrs <- apply(lyrs, 2, \(x) x + abs(min(x)))
    vars_sim.model <- key_naming[[model]]
    V <- ncdf4::ncvar_get(nc, "int_water_balance")

    Qe <- -1 * ncdf4::ncvar_get(nc, "qe")
    evap_flux <- abs(ncdf4::ncvar_get(nc, "evap"))
    EVAP <- evap_flux * 86400 # m/day
    A0 <- ncdf4::ncvar_get(nc, "Af") |>
      apply(2, max)
    evap_vol <- EVAP * A0

    flow_vars <- names(nc$var)[grepl("Q_", names(nc$var))]
    inflow_vars <- flow_vars[!grepl("outflow", flow_vars)]
    outflow_vars <- flow_vars[grepl("outflow", flow_vars)]
    if (length(inflow_vars) == 1) {
      inflow <- (ncdf4::ncvar_get(nc, inflow_vars[1]) * 86400) / A0
    } else {
      inflow <- A0 * 0
    }
    if (length(outflow_vars) == 1) {
      outflow <- abs(ncdf4::ncvar_get(nc, "Q_outflow") * 86400) / A0
    } else {
      outflow <- A0 * 0
    }
    precip <- ncdf4::ncvar_get(nc, "precip") * 86400

  } else if (model == 'glm_aed') {
    lyrs <- data.frame(ncdf4::ncvar_get(nc, "z")) |> # layer
      # map_df(., rev) |>
      dplyr::arrange(-dplyr::row_number()) |>
      data.frame()
    lyrs[lyrs > 1000000] <- NA
    # glm DOES NOT output initial profiles
    hours.since  <- ncdf4::ncvar_get(nc, "time")
    if (length(hours.since) == 0) {
      return(NULL)
    }
    date.start <- as.POSIXct(gsub("hours since ", "",
                                  ncdf4::ncatt_get(nc,'time','units')$value))
    dates <- as.POSIXct(hours.since * 3600 + date.start) |>
      as.Date()
    vars_sim.model = key_naming[["glm_aed"]]

    Qe <- -1 * ncdf4::ncvar_get(nc, "daily_qe")
    V <- ncdf4::ncvar_get(nc, "lake_volume")
    DEPTH <- ncdf4::ncvar_get(nc, "lake_level")
    evap_vol <- -ncdf4::ncvar_get(nc, "evaporation")
    evap_flux <- -ncdf4::ncvar_get(nc, "evap_mass_flux")
    A0 <- ncdf4::ncvar_get(nc, "surface_area")
    EVAP <- abs(evap_vol / A0)
    inflow <- ncdf4::ncvar_get(nc, "tot_inflow_vol") / A0
    outflow <- (ncdf4::ncvar_get(nc, "tot_outflow_vol") + ncdf4::ncvar_get(nc, "overflow_vol")) / A0
    precip <- ncdf4::ncvar_get(nc, "precipitation")

  } else if (model == 'dy_cd') {
    # dyresm outputs initial profiles as first col
    if (!("dyresmTime" %in% names(nc$var))) {
      return(NULL)
    }
    dates <- as.POSIXct((ncdf4::ncvar_get(nc, 'dyresmTime') - 2415018.5)*86400,
                        origin = "1899-12-30", tz = "UTC") |> as.Date()
    # lyrs are elevation from bottom, last row is bottom
    lyrs <- data.frame(ncdf4::ncvar_get(nc, "dyresmLAYER_HTS_Var"))
    DEPTH <- apply(lyrs, 2, \(x) max(x, na.rm = TRUE))
    vars_sim.model = paste0('dyresm', key_naming[["dy_cd"]],'_Var')

    H <- ncdf4::ncvar_get(nc, "morph_HEIGHT")
    A <- ncdf4::ncvar_get(nc, "morph_AREA")
    elev <- ncdf4::ncvar_get(nc, "morph_ELEV")
    init_H <- ncdf4::ncvar_get(nc, "initprofHeight")[2] + min(H)

    MET_wndspd <- ncdf4::ncvar_get(nc, "met_Uwind")
    MET_prvapr <- ncdf4::ncvar_get(nc, "met_Pvapour")
    Ts <- ncdf4::ncvar_get(nc, "dyresmTEMPTURE_Var") |>
      apply(2, \(x) {
        x[!is.na(x)][1]
      })
    es <- exp(2.3026 * (((7.5 * Ts) / (Ts + 237.3) + 0.7858)))
    #evaporative heat flux
    Qe <- -1 * ((0.622 / 981.9) *         #constant/mean station pressure
             0.0013 *               #latent heat transfer coefficient
             1.168 *                #density of air
             2453000 *              #latent heat of evaporation of water
             MET_wndspd *           #wind speed in m/s
             (MET_prvapr - es))

    EVAP <- ncdf4::ncvar_get(nc, "dyresmEVAP_DAILY_Var")
    inflow <- ncdf4::ncvar_get(nc, "stream_1_VOL")
    outflow <- (ncdf4::ncvar_get(nc, "withdrawal_outflow") + ncdf4::ncvar_get(nc, "overflow_VOL_Var"))
    precip <- ncdf4::ncvar_get(nc, "met_RAIN")
  }

  # format the lyrs and add as first list item (common to all three models)
  # h.surf <- map_df(lyrs, max, na.rm = T) |> as.numeric()
  h.surf <- apply(lyrs, 2, max, na.rm = T)# |> as.numeric()
  h.surf[is.infinite(h.surf)] <- NA
  h.surf[DEPTH == 0] <- 0
  each.layer <- h.surf / nlev
  LAYERS <- data.frame(lapply(each.layer, 2, FUN = rep, nlev)) |>
    cumsum() |>
    # map_df(rev) |>
    data.frame() |>
    `colnames<-`(dates)

  DEPTHS <- apply(LAYERS, 2, function(x) x-max(x, na.rm = TRUE) ) |>
    abs() |>
    data.frame() |>
    `colnames<-`(dates)

  # DEPTH <- h.surf #apply(DEPTHS, 2, max)

  if (model == 'dy_cd') {
    A0 <- sapply(1:length(DEPTH), function(d) approx((H- min(H)), A, xout = DEPTH[d])$y)
    dz <- 0.01
    V <- sapply(1:length(DEPTH), function(d) {
      if(is.na(DEPTH[d]) | is.infinite(DEPTH[d])) return(NA)
      layerD <- seq(dz, (DEPTH[d] - dz), dz)
      layerA <- stats::approx(H, A, layerD)$y
      sum((layerA) * dz)
    })
    inflow <- inflow / A0
    outflow <- outflow / A0
    dates <- seq.Date(from = dates[1], by = 1, length.out = length(dates))
    evap_flux <- EVAP / 86400
    evap_vol <- EVAP * A0
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


  nc_list <- list(Date = dates, HYD_wlev = DEPTH, HYD_V = V, HYD_dV = dV,
                  HYD_A0 = A0, HYD_evap = EVAP, HYD_evap_flux = evap_flux,
                  HYD_Qe = Qe, HYD_evap_vol = evap_vol, HYD_precip = precip,
                  HYD_inflow = inflow, HYD_outflow = outflow, LAYERS = LAYERS,
                  DEPTHS = DEPTHS)

  ### peel through the netcdf and make a list of the outputs
  vars_chk <- data.frame(vars = vars_sim.model, present = NA)
  for(v in seq_len(nrow(vars_chk))) {
    vars_chk[["present"]][v] <- vars_sim.model[v] %in% names(nc$var)
  }

  vars_list <- lapply(1:length(vars_sim.model), \(i) {


    if (is.na(vars_sim.model[i]) | !vars_chk[["present"]][i]) {

      this.var <- vars_sim[i]
      this.df <- nc_list[["LAYERS"]]
      this.df[,1:ncol(this.df)] <- -99

    } else {
      message(paste0("Retrieving and formatting ", vars_sim.model[i],
                     " for model ", model))
      # get the table from the nc file
      this.var <- data.frame(ncdf4::ncvar_get(nc, vars_sim.model[i])) |>
        `colnames<-`(dates)

      # process the table formats if necessary
      if (model == 'dy_cd') {

        # regularise the grid (interpolate)
        this.df <- delagrangify(elevs = lyrs, values = this.var, nlev = nlev)
        # this.df <- data.frame(mapply(elevs = lyrs, values = this.var,
        #                              FUN = delagrangify, nlev = nlev))# |>


      } else if (model == 'glm_aed') {

        # this.var %<>%  # layer
        #   map_df(., rev) |> data.frame()
        this.var <- apply(this.var, 2, rev)
        this.df <- delagrangify(elevs = lyrs, values = this.var, nlev = nlev)

        # this.df <- data.frame(mapply(elevs = lyrs, values = this.var,
        #                              FUN = delagrangify, nlev = nlev)) #|>
        # map_df(rev)
        ## unit conversions

        conv.fact <- key_naming[key_naming$name == vars_sim[i], "conversion_aed"]

        if (!is.na(conv.fact)) {
          this.df <- this.df * conv.fact
        }

      } else if (model == 'gotm_pclake' | model == "gotm_wet") {

        this.df <- sapply(1:ncol(this.var), \(c) {
          if(is.na(zeta[c])) {
            as.data.frame(rep(NA, nlev))
          } else {
            if(all(is.na(this.var[, c])) | length(unique(z[, c])) <= 1) {
              as.data.frame(rep(NA, nlev))
            } else {
              deps <- seq(zi[1, c], zeta[c], len = nlev)
              approx(y = this.var[, c], x = z[, c], xout = deps, rule = 2)$y
            }
          }
        }) |>
          as.data.frame()
      }
    }

    colnames(this.df) <- dates
    this.df
  })

  if (model == "dy_cd") {
    var_names <- gsub("dyresm|_.*", "", vars_sim.model)
  } else {
    var_names <- vars_sim.model
  }
  names(vars_list) <- key_naming$name[key_naming[[model]] %in% var_names]

  nc_list <- c(nc_list, vars_list)

  # names(nc_list) <- c("Date", "HYD_wlev", "HYD_evap", "HYD_precip", "HYD_inflow", "HYD_outflow","LAYERS", "DEPTHS",  vars_sim)

  # make derived variables if these are not already present


  # trim off the warmup period
  for (l in seq_along(nc_list)) {
    if (length(dim(nc_list[[l]])) == 1) {
      nc_list[[l]] <- nc_list[[l]][(spin_up + 1):length(nc_list[[l]])]
    } else if (length(dim(nc_list[[l]])) == 2) {
      nc_list[[l]] <- nc_list[[l]][(spin_up + 1):ncol(nc_list[[l]])]
    }
  }



  return(nc_list)
}

