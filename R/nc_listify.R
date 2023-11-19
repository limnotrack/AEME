#' Convert netCDF output to a standardised list
#'
#' @param nc file connection; to a netCDF file
#' @inheritParams delangrangify
#' @param model character; model name
#' @param vars_sim vector; of variables to extract
#' @param nlev numeric; number of vertical levels
#' @param aeme_time time list; retrieved from AEME object using `time`
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
#'

nc_listify <- function(nc, model, vars_sim, nlev, aeme_time,
                       remove_spin_up = TRUE, output_hour) {

  # Load Rdata
  utils::data("key_naming", package = "AEME", envir = environment())

  # Set timezone temporarily to UTC
  withr::local_locale(c("LC_TIME" = "C"))
  withr::local_timezone("UTC")

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

    mod_layers <- z |> abs()

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
    V <- ncdf4::ncvar_get(nc, "int_water_balance")[idx]

    Qe <- -1 * ncdf4::ncvar_get(nc, "qe")[idx]
    evap_flux <- abs(ncdf4::ncvar_get(nc, "evap")[idx])
    EVAP <- evap_flux * 86400 # m/day
    A0 <- ncdf4::ncvar_get(nc, "Af")[, idx] |>
      apply(2, max)
    evap_vol <- EVAP * A0

    flow_vars <- names(nc$var)[grepl("Q_", names(nc$var))]
    inflow_vars <- flow_vars[!grepl("outflow|wbal", flow_vars)]
    outflow_vars <- flow_vars[grepl("outflow|wbal", flow_vars)]
    if (length(inflow_vars) >= 1) {
      inflow <- sapply(seq_along(inflow_vars), \(x) {
        (ncdf4::ncvar_get(nc, inflow_vars[x])[idx] * 86400) / A0
      }) |>
        apply(1, sum)
    } else {
      inflow <- A0 * 0
    }
    if (length(outflow_vars) >= 1) {
      outflow <- sapply(seq_along(outflow_vars), \(x) {
        -1 * (ncdf4::ncvar_get(nc, outflow_vars[x])[idx] * 86400) / A0
      }) |>
        apply(1, sum)
    } else {
      outflow <- A0 * 0
    }
    precip <- ncdf4::ncvar_get(nc, "precip")[idx] * 86400

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
    vars_sim.model <- key_naming[["glm_aed"]]

    mod_layers <- ncdf4::ncvar_get(nc, "z")[, idx]
    mod_layers[mod_layers > 1000000] <- NA

    Qe <- -1 * ncdf4::ncvar_get(nc, "daily_qe")[idx]
    V <- ncdf4::ncvar_get(nc, "lake_volume")[idx]
    depth <- ncdf4::ncvar_get(nc, "lake_level")[idx]
    evap_vol <- -ncdf4::ncvar_get(nc, "evaporation")[idx]
    evap_flux <- -ncdf4::ncvar_get(nc, "evap_mass_flux")[idx]
    A0 <- ncdf4::ncvar_get(nc, "surface_area")[idx]
    EVAP <- abs(evap_vol / A0)
    inflow <- ncdf4::ncvar_get(nc, "tot_inflow_vol")[idx] / A0
    outflow <- (ncdf4::ncvar_get(nc, "tot_outflow_vol")[idx] +
                  ncdf4::ncvar_get(nc, "overflow_vol")[idx]) / A0
    precip <- ncdf4::ncvar_get(nc, "precipitation")[idx]

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
    Ts <- ncdf4::ncvar_get(nc, "dyresmTEMPTURE_Var")[, idx] |>
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

    EVAP <- ncdf4::ncvar_get(nc, "dyresmEVAP_DAILY_Var")[idx]
    inflow <- ncdf4::ncvar_get(nc, "stream_1_VOL")[idx]
    outflow <- (ncdf4::ncvar_get(nc, "withdrawal_outflow")[idx] +
                  ncdf4::ncvar_get(nc, "overflow_VOL_Var")[idx])
    precip <- ncdf4::ncvar_get(nc, "met_RAIN")[idx]
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

  if (model == 'dy_cd') {
    A0 <- sapply(1:length(depth), function(d) approx((H - min(H)), A,
                                                     xout = depth[d])$y)
    dz <- 0.01
    V <- sapply(1:length(depth), function(d) {
      if(is.na(depth[d]) | is.infinite(depth[d])) return(NA)
      layerD <- seq(dz, (depth[d] - dz), dz)
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


  nc_list <- list(Date = dates, LKE_lvlwtr = as.vector(depth),
                  LKE_V = as.vector(V), LKE_dV = as.vector(dV),
                  LKE_A0 = as.vector(A0), LKE_evprte = as.vector(EVAP),
                  LKE_evpflx = as.vector(evap_flux), LKE_Qe = as.vector(Qe),
                  LKE_evpvol = as.vector(evap_vol),
                  LKE_precip = as.vector(precip),
                  LKE_inflow = as.vector(inflow),
                  LKE_outflow = as.vector(outflow),
                  LKE_layers = as.matrix(layers),
                  LKE_depths = as.matrix(depths))

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
        out <- regularise_model_output(depth = depth, mod_layers = mod_layers,
                                       var = this.var, nlev = nlev)
      }


      # process the table formats if necessary
      if (model == 'dy_cd') {

        # regularise the grid (interpolate)
        # out <- delagrangify(depth = depth, elevs = lyrs, values = this.var,
        #                         nlev = nlev)

      } else if (model == 'glm_aed') {
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


  # Trim off the spin up period ----
  if (remove_spin_up) {

    idx2 <- which(nc_list$Date >= aeme_time$start &
                    nc_list$Date <= aeme_time$stop)

    for (l in seq_along(nc_list)) {
      if (length(dim(nc_list[[l]])) == 1 | is.null(dim(nc_list[[l]]))) {
        nc_list[[l]] <- nc_list[[l]][idx2]
      } else if (length(dim(nc_list[[l]])) == 2) {
        nc_list[[l]] <- nc_list[[l]][, idx2]
      }
    }
  }

  return(nc_list)
}


#' Regularise model output
#'
#' @param depth vector; of lake depths
#' @param mod_layers matrix; of model layer depths
#' @param var matrix; of model output
#' @param nlev integer; number of layers to interpolate to
#'
#' @return matrix; of regularised model output
#' @noRd
#'
#' @keywords internal

regularise_model_output <- function(depth, mod_layers, var, nlev) {

  # Loop through each column and interpolate to output depths
  vapply(1:ncol(var), \(c) {
    if (is.na(depth[c])) {
      return(rep(NA, nlev))
    } else {
      if (length(unique(mod_layers[!is.na(mod_layers[, c]), c])) == 1) {
        rep(var[!is.na(mod_layers[, c]), c], nlev)
      } else if(all(is.na(var[, c])) | length(unique(mod_layers[!is.na(mod_layers[, c]), c])) <= 1) {
        rep(NA, nlev)
      } else {
        deps <- seq(0, depth[c], len = nlev)
        non_na <- !is.na(var[, c])
        if (sum(non_na) <= 0) return(rep(NA, nlev))
        approx(y = var[non_na, c], x = mod_layers[non_na, c], xout = deps,
               rule = 2)$y
      }
    }
  }, numeric(nlev)) #|>
    # matrix(ncol = nlev, byrow = TRUE)

  # sapply(1:ncol(var), \(c) {
  #   if (is.na(depth[c])) {
  #     return(rep(NA, nlev))
  #   } else {
  #     if (length(unique(mod_layers[!is.na(mod_layers[, c]), c])) == 1) {
  #       rep(var[!is.na(mod_layers[, c])], nlev)
  #     } else if(all(is.na(var[, c])) | length(unique(mod_layers[!is.na(mod_layers[, c]), c])) <= 1) {
  #       rep(NA, nlev)
  #     } else {
  #       deps <- seq(0, depth[c], len = nlev)
  #       non_na <- !is.na(var[, c])
  #       if (sum(non_na) <= 0) return(rep(NA, nlev))
  #       approx(y = var[non_na, c], x = mod_layers[non_na, c], xout = deps,
  #              rule = 2)$y
  #     }
  #   }
  # })
}

