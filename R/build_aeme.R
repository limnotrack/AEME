#' Build model configuration directories
#'
#' Configure an ensemble of lake model simulations from basic set of inputs.
#'
#' @param aeme aeme; object.
#' @param config list; loaded via `config <- yaml::read_yaml("aeme.yaml")`
#' @param model vector; of models to be used. Can be `dy_cd`, `glm_aed`,
#'  `gotm_wet`.
#' @param model_controls dataframe; of configuration loaded from
#'  "model_controls.csv".
#' @param inf_factor vector; containing numeric factor to multiple the inflows.
#'  Needs to be named according to the model.
#' @param outf_factor vector; containing numeric factor to multiple the
#'  outflows. Needs to be named according to the model.
#' @inheritParams generate_hypsograph
#' @param use_bgc logical; switch to use the biogeochemical model.
#' @param print logical; print messages. Default = TRUE.
#' @param calc_wbal logical; calculate water balance. Default = TRUE.
#' @param wb_method numeric; method to use for calculating water balance. Must be
#' 1 (no inflows or outflows) or 2 (outflows calculated) or 3 (inflows and
#' outflows calculated). Default = 2
#' @param calc_wlev logical; calculate water level.
#' @param use_aeme logical; use AEME object to generate model confiuration
#' files.
#' @param coeffs numeric vector of length two; to be used to estimate surface
#' water temperature for estimating evaporation. Defaults to NULL. If water
#' temperature observations are included in `aeme` object, then it will use
#' those to build a linear relationship between air temperature and water
#' temperature. Otherwise. it uses the simple estimation
#'  \eqn{temp_water = 5 + 0.75 * temp_air} from Stefan & Preud'homme, 2007:
#'  www.doi.org/10.1111/j.1752-1688.1993.tb01502.x
#' @param hum_type numeric; GOTM humidity metric [1=relative humidity (%),
#' 2=wet-bulb temperature, 3=dew point temperature, 4=specific humidity (kg/kg)]
#' Default = 3.
#' @param est_swr_hr logical; estimate hourly shortwave radiation from daily
#' values. Default = TRUE.
#' @param path filepath; where input files are located relative to `config`.
#'
#' @return builds the model ensemble configuration.
#'
#' @importFrom sf sf_use_s2 st_transform st_centroid st_coordinates st_buffer
#' @importFrom dplyr select filter
#' @importFrom utils data read.csv
#' @importFrom withr local_locale local_timezone
#'
#' @return aeme object
#'
#' @export
#'
#' @examples
#' tmpdir <- tempdir()
#' aeme_dir <- system.file("extdata/lake/", package = "AEME")
#' # Copy files from package into tempdir
#' file.copy(aeme_dir, tmpdir, recursive = TRUE)
#' path <- file.path(tmpdir, "lake")
#' aeme <- yaml_to_aeme(path = path, "aeme.yaml")
#' model_controls <- get_model_controls()
#' inf_factor = c("glm_aed" = 1)
#' outf_factor = c("glm_aed" = 1)
#' model <- c("glm_aed")
#' build_aeme(path = path, aeme = aeme, model = model,
#'                model_controls = model_controls, inf_factor = inf_factor, ext_elev = 5,
#'                use_bgc = FALSE)

build_aeme <- function(aeme = NULL,
                       config = NULL,
                       model = c("dy_cd", "glm_aed", "gotm_wet"),
                       model_controls = NULL,
                       inf_factor = c("glm_aed" = 1, "dy_cd" = 1, "gotm_wet" = 1),
                       outf_factor = c("glm_aed" = 1, "dy_cd" = 1,
                                       "gotm_wet" = 1),
                       ext_elev = 0,
                       use_bgc = FALSE,
                       print = TRUE,
                       calc_wbal = TRUE,
                       wb_method = 2,
                       calc_wlev = TRUE,
                       use_aeme = FALSE,
                       coeffs = NULL,
                       hum_type = 3,
                       est_swr_hr = TRUE,
                       path = "."
) {

  # Load arguments
  # config = NULL
  # inf_factor = c("glm_aed" = 1, "dy_cd" = 1,
  #                "gotm_wet" = 1)
  # outf_factor = c("glm_aed" = 1, "dy_cd" = 1,
  #                 "gotm_wet" = 1)
  # ext_elev = 0
  # use_bgc = T
  # calc_wbal = T
  # calc_wlev = T
  # use_aeme = FALSE
  # coeffs = NULL
  # hum_type = 3
  # est_swr_hr = TRUE

  # Set timezone temporarily to UTC
  withr::local_locale(c("LC_TIME" = "C"))
  withr::local_timezone("UTC")

  if (is.null(aeme) & is.null(config)) {
    stop("Either 'aeme' or 'config' must be supplied.")
  }
  if (is.null(model_controls) & !is.null(aeme)) {
    cfg <- configuration(aeme)
    model_controls <- cfg$model_controls
  }
  if (is.null(model_controls) & !is.null(aeme)) {
    stop("model_controls must be supplied.")
  } else if (is.null(model_controls) & is.null(aeme)) {
    stop("Either 'aeme' or 'model_controls' must be supplied.")
  }

  # Metadata
  lvl <- NULL
  inf <- list()
  outf <- list()
  # these variables will be simulated
  if (use_bgc) {
    inf_vars <- model_controls |>
      dplyr::filter(simulate,
                    !var_aeme %in% c("RAD_extc", "PHS_tp", "NIT_pin", "NIT_tn",
                                     "PHY_tchla")) |>
      dplyr::pull(var_aeme)
  } else {
    inf_vars <- c("HYD_temp", "CHM_salt")
  }

  # AEME input ----
  if (!is.null(aeme)) {

    lke <- lake(aeme)
    if (print) {
      message("Building simulation for ", lke$name, " [", format(Sys.time()),
              "]")
    }
    aeme_time <- time(aeme)
    lake_dir <- get_lake_dir(aeme = aeme, path = path)
    date_range <- as.Date(c(aeme_time[["start"]], aeme_time[["stop"]]))
    spin_up <- aeme_time[["spin_up"]]

    # Use cache of models within AEME object ----
    if (use_aeme) {
      model_config <- configuration(aeme)
      if (all(sapply(model, \(x) !is.null(model_config[[x]][["hydrodynamic"]])))) {
        if (print) {
          message("Building existing configuration for ", lke$name, " [",
                  format(Sys.time()), "]")
        }
        write_configuration(model = model, aeme = aeme, path = path)
        overwrite <- FALSE
        # Potentially add in option to switch off bgc and/or use default bgc setup
        # return(aeme)
      } else {
        overwrite <- TRUE
      }
    } else {
      overwrite <- TRUE
    }


    if (!is.null(lke[["shape"]])) {
      lake_shape <- lke[["shape"]]
    } else {
      coords <- data.frame(lat = lke[["latitude"]],
                           lon = lke[["longitude"]])
      coords_sf <- sf::st_as_sf(coords, coords = c("lon", "lat"), crs = 4326)
      r <- sqrt(lke[["area"]] / pi)
      lake_shape <- sf::st_buffer(coords_sf, r)
    }
    lat <- round(lke[["latitude"]], 5)
    lon <- round(lke[["longitude"]], 5)
    elev <- round(lke[["elevation"]], 2)
    # coords.xyz <- c(lke[["longitude"]], lke[["latitude"]], lke[["elevation"]])

    # Inputs ----
    inp <- input(aeme)
    #* Hypsograph ----
    hyps <- add_hypsograph(hypsograph = inp[["hypsograph"]], surf_elev = elev, 
                           lake_depth = lke[["depth"]], 
                           lake_area = lke[["area"]], ext_elev = ext_elev)

    #* Initial depth
    if (!is.null(inp[["init_depth"]])) {
      init_depth <- inp[["init_depth"]]
    } else {
      init_depth <- max(hyps$elev) - min(hyps$elev)
      input(aeme) <- list(init_profile = inp$init_profile,
                          init_depth = init_depth,
                          hypsograph = hyps, meteo = inp$meteo,
                          use_lw = inp$use_lw, Kw = inp$Kw)
    }

    #* Initial profile ----
    if (!is.null(inp[["init_profile"]])) {
      init_prof <- inp[["init_profile"]]
    } else {
      init_prof <- data.frame(depth = c(0, init_depth),
                              temperature = c(10, 10),
                              salt = c(0, 0))
      input(aeme) <- list(init_profile = init_prof,
                          init_depth = init_depth,
                          hypsograph = hyps, meteo = inp$meteo,
                          use_lw = inp$use_lw, Kw = inp$Kw)
    }

    #* Meteorology ----
    if (is.null(inp[["meteo"]])) {
      stop("Meteorology data is not provided. You can download ERA5 data
using the following code:\n

path <- 'era5_folder'
site <- 'lake'
ecmwfr::wf_set_key(user = '123456',
                   key = 'XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX',
                   service = 'cds')

download_era5(lat = lat, lon = lon, year = 2022,
              user = user, path = path)
met <- convert_era5(lat = lat, lon = lon, year = 2022,
                    site = site, path = path)")
    }
    met <- inp[["meteo"]]
    check_time(df = met, model = model, aeme_time = aeme_time, name = "meteo")
    met <- met |>
      dplyr::mutate(Date = as.Date(Date)) |>
      expand_met(lat = lat, lon = lon, elev = elev, print.plot = FALSE)

    input(aeme) <- list(init_profile = init_prof,
                        init_depth = init_depth,
                        hypsograph = hyps, meteo = met,
                        use_lw = inp$use_lw, Kw = inp$Kw)

    # Inflow ----
    aeme_inf <- inflows(aeme)
    if (!is.null(aeme_inf[["data"]])) {
      for (i in 1:length(aeme_inf[["data"]])) {
        inf[[names(aeme_inf[["data"]])[i]]] <- aeme_inf[["data"]][[i]]
        if (any(!inf_vars %in% names(inf[[i]]))) {
          if (print) {
            message(paste0("Missing state variables in inflows: ",
                         paste0(setdiff(inf_vars, names(inf[[i]])), collapse = ", ")))
          }
          add_vars <- setdiff(inf_vars, names(inf[[i]]))
          for (v in add_vars) {
            inf[[i]][[v]] <- model_controls$inf_default[match(v, model_controls$var_aeme)]
          }
          if (print) {
            message("Added default values for missing variables.")
          }
        }
        check_time(df = inf[[names(aeme_inf[["data"]])[i]]], model = model,
                   aeme_time = aeme_time,
                   name = paste0("inflow-", names(aeme_inf[["data"]])[i]))
        
        pot_inf_vars <- c("Date","HYD_flow", inf_vars, "model")

        inf[[i]] <- inf[[i]] |>
          dplyr::select(dplyr::any_of(pot_inf_vars))
      }
    }
    inf_factor <- aeme_inf[["factor"]]

    #* Light extinction Kw ----
    Kw <- inp[["Kw"]]

    # Outflow ----
    aeme_outf <- outflows(aeme)
    if (!is.null(aeme_outf[["data"]]) & length(aeme_outf[["data"]]) > 0) {
      for (i in 1:length(aeme_outf[["data"]])) {
        outf[[names(aeme_outf[["data"]])[i]]] <- aeme_outf[["data"]][[i]]

        if (names(aeme_outf[["data"]])[i] == "wbal" & calc_wbal) next

        check_time(df = outf[[names(aeme_outf[["data"]])[i]]], model = model,
                   aeme_time = aeme_time,
                   name = paste0("outflow-", names(aeme_outf[["data"]])[i]))
      }
    }

    outf_factor <- aeme_outf[["factor"]]
    lakename <- tolower(lke[["name"]])

    # Check if any non-alphanumeric characters are in lakename
    # Causes DYRESM to crash
    if (any(grepl("[^[:alnum:]]", lakename))) {
      stop(strwrap(paste0("Lake name '", lakename, "' contains non-alphanumeric
                          characters. Please remove these characters from the
                          lake name.")))
    }

    # Calculate derived variables ----
    aeme <- calc_lake_obs_deriv(aeme)

    # Check for null observations and insert empty dataframes
    aeme_obs <- observations(aeme)
    # obs_col_names <- get_obs_column_names()
    # empty_df <- matrix(NA, nrow = 1, ncol = length(obs_col_names)) |>
    #   as.data.frame()
    # names(empty_df) <- obs_col_names
    # empty_df <- empty_df |>
    #   dplyr::filter(!is.na(value))
    # if (is.null(aeme_obs$lake)) {
    #   aeme_obs$lake <- empty_df
    # }
    # if (is.null(aeme_obs$level)) {
    #   aeme_obs$level <- empty_df
    # }

    # Lake level ----
    w_bal <- water_balance(aeme)
    w_bal$method <- wb_method
    if (w_bal$use == "obs") {
      level <- aeme_obs[["level"]]
    } else if(w_bal$use == "model") {
      if (is.null(w_bal[["data"]][["model"]])) {
        stop("No modelled lake level is provided. Please provide a
        water balance table with a modelled lake level.")
      }
      level <- w_bal[["data"]][["model"]]
    }

    # Calculate water balance ----
    if (calc_wbal | calc_wlev) {
      wbal <- calc_water_balance(aeme_time = aeme_time,
                                 model = model,
                                 method = w_bal$method,
                                 use = w_bal$use,
                                 hyps = hyps,
                                 inf = inf,
                                 outf = outf[["outflow"]],
                                 level = level,
                                 obs_lake = aeme_obs[["lake"]],
                                 obs_met = met,
                                 elevation = elev,
                                 print_plots = FALSE,
                                 coeffs = coeffs, print = print)
    }

    # Calculate water balance ----
    if (calc_wbal) {

      if (wb_method == 1) {
        msg <- paste(strwrap("No water balance correction applied
                             (method = 1)."),
                     collapse = "\n")
      } else if (wb_method == 2) {
        msg <- paste(strwrap("Correcting water balance using estimated
                             outflows (method = 2)."),
                     collapse = "\n")
      } else if (wb_method == 3) {
        msg <- paste(strwrap("Correcting water balance using estimated
                             inflows and outflows (method = 3)."),
                     collapse = "\n")
        inf[["wbal"]] <- wbal |>
          dplyr::select(Date, inflow,
                        HYD_temp, CHM_salt, model) |> 
          dplyr::rename(HYD_flow = inflow) |>
          dplyr::filter(!is.na(HYD_flow)) 
        # Add missing variables to water balance e.g. if use_bgc = TRUE
        if (any(!inf_vars %in% names(inf[["wbal"]]))) {
          add_vars <- setdiff(inf_vars, names(inf[["wbal"]]))
          for (v in add_vars) {
            inf[["wbal"]][[v]] <- model_controls$inf_default[match(v, model_controls$var_aeme)]
          }
        }
      }

      # Add water balance to outflow if method is 2 or 3
      if (wb_method %in% c(2, 3)) {
        outf[["wbal"]] <- wbal |>
          dplyr::select(Date, model, outflow) #|> 
          # pivot outflow to long format and separate the model
          # tidyr::pivot_longer(cols = -Date,
          #                     names_to = "model",
          #                     values_to = "outflow") |>
          # dplyr::filter(!is.na(outflow)) |>
          # dplyr::mutate(model = gsub("outflow_", "", model))
      }

      if (print) {
        message(msg)
      }
      w_bal[["data"]][["wbal"]] <- wbal
    } else {
      w_bal[["data"]][["wbal"]] <- NULL
      outf[["wbal"]] <- NULL
    }

    #* Update water balance slot in aeme object ----
    water_balance(aeme) <- w_bal
    if (length(outf) == 0) {
      outf <- NULL
    }

    outflows(aeme) <- list(data = outf,
                           outflow_lvl = aeme_outf[["lvl"]],
                           factor = aeme_outf[["factor"]])

    if (calc_wlev) {
      if (print) {
        message(paste(strwrap("Calculating lake level using lake depth
                            and a sinisoidal function."),
                    collapse = "\n"))
      }
      lvl <- wbal |>
        dplyr::select(Date, value) |>
        dplyr::mutate(var_aeme = "LKE_lvlwtr")
    } else {
      lvl <- aeme_obs[["level"]]
    }
    # observations(aeme) <- list(lake = aeme_obs[["lake"]],
    #                                 level = lvl)
    if (aeme_time[["start"]] %in% lvl[["Date"]]) {
      if (print) {
        message(strwrap("Observed lake level is present.\nUpdating initial lake
                        model depth..."))
      }
      init_depth <- lvl |>
        dplyr::filter(Date == aeme_time[["start"]] &
                        var_aeme == "LKE_lvlwtr") |>
        dplyr::pull(value)
      init_depth <- round(init_depth - min(hyps$elev), 2)
      inp <- input(aeme)

      # Update initial profile to match initial depth
      if (max(init_prof$depth) > init_depth) {
        init_prof$depth[which.max(init_prof$depth)] <- init_depth
      }

      input(aeme) <- list(init_profile = inp$init_profile,
                          init_depth = init_depth,
                          hypsograph = inp$hypsograph, meteo = inp$meteo,
                          use_lw = inp$use_lw, Kw = inp$Kw)
    }

    # Yaml config ----
  } else if (!is.null(config)) {
    lake_dir <- file.path(path, paste0(config$lake$lake_id, "_",
                                       tolower(config$lake$name)))
    date_range <- as.Date(c(config[["time"]][["start"]],
                            config[["time"]][["stop"]]))
    spin_up <- config[["time"]][["spin_up"]]

    if (!is.null(config[["lake"]][["shape_file"]])) {
      lake_shape <- sf::st_read(file.path(path,
                                          config[["lake"]][["shape"]]))
    } else {
      coords <- data.frame(lat = config[["lake"]][["latitude"]],
                           lon = config[["lake"]][["longitude"]])
      coords_sf <- sf::st_as_sf(coords, coords = c("lon", "lat"), crs = 4326)
      r <- sqrt(config[["lake"]][["area"]] / pi)
      lake_shape <- sf::st_buffer(coords_sf, r)
    }
    elev <- config[["lake"]][["elevation"]]
    # Hypsograph ----
    if (!file.exists(file.path(path, config[["input"]][["hypsograph"]]))) {
      stop(config[["input"]][["hypsograph"]],
           " does not exist. Check file path.")
    }
    hyps <- utils::read.csv(file.path(path, config[["input"]][["hypsograph"]]))
    # Extend & arrange hypsograph
    if (ext_elev > 0) {
      hyps <- bathy_extrap(hyps, ext_elev)
    }
    hyps <- hyps |>
      dplyr::arrange(elev) |>
      dplyr::mutate(elev = round(elev, 2),
                    depth = round(depth, 2),
                    area = round(area, 2))

    # Water level ----
    if (file.exists(file.path(path, config[["observations"]][["level"]]))) {
      # lvl <- readr::read_csv(config[["observations"]][["level"]],
      #                        show_col_types = FALSE)
      lvl <- utils::read.csv(file.path(path,
                                       config[["observations"]][["level"]]))
    }

    # Initial profile ----
    if (!is.null(config[["input"]][["init_profile"]])) {

    } else {
      init_prof <- data.frame(depth = c(0,
                                        floor(max(hyps$elev) - min(hyps$elev))),
                              temperature = rep(model_controls$initial_wc[model_controls$var_aeme == "HYD_temp"], 2),
                              salt = c(0, 0))
    }

    # Inflow ----
    if (!is.null(config[["inflows"]][["data"]])) {
      for(i in 1:length(config[["inflows"]][["data"]])) {
        inf[[names(config[["inflows"]][["data"]])[i]]] <-
          utils::read.csv(file.path(path, config[["inflows"]][["data"]][[i]]))
        if(any(!inf_vars %in% names(inf[[i]]))) {
          stop("missing state variables in inflow tables")
        }

        inf[[i]] <- inf[[i]] |>
          dplyr::select(all_of(c("Date","HYD_flow", inf_vars)))
      }
    }
    inf_factor <- config[["inflows"]][["factor"]]

    #--- meteorology
    if (!file.exists(file.path(path, config[["input"]][["meteo"]]))) {
      stop(config[["input"]][["meteo"]], " does not exist. Check file path.")
    }
    met <- utils::read.csv(file.path(path, config[["input"]][["meteo"]]))

    Kw <- config[["input"]][["Kw"]]

    # Outflow ----
    if (!is.null(config[["outflows"]][["data"]])) {
      for(i in 1:length(config[["outflows"]][["data"]])) {
        outf[[names(config[["outflows"]][["data"]])[i]]] <-
          utils::read.csv(file.path(path, config[["outflows"]][["data"]][[i]]))
      }
    }

    outf_factor <- config[["outflows"]][["factor"]]
    lakename <- tolower(config[["lake"]][["name"]])

  }

  # Create directory for lake ----
  dir.create(lake_dir, showWarnings = FALSE, recursive = TRUE)

  if (length(inf) == 0) {
    inf <- NULL
  }

  if ("dy_cd" %in% model) {
    #--- configure DYRESM-CAEDYM
    dates.dy <- c(date_range[1] - spin_up[["dy_cd"]], date_range[2]) |>
      `names<-`(NULL)
    build_dycd(lakename, model_controls = model_controls, date_range = dates.dy,
               lat = lat, lon = lon, hyps = hyps, lvl = lvl,
               inf = inf, outf = outf, met = met,
               lake_dir = lake_dir, init_prof = init_prof,
               init_depth = init_depth,
               inf_factor = inf_factor[["dy_cd"]],
               outf_factor = outf_factor[["dy_cd"]],
               Kw = Kw,
               use_bgc = use_bgc, use_lw = inp$use_lw,
               overwrite_cfg = overwrite, print = print)
    # run_dy_cd(sim_folder = lake_dir, verbose = TRUE)
  }
  if ("glm_aed" %in% model) {
    dates.glm <- c(date_range[1] - spin_up[["glm_aed"]], date_range[2]) |>
      `names<-`(NULL)
    build_glm(lakename, model_controls = model_controls, date_range = dates.glm,
              lake_shape = lake_shape, lat = lat, lon = lon,
              hyps = hyps, lvl = lvl, init_prof = init_prof,
              init_depth = init_depth, inf = inf, outf = outf,
              met = met, lake_dir = lake_dir,
              inf_factor = inf_factor[["glm_aed"]],
              outf_factor = outf_factor[["glm_aed"]],
              Kw = Kw, use_bgc = use_bgc,
              use_lw = inp$use_lw, overwrite_nml = overwrite, print = print)
    # run_glm_aed(sim_folder = lake_dir, verbose = TRUE)
  }
  if("gotm_wet" %in% model) {
    dates.gotm <- c(date_range[1] - spin_up[["gotm_wet"]], date_range[2]) |>
      `names<-`(NULL)
    depth <- max(hyps$elev) - min(hyps$elev)
    if (depth < 3) {
      div <- 0.1
    } else {
      div <- 0.33
    }
    nlev <- ceiling(depth / div)
    build_gotm(lakename, model_controls = model_controls, date_range = dates.gotm,
               lake_shape = lake_shape, lat = lat, lon = lon,
               lake_dir = lake_dir, hyps = hyps, lvl = lvl,
               init_prof = init_prof, init_depth = init_depth, inf = inf,
               outf = outf, met = met, inf_factor = inf_factor[["gotm_wet"]],
               outf_factor = outf_factor[["gotm_wet"]], Kw = Kw,
               nlev = nlev, use_bgc = use_bgc,
               hum_type = hum_type, overwrite_yaml = overwrite,
               est_swr_hr = est_swr_hr, print = print)
    # run_gotm_wet(sim_folder = lake_dir, verbose = TRUE)

  }

  # Model parameters ----
  param <- parameters(aeme = aeme)
  if (nrow(param) > 1) {
    input_model_parameters(aeme = aeme, model = model, param = param,
                           path = path)
  }

  # Load model configuration ----
  aeme <- load_configuration(model = model, aeme = aeme, 
                             model_controls = model_controls, use_bgc = use_bgc, 
                             path = path)

  return(aeme)
}
