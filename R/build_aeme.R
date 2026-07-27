#' Build model configuration directories
#'
#' Configure an ensemble of lake model simulations from a basic set of inputs.
#'
#' @param aeme Aeme object.
#' @param model character vector; models to use. One or more of `"dy_cd"`,
#'   `"glm_aed"`, `"gotm_wet"`, `"simstrat_aed2"`. Defaults to all models if
#'   not found in `aeme`.
#' @param path character; directory where input files are located. Defaults to
#'   the path stored in `aeme`, or the current working directory if not set.
#' @param use_bgc logical; enable the biogeochemical model. Default: `FALSE`.
#' @param ext_elev numeric; elevation (m) to extend the hypsograph to.
#'   Default: `0`.
#' @param wb_method integer; water balance method. One of:
#'   - `1` — no inflows or outflows
#'   - `2` — outflows calculated (default)
#'   - `3` — unexplained storage changes treated as effective inflows/outflows
#' @param model_controls data.frame; model configuration, typically loaded via
#'   [get_model_controls()].
#' @param inf_factor named numeric vector; factors to multiply inflows by,
#'   named by model.
#' @param outf_factor named numeric vector; factors to multiply outflows by,
#'   named by model.
#' @param calc_wbal logical; calculate water balance. Default: `TRUE`.
#' @param calc_wlev logical; calculate water level. Default: `TRUE`.
#' @param use_aeme logical; use the `aeme` object to generate model
#'   configuration files. Default: `FALSE`.
#' @param coeffs numeric vector of length 2; coefficients for estimating
#'   surface water temperature when calculating evaporation. If water
#'   temperature observations are present in `aeme`, a linear model is fitted
#'   against air temperature. Otherwise defaults to
#'   \eqn{T_{water} = 5 + 0.75 \times T_{air}} (Stefan & Preud'homme, 1993,
#'   \doi{10.1111/j.1752-1688.1993.tb01502.x}).
#' @param hum_type integer; humidity input type for GOTM. One of:
#'   - `1` — relative humidity (%)
#'   - `2` — wet-bulb temperature
#'   - `3` — dew point temperature (default)
#'   - `4` — specific humidity (kg/kg)
#' @param est_swr_hr logical; estimate hourly shortwave radiation from daily
#'   values. Default: `TRUE`.
#' @param config list; AEME configuration, typically loaded via
#'   `yaml::read_yaml("aeme.yaml")`.
#'
#' @importFrom sf sf_use_s2 st_transform st_centroid st_coordinates st_buffer
#' @importFrom dplyr select filter
#' @importFrom utils data read.csv
#' @importFrom withr local_locale local_timezone
#' @importFrom cli cli_abort
#'
#' @return An updated `aeme` object.
#' @export
#'
#' @examples
#' aeme_dir <- system.file("extdata/lake/", package = "AEME")
#' path <- "aeme"
#' aeme <- yaml_to_aeme(path = aeme_dir, "aeme.yaml")
#' model_controls <- get_model_controls()
#'
#' # Build configuration for GLM-AED
#' aeme <- aeme |>
#'   build_aeme(path = path, model = "glm_aed", model_controls = model_controls,
#'              ext_elev = 5)
#'
#' # Enable biogeochemistry
#' aeme <- aeme |>
#'   build_aeme(path = path, model = "glm_aed", model_controls = model_controls,
#'              ext_elev = 5, use_bgc = TRUE)

build_aeme <- function(aeme = NULL,
                       model = NULL,
                       path = NULL,
                       use_bgc = NULL,
                       ext_elev = NULL,
                       wb_method = NULL,
                       model_controls = NULL,
                       inf_factor = NULL,
                       outf_factor = NULL,
                       calc_wbal = NULL,
                       calc_wlev = NULL,
                       coeffs = NULL,
                       hum_type = NULL,
                       est_swr_hr = NULL,
                       use_aeme = FALSE,
                       config = NULL
) {
  # Set timezone temporarily to UTC
  withr::local_locale(c("LC_TIME" = "C"))
  withr::local_timezone("UTC")
  
  if (is.null(aeme) & is.null(config)) {
    stop("Either 'aeme' or 'config' must be supplied.")
  }
  aeme <- migrate_aeme(aeme)
  if (is.null(model)) {
    model <- list_models(aeme)
    if (length(model) == 0) {
      model <- list_models()
      cli_inform_safe(c("i" = "No models specified. Defaulting to all models:
                        glm_aed, dy_cd, gotm_wet, simstrat_aed2."))
    }
  }
  model <- check_model(model = model)
  aeme <- set_model(aeme = aeme, model = model)
  
  if (is.null(path)) {
    path <- get_config_value(aeme, key = "path")
    if (is.null(path)) {
      path <- getwd()
      cli::cli_alert_warning("`path` is not specified and could not be extracted
                             from `aeme`. Defaulting to current working 
                             directory: {.file {path}}")
    }
  }
  path <- check_path(path = path, create = TRUE)
  
  # Resolve NULL args from config, falling back to defaults
  cfg_defaults <- config_defaults()
  for (key in names(cfg_defaults)) {
    if (is.null(get(key))) {
      assign(key, get_config_value(aeme, key))
    }
  }
  
  
  if (!wb_method %in% 1:3) {
    cli::cli_abort(c("`wb_method` must be 1, 2, or 3.",
                     "i" = "1: no water balance correction applied.",
                     "i" = "2: correcting water balance using estimated outflows.",
                     "i" = "3: correcting water balance using estimated inflows and outflows."))
  }
  if (!hum_type %in% 1:4) {
    cli::cli_abort(c("`hum_type` must be 1, 2, 3, or 4.",
                     "i" = "1: relative humidity (%).",
                     "i" = "2: wet-bulb temperature.",
                     "i" = "3: dew point temperature.",
                     "i" = "4: specific humidity (kg/kg)."))
  }
  
  all_models <- c("glm_aed" = 1, "dy_cd" = 1, "gotm_wet" = 1, "simstrat_aed2" = 1)
  if (is.null(inf_factor))  inf_factor  <- all_models
  if (is.null(outf_factor)) outf_factor <- all_models
  
  if (is.null(model_controls)) {
    if (!is.null(aeme)) {
      cfg <- configuration(aeme)
      model_controls <- cfg$model_controls
    } else {
      model_controls <- get_model_controls(use_bgc = use_bgc)
    }
  }
  
  if (is.null(model_controls)) {
    cli::cli_abort(
      "{.arg model_controls} could not be extracted from {.arg aeme}. Please 
      provide a dataframe of model controls using the 
      {.code get_model_controls()} function.",
      class = "aeme_error_model_controls"
    )
  }
  
  # Metadata
  lvl <- NULL
  inf <- list()
  outf <- list()
  # these variables will be simulated
  if (use_bgc) {
    inf_vars <- model_controls |>
      dplyr::filter(simulate,
                    !is.na(inf_default),
                    !var_aeme %in% c("RAD_extc", "PHS_tp", "NIT_pin", "NIT_tn",
                                     "PHY_tchla", "CHM_ph")) |>
      dplyr::pull(var_aeme)
  } else {
    inf_vars <- c("HYD_temp", "CHM_salt")
  }
  
  
  # AEME input ----
  if (!is.null(aeme)) {
    
    lke <- lake(aeme)
    aeme_time <- time(aeme)
    lake_dir <- get_lake_dir(aeme = aeme, path = path)
    date_range <- as.Date(c(aeme_time[["start"]], aeme_time[["stop"]]))
    spin_up <- aeme_time[["spin_up"]]
    
    # Use cache of models within AEME object ----
    if (use_aeme) {
      model_config <- configuration(aeme)
      if (all(sapply(model, \(x) !is.null(model_config[[x]][["hydrodynamic"]])))) {
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
      expand_met(lat = lat, lon = lon, elev = elev, print.plot = FALSE) |> 
      standardise_met()
    # names(met) <- gsub("MET_", "", names(met))
    
    input(aeme) <- list(init_profile = init_prof,
                        init_depth = init_depth,
                        hypsograph = hyps, meteo = met,
                        use_lw = inp$use_lw, Kw = inp$Kw)
    
    # Inflow ----
    aeme_inf <- inflows(aeme)
    if (length(aeme_inf[["data"]]) > 0) {
      pot_inf_vars <- c("time", "HYD_flow", inf_vars, "model")
      verbose <- getOption("AEME.inform", TRUE)
      for (i in seq_along(aeme_inf[["data"]])) {
        inf_name <- names(aeme_inf[["data"]])[i]
        inf[[inf_name]] <- standardise_inflow(
          inflow        = aeme_inf[["data"]][[i]],
          model_controls = model_controls,
          inf_vars      = inf_vars,
          aeme_time     = aeme_time,
          inflow_name   = inf_name,
          model         = model,
          pot_inf_vars  = pot_inf_vars,
          verbose       = verbose
        )
      }
    }
    inf_factor <- aeme_inf[["factor"]]
    
    #* Light extinction Kw ----
    Kw <- inp[["Kw"]]
    
    # Outflow ----
    aeme_outf <- outflows(aeme)
    # Backwards compatibility with "lvl"
    if ("lvl" %in% names(aeme_outf)) {
      elevation_list <- list()
      if (length(aeme_outf[["data"]]) > 0) {
        for (i in 1:length(aeme_outf[["data"]])) {
          elevation_list[[names(aeme_outf[["data"]])[i]]] <- aeme_outf[["lvl"]][1]
        }
        names(elevation_list) <- names(aeme_outf[["data"]])
      }
      aeme_outf[["elevation"]] <- elevation_list
    }
    
    outf_combined <- NULL
    if (length(aeme_outf[["data"]]) > 0) {
      for (i in 1:length(aeme_outf[["data"]])) {
        
        if (names(aeme_outf[["data"]])[i] == "wbal" & calc_wbal) next
        
        outf[[names(aeme_outf[["data"]])[i]]] <- aeme_outf[["data"]][[i]]
        check_time(df = outf[[names(aeme_outf[["data"]])[i]]], model = model,
                   aeme_time = aeme_time,
                   name = paste0("outflow-", names(aeme_outf[["data"]])[i]))
      }
      outflow_names <- names(outf)
      # Select names not set to "wbal"
      outflow_names <- outflow_names[!outflow_names %in% "wbal"]
      
      # If 1 or more outflows present, combine and sum
      if (length(outflow_names) > 1) {
        # Bind together and then group by Date and sum outflows
        outf_combined <- dplyr::bind_rows(outf[outflow_names], .id = "outflow_name") |>
          dplyr::group_by(Date) |>
          dplyr::summarise(HYD_flow = sum(HYD_flow, na.rm = TRUE),
                           .groups = "drop")
        
      } else if (length(outflow_names) == 1) {
        outf_combined <- outf[[outflow_names]]
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
    wbal_params <- w_bal[["params"]]
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
      init_elev <- round(min(hyps$elev) + init_depth, 2)
      init_temp <- init_prof |> 
        dplyr::filter(depth == min(depth)) |> 
        dplyr::pull(temperature)
      wbal_list <- calc_water_balance(aeme_time = aeme_time,
                                      model = model,
                                      method = w_bal$method,
                                      use = w_bal$use,
                                      hyps = hyps,
                                      inf = inf,
                                      outf = outf_combined,
                                      level = level,
                                      init_elev = init_elev,
                                      init_temp = init_temp,
                                      obs_lake = aeme_obs[["lake"]],
                                      obs_met = met,
                                      elevation = elev,
                                      print_plots = FALSE,
                                      params = wbal_params,
                                      coeffs = coeffs)
      wbal <- wbal_list$wb
      wbal_params <- wbal_list$wbal_params
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
                        Ts, CHM_salt, model) |> 
          dplyr::rename(HYD_flow = inflow,
                        HYD_temp = Ts) |>
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
          dplyr::rename(outflow = spill_outflow) |>
          dplyr::select(Date, model, outflow)
        aeme_outf[["elevation"]][["wbal"]] <- -1
      }
      
      cli_inform_safe(c("i" = msg))
      w_bal[["data"]][["wbal"]] <- wbal
      w_bal[["params"]] <- wbal_params
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
                           elevation = aeme_outf[["elevation"]],
                           factor = aeme_outf[["factor"]])
    
    if (calc_wlev) {
      lvl <- wbal |>
        dplyr::select(Date, value) |>
        dplyr::mutate(var_aeme = "LKE_lvlwtr")
    } else {
      lvl <- aeme_obs[["level"]]
    }
    # observations(aeme) <- list(lake = aeme_obs[["lake"]],
    #                                 level = lvl)
    if (aeme_time[["start"]] %in% lvl[["Date"]]) {
      cli_inform_safe(c("i" = "Observed lake level is present.\nUpdating initial lake
                        model depth..."))
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
    hyps <- read.csv(file.path(path, config[["input"]][["hypsograph"]]))
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
      lvl <- read.csv(file.path(path,
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
          read.csv(file.path(path, config[["inflows"]][["data"]][[i]]))
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
    met <- read.csv(file.path(path, config[["input"]][["meteo"]]))
    
    Kw <- config[["input"]][["Kw"]]
    
    # Outflow ----
    if (!is.null(config[["outflows"]][["data"]])) {
      for(i in 1:length(config[["outflows"]][["data"]])) {
        outf[[names(config[["outflows"]][["data"]])[i]]] <-
          read.csv(file.path(path, config[["outflows"]][["data"]][[i]]))
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
               overwrite_cfg = overwrite)
    # run_dy_cd(sim_folder = lake_dir, verbose = TRUE)
  }
  if ("glm_aed" %in% model) {
    #--- configure GLM-AED
    dates.glm <- c(date_range[1] - spin_up[["glm_aed"]], date_range[2]) |>
      `names<-`(NULL)
    
    build_glm(lakename, model_controls = model_controls, date_range = dates.glm,
              lake_shape = lake_shape, lat = lat, lon = lon,
              hyps = hyps, lvl = lvl, init_prof = init_prof,
              init_depth = init_depth, inf = inf, outf = outf,
              heights_wdr = unlist(aeme_outf[["elevation"]]),
              met = met, lake_dir = lake_dir,
              inf_factor = inf_factor[["glm_aed"]],
              outf_factor = outf_factor[["glm_aed"]],
              Kw = Kw, use_bgc = use_bgc,
              use_lw = inp$use_lw, overwrite_nml = overwrite)
    
    if (use_bgc) {
      aeme <- aeme |> 
        set_glm_aed_models(path = path, 
                           aed_models = c("aed_sedflux", "aed_oxygen", 
                                          "aed_silica", "aed_nitrogen",
                                          "aed_phosphorus",
                                          "aed_organic_matter",
                                          "aed_phytoplankton", "aed_totals")) |> 
        set_aed_sed_const2d(path = path)
    }
    # run_glm_aed(sim_folder = lake_dir, verbose = TRUE)
  }
  if ("gotm_wet" %in% model) {
    #--- configure GOTM-WET
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
               est_swr_hr = est_swr_hr)
    # run_gotm_wet(sim_folder = lake_dir, verbose = TRUE)

  }
  if ("simstrat_aed2" %in% model) {
    #--- configure Simstrat-AED2
    dates.simstrat <- c(date_range[1] - spin_up[["simstrat_aed2"]], date_range[2]) |>
      `names<-`(NULL)

    build_simstrat(lakename, model_controls = model_controls,
                   date_range = dates.simstrat, lake_shape = lake_shape,
                   lat = lat, lon = lon, hyps = hyps, lvl = lvl,
                   init_prof = init_prof, init_depth = init_depth,
                   inf = inf, outf = outf,
                   heights_wdr = unlist(aeme_outf[["elevation"]]),
                   met = met, lake_dir = lake_dir,
                   inf_factor = inf_factor[["simstrat_aed2"]],
                   outf_factor = outf_factor[["simstrat_aed2"]],
                   Kw = Kw, use_bgc = use_bgc, overwrite_par = overwrite)
    # run_simstrat_aed2(sim_folder = lake_dir, verbose = TRUE)
  }
  
  # Model parameters ----
  param <- parameters(aeme = aeme)
  if (nrow(param) > 1) {
    # Add catch if index is missing
    if (!"index" %in% names(param)) {
      param$index <- NA_integer_
      aeme <- remove_param(aeme = aeme)
      aeme <- add_param(aeme = aeme, param = param)
    }
    input_model_parameters(aeme = aeme, model = model, param = param,
                           path = path)
  }
  
  # GLM-AED totals configuration ----
  if ("glm_aed" %in% model & use_bgc) {
    set_aed_totals(aeme = aeme, path = path)
  } 
  
  # Check model cfg files ----
  cfg_files <- get_model_config_files(aeme = aeme, model = model, path = path)
  if ("gotm_wet" %in% model) {
    check_gotm_yaml(file = cfg_files$gotm_wet["gotm"])
  }
  if ("glm_aed" %in% model) {
    check_glm_nml(file = cfg_files$glm_aed["glm3"])
  }
  if ("simstrat_aed2" %in% model) {
    check_simstrat_par(file = cfg_files$simstrat_aed2["simstrat"])
  }
  
  
  # Load model configuration ----
  aeme <- load_configuration(model = model, aeme = aeme, 
                             model_controls = model_controls, use_bgc = use_bgc, 
                             path = path, ext_elev = ext_elev,
                             calc_wbal = calc_wbal, wb_method = wb_method,
                             calc_wlev = calc_wlev, coeffs = coeffs, 
                             hum_type = hum_type, est_swr_hr = est_swr_hr)
  
  return(aeme)
}
