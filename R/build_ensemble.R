#' Build model configuration directories
#'
#' Configure an ensemble of lake model simulations from basic set of inputs.
#'
#' @param aeme_data aeme; data object.
#' @param config list; loaded via `config <- yaml::read_yaml("aeme.yaml")`
#' @param model vector; of models to be used. Can be `dy_cd`, `glm_aed`,
#'  `gotm_wet`.
#' @param mod_ctrls dataframe; of configuration loaded from
#'  "mod_ctrls.csv".
#' @param inf_factor vector; containing numeric factor to multiple the inflows.
#'  Needs to be named according to the model.
#' @param outf_factor vector; containing numeric factor to multiple the
#'  outflows. Needs to be named according to the model.
#' @param ext_elev numeric; extension in elevation for the hypsogrph in metres.
#' @param use_bgc boolean; switch to use the biogeochemical model.
#' @param use_lw boolean; use incoming longwave radiation. Only applies to
#' GLM-AED.
#' @param hum_type numeric; GOTM humidity metric [1=relative humidity (%),
#' 2=wet-bulb temperature, 3=dew point temperature, 4=specific humidity (kg/kg)]
#' Default = 3.
#' @param path filepath; where input files are located relative to `config`.
#'
#' @return builds the model ensemble configuration.
#'
#' @importFrom sf sf_use_s2 st_transform st_centroid st_coordinates st_buffer
#' @importFrom dplyr select filter
#' @importFrom utils data read.csv
#'
#' @export
#'
#' @examples
#' install.packages("configr", repos = "http://cran.us.r-project.org")
#' tmpdir <- tempdir()
#' aeme_dir <- system.file("extdata/lake/", package = "AEME")
#' # Copy files from package into tempdir
#' file.copy(aeme_dir, tmpdir, recursive = TRUE)
#' path <- file.path(tmpdir, "lake")
#' aeme_data <- yaml_to_aeme(path = path, "aeme.yaml")
#' mod_ctrls <- read.csv(file.path(path, "model_controls.csv"))
#' inf_factor = c("glm_aed" = 1)
#' outf_factor = c("glm_aed" = 1)
#' model <- c("glm_aed")
#' build_ensemble(path = path, aeme_data = aeme_data, model = model,
#'                mod_ctrls = mod_ctrls, inf_factor = inf_factor, ext_elev = 5,
#'                use_bgc = FALSE, use_lw = TRUE)

build_ensemble <- function(aeme_data = NULL,
                           config = NULL,
                           model = c("dy_cd", "glm_aed", "gotm_wet"),
                           mod_ctrls,
                           inf_factor = c("glm_aed" = 1, "dy_cd" = 1,
                                          "gotm_wet" = 1),
                           outf_factor = c("glm_aed" = 1, "dy_cd" = 1,
                                           "gotm_wet" = 1),
                           ext_elev = 0,
                           use_bgc = TRUE,
                           use_lw = FALSE,
                           hum_type = 3,
                           path = "."
                           ) {

  if (is.null(aeme_data) & is.null(config)) {
    stop("Either 'aeme_data' or 'config' must be supplied.")
  }

  #--- metadata
  lvl <- NULL
  inf <- list()
  outf <- list()
  # these variables will be simulated
  if (use_bgc) {
    inf_vars <- mod_ctrls |>
      dplyr::filter(simulate == 1,
                    !name %in% c("RAD_extc", "PHS_tp", "NIT_pin", "NIT_tn",
                                 "PHY_tchla")) |>
      dplyr::pull(name)
  } else {
    inf_vars <- c("HYD_temp", "CHM_salt")
  }


  if (!is.null(aeme_data)) {
    message("Building simulation for ", aeme_data@lake$name, " [", Sys.time(),
            "]")
    lake_dir <- file.path(path, paste0(aeme_data@lake$id, "_",
                                      tolower(aeme_data@lake$name)))
    date_range <- as.Date(c(aeme_data@time[["start"]],
                            aeme_data@time[["stop"]]))
    spin_up <- aeme_data@time[["spin_up"]]

    if (!is.null(aeme_data@lake[["shape"]])) {
      lake_shape <- aeme_data@lake[["shape"]]
    } else {
      coords <- data.frame(lat = aeme_data@lake[["latitude"]],
                           lon = aeme_data@lake[["longitude"]])
      coords_sf <- sf::st_as_sf(coords, coords = c("lon", "lat"), crs = 4326)
      r <- sqrt(aeme_data@lake[["area"]] / pi)
      lake_shape <- sf::st_buffer(coords_sf, r)
    }
    elevation <- aeme_data@lake[["elevation"]]
    # Hypsograph ----
    if (is.null(aeme_data@input[["hypsograph"]])) {
      stop("Hypsograph is not present. This is required to build the models.")
    }
    hyps <- aeme_data@input[["hypsograph"]]

    if (is.null(aeme_data@observations[["level"]])) {
      stop("Lake level is not present. This is required to build the models.")
    }
    lvl <- aeme_data@observations[["level"]]

    # Initial profile ----
    if (!is.null(aeme_data@input[["init_profile"]])) {

    } else {
      init_prof <- data.frame(depth = c(0,
                                        floor(max(hyps$elev) - min(hyps$elev))),
                              temperature = c(10, 10),
                              salt = c(0, 0))
    }

    # Inflow ----
    if (!is.null(aeme_data@inflows[["data"]])) {
      for (i in 1:length(aeme_data@inflows[["data"]])) {
        inf[[names(aeme_data@inflows[["data"]])[i]]] <- aeme_data@inflows[["data"]][[i]]
        if (any(!inf_vars %in% names(inf[[i]]))) {
          stop("missing state variables in inflow tables")
        }

        inf[[i]] <- inf[[i]] |>
          dplyr::select(all_of(c("Date","HYD_flow", inf_vars)))
      }
    }
    inf_factor <- aeme_data@inflows[["factor"]]

    #--- meteorology
    if (is.null(file.path(path, aeme_data@input[["meteo"]]))) {
      stop(aeme_data@input[["meteo"]], " does not exist. Check file path.")
    }
    met <- aeme_data@input[["meteo"]]

    Kw <- aeme_data@input[["Kw"]]

    # Outflow ----
    if (!is.null(aeme_data@outflows[["data"]])) {
      for(i in 1:length(aeme_data@outflows[["data"]])) {
        outf[[names(aeme_data@outflows[["data"]])[i]]] <-
          aeme_data@outflows[["data"]][[i]]
      }
    }

    outf_factor <- aeme_data@outflows[["factor"]]
    lakename <- tolower(aeme_data@lake[["name"]])

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
    elevation <- config[["lake"]][["elevation"]]
    # Hypsograph ----
    if (!file.exists(file.path(path, config[["input"]][["hypsograph"]]))) {
      stop(config[["input"]][["hypsograph"]],
           " does not exist. Check file path.")
    }
    hyps <- utils::read.csv(file.path(path, config[["input"]][["hypsograph"]]))

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
                              temperature = c(10, 10),
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

  #--------------------------
  dir.create(lake_dir, showWarnings = FALSE)

  sf::sf_use_s2(FALSE)
  coords.xyz <- c(lake_shape |>
                    sf::st_transform(3857) |>
                    sf::st_geometry() |>
                    sf::st_centroid() |>
                    sf::st_transform(4326) |>
                    sf::st_coordinates() |>
                    as.numeric(), elevation)
  sf::sf_use_s2(TRUE)

  met <- met |>
    dplyr::filter(Date >= (date_range[1]),
           Date <= date_range[2])
  met <- met |>
    dplyr::mutate(Date = as.Date(Date)) |>
    dplyr::mutate(MET_pprain = MET_pprain / 1000,
                  MET_ppsnow = MET_ppsnow / 1000) |>
    expand_met(coords.xyz = coords.xyz, print.plot = FALSE)

  # add snow if needs be
  if (!any(grepl("snow", colnames(met)))) {
    met$MET_ppsnow <- 0
  }

  #--- outflows
  if (length(outf) == 1) {
    outf <- outf[[1]]
    if(ncol(outf) > 2) {
      dy_cd_outf <- outf |>
        dplyr::select(Date, outflow_dy_cd) |>
        dplyr::rename(outflow = outflow_dy_cd)
      glm_aed_outf <- outf |>
        dplyr::select(Date, outflow_glm_aed) |>
        dplyr::rename(outflow = outflow_glm_aed)
      gotm_wet_outf <- outf |>
        dplyr::select(Date, outflow_gotm_wet) |>
        dplyr::rename(outflow = outflow_gotm_wet)
    } else {
      dy_cd_outf <- outf
      glm_aed_outf <- outf
      gotm_wet_outf <- outf
    }
  } else {
    dy_cd_outf <- NULL
    glm_aed_outf <- NULL
    gotm_wet_outf <- NULL
  }


  gps <- coords.xyz[1:2]

  # use_bgc <- config[["bgc_model"]][["use"]]

  if ("dy_cd" %in% model) {
    #--- configure DYRESM-CAEDYM
    dates.dy <- c(date_range[1] - spin_up[["dy_cd"]], date_range[2]) |>
      `names<-`(NULL)
    build_dycd(lakename, mod_ctrls = mod_ctrls, date_range = dates.dy,
               gps = gps, hyps = hyps, lvl = lvl,
               inf = inf, outf = dy_cd_outf, met = met,
               lake_dir = lake_dir, init_prof = init_prof,
               inf_factor = inf_factor[["dy_cd"]],
               outf_factor = outf_factor[["dy_cd"]],
               Kw = Kw, ext_elev = ext_elev,
               use_bgc = use_bgc)
    # run_dy_cd(lake_dir = lake_dir, bin_path = here::here("data", "bin"))
  }
  if ("glm_aed" %in% model) {
    dates.glm <- c(date_range[1] - spin_up[["glm_aed"]], date_range[2]) |>
      `names<-`(NULL)
    build_glm(lakename, mod_ctrls = mod_ctrls, date_range = dates.glm,
              lake_shape = lake_shape, gps = gps,
              hyps = hyps, lvl = lvl, init_prof = init_prof,
              inf = inf, outf = glm_aed_outf, met = met,
              lake_dir = lake_dir,
              inf_factor = inf_factor[["glm_aed"]],
              outf_factor = outf_factor[["glm_aed"]],
              Kw = Kw, ext_elev = ext_elev,
              use_bgc = use_bgc, use_lw = use_lw)
    # run_glm_aed(lake_dir = lake_dir, bin_path = here::here("data", "bin"),
    #             verbose = TRUE)
  }
  if("gotm_wet" %in% model) {
    dates.gotm = c(date_range[1] - spin_up[["gotm_wet"]], date_range[2]) |>
      `names<-`(NULL)
    depth <- max(hyps$elev) - min(hyps$elev)
    if (depth < 3) {
      div <- 0.1
    } else {
      div <- 0.33
    }
    nlev <- ceiling((depth) / div)
    build_gotm(lakename, mod_ctrls = mod_ctrls, date_range = dates.gotm,
               lake_shape = lake_shape, gps = gps, lake_dir = lake_dir,
               hyps = hyps, lvl = lvl, init_prof = init_prof,
               inf = inf, outf = gotm_wet_outf, met = met,
               inf_factor = inf_factor[["gotm_wet"]],
               outf_factor = outf_factor[["gotm_wet"]],
               Kw = Kw, nlev = nlev,
               use_bgc = use_bgc, hum_type = hum_type)
    # run_gotm_wet(lake_dir = lake_dir, bin_path = here::here("data", "bin"),
    #              verbose = TRUE)

  }
}
