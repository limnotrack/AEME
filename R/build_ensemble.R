#' Configure an ensemble of lake model simulations from basic set of inputs
#'
#' @param lid character; of ID number used for the lake
#' @param lake sf polygon of the lake shoreline.
#' @param date_range vector; of start and end dates
#' @param hyps dataframe; of the hypsograph. Required columns are `elev` and
#' `area`.
#' @param lvl dataframe; of average surface water level or time-series of daily
#'  water level (masl).
#' @param met dataframe of meteorological data.
#' @param inf list; of dataframe containing inflows. Each inflow should be named
#' in the list.
#' @param wdr dataframe; containing outflow for the lake. Date plus a col for
#' each outflow.
#' @param model vector; of models to be used. Can be `dy_cd`, `glm_aed`,
#'  `gotm_wet`.
#' @param spinups vector; containing the number of days in the simulation to be
#'  allocated to a spin-up period. Needs to be named using the models.
#' @param key.cfg dataframe; of configuration loaded from
#'  "key_ensemble_Default.xlsx".
#' @param key_naming dataframe; of corresponding names for the different models.
#' @param inf.factor vector; containing factor to multiple the inflows. Needs to
#' be named according to the model.
#' @param wdr.factor vector; containing factor to multiple the outflows. Needs to
#' be named according to the model.
#' @param ext_elev numeric; extension in elevation for the hypsogrph in (m).
#' @param config_dir filepath; to directory which contains the model
#' configuration files.
#' @param use_bgc boolean; switch to use the biogeochemical model.
#'
#' @return builds the model ensemble configuration.
#'
#' @importFrom magrittr %>% %T>%
#' @importFrom sf sf_use_s2 st_transform st_centroid st_coordinates st_buffer
#'
#' @export
#'
#' @examples
build_ensemble <- function(config,
                           model = c("dy_cd", "glm_aed", "gotm_pclake"),
                           spinups = c("glm_aed" = 0, "dy_cd" = 0, "gotm_pclake" = 365, "gotm_wet" = 365),
                           # key.cfg = data.frame(readxl::read_excel("data/config_files/key_ensemble_Default.xlsx")),
                           # key_naming,
                           inf.factor = c("glm_aed" = 1, "dy_cd" = 1,"gotm_wet" = 1),
                           wdr.factor = c("glm_aed" = 1, "dy_cd" = 1,"gotm_wet" = 1),
                           ext_elev = 0,
                           config_dir = "data/config_files/",
                           use_bgc = TRUE,
                           use_lw = FALSE,
                           hum_type = 3,
                           dir = "."
                           ) {

  #--- metadata

  message("Building simulation for ", config$location$name, " [", Sys.time(), "]")

  #--------------------------
  lake_dir <- file.path(paste0(config$location$lake_id,"_",
                               config$location$name)) %T>%
    dir.create(showWarnings = FALSE)

  # ensure date range are Dates
  date_range = as.Date(c(config[["time"]][["start"]], config[["time"]][["stop"]]))

  if (!is.null(config[["location"]][["shape_file"]])) {
    lake_shape <- sf::st_read(config[["location"]][["shape_file"]])
  } else {
    coords <- data.frame(lat = config[["location"]][["latitude"]],
                         lon = config[["location"]][["longitude"]])
    coords_sf <- sf::st_as_sf(coords, coords = c("lon", "lat"), crs = 4326)
    r <- sqrt(config[["location"]][["area"]] / pi)
    lake_shape <- sf::st_buffer(coords_sf, r)
  }

  sf::sf_use_s2(FALSE)
  coords.xyz = c(lake_shape %>%
                   sf::st_transform(4326) %>%
                   sf::st_centroid() %>%
                   sf::st_coordinates() %>%
                   as.numeric(), config[["location"]][["elevation"]])
  sf::sf_use_s2(TRUE)

  # these variables will be simulated
  if (use_bgc) {
    inf_vars <- key.cfg %>%
      filter(simulate == 1,
             !name %in% c("RAD_extc", "PHS_tp", "NIT_pin", "NIT_tn", "PHY_tchla")) %>%
      pull(name)
  } else {
    inf_vars <- c("HYD_temp", "CHM_salt")
  }


  #--- filter inflow table to simulated variables
  if(!is.null(inf)) {
    for(i in 1:length(inf)) {

      if(any(!inf_vars %in% names(inf[[i]]))) {
        stop("missing state variables in inflow tables")
      }

      inf[[i]] %<>%
        select(all_of(c("Date","HYD_flow",inf_vars)))
    }
  }

  #--- meteorology
  met %<>%
    filter(Date >= (date_range[1] - max(spinups)),
           Date <= date_range[2])

  # add snow if needs be
  if(!any(grepl("snow", colnames(met)))) { met$MET_ppsnow <- 0}


  #--- outflows
  # wdr.heights = wdr %>% select(-1) %>% colnames() %>% gsub("^.*_","",.) %>% as.numeric()
  Kw = lake$Kw

  if(!is.null(wdr)) {
    if(ncol(wdr) > 2) {
      dy_cd_wdr <- wdr %>%
        select(Date, outflow_dy_cd) %>%
        rename(outflow = outflow_dy_cd)
      glm_aed_wdr <- wdr %>%
        select(Date, outflow_glm_aed) %>%
        rename(outflow = outflow_glm_aed)

      gotm_wet_wdr <- wdr %>%
        select(Date, outflow_gotm_wet) %>%
        rename(outflow = outflow_gotm_wet)
    } else {
      dy_cd_wdr <- wdr
      glm_aed_wdr <- wdr
      gotm_wet_wdr <- wdr
    }
  } else {
    dy_cd_wdr <- NULL
    glm_aed_wdr <- NULL
    gotm_wet_wdr <- NULL
  }

  if("dy_cd" %in% model) {
  #--- configure DYRESM-CAEDYM
  dates.dy = c(date_range[1]-spinups["dy_cd"],date_range[2]) %>%
    `names<-`(NULL)
  build_dycd(lakename, key.cfg = key.cfg, date_range = dates.dy, gps = coords.xyz[1:2],
             hyps = hyps, lvl = lvl,
             inf = inf, wdr = dy_cd_wdr, met = met,
             lake_dir = lake_dir, config_dir = config_dir,
             inf.factor = inf.factor[["dy_cd"]],
             wdr.factor = wdr.factor[["dy_cd"]],
             Kw = Kw, ext_elev = ext_elev,
             key_naming = key_naming, use_bgc = use_bgc)
  # run_dy_cd(lake_dir = lake_dir, bin_path = here::here("data", "bin"))
  }
  if("glm_aed" %in% model) {
    dates.glm = c(date_range[1]-spinups["glm_aed"], date_range[2]) %>%
      `names<-`(NULL)
    build_glm(lakename, key.cfg = key.cfg, date_range = dates.glm,
              shoreline = lake, gps = coords.xyz[1:2],
              hyps = hyps, lvl = lvl,
              inf = inf, wdr = glm_aed_wdr, met = met,
              lake_dir = lake_dir, config_dir = config_dir,
              inf.factor = inf.factor[["glm_aed"]],
              wdr.factor = wdr.factor[["glm_aed"]],
              Kw = Kw, ext_elev = ext_elev,
              key_naming = key_naming, use_bgc = use_bgc, use_lw = use_lw)
    # run_glm_aed(lake_dir = lake_dir, bin_path = here::here("data", "bin"),
    #             verbose = TRUE)
  }
  if("gotm_wet" %in% model) {
    dates.gotm = c(date_range[1] - spinups["gotm_wet"], date_range[2]) %>%
      `names<-`(NULL)
    depth <- max(hyps$elev) - min(hyps$elev)
    if (depth < 3) {
      div <- 0.1
    } else {
      div <- 0.33
    }
    nlev <- ceiling((depth) / div)
    build_gotm(lakename, key.cfg = key.cfg, date_range = dates.gotm,
               lake, gps = coords.xyz[1:2],
               hyps = hyps, lvl = lvl,
               inf = inf, wdr = gotm_wet_wdr, met = met,
               lake_dir = lake_dir, config_dir = config_dir, version = "wet",
               inf.factor = inf.factor[["gotm_wet"]],
               wdr.factor = wdr.factor[["gotm_wet"]],
               Kw = Kw, nlev = nlev,
               key_naming = key_naming, use_bgc = use_bgc, hum_type = hum_type)
    # run_gotm_wet(lake_dir = lake_dir, bin_path = here::here("data", "bin"),
    #              verbose = TRUE)

  }
}

