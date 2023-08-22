#' Build model configuration directories
#'
#' Configure an ensemble of lake model simulations from basic set of inputs.
#'
#' @param config list; loaded via `config <- configr::read.config("aeme.yaml")`
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
#' @param dir filepath; where input files are located relative to `config`.
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
#' dir <- file.path(tmpdir, "lake")
#' config <- configr::read.config(file.path(dir, "aeme.yaml"))
#' mod_ctrls <- read.csv(file.path(dir, "model_controls.csv"))
#' inf_factor = c("glm_aed" = 1)
#' outf_factor = c("glm_aed" = 1)
#' model <- c("glm_aed")
#' build_ensemble(dir = dir, config = config, model = model,
#'                mod_ctrls = mod_ctrls, inf_factor = inf_factor, ext_elev = 5,
#'                use_bgc = FALSE, use_lw = TRUE)

build_ensemble <- function(config,
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
                           dir = "."
                           ) {

  #--- metadata
  message("Building simulation for ", config$location$name, " [", Sys.time(), "]")

  # Load Rdata
  utils::data("key_naming", package = "AEME", envir = environment())

  #--------------------------
  lake_dir <- file.path(dir, paste0(config$location$lake_id,"_",
                               tolower(config$location$name)))
  dir.create(lake_dir, showWarnings = FALSE)

  # ensure date range are Dates
  date_range = as.Date(c(config[["time"]][["start"]],
                         config[["time"]][["stop"]]))
  spin_up <- config[["time"]][["spin_up"]]


  if (!is.null(config[["location"]][["shape_file"]])) {
    lake_shape <- sf::st_read(file.path(dir,
                                        config[["location"]][["shape_file"]]))
  } else {
    coords <- data.frame(lat = config[["location"]][["latitude"]],
                         lon = config[["location"]][["longitude"]])
    coords_sf <- sf::st_as_sf(coords, coords = c("lon", "lat"), crs = 4326)
    r <- sqrt(config[["location"]][["area"]] / pi)
    lake_shape <- sf::st_buffer(coords_sf, r)
  }

  sf::sf_use_s2(FALSE)
  coords.xyz <- c(lake_shape |>
                    sf::st_transform(3857) |>
                    sf::st_geometry() |>
                    sf::st_centroid() |>
                    sf::st_transform(4326) |>
                    sf::st_coordinates() |>
                    as.numeric(), config[["location"]][["elevation"]])
  sf::sf_use_s2(TRUE)

  # these variables will be simulated
  if (use_bgc) {
    inf_vars <- mod_ctrls |>
      dplyr::filter(simulate == 1,
             !name %in% c("RAD_extc", "PHS_tp", "NIT_pin", "NIT_tn", "PHY_tchla")) |>
      dplyr::pull(name)
  } else {
    inf_vars <- c("HYD_temp", "CHM_salt")
  }

  # Hypsograph ----
  if (!file.exists(file.path(dir, config[["input"]][["hypsograph"]]))) {
    stop(config[["input"]][["hypsograph"]], " does not exist. Check file path.")
  }
  # hyps <- readr::read_csv(config[["input"]][["hypsograph"]],
  #                         show_col_types = FALSE)
  hyps <- utils::read.csv(file.path(dir, config[["input"]][["hypsograph"]]))
  # Water level ----
  lvl <- NULL
  if (file.exists(file.path(dir, config[["observations"]][["level"]]))) {
    # lvl <- readr::read_csv(config[["observations"]][["level"]],
    #                        show_col_types = FALSE)
    lvl <- utils::read.csv(file.path(dir, config[["observations"]][["level"]]))
  }

  # Initial profile ----
  if (!is.null(config[["input"]][["init_temp_profile"]])) {

  } else {
    init_prof <- data.frame(depth = c(0, floor(max(hyps$elev) - min(hyps$elev))),
                       temperature = c(10, 10),
                       salt = c(0, 0))
  }

  # Inflow ----
  inf <- list()
  if (config[["inflows"]][["use"]]) {
    for(i in 1:length(config[["inflows"]][["file"]])) {

      inf[[names(config[["inflows"]][["file"]])[i]]] <-
        utils::read.csv(file.path(dir, config[["inflows"]][["file"]][[i]]))
      if(any(!inf_vars %in% names(inf[[i]]))) {
        stop("missing state variables in inflow tables")
      }

      inf[[i]] <- inf[[i]] |>
        dplyr::select(all_of(c("Date","HYD_flow",inf_vars)))
    }
  }
  inf_factor <- config[["inflows"]][["factor"]]

  #--- meteorology
  if (!file.exists(file.path(dir, config[["input"]][["meteo"]]))) {
    stop(config[["input"]][["meteo"]], " does not exist. Check file path.")
  }
  # met <- readr::read_csv(config[["input"]][["meteo"]], show_col_types = FALSE)
  met <- utils::read.csv(file.path(dir, config[["input"]][["meteo"]]))
  met <- met |>
    dplyr::filter(Date >= (date_range[1]),
           Date <= date_range[2])
  met <- met |>
    dplyr::mutate(Date = as.Date(Date)) |>
    dplyr::mutate(MET_pprain = MET_pprain / 1000, MET_ppsnow = MET_ppsnow / 1000) |>
    expand_met(coords.xyz = coords.xyz, print.plot = FALSE)
  summary(met)

  # add snow if needs be
  if (!any(grepl("snow", colnames(met)))) {
    met$MET_ppsnow <- 0
  }

  #--- outflows
  Kw <- config[["input"]][["Kw"]]

  if(config[["outflows"]][["use"]]) {
    outf <- utils::read.csv(file.path(dir, config[["outflows"]][["file"]]))
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
  outf_factor <- config[["outflows"]][["factor"]]

  lakename <- tolower(config[["location"]][["name"]])

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
              lake_dir = lake_dir, config_dir = config_dir,
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
