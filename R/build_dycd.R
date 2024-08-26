#' Build DYRESM-CAEDYM configuration
#'
#' @param lakename string; for lake name
#' @param model_controls dataframe; with model controls
#' @param date_range vector; of dates
#' @param lat numeric; latitude
#' @param lon numeric; longitude
#' @param inf list of inflows
#' @param outf list; of outflows
#' @param met dataframe; of meteorological data
#' @param hyps dataframe; with hypsography data
#' @param lvl dataframe; with lake level data
#' @param lake_dir filepath; for outputting model configuration
#' @param inf_factor numeric; scaling factor for inflows
#' @param outf_factor numeric; scaling factor for outflows
#' @param ext_elev numeric; to extend the elevation of the hypsograph
#' @param Kw numeric; light extinction coefficient
#' @param init_prof dataframe; of initial profile with depth, temperature and
#'  salinity.
#' @param init_depth numeric; depth at which to start the simulation.
#' @param use_bgc logical; switch for biogrochemistry model.
#'
#' @importFrom dplyr filter pull slice
#'
#' @return directory with DY-CD configuration.
#' @noRd

build_dycd <- function(lakename, model_controls, date_range, lat, lon,
                       inf, outf, met, hyps, lvl, lake_dir,
                       inf_factor = 1.0, outf_factor = 1.0,
                       ext_elev = 0, Kw, init_prof, init_depth, use_bgc, use_lw,
                       overwrite_cfg = TRUE) {

  message(paste0("Building DYRESM-CAEDYM for lake ", lakename))

  #------- MODEL FOLDERS SETUP --------
  verDY <- 3.1
  verCD <- 3.1

  path.dy <- file.path(lake_dir, "dy_cd")

  par_file <- file.path(lake_dir, "dy_cd", "dyresm3p1.par")
  cfg_file <- file.path(lake_dir, "dy_cd", paste0(lakename, ".cfg"))
  con_file <- file.path(lake_dir, "dy_cd", paste0(lakename, ".con"))

  # print(path.dy)
  # if running caedym, setup the dummy folders
  if ( !is.null(verCD) ) {

    dir.create(file.path(path.dy,'files'), recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(path.dy,'files/sediment'), showWarnings = FALSE)
  }

  par_file <- file.path(path.dy, "dyresm3p1.par")

  if (!file.exists(par_file)) {
    par_file <- system.file("extdata/dy_cd/dyresm3p1.par",
                           package = "AEME")
    config_dir <- dirname(par_file)
    fils <- list.files(config_dir, full.names = TRUE)
    file.copy(fils, file.path(path.dy, basename(fils)))
    overwrite_cfg <- TRUE
    message("Copied in DYRESM par file")
  }

  # make the batch file to run the model
  # make_DYbat(lakename, runCD = T, filePath = path.dy, pause = T)

  depth <- max(hyps$elev) - min(hyps$elev)
  sub_layers <- get_model_layers(depth = depth)
  minLyrThk <- min(sub_layers$h)
  maxLyrThk <- max(sub_layers$h)

  #----- CONFIGURATION ------
  vars.dy <- model_controls |>
    dplyr::filter(simulate) |>
    dplyr::pull(var_aeme) |>
    rename_modelvars(type_output = "dy_cd")

  if (overwrite_cfg | !file.exists(cfg_file)) {
    message("Writing DYRESM configuration")
    # make the .cfg file
    make_DYCDcfg(lakename = lakename,
                 date_range = date_range,
                 verDY = verDY, runCD = use_bgc,
                 EXTC = Kw,
                 minLyrThk = minLyrThk,
                 maxLyrThk = maxLyrThk,
                 simVars = vars.dy,
                 filePath = path.dy)

  }

  if (overwrite_cfg | !file.exists(con_file)) {
    message("Writing DYRESM control file")
    # make the .con file
    make_DYCDcon(lakename = lakename,
                 verCD = verCD,
                 simVars = vars.dy,
                 nFix = FALSE,
                 filePath = path.dy)
  }



  #---------- MODEL SETUP ------------

  #----- STORAGE -----
  if (nrow(hyps) > 20) {
    message("Downsampling bathymetry")
    hyps <- hyps |>
      dplyr::slice(c(seq(1, (nrow(hyps)-1), round(nrow(hyps) / 20)),
                     nrow(hyps)))
  }

  # extend the bathymetry above crest for temporary storage as required by DYRESM (sometime)
  max_d <- max(hyps$elev)
  if (ext_elev != 0) {
    bathy_fmt <- hyps |>
      dplyr::arrange(elev) |>
      # use slope to extend hyps by 5 m
      bathy_extrap(0.75, new.max = max_d + ext_elev) |>
      dplyr::mutate(elev = round(elev, 2))
  } else {
    bathy_fmt <- hyps
  }


  # print(bathy_fmt)

  # outHeights <- c(mean(lvl[,2]) - 2,
                  # (0.75 * (mean(lvl[,2]) - min(bathy_fmt[, 1])) + min(bathy_fmt[, 1])))
  outHeights <- min(bathy_fmt[["elev"]]) + 0.5
  # print(outHeights)
  # outHeights <- round(min(outHeights[outHeights > min(bathy_fmt[, 1])]), 2)

  if (!is.null(outf)) {
    outNames <- names(outf)
    outHeights <- rep(outHeights, length(names(outf)))
  } else {
    outNames <- "EMPTY"
  }

  # write
  z.start <- c()
  surfElev <- init_depth + min(hyps$elev)
  if (!is.null(lvl)) {
    # surfElev <- mean(lvl[,2], na.rm = TRUE)
    z_max <- mean(lvl[["value"]]) - min(hyps$elev)
    # get starting depth
    z.start <- round((dplyr::filter(lvl, Date == date_range[1] &
                                      var_aeme == "LKE_lvlwtr") |>
                        dplyr::pull(value)) - min(hyps$elev), 2)
  } else {
    # surfElev <- max(hyps$elev)
    z_max <- max(hyps$elev) - min(hyps$elev)
  }
  if (length(z.start) == 0) {
    z.start <- max(hyps$elev)
  }

  make_DYstg(lakename = lakename,
             latitude = lat,
             bathy = bathy_fmt,
             surfElev = surfElev,
             infNames = names(inf),
             outNames = outNames,
             filePath = path.dy,
             outHeights = outHeights
             )



  #------ INFLOWS -------
  # write the model input file
  make_DYinf(lakename, info = "test", infList = inf, filePath = path.dy,
             date_range = date_range, inf_factor = inf_factor)





  #----- OUTFLOWS -----
  if (length(outf) == 0) {
    outf <- list(outflow = data.frame(Date = seq.Date(date_range[1],
                                                      date_range[2], by = 1),
                                      outflow = 0))
  }

  make_DYwdr(lakename, info = "built for ensemble", wdrData = outf,
             filePath = path.dy, outf_factor = outf_factor)


  #----- METEOROLOGY -----
  make_DYmet(lakename, info = "test", verDY = verDY,
             obsMet = met, infRain = FALSE, wndType = 0,
             metHeight = 15,
             z_max = z_max,
             filePath = path.dy,
             use_lw = use_lw)


  #----- INITIALISATION -----
  # DYRESM
  # write the initial profile (auto-retrieves correct starting depth)
  make_DYpro(lakename = lakename, startSim = date_range[1], lvlBottom = 0,
             lvlStart = init_depth, verDY = verDY,
             obsTable = init_prof,
             tmpStart = 10, filePath = path.dy)

  # set the variables to output from dycd
  initials <- model_controls |>
    dplyr::filter(!is.na(initial_wc),
           simulate | var_aeme == "NCS_ss2", # must initialise both SSOL groups!?!
           !var_aeme %in% c("Date", "HYD_flow", "HYD_temp", "HYD_dens",
                        "CHM_salt", "RAD_par", "RAD_extc", "RAD_secchi",
                        "PHS_tp","NIT_tn","PHY_tchla")) |>
    dplyr::select(c("var_aeme", "initial_wc", "initial_sed"))

  # write the .int file
  make_DYCDint(lakename,
               intVars = rename_modelvars(initials$var_aeme, type_output = "dy_cd"),
               wcVals = initials$initial_wc,
               sedVals = initials$initial_sed,
               verCD,
               filePath = path.dy)



}
