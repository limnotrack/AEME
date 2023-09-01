#' Build DYRESM-CAEDYM configuration
#'
#' @param lakename string; for lake name
#' @param mod_ctrls dataframe; with model controls
#' @param date_range vector; of dates
#' @param gps vector; of latitude and longitude
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
#' @param use_bgc boolean; switch for biogrochemistry model.
#'
#' @importFrom dplyr filter pull slice
#'
#' @return directory with DY-CD configuration.
#' @noRd

build_dycd <- function(lakename, mod_ctrls, date_range, gps,
                       inf, outf, met, hyps, lvl, lake_dir,
                       inf_factor = 1.0, outf_factor = 1.0,
                       ext_elev = 0, Kw, init_prof, init_depth, use_bgc) {

  message(paste0("Building DYRESM-CAEDYM for lake ", lakename))

  #------- MODEL FOLDERS SETUP --------
  verDY <- 3.1
  verCD <- 3.1

  path.dy <- file.path(lake_dir, "dy_cd")
  # print(path.dy)
  # if running caedym, setup the dummy folders
  if ( !is.null(verCD) ) {

    dir.create(file.path(path.dy,'files'), recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(path.dy,'files/sediment'), showWarnings = FALSE)
  }

  dy_file <- file.path(path.dy, "dyresm3p1.par")

  if (!file.exists(dy_file)) {
    dy_file <- system.file("extdata/dy_cd/dyresm3p1.par",
                           package = "AEME")
    config_dir <- dirname(dy_file)
    fils <- list.files(config_dir, full.names = TRUE)
    file.copy(fils, file.path(path.dy, basename(fils)))
    message("Copied in DYRESM par file")
  }

  # make the batch file to run the model
  # make_DYbat(lakename, runCD = T, filePath = path.dy, pause = T)

  depth <- max(hyps$elev) - min(hyps$elev)
  if (depth < 5) {
    minLyrThk <- 0.1
    maxLyrThk <- 0.3
  } else {
    minLyrThk <- 0.1
    maxLyrThk <- 0.8
  }


  #----- CONFIGURATION ------
  vars.dy <- mod_ctrls |>
    dplyr::filter(simulate == 1) |>
    dplyr::pull(name) |>
    rename_modelvars(type_output = "dy_cd")

  # make the .cfg file
  make_DYCDcfg(lakename = lakename,
               date_range = date_range,
               verDY = verDY, runCD = use_bgc,
               EXTC = Kw,
               minLyrThk = minLyrThk,
               maxLyrThk = maxLyrThk,
               simVars = vars.dy,
               filePath = path.dy)

  # make the .con file
  make_DYCDcon(lakename = lakename,
               verCD = verCD,
               simVars = vars.dy,
               nFix = FALSE,
               filePath = path.dy)


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
      `names<-`(c("elev","area")) |>
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
  outHeights <- min(bathy_fmt[, 1]) + 0.5
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
  surfElev <- init_depth
  if (!is.null(lvl)) {
    # surfElev <- mean(lvl[,2], na.rm = TRUE)
    z_max <- mean(lvl[, 2]) - min(hyps$elev)
    # get starting depth
    z.start <- round((dplyr::filter(lvl, Date == date_range[1]) |>
                        dplyr::pull(lvlwtr)) - min(hyps$elev), 2)
  } else {
    # surfElev <- max(hyps$elev)
    z_max <- max(hyps$elev) - min(hyps$elev)
  }
  if (length(z.start) == 0) {
    z.start <- max(hyps$elev)
  }

  make_DYstg(lakename = lakename,
             latitude = gps[2],
             bathy = bathy_fmt,
             surfElev = surfElev,
             infNames = names(inf),
             outNames = outNames,
             filePath = path.dy,
             outHeights = outHeights
             )



  #------ INFLOWS -------
  if (!is.null(inf)) {
    for (i in 1:length(inf)) {
      # format the tables
      colnames(inf[[i]]) <- rename_modelvars(input = names(inf[[i]]),
                                             type_output = "dy_cd")
    }
  } else {
    inf <- list("EMPTY" = data.frame(
      # InfNum = 1,
      Date = seq.Date(date_range[1], date_range[2], by = 1),
      VOL = 0, TEMPTURE = 10, SALINITY = 0, DO = 0, PO4 = 0, DOPL = 0,
      POPL = 0, PIP = 0,	NH4 = 0, NO3 = 0, DONL = 0, PONL = 0, DOCL = 0,
      POCL = 0, SiO2 = 0, CYANO = 0, CHLOR = 0, FDIAT = 0, SSOL1 = 0
      )
      )
  }
  # write the model input file
  make_DYinf(lakename, info = "test", infList = inf, filePath = path.dy,
             inf_factor = inf_factor)





  #----- OUTFLOWS -----
  if (is.null(outf)) {
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
             filePath = path.dy)


  #----- INITIALISATION -----
  # DYRESM
  # write the initial profile (auto-retrieves correct starting depth)
  make_DYpro(lakename = lakename, startSim = date_range[1], lvlBottom = 0,
             lvlStart = init_depth, verDY = verDY,
             obsTable = init_prof,
             tmpStart = 10, filePath = path.dy)

  # set the variables to output from dycd
  initials <- mod_ctrls |>
    dplyr::filter(!is.na(initial_wc),
           simulate == 1 | name == "NCS_ss2", # must initialise both SSOL groups!?!
           !name %in% c("Date", "HYD_flow", "HYD_temp", "HYD_dens",
                        "CHM_salt", "RAD_par", "RAD_extc", "RAD_secchi",
                        "PHS_tp","NIT_tn","PHY_tchla")) |>
    dplyr::select(c("name", "initial_wc", "initial_sed"))

  # write the .int file
  make_DYCDint(lakename,
               intVars = rename_modelvars(initials$name, type_output = "dy_cd"),
               wcVals = initials$initial_wc,
               sedVals = initials$initial_sed,
               verCD,
               filePath = path.dy)



}
