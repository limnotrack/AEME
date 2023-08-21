#' Make a cfg file for dyresm(caedym) simulation
#' 
#' @param lakename name of lake being simulated
#' @param date_range vector of start and end dates e.g., c(YYYY-MM-DD, YYYY-MM-DD)
#' @param verDY dyresm version number
#' @param runCD turn CAEDYM on (TRUE) or off (FALSE)
#' @param EXTC light extinction coefficient (only used if runCD == FALSE)
#' @param minLyrThk minimum layer thickness (default 0.2)
#' @param maxLyrThk maximum layer thickness (default 3.0)
#' @param simVars variables to be simulated (default = DYRESM vars only)
#' @param filePath dir of model, relative to working dir
#' @keywords inputs
#' 
#' @importFrom lubridate year
#' 

make_DYCDcfg <-  function(lakename = "unknown", 
                          date_range, verDY = 3.1, 
                          runCD = FALSE, 
                          EXTC = 0.2, 
                          minLyrThk = 0.2, 
                          maxLyrThk = 0.3, 
                          simVars, filePath) {
  
  ######## set defaults ########
  
  if (missing(lakename)){
    lakename <- "unknownDY"
  }
  if (missing(runCD)){
    runCD <- FALSE
  }
  if (missing(EXTC)){
    EXTC <- 0.2
  }
  if (missing(minLyrThk)){
    minLyrThk <- 0.2
  }
  if (missing(maxLyrThk)){
    maxLyrThk <- 3.0
  }
  if (missing(simVars)){
    simVars <- c("TEMPTURE", "DENSITY", "SALINITY")  
  }
  if (missing(filePath)){
    filePath <- ""
  }
  
  ######## end set defaults ########
  
  
  ############## main function ###############  
  
  startSim <- as.Date(date_range[1], origin = '1970-01-01')
  endSim   <- as.Date(date_range[2], origin = '1970-01-01')
  
  cfgStart <- paste0(lubridate::year(startSim),strftime(startSim, format = "%j") )
  nDays    <- as.numeric(as.Date(strptime(endSim, "%Y-%m-%d")) - as.Date(strptime(startSim, "%Y-%m-%d")) ) + 1
  
  if (runCD == TRUE) {
    
    runCD <- ".TRUE."
    # number of output variables for cfg file
    nVars <- length(simVars)
    # collapse simVars for cfg input
    simVars <- paste(simVars, collapse = " ")
    
    print(simVars)
    
  } else {
    runCD <- ".FALSE."
    simVars <- c("TEMPTURE", "DENSITY", "SALINITY")
    nVars <- length(simVars)
    # collapse simVars for cfg input
    simVars <- paste(simVars, collapse = " ")
    
  }
  
  #<<<<<< make the file! >>>>>>>#
  
  ### make a header to print to file
  f <- file(paste0(filePath,"/",lakename,".cfg"),"w")
  
  if (verDY < 3) {
    writeLines("<#4>", f)
  } else {
    writeLines("<#5>", f)    
  }
  
  writeLines(paste0("! DYRESM-CAEDYM configuration file for ",lakename),f)
  
  writeLines(txtComment(cfgStart, width = 37, "# start date"), f)
  writeLines(txtComment(nDays, width = 37, "# simulation length (days)"), f)
  writeLines(txtComment(runCD, width = 37, "# run CAEDYM (.TRUE. or .FALSE.)"), f)
  writeLines(txtComment('1', width = 37, "# output Interval (in days, -9999 for every time step)"), f)
  writeLines(txtComment(EXTC, width = 37, "# light extinction coefficient (m-1)"), f)
  
  if (verDY < 3) {
    writeLines(txtComment('0.05', width = 37, paste0("# benthic boundary thickness (m)")), f)
    
  }
  
  writeLines(txtComment(minLyrThk,37,"# minimum layer thickness"), f)
  writeLines(txtComment(maxLyrThk,37, "# maximum layer thickness"), f)
  writeLines(txtComment("3600", 37, "# time Step (s)"), f)
  writeLines(txtComment(nVars,37,"# number of Output Selections"), f)
  writeLines(simVars, f)
  writeLines(txtComment(".FALSE.",37,"# activate bubbler (.TRUE. or .FALSE.)"), f)
  writeLines(txtComment(".FALSE.", 37,"# activate non-neutral atmospheric stability (.TRUE. or .FALSE.)"), f)
  
  # close and write file
  close(f)
  
  txt <- readLines(file.path(filePath, "caedym3p1.bio"))
  sel <- grep("PAR,", txt)
  txt[sel] <- paste0("     ", formatC(EXTC, digits = 5, format = "f"), "            0.450               : PAR, Photosynthetically Active                                                                                                                                     ")
  writeLines(txt, file.path(filePath, "caedym3p1.bio"))
  
}
