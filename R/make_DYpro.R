#' Make a .pro (initial profile) file for DY(CD) simulation
#' 
#' @export
#' @param lakename name of lake being simulated
#' @param verDY DYRESM version
#' @param startSim date of pro file (first day of simulation)
#' @param lvlStart starting water level 
#' @param tmpStart set a constant (isothermal) water temperature (salinity will = 0)
#' @param obsTable data frame of format Depth, Temperature, Salinity (for non-isothermal start and/or SAL > 0)
#' @param iceVals a vector of three values representing initial thicknesses (m) of blue ice, white ice, and snow respectively 
#' @param filePath path to write the complete .inf file
#' @keywords inputs
#' @examples make_DYpro(lakename, verDY, startSim, obsData, filePath)
#' 
#' @noRd
#' 


make_DYpro <-  function(lakename = "!unknown", 
                        verDY = 3.1, 
                        startSim = "!unknown date",
                        lvlBottom, lvlStart, 
                        tmpStart = 10, 
                        obsTable = NULL, 
                        iceVals = c(0,0,0), 
                        filePath = "") {
  
  # define the proTable (intial profiles for T and SAL)
  if (!is.null(obsTable)) {
    
    proTable <- obsTable
    
  } else {
    
    proTable <- data.frame(c(lvlBottom,lvlStart),c(tmpStart,tmpStart),c(0,0) )
    
  }
  
  
  #-------- make the file! ---------
  
  # open file connection
  f <- file(paste0(filePath,"/",lakename,".pro"),"w")
  
  # add headers
  writeLines(paste0("Initial temperature (and snow/ice if verDY > 3.1) profile for lake ",  lakename,", ", startSim), f)
  
  if ( verDY >= 3.2) {
    writeLines(paste0(iceVals[1]), f)
    writeLines(paste0(iceVals[2]), f)
    writeLines(paste0(iceVals[3]), f)
  }
  
  writeLines(paste0(nrow(proTable),"   # initial profile n layers"),f)
  writeLines("Elev(m)   T(degC)    S(ppt)",f)
  write.table(unname(proTable), f, sep="\t", quote=FALSE, col.names = F, row.names=FALSE)
  
  close(f)
  
}
