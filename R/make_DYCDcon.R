#' Make a con file for caedym simulation
#' 
#' @noRd
#' @param lakename name of lake being simulated
#' @param verCD CAEDYM version
#' @param simVars variables being simulated (all)
#' @param nFix allow cyanobacteria (ppn Group 2) to fix atmospheric N
#' @param filePath dir of model, relative to working dir
#' @keywords inputs
#' @examples make_DYCDcon(lakename, verCD, simVars, nFix, ppnTS, filePath)



make_DYCDcon <-  function(lakename = "unknown", verCD = 3.1, simVars, 
                          nFix = FALSE, filePath = "") {
  
  
  simPpn <- which(c("DINOF", "CYANO", "NODUL", "CHLOR", "CRYPT", "MDIAT", "FDIAT") %in% simVars)
  
  nm.caedym <- paste0("caedym", gsub("\\.","p", verCD))
  
  
  #------- create .con file -----------
  
  # open file connection
  f <- file(paste0(filePath,"/",lakename,".con"),"w")
  
  # headers
  
  if (verCD < 3) {
    writeLines(paste0("! CAEDYM v", verCD," configuration file: ", lakename), f)
    writeLines("11	     	driver mode (5=ELCOM, 11=DYRESM)", f)
    
    # writeLines(paste0("! CAEDYM parameters file: Valid for V2.3 only"), f)
    
  } else  {
    writeLines(paste0("! CAEDYM v3 Configuration File: ", lakename), f)
    writeLines(paste0(format(verCD,nsmall = 1),"                       CAEDYM Version Number"), f)
  }
  
  writeLines("!-------------------------------------------------------------------------------!", f)
  writeLines("! I/O Configuration:", f)
  
  if (verCD < 3) {
    writeLines(paste0(nm.caedym,".dat                      Parameters file"), f)
  } else {
    writeLines(paste0(nm.caedym,".bio		Biological parameters file"), f)
    writeLines(paste0(nm.caedym,".chm		Geo-chemistry parameters file"), f)
    writeLines(paste0(nm.caedym,".sed		Sediment parameters file"), f)
  }
  
  writeLines(paste0(lakename,".int		Initial conditions file"), f)
  writeLines("NULL			Inflow forcing file", f)
  writeLines("NULL			3D forcing file", f)
  writeLines("T			Print progress messages to screen", f)
  writeLines("T			Write debug information to file (debug.dat)", f)
  writeLines("1.0			Print time step (days)", f)	
  writeLines("1440			Time series time step (minutes)", f)
  writeLines("1 1 -0.1			Time series location", f)
  writeLines("F			Print integrated time series files", f)
  
  
  writeLines("!-------------------------------------------------------------------------------!", f)
  writeLines("! Biological Configuration:", f)
  
  
  writeLines("! PHYTOPLANKTON-----", f)
  writeLines(paste0(' ', length(simPpn),"                   Number of phyto groups to simulate"), f)
  writeLines(paste0(' ', paste(simPpn, collapse = " "),"                   Phyto groups being simulated"), f)
  
  writeLines(" 1                   Carbon (2 or 3) or Chlorophyll-a (1) units", f)
  
  writeLines(paste0(' ', length(simPpn),"                   Number of phyto. groups with internal nitrogen stores"), f)
  writeLines(paste0(' ', paste(simPpn, collapse = " "),"                   Groups with internal N stores"), f)
  writeLines(paste0(' ', length(simPpn),"                   Number of phyto. groups with internal phosphorus stores"), f)
  writeLines(paste0(' ', paste(simPpn, collapse = " "),"                   Groups with internal P stores"), f)
  writeLines(paste0(' ', 0,"                   Number of phyto. groups with internal carbon stores"), f)
  writeLines(" 0                   Number of phyto groups able to produce toxins", f)
  
  if (nFix == TRUE) {
    writeLines(" 2                   Number of phyto groups able to fix nitrogen", f)
  } else {
    writeLines(" 0                   Number of phyto groups able to fix nitrogen", f)
  }
  
  writeLines(" 0.1                   Minimum allowable biomass for phytoplankton", f)
  writeLines(" T                   Direcion of advection of motile phytoplankton", f)
  writeLines(" F                   Stokes settling into the sediment", f)
  writeLines(" F                   Motile settling into the sediment", f)
  
  writeLines(paste0(" 0                   Phytoplankton time series group"), f)
  
  writeLines("! ZOOPLANKTON-------", f)
  writeLines(" 0                   Number of Zooplankton Groups to simulate", f)
  writeLines(" 1e-3                   Minimum allowable biomass for zooplankton", f)
  writeLines(" 0                   Zooplankton time series group", f)
  
  
  
  writeLines("! FISH--------------", f)
  writeLines(" 0                   Number of fish groups to simulate", f)
  writeLines(" 0                   Fish time series group", f)
  if (verCD >= 3) {
    writeLines(" 0                   Number of fish EGG/LARVAE cohorts", f)
  }
  
  
  
  writeLines("! MISCELLANEOUS-----", f)
  writeLines(" 0                   Number of pathogen groups to simulate", f)
  writeLines(" 0                   Number of jellyfish groups to simulate", f)
  writeLines(" 0                   Number of seagrass groups to simulate", f)
  writeLines(" 0                   Number of macroalgae groups to simulate", f)
  writeLines(" 0                   Number of clam/mussel groups to simulate", f)
  writeLines(" 0                   Number of invertebrate groups to simulate", f)
  writeLines(" 1e-2                   Minimum allowable biomass for all other species", f)
  
  if (verCD < 3) {
    writeLines(" 0                       Jellyfish ts group", f)
    writeLines(" 0                    	Seagrass time series group", f)
    writeLines(" 0                       Macroalgae time series group", f)
    writeLines(" 0                    	Invertebrate time series group", f)
  }
  
  writeLines("!-------------------------------------------------------------------------------!", f)
  writeLines("! Nutrient/Chemistry Configuration:", f)
  
  if (verCD < 3) {
    writeLines(" F               		Simulate colour / tracer", f)
    writeLines(" F               		Simulate metals (Al, Fe & Mn)", f)
    writeLines(" T               		Simulate suspended solids & PIP/PIN (inorganic particles)", f)
    writeLines(" T               		Simulate refractory C N and P", f)
    writeLines(" F                   Simulate pH & DIC ", f)
    writeLines(" F                   Simulate Bacterial Biomass", f)
    writeLines(" 1      	   	 	      Method of sediment nutrient flux calcs (set to 1)", f)
    writeLines(" F               		Type of sediment oxygen flux model (set to F)", f)
    writeLines(" F 			           	Output method for sediment oxygen", f)
  } else {
    writeLines(" 2                   Simulate suspended solids  (SSOL1, SSOL2) (set to 2 to run both SSOL1 and SSOL2)", f)
    writeLines(" F                   Simulate refractory OM pools (POMR, DOMR)", f)
    writeLines(" F                   Simulate Bacterial Biomass          (BAC)", f)
    writeLines(" 0                   Simulate Biologically Active Components", f)
    writeLines(" F                   Type of sediment model (F=STATIC;T=CANDI)", f)
    writeLines(" F                   Simulate Geochemistry Module (has to be T if running CANDI)", f)
    writeLines(" 1                   Geochemistry time-step (days) 0 means CAEDYM time step", f)
    writeLines(" 3                   Number of chemical components (excluding H+, H2O, e-)", f)
    writeLines(" PO4 NO3 NH4                   !!!!  MnII MnIV SiO2 Cl Ca Na K Mg", f)
    writeLines(" 1                   Number of minerals/pure phases to include", f)
    writeLines("  Fe(OH)3_a !                   FeS(ppt) Calcite Birnessite Siderite Rhodchros Aragonite Birnessite", f) 
  }
  
  
  writeLines("!-------------------------------------------------------------------------------!", f)
  writeLines("! Miscellaneous Configuration:", f)
  writeLines(" T                   Simulate settling (& migration for phytos)", f)
  writeLines(" T                   Simulate resuspension", f)
  if ( verCD >= 3) { writeLines("F                   Simulate colour / tracer", f) }
  writeLines(" 0                   Experimental configuration flag", f)
  writeLines(" 0                   3D variables subject to error analysis", f)
  writeLines(" T		               	Forcing domain for 3D forcing", f)
  writeLines(" T                   Sparse locations where inflows are written", f)
  writeLines(" 0                   Open boundary condition type (0 = no open boundaries)", f)
  writeLines(" 0                   Type of water system", f)
  if ( verCD < 3) {
    writeLines(" 1                   Along-river init: Start sparse coordinate; lower bank", f)
    writeLines(" 442                 Along-river init: Start sparse coordinate; upper bank", f)
    writeLines(" 4                   Along-river init: Initial search direction, start edge", f)
    writeLines(" 1423                Along-river init: End sparse coordinate; lower bank", f)
    writeLines(" 1476                Along-river init: End sparse coordinate; upper bank", f)
    writeLines(" 4                   Along-river init: Initial search direction, end edge", f)
    writeLines(" 0                   Number of salinity divisions for time series output", f)
    writeLines(" -1.  10. 25. 99.                   Salinity bounds for time series output", f)
  }
  writeLines("!-------------------------------------------------------------------------------!", f)
  
  close(f)
  
}
