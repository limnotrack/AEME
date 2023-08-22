#' Make a int (initial conditions) file for caedym simulation (iso values and 3D vars only at this point)
#'
#' @noRd
#' @param lakename name of lake being simulated
#' @param intVars vector of variables being simulated (all)
#' @param wcVals matching vector of initial water column values
#' @param sedVals matching vector of initial sed values
#' @param verCD CAEDYM version number
#' @param filePath dir of model, relative to working dir
#' @keywords inputs
#' @examples make_DYCDcon(lakename, verCD, intVars, nFix, ppnTS, filePath)

make_DYCDint <-  function(lakename = "unknown",
                          intVars, wcVals, sedVals, verCD,
                          filePath = "") {

  # constants
  CNratio  <- (106 * 12) / (16 * 14)

  ppnVars <- c("DINOF", "CYANO", "NODUL", "CHLOR", "CRYPT", "MDIAT",
               "FDIAT") |>
    dplyr::intersect(intVars)


  # compile inputs into a dataframe for use
  intVars <- data.frame(VAR =intVars) |>
    dplyr::mutate(wc =  round(as.numeric(wcVals),4),
                  sed = round(as.numeric(sedVals),4))

  # check pH vs PH
  if (verCD < 3) { intVars[intVars$VAR == "pH", "VAR"] <- "PH" }

  # additional phytoplankton intialisations
  for (i in 1:length(ppnVars)) {

    var.short = substr(ppnVars[i],1,3)

    intVars <- intVars |>
      rbind(c(paste0("IC_", var.short), -50,0),
            c(paste0("IN_", var.short), round(-50 / CNratio, 3),0),
            c(paste0("IP_", var.short), round(-50 / CNratio / 7.2, 3),0) )
  }

  # force PIN to avoid error
  intVars <- intVars |>
    rbind(c("PIN",0,0))



  #-------- make the file! ---------

  f <- file(paste0(filePath, "/", lakename, ".int"), "w")
  on.exit({
    # close and write file
    close(f)
  })

  writeLines("3D DATA", f)

  for (i in 1:nrow(intVars) ) {

    list(intVars[i,1],
         paste0("  CO_I"),
         # find water column value
         paste0("  ",intVars[i,2]),
         paste0("  ",intVars[i,3]) ) |>
      lapply(writeLines, f)
  }

  writeLines("2D DATA", f)
  writeLines("EOF", f)

}
