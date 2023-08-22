#' Make a .stg (physiographic information) file for DY(CD) simulation
#'
#' @noRd
#' @param lakename name of lake being simulated
#' @param latitude latitude of lake centre
#' @param bathy two column data frame of elevation (masl) and planar area
#' @param surfElev surface elevation at long-term average water depth (masl)
#' @param gwHeight height of groundwater intrusion (if included; "SURF" or numeric elevation above bottom)
#' @param outHeights vector of outflow elevations (masl)
#' @param crest elevation at full supply/spillover (masl)
#' @param outNames vector of outflow names
#' @param infNames vector of inflow names
#' @param filePath path to write the complete .inf file
#' @keywords inputs
#'
#' @importFrom dplyr mutate
#'

make_DYstg <-  function(lakename = "!unknown",
                        latitude = -9999,
                        bathy,
                        surfElev, crest, outHeights,
                        gwHeight = "SURF",
                        infNames, outNames,
                        filePath = "") {

  #------ set defaults ------

  if (missing(surfElev)){
    surfElev = max(bathy[,1]) - 0.5
  }

  if (missing(crest)){
    crest = max(bathy[,1])
  }

  if (missing(outHeights)){
    outHeights = rep(surfElev - 1, length(outNames))
  }

  bathy[, 1] <- round(bathy[, 1], 2)


  #----- check for outflow heights
  if (length(outHeights) != length(outNames) ) {
    stop("Oops! length of outHeights doesn't match number of outflows")
  }

  # inflows table
  if(!is.null(infNames)) {
    infTable <- data.frame(infNames) |>
      `names<-`("infName") |>
      dplyr::mutate(infName   = as.character(infName),
                    height    = "SURF",
                    hlfAngle  = 85,
                    sbSlope   = 0.002,
                    dragCoeff = 0.016)

    # check for rain and gw inflow and overwrite
    for (i in 1:nrow(infTable)) {

      if(grepl("oundwater", infTable[i,1])) {

        infTable[i,] <- c("Groundwater",gwHeight,88,0.001,0.016)

      }

      if(grepl("ainfall", infTable[i,1])) {

        infTable[i,] <- c("Rainfall","SURF",89,0.001,0.016)

      }
    }
  } else {
    infNames <- "EMPTY"
    infTable <- data.frame(infName = "EMPTY",
                           height    = "SURF",
                           hlfAngle  = 85,
                           sbSlope   = 0.002,
                           dragCoeff = 0.016)
  }



  #-------- make the file! ---------

  # open file connection
  f <- file(paste0(filePath,"/",lakename,".stg"),"w")

  list("<#3>",
       paste0("DYRESM morphometry file for ",lakename),
       txtComment(round(latitude,4), 40, "# latitude of lake centre"),
       txtComment(round(surfElev,2), 40, "# lake surface elevation (m above sea level)"),
       txtComment(length(infNames),40, "# number of inflows")) |>
    lapply(writeLines, con = f)

  # if(!is.null(infNames)) {
  write.table(infTable[,c(2:5,1)], f, sep="\t", quote=FALSE, col.names = F, row.names=FALSE)
  # }

  list(txtComment(round(min(bathy[,1]), 2), 40,"# base elevation (m above sea level)"),
       txtComment(round(crest, 2), 40, "# crest/full supply elevation (m above sea level)"),
       txtComment(as.numeric(length(outNames)),40, "# number of outlets"),
       txtComment(round(outHeights, 2), 40, "# outlet heights (m above sea level)"),
       txtComment(nrow(bathy), 40, "# number of bathymetry records")) |>
    lapply(writeLines, con = f)

  writeLines("Elevation_[m]     Area_[m^2]",f)

  write.table(bathy, f, sep = "\t", quote = FALSE, col.names = F,
              row.names = FALSE)

  close(f)
}


############## fin ###############
