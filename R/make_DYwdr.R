#' Make a .wdr (withdrawl/outflow) file for DY(CD) simulation
#'
#' @noRd
#' @param lakename name of lake being simulated
#' @param info extra text to be printed in file header (base = 'DYRESM inflows file')
#' @param wdrData a data frame of dates and discharge of format Date, wdr1, wdr2...
#' @param discharge a vector of numeric values (constant discharge, 1 per outflow) or a dataframe of measurements (Date, outName, vol)
#' @param filePath path to write the complete .wdr file
#' @param outf_factor numeric; scaling factor for outflow
#' @keywords inputs
#' @examples make_DYinf(lakename, info, infNames, filePath)
#'
#' @importFrom dplyr filter mutate across arrange
#' @importFrom lubridate year
#' @importFrom stats complete.cases
#' @importFrom utils write.table
#'


make_DYwdr <-  function(lakename = "unknown", wdrData, info = "", filePath = "",
                        outf_factor = 1.0) {

  if (length(wdrData) > 1) {
    wdrData <- Reduce(merge, wdrData) |>
      dplyr::select(c(Date, outflow, outflow_dy_cd)) |>
      dplyr::rename(wbal = outflow_dy_cd)
  } else {
    wdrData <- wdrData[[1]]
  }
  wdrData <- wdrData[stats::complete.cases(wdrData), ] |>
    # round discharge data
    dplyr::mutate(dplyr::across(2:ncol(wdrData), \(x) x * outf_factor),
                  dplyr::across(2:ncol(wdrData), \(x) round(x, digits = 3)),
                  dplyr::across(2:ncol(wdrData), \(x) format(x, nsmall = 3)),
           # dyresm date format
           Date = paste0(lubridate::year(Date), strftime(Date,
                                                         format = "%j"))) |>
    dplyr::arrange(Date)



  #-------- make the file! ---------

  # open the file connection
  f <- file(paste0(filePath, "/", lakename,".wdr"),"w")

  # make a header to print to file
  writeLines(paste0("DYRESM-CAEDYM outflow file (m^3/d) for ",lakename,". ",
                    info), f)
  writeLines(paste0(ncol(wdrData) - 1,
                    "                                  # Number of outflows"),f)

  #add data
  utils::write.table(wdrData, f, sep = "\t", quote = FALSE, row.names = FALSE)

  # close and write file
  close(f)
}
