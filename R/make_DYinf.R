#' Make a .inf (inflows) file for DY(CD) simulation
#'
#' @noRd
#' @param lakename name of lake being simulated
#' @param info extra text to be printed in file header (base = 'DYRESM inflows file')
#' @param infList a list containing all inflow data frames to be written to the file (including groundwater and rainfall if applicable)
#' @param filePath path to write the complete .inf file
#' @keywords inputs
#'
#' @importFrom dplyr bind_rows select mutate across arrange
#' @importFrom utils write.table
#'

make_DYinf <-  function(lakename = "unknown", info = "", infList, filePath = "",
                        inf_factor = 1) {

  # get names for inflows
  infNames <- names(infList)

  # set infNum
  names(infList) <- 1:length(infList)
  inf <- infList |>
    # `names<-`(1:length(.))|>
    dplyr::bind_rows(.id = "column_label") |>
    data.frame() |>
    dplyr::mutate(
      dplyr::across(!dplyr::contains("Date"), \(x) ifelse(is.na(x), 0, x))
    )
  inf <- inf |>
    # re-arrange and re-name for DYRESM format
    dplyr::select(c(2, 1, 3:ncol(inf))) |>
    `names<-`(c("Date", "InfNum", names(inf)[3:ncol(inf)])) |>
    # convert date format
    dplyr::mutate(VOL = VOL * inf_factor)|>
    dplyr::mutate(Date = paste0(lubridate::year(Date), strftime(Date,
                                                                format = "%j")),
                  # tidy rounding
                  dplyr::across(3:ncol(inf), \(x) round(x, digits = 3)),
                  dplyr::across(3:ncol(inf), \(x) format(x, nsmall = 3)),
                  dplyr::across(3:ncol(inf), \(x) ifelse(is.na(x), 0, x)))|>
    # sort by date
    dplyr::arrange(Date)



  #-------- make the file! ---------

  # open the file connection
  f <- file(paste0(filePath,"/",lakename,".inf"),"w")

  # make a header to print to file
  writeLines(paste0("DYRESM-CAEDYM Inflow file for lake ",lakename,". ", info), f)
  writeLines(paste0(length(infNames),"         # Number of inflows"),f)

  # add inf IDs
  for (i in 1:length(infNames) ) {

    writeLines(txtComment(infNames[i], width = 37, paste0("# Inflow ",i)), f)

  }

  #add data
  utils::write.table(inf, f, sep = "\t", quote = FALSE, row.names = FALSE)

  # close and write file
  close(f)
}
