#' Make GOTM outflows
#'
#' @inheritParams initialise_gotm
#' @inheritParams build_dycd
#' @inheritParams build_aeme
#'
#' @return GOTM yaml configuration as a list object
#' @noRd

make_wdr_gotm <- function(outf, path_gotm, outf_factor = 1) {
  names.outf <- names(outf)

  for (w in seq_len(length(names.outf))) {

    outf_df <- outf[[w]]
    if ("model" %in% colnames(outf_df)) {
      outf_df <- outf_df |>
        dplyr::filter(model == "gotm_wet") |> 
        dplyr::select(-model) 
    }
    outf_df <- outf_df[complete.cases(outf_df), ]
    
    if ("HYD_flow" %in% colnames(outf_df)) {
      outf_df <- outf_df |>
        dplyr::rename(outflow = HYD_flow) 
    }

    outf_df <- outf_df |>
      dplyr::mutate(outflow = (outflow / 86400 * -1 * outf_factor))

    # Sub-daily outflow carries a real time-of-day; daily keeps "12:00:00".
    # GOTM expects column 1 = date, column 2 = time-of-day, so the timestamp
    # must be split -- writing a "YYYY-MM-DD HH:MM:SS" string into column 1
    # shifts every subsequent field and breaks the value column.
    if (is_subdaily(outf_df[["Date"]])) {
      .dts <- as.POSIXct(outf_df[["Date"]], tz = "UTC")
      outf_df[["time"]] <- format(.dts, "%H:%M:%S")
      outf_df[["Date"]] <- format(.dts, "%Y-%m-%d")
    } else {
      outf_df[["time"]] <- "12:00:00"
      outf_df[["Date"]] <- format(as.Date(outf_df[["Date"]]), "%Y-%m-%d")
    }

    ## Write the discharge file
    write.table(outf_df[, c("Date", "time", "outflow")],
                       file.path(path_gotm, "inputs",
                                 paste0("outf_", names.outf[w],".dat")),
                       row.names = FALSE, col.names = FALSE, quote = FALSE, na = "",
                       sep = "\t")
  }
}
