#' Make GOTM outflows
#'
#' @inheritParams initialiseGOTM
#' @inheritParams build_dycd
#' @inheritParams build_aeme
#'
#' @return GOTM yaml configuration as a list object
#' @noRd

make_wdrGOTM <- function(outf, path_gotm, outf_factor = 1) {
  names.outf <- names(outf)

  for (w in seq_len(length(names.outf))) {

    outf_df <- outf[[w]]
    if ("model" %in% colnames(outf_df)) {
      outf_df <- outf_df |>
        dplyr::filter(model == "gotm_wet") |> 
        dplyr::select(-model) 
    }
    outf_df <- outf_df[stats::complete.cases(outf_df), ]

    outf_df <- outf_df |>
      dplyr::mutate(outflow = (outflow / 86400 * -1 * outf_factor),
                    time = "12:00:00")
    ## Write the discharge file
    utils::write.table(outf_df[, c("Date", "time", "outflow")],
                       file.path(path_gotm, "inputs",
                                 paste0("outf_", names.outf[w],".dat")),
                       row.names = FALSE, col.names = FALSE, quote = FALSE, na = "",
                       sep = "\t")
  }
}
