#' Make GOTM outflows
#'
#' @inheritParams initialiseGOTM
#' @inheritParams build_dycd
#' @inheritParams build_ensemble
#'
#' @return GOTM yaml configuration as a list object
#' @noRd

make_wdrGOTM <- function(outf, gotm, outf_factor, path_gotm) {
  names.outf <- names(outf)

  for(w in 1:length(names.outf)) {

    outf_df <- outf[[w]]
    if (ncol(outf_df) > 2) {
      outf_df <- outf_df |>
        dplyr::select(c(Date, outflow_gotm_wet)) |>
        dplyr::rename(outflow = outflow_gotm_wet)
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



    gotm[["streams"]][[names.outf[w]]] <- list(method = 1, zu = 0, zl = -1,
                                               flow = list(method = 2,
                                                           constant_value = 0,
                                                           file = paste0("inputs/outf_",
                                                                         names.outf[w],
                                                                         ".dat"),
                                                           column = 1,
                                                           scale_factor = 1,
                                                           offset = 0)
    )
  }
  gotm
}
