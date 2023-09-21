#' Make GLM outflow
#'
#' @param outf list of outflow
#' @param heights_wdr numeric vector; height of outflow
#' @inheritParams make_stgGLM
#' @inheritParams make_metGLM
#' @param wdr_factor numeric; scaling factor to be applied to the outflow.
#' @param update_nml Logical; update the nml object?
#' @inheritParams set_nml
#'
#' @return updated nml object
#' @noRd
#'
#' @importFrom utils write.csv
#' @importFrom dplyr mutate bind_rows

make_wdrGLM <- function(outf, heights_wdr, bathy, dims_lake, wdr_factor = 1,
                        update_nml = TRUE, glm_nml, path_glm) {


  if (length(outf) > 1) {
    df_wdr <- Reduce(merge, outf) |>
      dplyr::select(c(Date, outflow, outflow_glm_aed)) |>
      dplyr::rename(wbal = outflow_glm_aed)
  } else {
    df_wdr <- outf[[1]]
    if (ncol(df_wdr) > 2) {
      df_wdr <- df_wdr |>
        dplyr::select(c(Date, outflow_glm_aed)) |>
        dplyr::rename(outflow = outflow_glm_aed)
    }
  }


  if (!is.null(df_wdr)) {
    df_wdr <- df_wdr |>
      # convert discharge values to cumecs
      dplyr::mutate(dplyr::across(2:ncol(df_wdr), \(x) (x * wdr_factor)
                                  / 86400))

    if (update_nml) {
      crest <- max(bathy$elev)

      # get the inflow attributes
      names_wdr <- names(df_wdr)[2:ncol(df_wdr)]
      n_wdr <- length(names_wdr)

      # default to outflow = 3 m below crest if not supplied
      if (missing(heights_wdr)) {
        heights_wdr <- rep(crest - 3, length(names_wdr))
      }

      # get the glm outlet elevations (neg depths)
      heights_wdr.glm <- heights_wdr# - crest

      dims_outf <- lapply((heights_wdr + min(bathy$elev)), FUN = elipse_dims, bathy = bathy,
                          dims_lake = dims_lake)  |>
        dplyr::bind_rows()

      lengths <- dims_outf$length
      widths  <- dims_outf$width

      outflow <- list(
        num_outlet = n_wdr,
        outlet_type = rep(2, n_wdr),
        flt_off_sw = rep(TRUE, n_wdr),
        outl_elvs = rep(round(heights_wdr, 2), n_wdr),
        bsn_len_outl = round(lengths, 2),
        bsn_wid_outl = round(widths, 2),
        outflow_fl = paste0("'bcs/outflow_", names_wdr,
                            ".csv'"),
        outflow_factor = rep(1, n_wdr)
      )
      glm_nml[["outflow"]] <- outflow
    }

    # write the outflow files
    for (i in 2:ncol(df_wdr)) {

      this.out <- df_wdr[,c(1, i)] |>
        `colnames<-`(c("time", "flow"))

      this.name <- paste0("outflow_", colnames(df_wdr)[i], ".csv")

      utils::write.csv(this.out, file.path(path_glm, "bcs", this.name),
                row.names = FALSE, quote = FALSE)

    }

  } else {
    if (update_nml) {
      glm_nml[["outflow"]] <- NULL
    }
  }
  if (update_nml) {
    return(glm_nml)
  }
}

#' Get the characteristics of all the outlet heights
#'
#' @inheritParams bathy_extrap
#' @param height numeric; height of outflow from lake bottom.
#' @inheritParams make_stgGLM
#'
#' @return vector of length 2; with length and width at the outflow.
#' @noRd
#'
#' @importFrom stats approx
elipse_dims <- function(bathy, height, dims_lake) {

  # planar area of the lake at outflow elevation
  a.wdr <- stats::approx(bathy[,1], bathy[,2], xout = height)[2] |>
    as.numeric() |>
    round()

  # length at outflow (per GLM manual)
  L.outf <- sqrt(a.wdr * (4/pi) * (dims_lake[1] / dims_lake[2]))
  W.outf <- L.outf * (dims_lake[2] / dims_lake[1])

  return(data.frame(length = L.outf, width = W.outf))

}
