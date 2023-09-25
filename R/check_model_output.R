#' Check model output
#'
#' @inheritParams build_ensemble
#'
#' @return logical; if model output passes checks or not.
#' @export

check_model_output <- function(path, aeme_data, model) {

  lke <- lake(aeme_data)
  lake_dir <- file.path(path, paste0(lke$id, "_",
                                     tolower(lke$name)))
  out_file <- dplyr::case_when(model == "dy_cd" ~ file.path(lake_dir, model,
                                                            "DYsim.nc"),
                               model == "glm_aed" ~ file.path(lake_dir, model,
                                                              "output",
                                                              "output.nc"),
                               model == "gotm_wet" ~ file.path(lake_dir, model,
                                                               "output",
                                                               "output.nc")
  )

  # DYRESM-CAEDYM
  if ("dy_cd" %in% model) {
    if (!file.exists(out_file)) {
      message("DYRESM-CAEDYM output file: ", out_file, " does not exist!")
      return(FALSE)
    } else {
      nc <- ncdf4::nc_open(out_file, return_on_error = TRUE)
      on.exit({
        ncdf4::nc_close(nc)
      })
      if (nc$error) {
        message("DYRESM-CAEDYM output file: ", out_file, " does not exist!")
        return(FALSE)
      }
    }
  }

  # GLM-AED ----
  if ("glm_aed" %in% model) {
    if (!file.exists(out_file)) {
      message("GLM-AED output file: ", out_file, " does not exist!")
      return(FALSE)
    } else {
      nc <- ncdf4::nc_open(out_file, return_on_error = TRUE)
      on.exit({
        ncdf4::nc_close(nc)
      })
      if (nc$error) {
        message("GLM-AED output file: ", out_file, " does not exist!")
        return(FALSE)
      }
    }
  }

  # GOTM-WET ----
  if ("gotm_wet" %in% model) {
    if (!file.exists(out_file)) {
      message("GOTM-WET output file: ", out_file, " does not exist!")
      return(FALSE)
    } else {
      nc <- ncdf4::nc_open(out_file, return_on_error = TRUE)
      on.exit({
        ncdf4::nc_close(nc)
      })
      if (nc$error) {
        message("GOTM-WET output file: ", out_file, " does not exist!")
        return(FALSE)
      }
      # Check elevation
      zi <- ncdf4::ncvar_get(nc, "zi")
      init_z <- min(zi[, 1])- max(zi[, 1])
      if (any(zi < init_z)) {
        message(strwrap("GOTM-WET output file: Model depth goes below it's
                        initial depth. Possible source of error is the
                        in/outflows.\nAdjust scaling factors and re-run the
                        model."))
        return(FALSE)
      }
      return(TRUE)
    }
  }
}
