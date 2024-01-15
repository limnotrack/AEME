#' Check model output
#'
#' @inheritParams build_ensemble
#'
#' @return logical; if model output passes checks or not.
#' @export

check_model_output <- function(path, aeme_data, model) {

  lke <- lake(aeme_data)
  aeme_time <- time(aeme_data)
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
      dates <- ncdf4::ncvar_get(nc, "dyresmTime")
      dates[dates > 9e36] <- NA
      if (any(is.na(dates))) {
        last_date <- dates[which.max(is.na(dates)) - 1]
        last_date <- as.POSIXct((last_date - 2415018.5) * 86400,
                                origin = "1899-12-30")
        if (length(last_date) == 0) {
          message(strwrap("DYRESM-CAEDYM crashed when initialising. No
                          output available for the simulation period."))
          return(FALSE)
        }
        if (last_date < aeme_time$start) {
          message(strwrap("DYRESM-CAEDYM crashed during the spin-up period! No
                          output available for the simulation period."))
          return(FALSE)
        }
        message("DYRESM-CAEDYM output file: ", out_file, " has NA dates!")
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
      if(is.null(dim(zi))) {
        return(FALSE)
      }
      init_z <- round(min(zi[, 1]) - max(zi[, 1]), 2)
      if (any(zi < init_z)) {
        message(strwrap("GOTM-WET output file: Model depth goes below it's
                        initial depth. Possible source of error is the
                        in/outflows.\nAdjust scaling factors and re-run the
                        model."))
        return(FALSE)
      }
    }
  }
  TRUE
}
