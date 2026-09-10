#' Align observation depth data with model data
#'
#' @inheritParams get_var
#' @inheritParams plot_output
#'
#' @return A data frame with the following columns:
#' \itemize{
#'  \item \code{Date}: Observation date, as a calendar \code{Date}
#'  \item \code{depth}: Depth of observation (m, positive-down from the surface)
#'  \item \code{elev}: Elevation of observation
#'  \item \code{Model}: Model name
#'  \item \code{var_sim}: Variable name
#'  \item \code{value}: Value of the variable
#'  }
#'
#' @importFrom dplyr filter left_join mutate bind_rows case_when
#'
#' @export

align_depth_data <- function(aeme, model, var_sim, ens_n = 1,
                             return_df = TRUE) {
  withr::local_locale(c("LC_TIME" = "C"))
  withr::local_timezone("UTC")
  if (missing(model)) {
    model <- list_models(aeme)
  } else {
    model <- check_model(model = model)
  }
  outp <- output(aeme)
  obs <- observations(aeme)
  inp <- input(aeme)
  ens_lab <- format_ens_label(ens_n = ens_n)
  
  
  # Align the observed data with the model data ----
  # Lake observations are daily; model output may be sub-daily (POSIXct). In
  # that case collapse the modelled surface elevation to one value per
  # calendar day and match observations on the day, otherwise an exact
  # timestamp join drops every observation.
  lst <- lapply(model, \(m) {
    surface <- data.frame(Date = outp[[ens_lab]][[m]][["Date"]],
                          surface_elev = outp[[ens_lab]][[m]][["LKE_lvlwtr"]])
    subdaily <- inherits(surface$Date, "POSIXct")
    if (subdaily) {
      surface$Date <- as.Date(surface$Date)
      surface <- stats::aggregate(surface_elev ~ Date, data = surface,
                                  FUN = mean, na.rm = TRUE)
    }

    if (!is.null(obs$lake)) {
      # `surface$Date` is a calendar Date here (a daily model axis is already
      # Date; a sub-daily one was collapsed above). Reduce the observation
      # Date to the same calendar day for the join -- it is stored as noon
      # POSIXct.
      obs$lake |>
        dplyr::mutate(Date = as.Date(Date, tz = "UTC")) |>
        dplyr::filter(Date %in% surface$Date & var_aeme == var_sim) |>
        dplyr::left_join(surface, by = "Date") |>
        dplyr::mutate(elev = surface_elev - depth,
                      Model = toggle_models(m, to = "display")) |>
        dplyr::filter(elev >= 0)
    }
  })

  # Adjust water level observations
  if (!is.null(obs$level)) {
    mod_days <- outp[[ens_lab]][[model[1]]][["Date"]]
    if (inherits(mod_days, "POSIXct")) mod_days <- as.Date(mod_days, tz = "UTC")
    obs$level_adj <- obs$level |>
      dplyr::mutate(Date = as.Date(Date, tz = "UTC")) |>
      dplyr::filter(Date %in% mod_days & var_aeme == "LKE_lvlwtr")
    if (nrow(obs$level_adj) > 0) {
      obs$level_adj <- obs$level_adj |>
        dplyr::mutate(lvl_adj = value - min(inp$hypsograph$elev))
    } else {
      obs$level_adj <- NULL
    }
  }
  
  if (return_df) {
    obs$lake_adj <- dplyr::bind_rows(lst)
    if (!is.data.frame(obs$lake_adj) || nrow(obs$lake_adj) == 0) {
      obs[["lake_adj"]] <- NULL
    }
  } else {
    obs$lake_adj <- lst
  }
  obs
}
