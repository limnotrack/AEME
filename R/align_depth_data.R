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
  # Lake observations are daily; model output may be sub-daily (POSIXct). A
  # daily model axis is joined on the calendar day directly. A sub-daily one
  # is matched to the model timestep *nearest* each observation's own
  # timestamp (obs are stored as noon POSIXct) rather than collapsed to a
  # daily mean -- otherwise an observation can be placed above the
  # instantaneous modelled surface actually drawn for its timestep whenever
  # the sub-daily lake level swings over the day.
  lst <- lapply(model, \(m) {
    surface <- data.frame(Date = outp[[ens_lab]][[m]][["Date"]],
                          surface_elev = outp[[ens_lab]][[m]][["LKE_lvlwtr"]])
    subdaily <- inherits(surface$Date, "POSIXct")
    surface_days <- if (subdaily) as.Date(surface$Date) else surface$Date

    if (!is.null(obs$lake)) {
      obs_sub <- obs$lake |>
        dplyr::filter(as.Date(Date, tz = "UTC") %in% surface_days &
                        var_aeme == var_sim)

      if (subdaily) {
        obs_sub |>
          dplyr::mutate(surface_elev = .nearest_value(obs_time = Date,
                                                       ref_time = surface$Date,
                                                       ref_value = surface$surface_elev),
                        Date = as.Date(Date, tz = "UTC"),
                        elev = surface_elev - depth,
                        Model = toggle_models(m, to = "display")) |>
          dplyr::filter(elev >= 0)
      } else {
        obs_sub |>
          dplyr::mutate(Date = as.Date(Date, tz = "UTC")) |>
          dplyr::left_join(surface, by = "Date") |>
          dplyr::mutate(elev = surface_elev - depth,
                        Model = toggle_models(m, to = "display")) |>
          dplyr::filter(elev >= 0)
      }
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

#' Look up the value in `ref_value` whose `ref_time` is closest to each
#' `obs_time`
#'
#' `ref_time` must be sorted ascending (as model output timestamps are).
#'
#' @noRd
.nearest_value <- function(obs_time, ref_time, ref_value) {
  if (length(ref_time) < 2) return(rep(ref_value[1], length(obs_time)))
  obs_num <- as.numeric(obs_time)
  ref_num <- as.numeric(ref_time)
  idx <- findInterval(obs_num, ref_num)
  idx <- pmin(pmax(idx, 1), length(ref_num) - 1)
  use_upper <- abs(obs_num - ref_num[idx + 1]) < abs(obs_num - ref_num[idx])
  ref_value[idx + as.integer(use_upper)]
}
