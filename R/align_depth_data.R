#' Align observation depth data with model data
#'
#' @inheritParams get_var
#' @inheritParams plot_output
#'
#' @return A data frame with the following columns:
#' \itemize{
#'  \item \code{Date}: Date of observation
#'  \item \code{depth}: Depth of observation
#'  \item \code{elev}: Elevation of observation
#'  \item \code{Model}: Model name
#'  \item \code{var_sim}: Variable name
#'  \item \code{value}: Value of the variable
#'  \item \code{depth_from}: Depth from which the variable is extracted
#'  \item \code{depth_to}: Depth to which the variable is extracted
#'  }
#'
#' @importFrom dplyr filter left_join mutate bind_rows case_when
#'
#' @export

align_depth_data <- function(aeme, model, var_sim, ens_n = 1,
                             return_df = TRUE) {
  if (missing(model)) {
    model <- list_models(aeme)
  }
  outp <- output(aeme)
  obs <- observations(aeme)
  inp <- input(aeme)
  ens_lab <- format_ens_label(ens_n = ens_n)
  
  
  # Align the observed data with the model data ----
  lst <- lapply(model, \(m) {
    depth <- data.frame(Date = outp[[ens_lab]][[m]][["Date"]],
                        depth = outp[[ens_lab]][[m]][["LKE_lvlwtr"]])
    
    df <- get_var(aeme = aeme, model = m, var_sim = var_sim,
                  return_df = TRUE)
    
    if (!is.null(obs$lake)) {
      obs$lake |>
        dplyr::filter(Date %in% df$Date & var_aeme == var_sim) |>
        # merge(x = _, depth, by = "Date") |>
        dplyr::left_join(depth, by = "Date") |>
        dplyr::mutate(elev = depth - depth_from, 
                      Model = toggle_models(m, to = "display")) |>
        dplyr::filter(elev >= 0)
    }
  })
  
  # Adjust water level observations
  if (!is.null(obs$level)) {
    obs$level_adj <- obs$level |>
      dplyr::filter(Date %in% outp[[ens_lab]][[model[1]]][["Date"]] &
                      var_aeme == "LKE_lvlwtr")
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
