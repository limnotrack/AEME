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
#' @export

align_depth_data <- function(aeme_data, model, var_sim, return_df = TRUE) {
  outp <- output(aeme_data)
  obs <- observations(aeme_data)
  inp <- input(aeme_data)

  # Align the observed data with the model data ----
  lst <- lapply(model, \(m) {
    depth <- data.frame(Date = outp[[m]][["Date"]],
                        depth = outp[[m]][["LKE_lvlwtr"]])

    df <- get_var(aeme_data = aeme_data, model = m, var_sim = var_sim,
                  return_df = TRUE)

    if (!is.null(obs$lake)) {
      obs$lake |>
        dplyr::filter(Date %in% df$Date & var == var_sim) |>
        merge(x = _, depth, by = "Date") |>
        dplyr::mutate(elev = depth - depth_from, Model = m) |>
        dplyr::filter(elev >= 0)
    }
  })

  # Adjust water level observations
  if (!is.null(obs$level)) {
    obs$level_adj <- obs$level |>
      dplyr::filter(Date %in% outp[[model[1]]][["Date"]] & var == "LKE_lvlwtr")
    if (nrow(obs$level_adj) > 0) {
      obs$level_adj <- obs$level_adj |>
        dplyr::mutate(lvl_adj = value - min(inp$hypsograph$elev))
    }
  }

  if (return_df) {
    obs$lake_adj <- do.call(rbind, lst)
    if (!is.null(obs$lake_adj)) {
      obs$lake_adj <- obs$lake_adj |>
        dplyr::mutate(Model = dplyr::case_when(
          Model == "dy_cd" ~ "DYRESM-CAEDYM",
          Model == "glm_aed" ~ "GLM-AED",
          Model == "gotm_wet" ~ "GOTM-WET"
        ),
        var_sim = var_sim
        )
    }

  } else {
    obs$lake_adj <- lst
  }
  obs
}
