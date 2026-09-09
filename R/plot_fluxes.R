#' @title Plot fluxes
#'
#' @description Plot heat fluxes from AEME simulations. This includes incoming
#' shortwave radiation, net longwave radiation, evaporative heat flux, and
#' sensible heat flux.
#'
#' @inheritParams build_aeme
#' @param facet_by character; either \code{"flux"} or \code{"model"}. If
#' \code{"flux"}, create a separate facet for each flux. If \code{"model"}, 
#' create a separate facet for each model.
#' @param ... additional arguments passed to \code{\link{get_var}}
#'
#' @return ggplot2 object
#'
#' @export
#'
#' @importFrom dplyr left_join mutate bind_rows
#' @importFrom ggplot2 aes facet_wrap geom_hline geom_line labs theme_bw
#' @importFrom utils data
#'

plot_fluxes <- function(aeme, model, facet_by = c("flux", "model"), ...) {
  
  facet_by <- rlang::arg_match(facet_by)
  aeme <- check_aeme(aeme)
  if (missing(model)) {
    model <- list_models(aeme)
  } else {
    model <- check_model(model = model)
  }
  data("key_naming", package = "AEME", envir = environment())
  
  vars <- c("LKE_Qsw", "LKE_Qlw", "LKE_Qe", "LKE_Qh")
  df <- lapply(vars, \(v) {
    get_var(aeme, model = model, var_sim = v, return_df = TRUE, ...)
  }) |>
    dplyr::bind_rows()
  
  # Calculate net flux
  df_net <- df |>
    tidyr::pivot_wider(names_from = var_sim, values_from = value) |>
    dplyr::group_by(Date, Model) |>
    dplyr::mutate(Net_Flux = LKE_Qsw + LKE_Qlw + LKE_Qe + LKE_Qh) |>
    dplyr::ungroup() |> 
    dplyr::select(Date, Model, Net_Flux) |> 
    dplyr::mutate(
      name_parse = "Net~Flux~(W~m^-2)",
      name_text = "Net flux"
    ) |> 
    dplyr::rename(value = Net_Flux)
  
  df <- df |>
    dplyr::left_join(key_naming[, c("var_aeme", "name_parse", "name_text")],
                     by = c("var_sim" = "var_aeme")) |>
    dplyr::bind_rows(df_net) |> 
    dplyr::mutate(
      name_text = factor(name_text, levels = c("Net flux",
                                               "Shortwave radiation",
                                               "Longwave radiation" ,
                                               "Evaporative heat flux",
                                               "Sensible heat flux"
                                               ))
    ) 
  
  y_lab <- eval(parse(text = "Flux~(W~m^-2)"))
  col_lab <- ifelse(facet_by == "flux", "Model", "Flux")
  
  if (all(is.na(df$value))) {
    message("No data to plot. Returning empty plot")
    p <- ggplot2::ggplot() +
      ggplot2::theme_bw()
    return(p)
  }
  
  p <- ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = 0) 
  if (facet_by == "flux") {
    p <- p +
      ggplot2::geom_line(data = df, ggplot2::aes(x = Date, y = value, colour = Model)) +
      ggplot2::facet_wrap(~name_text, scales = "free_y", ncol = 1,
                          strip.position = "right")
  } else if (facet_by == "model") {
    p <- p +
      ggplot2::geom_line(data = df, ggplot2::aes(x = Date, y = value, colour = name_text, linewidth = name_text)) +
      ggplot2::facet_wrap(~Model, scales = "free_y", ncol = 1,
                          strip.position = "right") +
      # Increase size of net flux line
      ggplot2::scale_colour_manual(values = c(
        "Net flux"             = "#333333",   # Dark charcoal (neutral summary)
        "Shortwave radiation"  = "#F5A623",   # Amber/solar yellow
        "Longwave radiation"   = "#D0021B",   # Deep red (thermal infrared)
        "Evaporative heat flux"= "#4A90D9",   # Sky blue (water/evaporation)
        "Sensible heat flux"   = "#7B68EE"    # Medium slate purple (turbulent heat)
      )) +
      ggplot2::scale_linewidth_manual(values = c(
        "Net flux"             = 1.3,   # Thicker line for net flux
        "Shortwave radiation"  = 1,     # Standard line width
        "Longwave radiation"   = 1,
        "Evaporative heat flux"= 1,
        "Sensible heat flux"   = 1
      )) +
      ggplot2::guides(
        linewidth = "none",   # Hide linewidth legend
        # Increase linewidth of net flux in legend
        colour = ggplot2::guide_legend(override.aes = list(linewidth = c(1.3)))
      )
  }
  p +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "Date", y = y_lab, colour = col_lab)
  
}
