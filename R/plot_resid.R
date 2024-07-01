#' Plot model residuals
#'
#' @inheritParams build_aeme
#' @inheritParams plot_output
#'
#' @return list of ggplot2 objects
#'
#' @importFrom dplyr mutate
#' @importFrom lubridate yday
#' @importFrom ggplot2 geom_point geom_abline labs scale_colour_viridis_d facet_grid coord_equal theme_bw geom_hline geom_vline geom_density scale_fill_viridis_d scale_colour_viridis_d guides
#' @importFrom patchwork wrap_plots
#' @importFrom RColorBrewer brewer.pal
#'
#' @export
#'

plot_resid <- function(aeme, model, var_sim = "HYD_temp") {

  # Extract observations
  obs <- observations(aeme)
  my_cols <- RColorBrewer::brewer.pal(11, "Spectral")

  # Loop through variables and generate plots
  plist <- list()
  lst <- lapply(var_sim, \(v) {
    ls <- list()

    # Extract variable from aeme
    df <- get_var(aeme = aeme, model = model, var_sim = v,
                  use_obs = TRUE) |>
      dplyr::mutate(resid = sim - obs, yday = lubridate::yday(Date))
    dep_chk <- "depth_mid" %in% names(df)

    if ("depth_mid" %in% names(df)) {
      df <- df |>
        dplyr::mutate(fdepth = factor(depth_mid))
    }
    n_depths <- unique(df$depth_mid) |> length()
    if (n_depths > 15) {
      # Group depths into 10 categories
      df <- df |>
        dplyr::mutate(fdepth = cut(depth_mid, breaks = 15))
      levs <- levels(df$fdepth)
      levs <- gsub(",", "-", levs)
      levs <- gsub("\\(|]", "", levs)
      df <- df |>
        dplyr::mutate(fdepth = factor(fdepth, labels = levs))
    }
    col_label <- ifelse(dep_chk, "Depth (m)", "")

    lims <- range(df$sim, df$obs)
    ls[[length(ls) + 1]] <- ggplot2::ggplot() +
      {if (dep_chk) ggplot2::geom_point(data = df,
                                        ggplot2::aes(x = obs, y = sim,
                                                     colour = fdepth))} +
      {if (!dep_chk) ggplot2::geom_point(data = df, ggplot2::aes(x = obs,
                                                                 y = sim))} +
      ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
      ggplot2::labs(x = "Observations", y = "Modelled",
                    colour = col_label) +
      ggplot2::scale_colour_viridis_d(direction = -1, option = "C") +
      ggplot2::facet_grid(var_sim ~ Model) +
      ggplot2::coord_equal(xlim = lims, ylim = lims) +
      ggplot2::theme_bw()
    # p1

    ls[[length(ls) + 1]] <- ggplot2::ggplot() +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
      {if (dep_chk) ggplot2::geom_point(data = df,
                                        ggplot2::aes(x = yday, y = resid,
                                                     colour = fdepth))} +
      {if (!dep_chk) ggplot2::geom_point(data = df, ggplot2::aes(x = yday, y = resid))} +
      ggplot2::labs(x = "Day of year", y = "Residuals",
                    colour = col_label) +
      ggplot2::facet_grid(var_sim ~ Model) +
      ggplot2::scale_colour_viridis_d(direction = -1, option = "C") +

      ggplot2::theme_bw()
    # p2

    if (dep_chk) {
      ls[[length(ls) + 1]] <- ggplot2::ggplot() +
        ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
        {if (dep_chk) ggplot2::geom_point(data = df,
                                          ggplot2::aes(x = resid, y = depth_mid,
                                                       colour = obs))} +
        ggplot2::labs(x = "Residuals", y = "Depth (m)",
                      colour = "") +
        ggplot2::facet_grid(var_sim ~ Model) +
        ggplot2::scale_colour_gradientn(colors = rev(my_cols)) +
        ggplot2::scale_y_reverse() +
        ggplot2::theme_bw()
    }

    # p3
    ls[[length(ls) + 1]] <- ggplot2::ggplot() +
      ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
      ggplot2::geom_density(data = df, ggplot2::aes(x = resid,
                                                    fill = Model,
                                                    colour = Model,
                                                    alpha = 0.3)) +
      ggplot2::scale_colour_viridis_d(option = "A") +
      ggplot2::guides(alpha = "none") +
      ggplot2::scale_fill_viridis_d(option = "G", begin = 0.3,
                                    end = 0.8) +
      ggplot2::labs(y = "Density", x = "Residuals") +
      ggplot2::facet_grid(~ var_sim) +
      ggplot2::theme_bw()


    pall <- patchwork::wrap_plots(ls, ncol = 2, guides = "collect")

    return(pall)
  })
  names(lst) <- var_sim

  return(lst)
}
