#' Plot AEME variable
#'
#' @param df dataframe; output from \code{\link{get_var}}. If \code{NULL},
#' \code{\link{get_var}} will be called and \code{var_sim} will be used to
#' extract the variable of interest from \code{aeme} for each model.
#' @inheritParams plot_output
#' @param obs list; output from \code{\link{observations}}
#' @param xlim numeric; x-axis limits
#'
#' @return ggplot2 object or list of ggplot2 objects
#' @export

plot_var <- function(df = NULL, aeme, model, var_sim, ylim = NULL, xlim,
                     var_lims, obs = NULL, add_obs = TRUE, level = FALSE,
                     facet = FALSE, cumulative = FALSE, print_plots = FALSE) {

  utils::data("key_naming", package = "AEME", envir = environment())

  if (is.null(df)) {
    df <- get_var(aeme = aeme, model = model, var_sim = var_sim,
                  return_df = TRUE, cumulative = cumulative)
  } else {
    var_sim <- unique(df$var_sim)
    var_lims <- range(df$value, na.rm = TRUE)
    xlim <- range(df$Date)
  }

  df <- df |>
    dplyr::left_join(key_naming[, c("name", "name_parse", "name_text")],
                     by = c("var_sim" = "name"))

  if (!all(is.na(df$lyr_thk))) {
    # Plot variables with depth
    if (all(is.na(df$value))) {
      p <- ggplot2::ggplot() +
        ggplot2::theme_bw()
      if (print_plots) print(p)
      return(p)
    }

    if (facet) {
      p <- plot_var_depth(df, obs, ylim, xlim, var_lims, add_obs)
      if (print_plots) print(p)
      return(p)
    } else {
      # Create a list of plots for each model
      model2 <- unique(df$Model)
      names(model2) <- model2
      lst <- lapply(model2, \(m) {
        df2 <- df |>
          dplyr::filter(Model == m)
        obs2 <- obs
        if (!is.null(obs2[["lake_adj"]])) {
          obs2$lake_adj <- obs2$lake_adj |>
            dplyr::filter(Model == m)
        }
        plot_var_depth(df = df2, obs = obs2, ylim = ylim, xlim = xlim,
                       var_lims = var_lims, add_obs = add_obs,
                       print_plots = print_plots)
      })
      return(lst)
    }
  } else {
    # Plot variables with no depth
    y_lab <- eval(parse(text = df$name_parse[1]))

    p <- ggplot2::ggplot() +
      ggplot2::geom_line(data = df, ggplot2::aes(Date, value,
                                                 colour = Model)) +
      {if (!is.null(ylim)) ggplot2::coord_cartesian(ylim = ylim)} +
      {if (facet) ggplot2::facet_grid(Model ~ name_text)} +
      ggplot2::ylab(y_lab) +
      ggplot2::xlim(xlim) +
      ggplot2::theme_bw()
    # Add water level observations
    if ("LKE_lvlwtr" %in% var_sim & !is.null(obs[["level_adj"]])) {

      obs_lvl <- obs$level_adj
      if (add_obs & nrow(obs_lvl) > 0) {
        p <- p +
          ggplot2::geom_point(data = obs_lvl,
                              ggplot2::aes(Date, lvl_adj, fill = "Obs")) +
          ggplot2::labs(fill = "")
      }
    }

    # Catch no observations
    if (!is.atomic(obs$lake)) {
      # Initialise an empty dataframe
      obs_sub <- data.frame(Date = as.Date(character(0)),
                             value = numeric(0),
                             var_aeme = character(0),
                             Model = character(0))
    } else {
      obs_sub <- obs$lake
    }


    if (nrow(obs_sub) > 0) {
      obs_sub <- obs_sub |>
        dplyr::filter(var_aeme == var_sim & Date >= min(df$Date) &
                        Date <= max(df$Date))
    }

    if (nrow(obs_sub) > 0 & add_obs) {
      p <- p +
        ggplot2::geom_point(data = obs_sub,
                            ggplot2::aes(Date, value, fill = "Obs"),
                            colour = "black") +
        ggplot2::labs(fill = "")
    }

  }
  if (print_plots) print(p)
  return(p)
}

#' Plot variable with depth component
#' @noRd
plot_var_depth <- function(df, obs, ylim, xlim, var_lims, add_obs,
                           print_plots = FALSE) {

  my_cols <- RColorBrewer::brewer.pal(11, "Spectral")
  fill_lab <- eval(parse(text = df$name_parse[1]))

  p <- ggplot2::ggplot() +
    ggplot2::geom_col(data = df, ggplot2::aes(x = Date, y = lyr_thk,
                                              fill = value),
                      position = 'stack', width = 1) +
    ggplot2::scale_fill_gradientn(colors = rev(my_cols),
                                  # name = bquote(.(df$name_parse[1])),
                                  limits = var_lims) +
    {if(!is.null(ylim)) ggplot2::coord_cartesian(ylim = ylim)} +
    ggplot2::labs(fill = bquote(.(fill_lab))) +
    ggplot2::facet_grid(Model ~ name_text) +
    ggplot2::xlim(xlim) +
    ggplot2::ylab("Elevation (m)") +
    ggplot2::xlab(NULL) +
    ggplot2::theme_bw()

  if (!is.null(obs[["lake_adj"]]) & add_obs) {

    p <- p +
      ggplot2::geom_point(data = obs$lake_adj,
                          ggplot2::aes(Date, elev, fill = value,
                                       size = "Obs"), shape = 21,
                          colour = "black") +
      ggplot2::labs(size = "")
  }

  if (!is.null(obs[["level_adj"]])) {
    obs_lvl <- obs[["level_adj"]]

    p <- p +
      ggplot2::geom_line(data = obs_lvl, ggplot2::aes(Date, lvl_adj),
                         linewidth = 0.7, colour = "grey")
  }
  if (print_plots) print(p)
  p
}
