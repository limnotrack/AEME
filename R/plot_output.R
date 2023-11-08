#' Plot AEME output
#'
#' Generates a contour plot for z-dimension variables and line plot for 1-d
#' variables.
#'
#' @inheritParams build_ensemble
#' @param var_sim string; of variable to plot
#' @param level boolean; include lake level. Only applies for contour plots.
#' @param label boolean; include variable label as subtitle for contour plots.
#' @param print_plots boolean; print plots
#' @param var_lims numeric vector of length 2; limits for the variable.
#' Defaults to NULL and will generate common limits for all variables.
#' @param ylim numeric vector of length 2; limits for the y-axis. Defaults to
#' NULL and calculates this based on the data to be plotted.
#' @param add_obs boolean; add observations to plot
#'
#' @return list of plots for z-dimensional variables or a ggplot2 object for 1-d
#' variables.
#' @export
#'
#' @importFrom RColorBrewer brewer.pal
#' @importFrom ggplot2 ggplot geom_col aes scale_fill_gradientn coord_cartesian
#' xlab ylab labs ggtitle theme_bw
#' @examples
#' \dontrun{
#'   tmpdir <- tempdir()
#'   aeme_dir <- system.file("extdata/lake/", package = "AEME")
#'   # Copy files from package into tempdir
#'   file.copy(aeme_dir, tmpdir, recursive = TRUE)
#'   path <- file.path(tmpdir, "lake")
#'   aeme_data <- yaml_to_aeme(path = path, "aeme.yaml")
#'   mod_ctrls <- read.csv(file.path(path, "model_controls.csv"))
#'   inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
#'   outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
#'   #'   model <- c("glm_aed", "gotm_wet")
#'   build_ensemble(path = path, aeme_data = aeme_data, model = model,
#'                  mod_ctrls = mod_ctrls, inf_factor = inf_factor, ext_elev = 5,
#'                  use_bgc = TRUE)
#'   run_aeme(aeme_data = aeme_data, model = model, verbose = FALSE, path = path,
#'            parallel = TRUE)
#'
#'   aeme_data <- load_output(model = model, aeme_data = aeme_data, path = path,
#'                            mod_ctrls = mod_ctrls, parallel = TRUE)
#'
#'   p1 <- plot_output(aeme_data = aeme_data, model = model,
#'                     var_sim = "HYD_temp", level = TRUE, label = TRUE,
#'                     print_plots = FALSE, var_lims = c(0, 30),
#'                     ylim = c(0, 16))
#'   p1[[1]]
#'
#'   p2 <- plot_output(aeme_data = aeme_data, model = model,
#'                     var_sim = "HYD_evap", print_plots = TRUE,
#'                     ylim = c(0, 0.02))
#' }
#'

plot_output <- function(aeme_data, model, var_sim = "HYD_temp", add_obs = TRUE,
                        level = FALSE, label = FALSE, print_plots = TRUE,
                        var_lims = NULL, ylim = NULL) {

  obs <- observations(aeme_data)
  inp <- input(aeme_data)
  if (!is.null(obs$lake)) {
    obs_lake <- obs$lake |>
      dplyr::filter(var == var_sim)
  } else {
    obs_lake <- NULL
  }
  if (!is.null(obs$level)) {
    obs_level <- obs$level
  }

  outp <- output(aeme_data)

  # Check if var_sim is in output
  chk <- sapply(model, \(m){
    var_sim %in% names(outp[[m]])
  })
  if (!all(chk)) {
    warning(paste0("Variable '", var_sim, "' not in output for model(s) ",
                   paste0(model[!chk], collapse = ", ")))
    model <- model[chk]
  }

  # colour lims
  if (is.null(var_lims)) {
    this.list <- sapply(model, \(m){
      "[["(outp[[m]], var_sim)
    })
    vect <- unlist(this.list)
    var_lims <- range(c(vect, obs_lake[["value"]]), na.rm = TRUE)
  }

  mod_labels <- data.frame(model = c("dy_cd", "glm_aed", "gotm_wet"),
                           name = c("DYRESM-CAEDYM", "GLM-AED", "GOTM-WET"))

  lst <- lapply(model, \(m) {

    variable <- outp[[m]][[var_sim]]

    if (is.null(variable)) {
      df <- data.frame(Date = NA, value = NA, Model = NA, lyr_thk = NA)
    } else if (length(dim(variable)) == 1 | is.numeric(variable)) {
      df <- data.frame(Date = outp[[m]][["Date"]],
                       value = variable,
                       Model = m)
      return(df)
    } else {
      depth <- data.frame(Date = outp[[m]][["Date"]],
                          depth = outp[[m]][["HYD_wlev"]])
      lyr <- outp[[m]][["LAYERS"]]
      df <- data.frame(Date = rep(outp[[m]][["Date"]], each = nrow(variable)),
                       lyr_top = unlist(lyr),
                       value = unlist(variable),
                       Model = m) |>
        dplyr::mutate(lyr_thk = ifelse(c(-999, diff(lyr_top)) < 0, # | is.na(-c(NA,diff(lyr_top))),
                                       c(diff(lyr_top),NA),
                                       c(NA,diff(lyr_top))))
    }



    if (all(is.na(df$value))) {
      p <- ggplot2::ggplot() +
        ggplot2::ggtitle(mod_labels$name[mod_labels$model == m]) +
        #  facet_wrap(~contourDF$hydroyr) +
        ggplot2::theme_bw()
      return(p)
    }

    my_cols <- RColorBrewer::brewer.pal(11, "Spectral")

    p <- ggplot2::ggplot() +
      ggplot2::geom_col(data = df, ggplot2::aes(x = Date, y = lyr_thk, fill = value),
                        position = 'stack', width = 1) +
      ggplot2::scale_fill_gradientn(colors = rev(my_cols), name = var_sim,
                                    limits = var_lims) +
      {if(!is.null(ylim)) ggplot2::coord_cartesian(ylim = ylim)} +
      ggplot2::ylab("Elevation (m)") +
      ggplot2::xlab(NULL) +
      ggplot2::labs(fill="xyz") +
      ggplot2::ggtitle(mod_labels$name[mod_labels$model == m]) +
      #  facet_wrap(~contourDF$hydroyr) +
      ggplot2::theme_bw()

    if (label) {
      p <- p +
        ggplot2::labs(subtitle = var_sim)
    }

    if (!is.null(obs$lake) & add_obs) {

      obs_df <- obs_lake |>
        dplyr::filter(Date %in% df$Date) |>
        merge(x = _, depth, by = "Date") |>
        dplyr::mutate(elev = depth - depth_from) |>
        dplyr::filter(elev >= 0)

      p <- p +
        ggplot2::geom_point(data = obs_df,
                            ggplot2::aes(Date, elev, fill = value,
                                         size = "Obs"), shape = 21,
                            colour = "black") +
        ggplot2::labs(size = "")
    }

    if (!is.null(obs$level)) {
      obs_lvl <- obs$level |>
        dplyr::filter(Date %in% df$Date) |>
        dplyr::mutate(lvl_adj = lvlwtr - min(inp$hypsograph$elev))
      # obs_level <- obs_level |>
      #   dplyr::filter(Date %in% df$Date)
      p <- p +
        ggplot2::geom_line(data = obs_lvl, ggplot2::aes(Date, lvl_adj),
                           linewidth = 0.7, colour = "grey")
    }

    if (print_plots) {
      print(p)
    }

    return(p)
  })
  names(lst) <- model

  if (is.data.frame(lst[[1]])) {
    df2 <- do.call(rbind, lst) |>
      dplyr::mutate(Model = dplyr::case_when(
        Model == "dy_cd" ~ "DYRESM-CAEDYM",
        Model == "glm_aed" ~ "GLM-AED",
        Model == "gotm_wet" ~ "GOTM-WET"
        )
      )

    p <- ggplot2::ggplot() +
      ggplot2::geom_line(data = df2, ggplot2::aes(Date, value,
                                                  colour = Model)) +
      {if(!is.null(ylim)) ggplot2::coord_cartesian(ylim = ylim)} +
      ggplot2::ggtitle(var_sim) +
      ggplot2::theme_bw()

    if (var_sim == "HYD_wlev" & !is.null(obs$level)) {

      obs_lvl <- obs$level |>
        dplyr::filter(Date %in% df2$Date) |>
        dplyr::mutate(lvl_adj = lvlwtr - min(inp$hypsograph$elev))

      if (add_obs) {
        p <- p +
          ggplot2::geom_point(data = obs_lvl,
                              ggplot2::aes(Date, lvl_adj, fill = "Obs")) +
          # ggplot2::scale_colour_manual(values = c("Obs" = "black")) +
          ggplot2::labs(fill = "")
      }
    }

    if (print_plots) {
      print(p)
    }
    lst <- p
  }

  return(lst)
}
