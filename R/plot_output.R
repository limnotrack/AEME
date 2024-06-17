#' Plot AEME output
#'
#' Generates a contour plot for z-dimension variables and line plot for 1-d
#' variables.
#'
#' @inheritParams build_aeme
#' @param var_sim string; of variable to plot
#' @param level logical; include lake level. Only applies for contour plots.
#' @param remove_spin_up logical; remove spin-up period from plot. Default is
#' TRUE.
#' @param print_plots logical; print plots
#' @param var_lims numeric vector of length 2; limits for the variable.
#' Defaults to NULL and will generate common limits for all variables.
#' @param ylim numeric vector of length 2; limits for the y-axis. Defaults to
#' NULL and calculates this based on the data to be plotted.
#' @param add_obs logical; add observations to plot
#' @param cumulative logical; plot cumulative sum of variable
#' @param facet logical; if \code{TRUE}, for variables with depth, plot each
#' model in a separate facet. If \code{FALSE}, plot each model in a separate
#' plot and return a list of plots. If \code{TRUE}, for variables without depth,
#' plot each model in a separate facet. If \code{FALSE}, plot each model as a
#' separate line and return a plot.
#' This only applies to variables without a depth component.
#'
#' @return list of plots for z-dimensional variables or a ggplot2 object for 1-d
#' variables.
#' @export
#'
#' @importFrom RColorBrewer brewer.pal
#' @importFrom ggplot2 ggplot geom_col aes scale_fill_gradientn coord_cartesian
#' xlab ylab labs ggtitle theme_bw
#' @importFrom withr local_locale local_timezone
#'
#' @examples
#' \dontrun{
#'   tmpdir <- tempdir()
#'   aeme_dir <- system.file("extdata/lake/", package = "AEME")
#'   # Copy files from package into tempdir
#'   file.copy(aeme_dir, tmpdir, recursive = TRUE)
#'   path <- file.path(tmpdir, "lake")
#'   aeme <- yaml_to_aeme(path = path, "aeme.yaml")
#'   model_controls <- get_model_controls()
#'   inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
#'   outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
#'   #'   model <- c("glm_aed", "gotm_wet")
#'   build_aeme(path = path, aeme = aeme, model = model,
#'                  model_controls = model_controls, inf_factor = inf_factor, ext_elev = 5,
#'                  use_bgc = TRUE)
#'   run_aeme(aeme = aeme, model = model, verbose = FALSE, path = path,
#'            parallel = TRUE)
#'
#'   aeme <- load_output(model = model, aeme = aeme, path = path,
#'                            model_controls = model_controls, parallel = TRUE)
#'
#'   p1 <- plot_output(aeme = aeme, model = model,
#'                     var_sim = "HYD_temp", level = TRUE,
#'                     print_plots = FALSE, var_lims = c(0, 30),
#'                     ylim = c(0, 16))
#'   p1[[1]]
#'
#'   p2 <- plot_output(aeme = aeme, model = model,
#'                     var_sim = "LKE_evpvol", print_plots = TRUE,
#'                     ylim = c(0, 0.02))
#' }
#'

plot_output <- function(aeme, model, var_sim = "HYD_temp", add_obs = TRUE,
                        level = FALSE, remove_spin_up = TRUE,
                        print_plots = FALSE, var_lims = NULL, ylim = NULL,
                        cumulative = FALSE, facet = TRUE) {

  # Set timezone temporarily to UTC
  withr::local_locale(c("LC_TIME" = "C"))
  withr::local_timezone("UTC")

  # Check if aeme is a aeme class
  if (!inherits(aeme, "aeme")) stop("aeme must be an aeme class")

  # Check if model is a character vector
  if (!is.character(model)) stop("model must be a character vector")

  # Check if model length is 0
  if (length(model) == 0) stop("model must be a character vector of length >0")

  # Check if var_sim is a character vector
  if (!is.character(var_sim)) stop("var_sim must be a character vector")

  # Load data from aeme
  obs <- observations(aeme)
  inp <- input(aeme)
  tme <- time(aeme)

  if (!is.null(obs$level)) {
    obs_level <- obs$level
  }

  outp <- output(aeme)

  # Check if var_sim is in output
  chk <- sapply(model, \(m){
    var_sim %in% names(outp[[m]])
  })
  if (any(!chk)) {
    if (all(!chk)) {
      stop(paste0("Variable '", var_sim, "' not in output for model(s) ",
                  paste0(model, collapse = ", ")))
    }
    warning(paste0("Variable '", var_sim, "' not in output for model(s) ",
                   paste0(model[!chk], collapse = ", ")))
    model <- model[chk]
  }

  # Date lims
  # Find date range and have output in Date format
  this.list <- sapply(model, \(m){
    "[["(outp[[m]], "Date")
  })
  xlim <- as.Date(range(this.list, na.rm = TRUE))
  if (remove_spin_up) {
    xlim <- c(as.Date(tme$start), as.Date(tme$stop))
  }

  # Filter observations by variable and Date
  if (!is.null(obs$lake)) {
    obs_lake <- obs$lake |>
      dplyr::filter(var_aeme == var_sim & Date >= xlim[1] & Date <= xlim[2])
  } else {
    obs_lake <- NULL
  }

  # colour lims
  if (is.null(var_lims)) {
    this.list <- sapply(model, \(m){
      "[["(outp[[m]], var_sim)
    })
    vect <- unlist(this.list)
    if (add_obs) {
      var_lims <- range(c(vect, obs_lake[["value"]]), na.rm = TRUE)
    } else {
      var_lims <- range(vect, na.rm = TRUE)
    }
  }

  # mod_labels <- data.frame(model = c("dy_cd", "glm_aed", "gotm_wet"),
  #                          name = c("DYRESM-CAEDYM", "GLM-AED", "GOTM-WET"))

  df <- get_var(aeme = aeme, model = model, var_sim = var_sim,
                return_df = TRUE, remove_spin_up = remove_spin_up,
                cumulative = cumulative)

  # Align observations to modelled depths because observations are relative to
  # the lake surface while modelled depths are relative to the lake bottom
  obs <- align_depth_data(aeme = aeme, model = model,
                          var_sim = var_sim)


  plot_var(df = df, ylim = ylim, xlim = xlim, var_lims = var_lims, obs = obs,
           add_obs = add_obs, level = level, facet = facet,
           print_plots = print_plots)
}
