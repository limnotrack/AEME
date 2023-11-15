#' Plot AEME output
#'
#' Generates a contour plot for z-dimension variables and line plot for 1-d
#' variables.
#'
#' @inheritParams build_ensemble
#' @param var_sim string; of variable to plot
#' @param level boolean; include lake level. Only applies for contour plots.
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
#'                     var_sim = "HYD_temp", level = TRUE,
#'                     print_plots = FALSE, var_lims = c(0, 30),
#'                     ylim = c(0, 16))
#'   p1[[1]]
#'
#'   p2 <- plot_output(aeme_data = aeme_data, model = model,
#'                     var_sim = "LKE_evpvol", print_plots = TRUE,
#'                     ylim = c(0, 0.02))
#' }
#'

plot_output <- function(aeme_data, model, var_sim = "HYD_temp", add_obs = TRUE,
                        level = FALSE, print_plots = FALSE,
                        var_lims = NULL, ylim = NULL, cumulative = FALSE,
                        facet = TRUE) {

  # Check if aeme_data is a aeme class
  if (!inherits(aeme_data, "aeme")) stop("aeme_data must be an aeme class")

  # Check if model is a character vector
  if (!is.character(model)) stop("model must be a character vector")

  # Check if model length is 0
  if (length(model) == 0) stop("model must be a character vector of length >0")

  # Check if var_sim is a character vector
  if (!is.character(var_sim)) stop("var_sim must be a character vector")

  # Load data from aeme_data
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
  if (any(!chk)) {
    if (all(!chk)) {
      stop(paste0("Variable '", var_sim, "' not in output for model(s) ",
                  paste0(model, collapse = ", ")))
    }
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

  # Date lims
  # Find date range and have output in Date format
  this.list <- sapply(model, \(m){
    "[["(outp[[m]], "Date")
  })
  xlim <- as.Date(range(this.list, na.rm = TRUE))

  # mod_labels <- data.frame(model = c("dy_cd", "glm_aed", "gotm_wet"),
  #                          name = c("DYRESM-CAEDYM", "GLM-AED", "GOTM-WET"))

  df <- get_var(aeme_data = aeme_data, model = model, var_sim = var_sim,
                return_df = TRUE, cumulative = cumulative)

  # Align observations to modelled depths because observations are relative to
  # the lake surface while modelled depths are relative to the lake bottom
  obs <- align_depth_data(aeme_data = aeme_data, model = model,
                          var_sim = var_sim)


  plot_var(df = df, ylim = ylim, xlim = xlim, var_lims = var_lims, obs = obs,
           add_obs = add_obs, level = level, facet = facet,
           print_plots = print_plots)
}
