# Plot AEME output

Generates a contour plot for z-dimension variables and line plot for 1-d
variables.

## Usage

``` r
plot_output(
  aeme,
  model,
  var_sim = "HYD_temp",
  point_size = 2,
  ens_n = 1,
  add_obs = TRUE,
  level = FALSE,
  remove_spin_up = TRUE,
  print_plots = FALSE,
  var_lims = NULL,
  ylim = NULL,
  cumulative = FALSE,
  facet = TRUE
)
```

## Arguments

- aeme:

  aeme; object.

- model:

  character vector; model(s) to plot. If missing, all models in the Aeme
  object will be plotted.

- var_sim:

  string; of variable to plot

- ens_n:

  integer; ensemble number to plot. Default is 1.

- add_obs:

  logical; add observations to plot

- level:

  logical; include lake level. Only applies for contour plots.

- remove_spin_up:

  logical; remove spin-up period from plot. Default is TRUE.

- print_plots:

  logical; print plots

- var_lims:

  numeric vector of length 2; limits for the variable. Defaults to NULL
  and will generate common limits for all variables.

- ylim:

  numeric vector of length 2; limits for the y-axis. Defaults to NULL
  and calculates this based on the data to be plotted.

- cumulative:

  logical; plot cumulative sum of variable

- facet:

  logical; if `TRUE`, for variables with depth, plot each model in a
  separate facet. If `FALSE`, plot each model in a separate plot and
  return a list of plots. If `TRUE`, for variables without depth, plot
  each model in a separate facet. If `FALSE`, plot each model as a
  separate line and return a plot. This only applies to variables
  without a depth component.

## Value

list of plots for z-dimensional variables or a ggplot2 object for 1-d
variables.

## Examples

``` r
if (FALSE) { # \dontrun{
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- get_model_controls()
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  #'   model <- c("glm_aed", "gotm_wet")
  build_aeme(path = path, aeme = aeme, model = model,
                 model_controls = model_controls, inf_factor = inf_factor, ext_elev = 5,
                 use_bgc = TRUE)
  run_aeme(aeme = aeme, model = model, verbose = FALSE, path = path,
           parallel = TRUE)

  aeme <- load_output(model = model, aeme = aeme, path = path,
                           model_controls = model_controls, parallel = TRUE)

  p1 <- plot_output(aeme = aeme, model = model,
                    var_sim = "HYD_temp", level = TRUE,
                    print_plots = FALSE, var_lims = c(0, 30),
                    ylim = c(0, 16))
  p1[[1]]

  p2 <- plot_output(aeme = aeme, model = model,
                    var_sim = "LKE_evpvol", print_plots = TRUE,
                    ylim = c(0, 0.02))
} # }
```
