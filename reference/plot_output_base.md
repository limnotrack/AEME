# Base plotting function for AEME output

Base plotting function for AEME output

## Usage

``` r
plot_output_base(
  aeme,
  var_sim = "HYD_temp",
  model,
  ens_n = 1,
  var_lims = NULL,
  ylim = NULL,
  plot_width = 400,
  plot_height = 200,
  bar_width = 0.08
)
```

## Arguments

- aeme:

  Aeme object.

- var_sim:

  string; of variable to plot

- model:

  character vector; model(s) to plot. If missing, all models in the Aeme
  object will be plotted.

- ens_n:

  integer; ensemble number to plot. Default is 1.

- var_lims:

  numeric vector of length 2; limits for the variable. Defaults to NULL
  and will generate common limits for all variables.

- ylim:

  numeric vector of length 2; limits for the y-axis. Defaults to NULL
  and calculates this based on the data to be plotted.

- plot_width:

  numeric; width in pixels of each panel, used to size the plotting
  device. Default is 400.

- plot_height:

  numeric; height in pixels of each panel, used to size the plotting
  device. Default is 200.

- bar_width:

  numeric; width of the colour bar as a fraction of `plot_width`.
  Default is 0.08.

## Value

A list of matrices with the plotted output, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
tmpdir <- tempdir()
aeme_dir <- system.file("extdata/lake/", package = "AEME")
# Copy files from package into tempdir
file.copy(aeme_dir, tmpdir, recursive = TRUE)
path <- file.path(tmpdir, "lake")
aeme <- yaml_to_aeme(path = path, "aeme.yaml")
path <- tempdir()
aeme <- build_aeme(aeme = aeme, model = model, path = path, 
                   ext_elev = 5) |>
  run_aeme(aeme)

plot_output_base(aeme)

# Can also use plot_output() with `backend` set to "base"
plot_output(aeme, backend = "base")
} # }
```
