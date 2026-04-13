# Plot GLM-AED diagnostic variables to debug unrealistic oxygen values

Reads model output using read_model_outputs() and produces a multi-panel
diagnostic plot organised into thematic groups: oxygen budget, physical
drivers, mixing, biological demand, and sediment fluxes.

## Usage

``` r
plot_glm_diagnostics(
  aeme,
  lake_dir,
  surface_depth = 0.5,
  bottom_depth = NULL,
  dates = NULL,
  phyto_pars = NULL,
  output_dir = NULL
)
```

## Arguments

- aeme:

  An AEME object. Required if `lake_dir` is not provided.

- lake_dir:

  Character. Path to the lake model output directory. If missing,
  derived from `aeme` and `path`.

- surface_depth:

  Numeric. Depth (m) to extract for "surface" values. Defaults to 0.5.

- bottom_depth:

  Numeric or NULL. Depth (m) to extract for "bottom" values. If NULL,
  the deepest available layer is used. Defaults to NULL.

- dates:

  Date vector or NULL. Subset of dates to plot. NULL = all.

- phyto_pars:

  Dataframe of phytoplankton parameters (passed through to
  read_model_outputs). Defaults to NULL.

- output_dir:

  Character or NULL. Directory to save PDF output. If NULL, plots are
  displayed interactively. Defaults to NULL.

## Value

Invisibly returns a list containing the `out` object from
read_model_outputs and the assembled ggplot page objects.
