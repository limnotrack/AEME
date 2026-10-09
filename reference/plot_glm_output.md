# Plot a variable from a raw read_glm_output()/read_model_outputs() list

A thin, backward-compatible alias for
[`plot_model_output()`](https://limnotrack.com/reference/plot_model_output.md)
– kept for existing callers. New code should call
[`plot_model_output()`](https://limnotrack.com/reference/plot_model_output.md)
directly, which also accepts an `Aeme` object.

## Usage

``` r
plot_glm_output(out, var_sim, var_lims = NULL, ylim = NULL)
```

## Arguments

- out:

  list; as returned by
  [`read_glm_output()`](https://limnotrack.com/reference/read_glm_output.md)
  or
  [`read_model_outputs()`](https://limnotrack.com/reference/read_model_outputs.md)
  (with `model = "glm_aed"`).

- var_sim:

  character; name of the variable to plot, as it appears in `names(x)`
  (or in the model output list, if `x` is an `Aeme` object) – a
  `var_aeme` name if
  [key_naming](https://limnotrack.com/reference/key_naming.md) has a
  translation for it, otherwise its raw model/netCDF name.

- var_lims:

  numeric vector of length 2; colour scale limits for a depth x time
  plot. Default `NULL` (ranged to the data).

- ylim:

  numeric vector of length 2; y-axis limits for a line plot. Default
  `NULL` (ranged to the data).

## Value

A ggplot2 object – see
[`plot_model_output()`](https://limnotrack.com/reference/plot_model_output.md).

## Examples

``` r
if (FALSE) { # \dontrun{
out <- read_glm_output(file = outfile)
plot_glm_output(out, "HYD_temp")        # depth x time tile plot
plot_glm_output(out, "LKE_lvlwtr")      # simple time series
plot_glm_output(out, "SDF_Fsed_oxy_Z")  # one line per sediment zone
} # }
```
