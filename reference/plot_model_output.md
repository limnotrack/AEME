# Plot a variable from model output – an `Aeme` object or a raw output list

A lightweight ggplot2 plotting helper that works either directly on the
list returned by
[`read_glm_output()`](https://limnotrack.com/reference/read_glm_output.md),
[`read_gotm_output()`](https://limnotrack.com/reference/read_gotm_output.md),
[`read_simstrat_output()`](https://limnotrack.com/reference/read_simstrat_output.md),
or
[`read_model_outputs()`](https://limnotrack.com/reference/read_model_outputs.md)
– for when you don't have (or don't want to build) a full `Aeme` object
– or on an `Aeme` object itself, in which case
[`get_var()`](https://limnotrack.com/reference/get_var.md)/[`plot_var()`](https://limnotrack.com/reference/plot_var.md)
do the work for you. Dispatches on the shape of the requested variable:
a depth x time tile plot for `(z, time)` variables (same rendering as
[`plot_output()`](https://limnotrack.com/reference/plot_output.md)'s
default backend), a simple time series for `(time)`-only variables (e.g.
evaporation), and a line plot (one line per combination of its non-time
dimensions) for a
[`new_grouped_var()`](https://limnotrack.com/reference/new_grouped_var.md)
variable, i.e. one with dimensions other than `(time)`/`(z, time)` –
e.g. one line per sediment zone for a GLM-AED `(nzones, time)` AED flux
variable.

## Usage

``` r
plot_model_output(
  x,
  var_sim,
  model = NULL,
  ens_n = 1,
  remove_spin_up = TRUE,
  var_lims = NULL,
  ylim = NULL
)
```

## Arguments

- x:

  either an `Aeme` object, or a list as returned by
  [`read_glm_output()`](https://limnotrack.com/reference/read_glm_output.md)/[`read_gotm_output()`](https://limnotrack.com/reference/read_gotm_output.md)/[`read_simstrat_output()`](https://limnotrack.com/reference/read_simstrat_output.md)/
  [`read_model_outputs()`](https://limnotrack.com/reference/read_model_outputs.md).

- var_sim:

  character; name of the variable to plot, as it appears in `names(x)`
  (or in the model output list, if `x` is an `Aeme` object) – a
  `var_aeme` name if
  [key_naming](https://limnotrack.com/reference/key_naming.md) has a
  translation for it, otherwise its raw model/netCDF name.

- model:

  character; model to plot, when `x` is an `Aeme` object with more than
  one model. Ignored (and unnecessary) when `x` is already a raw output
  list. Defaults to the first model in
  [`list_models()`](https://limnotrack.com/reference/list_models.md) if
  not supplied.

- ens_n:

  integer; ensemble member to plot, when `x` is an `Aeme` object.
  Default `1`.

- remove_spin_up:

  logical; when `x` is an `Aeme` object, drop the spin-up period before
  plotting (see
  [`get_date_index()`](https://limnotrack.com/reference/get_date_index.md)).
  Default `TRUE`. Ignored when `x` is already a raw output list – trim
  it yourself first if needed.

- var_lims:

  numeric vector of length 2; colour scale limits for a depth x time
  plot. Default `NULL` (ranged to the data).

- ylim:

  numeric vector of length 2; y-axis limits for a line plot. Default
  `NULL` (ranged to the data).

## Value

A ggplot2 object (the long-format data frame instead, with a warning,
for a grouped variable that has no `Date` dimension at all).

## Details

For observation overlay, faceting across multiple models/variables at
once, or the base-graphics backend, use
[`plot_output()`](https://limnotrack.com/reference/plot_output.md)
instead – this function is intentionally minimal, and does not (yet)
support `aeme_grouped_var` variables when called on an `Aeme` object
through
[`plot_output()`](https://limnotrack.com/reference/plot_output.md)/[`plot_output_base()`](https://limnotrack.com/reference/plot_output_base.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# On a raw output list
out <- read_glm_output(file = outfile)
plot_model_output(out, "HYD_temp")        # depth x time tile plot
plot_model_output(out, "LKE_lvlwtr")      # simple time series
plot_model_output(out, "SDF_Fsed_oxy_Z")  # one line per sediment zone

# On an Aeme object directly
plot_model_output(aeme, "HYD_temp", model = "glm_aed")
} # }
```
