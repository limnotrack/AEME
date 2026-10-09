# Set inflow data for a GLM-AED simulation directory

Thin, `aeme`-free wrapper around the internal inflow writer used by
[`build_aeme()`](https://limnotrack.com/reference/build_aeme.md). Writes
one boundary-condition csv per inflow into `path_glm/bcs` and updates
the `&inflow` block of the GLM nml file to point at them.

## Usage

``` r
set_glm_inflows(
  path_glm,
  list_inf,
  inf_factor = 1,
  mass = TRUE,
  glm_file = find_glm_nml(path_glm)
)
```

## Arguments

- path_glm:

  filepath; to GLM-AED directory (containing the nml file and a `bcs/`
  subdirectory)

- list_inf:

  named list of data.frames, one per inflow. Each must have a `Date`
  column plus flow/temperature/salt columns (e.g. `HYD_flow`,
  `HYD_temp`, `CHM_salt`) – see
  [`add_inflow()`](https://limnotrack.com/reference/add_inflow.md) for
  the expected schema.

- inf_factor:

  numeric; scaling factor applied to all inflow flow rates. Default is
  `1`.

- mass:

  logical; convert inflow variables to GLM-AED mass units using the
  package's built-in conversion table. Default is `TRUE`.

- glm_file:

  filepath; path to the GLM hydrodynamic nml file to update. Defaults to
  the file found in `path_glm` via
  [`find_glm_nml()`](https://limnotrack.com/reference/find_glm_nml.md).

## Value

invisibly, the updated nml object

## Examples

``` r
if (FALSE) { # \dontrun{
set_glm_inflows(path_glm, list_inf = list(stream1 = inflow_df))
} # }
```
