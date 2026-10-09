# Set outflow data for a GLM-AED simulation directory

Thin, `aeme`-free wrapper around the internal outflow writer used by
[`build_aeme()`](https://limnotrack.com/reference/build_aeme.md). Writes
one boundary-condition csv per outflow into `path_glm/bcs` and updates
the `&outflow` block of the GLM nml file. Basin geometry (`bathy`,
`dims_lake`) needed to size the outlets is read from the existing
`&morphometry` block by default, so an existing GLM-AED configuration
can have its outflows edited without a lake shapefile.

## Usage

``` r
set_glm_outflows(
  path_glm,
  outf,
  heights_wdr,
  outlet_type = NULL,
  flt_off_sw = NULL,
  bathy = NULL,
  dims_lake = NULL,
  wdr_factor = 1,
  glm_file = find_glm_nml(path_glm)
)
```

## Arguments

- path_glm:

  filepath; to GLM-AED directory (containing the nml file and a `bcs/`
  subdirectory)

- outf:

  named list of data.frames, one per outflow, each with a `Date` column
  and a flow column (`HYD_flow`, or `outflow` for a `"wbal"`
  water-balance outflow) – see
  [`add_outflows()`](https://limnotrack.com/reference/add_outflows.md)
  for the expected schema.

- heights_wdr:

  named numeric vector; outlet elevation (m) for each name in `outf`.

- outlet_type:

  named numeric vector; GLM outlet type per outflow (see the GLM
  manual). Defaults to `1` (fixed height) for every outflow.

- flt_off_sw:

  named logical vector; floating offtake switch per outflow. Defaults to
  `FALSE` for every outflow.

- bathy:

  data.frame with `elev`/`area` columns describing the lake hypsograph,
  used to size the outlet. Defaults to the `H`/`A` arrays already in the
  GLM nml's `&morphometry` block.

- dims_lake:

  numeric vector of length 2, `c(basin_length, basin_width)` at the
  crest. Defaults to the `bsn_len`/`bsn_wid` values already in the GLM
  nml's `&morphometry` block.

- wdr_factor:

  numeric; scaling factor applied to all outflow flow rates. Default is
  `1`.

- glm_file:

  filepath; path to the GLM hydrodynamic nml file to update. Defaults to
  the file found in `path_glm` via
  [`find_glm_nml()`](https://limnotrack.com/reference/find_glm_nml.md).

## Value

invisibly, the updated nml object

## Examples

``` r
if (FALSE) { # \dontrun{
set_glm_outflows(path_glm, outf = list(outlet_1 = outflow_df),
                 heights_wdr = c(outlet_1 = -2.5))
} # }
```
