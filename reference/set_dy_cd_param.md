# Set one or more parameter values in a DYRESM-CAEDYM configuration

Thin, `aeme`-free wrapper for editing a DYRESM-CAEDYM model directory in
place. DYRESM-CAEDYM has no keyed config file like GLM-AED's `.nml` –
its tunables are split between the positional `<lakename>.cfg` (light
extinction, layer thickness limits, time step, output interval) and the
positional `dyresm3p1.par` (bulk aerodynamic drag, albedo, emissivity,
mixing efficiencies, ...). This function exposes a curated set of those
by friendly name and edits the right line of the right file, leaving
formatting otherwise untouched. Intended for a DYRESM-CAEDYM-only
workflow where a user just wants to tweak parameters, run the model, and
load the output.

## Usage

``` r
set_dy_cd_param(
  path_dy,
  ...,
  cfg_file = find_dy_cd_cfg(path_dy),
  par_file = file.path(path_dy, "dyresm3p1.par")
)
```

## Arguments

- path_dy:

  filepath; directory containing the DYRESM-CAEDYM configuration (the
  `dy_cd` model directory).

- ...:

  named parameter/value pairs to set, e.g. `Kw = 0.5`,
  `max_layer_thickness = 2`, `eta_S = 0.5`. See Details for accepted
  names.

- cfg_file:

  filepath; the `<lakename>.cfg` file to edit. Defaults to the one found
  in `path_dy` via
  [`find_dy_cd_cfg()`](https://limnotrack.com/reference/find_dy_cd_cfg.md).

- par_file:

  filepath; the `dyresm3p1.par` file to edit. Defaults to
  `dyresm3p1.par` in `path_dy`.

## Value

invisibly, a named list of the values that were set.

## Details

Setting `Kw` also updates the `PAR` line of `caedym3p1.bio` when that
file is present, mirroring
[`build_aeme()`](https://limnotrack.com/reference/build_aeme.md)'s own
behaviour so a BGC run stays consistent.

Accepted names:

- `<lakename>.cfg`:

  `start_date`, `sim_days`, `run_caedym`, `output_interval`, `Kw`,
  `min_layer_thickness`, `max_layer_thickness`, `timestep`

- `dyresm3p1.par`:

  `drag_coef`, `albedo`, `emissivity`, `crit_wind_speed`, `output_time`,
  `bubbler_entrain_coef`, `plume_entrain_coef`, `eta_K`, `eta_P`,
  `eta_S`, `eff_surf_area_coef`, `bbl_dissip_coef`, `vert_mix_coef`

## Examples

``` r
if (FALSE) { # \dontrun{
set_dy_cd_param(path_dy, Kw = 0.5, max_layer_thickness = 2)
set_dy_cd_param(path_dy, eta_K = 0.1, eta_P = 0.3, eta_S = 0.5)
} # }
```
