# Set initial conditions for a Simstrat simulation

Thin wrapper for editing the `InitialConditions.dat` file (temperature/
salinity) and, for a Simstrat-AED/AED2 simulation, the per-variable
`<var>_ini.dat` override files of a Simstrat model directory in place,
without needing an `aeme` object. Intended for a Simstrat-only workflow
where a user just wants to tweak initial conditions, run the model, and
load the output.

## Usage

``` r
set_simstrat_init(path_simstrat, temp = NULL, salt = NULL, wq_init = NULL)
```

## Arguments

- path_simstrat:

  filepath; directory containing the Simstrat configuration

- temp:

  numeric; new initial water temperature profile. Recycled to the number
  of depths in `InitialConditions.dat`. `NULL` (default) leaves it
  unchanged.

- salt:

  numeric; new initial salinity profile, same recycling rule as `temp`.
  `NULL` (default) leaves it unchanged.

- wq_init:

  named list; new initial values for water quality variables, keyed by
  `var_aeme` name, e.g. `list(NIT_amm = 0.5, CHM_oxy = 300)`. Each value
  is recycled across the same depths as `InitialConditions.dat` and
  written to `<AED_initcond|AED2_initcond>/<var>_ini.dat`. `NULL`
  (default) leaves water quality initial values unchanged. Requires a
  Simstrat-AED or Simstrat-AED2 configuration (an `aed.nml`/`aed2.nml`
  in `path_simstrat`).

## Value

invisibly, the updated initial conditions data.frame

## Details

The existing profile depths (and `U`, `V`, `k`, `eps` columns) in
`InitialConditions.dat` are left unchanged – `temp`/`salt`/`wq_init`
values are recycled (via [`rep_len()`](https://rdrr.io/r/base/rep.html))
across however many depths are already defined.

Water quality initial conditions in Simstrat-AED/AED2 follow a two-layer
scheme: the `aed.nml`/`aed2.nml` `<var>_initial` field is a fallback
constant used for every grid cell, and an optional per-variable
`<path_aed_initial>/<var>_ini.dat` (depth, value) profile – if present –
overrides it, interpolated onto the vertical grid. `wq_init` writes this
override file directly (it takes precedence over the nml default
regardless of the nml value), rather than editing the nml fallback.
Which coupler is in use (Simstrat-AED vs Simstrat-AED2, and therefore
the `AED_initcond/`/`AED2_initcond/` directory and variable-naming
convention) is detected from whichever of `aed.nml`/`aed2.nml` exists in
`path_simstrat`.

## Examples

``` r
if (FALSE) { # \dontrun{
set_simstrat_init(path_simstrat, temp = seq(20, 10, length.out = 10))
set_simstrat_init(path_simstrat, wq_init = list(NIT_amm = 0.5, CHM_oxy = 300))
} # }
```
