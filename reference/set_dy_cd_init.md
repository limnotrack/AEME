# Set initial conditions for a DYRESM-CAEDYM simulation

Thin, `aeme`-free wrapper for editing the initial temperature/salinity
profile (`<lakename>.pro`) and, for a CAEDYM (BGC) run, the
water-quality initial values (`<lakename>.int`) of a DYRESM-CAEDYM model
directory in place. Intended for a DYRESM-CAEDYM-only workflow where a
user just wants to tweak initial conditions, run the model, and load the
output.

## Usage

``` r
set_dy_cd_init(
  path_dy,
  temp = NULL,
  salt = NULL,
  wq_init = NULL,
  pro_file = NULL,
  int_file = NULL
)
```

## Arguments

- path_dy:

  filepath; directory containing the DYRESM-CAEDYM configuration (the
  `dy_cd` model directory).

- temp:

  numeric; new initial water temperature profile. Recycled to the number
  of rows in `<lakename>.pro`. `NULL` (default) leaves it unchanged.

- salt:

  numeric; new initial salinity profile, same recycling rule as `temp`.
  `NULL` (default) leaves it unchanged.

- wq_init:

  named list; new water-column initial values for CAEDYM water quality
  variables, keyed by `var_aeme` name, e.g.
  `list(CHM_oxy = 300, NIT_amm = 0.5)`. Names are translated to CAEDYM's
  own variable names via `rename_modelvars()`. Each value is a single
  number (CAEDYM `.int` water-column initials are not depth-resolved).
  `NULL` (default) leaves water quality initial values unchanged.
  Requires a `<lakename>.int` file (a CAEDYM configuration).

- pro_file, int_file:

  filepath; the `.pro` / `.int` files to edit. Default to the files
  found in `path_dy` via
  [`find_dy_cd_cfg()`](https://limnotrack.com/reference/find_dy_cd_cfg.md)'s
  prefix.

## Value

invisibly, the updated initial-profile data.frame
(`depth`/`temp`/`salt`).

## Details

Existing profile depths in `<lakename>.pro` are left unchanged –
`temp`/`salt` values are recycled (via
[`rep_len()`](https://rdrr.io/r/base/rep.html)) across however many rows
are already defined. `wq_init` overwrites the water-column initial value
for the named CAEDYM variables in `<lakename>.int`, leaving their
sediment initial values (and every other variable) untouched.

## Examples

``` r
if (FALSE) { # \dontrun{
set_dy_cd_init(path_dy, temp = seq(20, 10, length.out = 10))
set_dy_cd_init(path_dy, wq_init = list(CHM_oxy = 300, NIT_amm = 0.5))
} # }
```
