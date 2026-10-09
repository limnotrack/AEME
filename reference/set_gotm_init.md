# Set initial conditions for a GOTM-WET simulation

Thin wrapper for editing the initial temperature/salinity profile files
(`inputs/t_prof_file.dat`/`inputs/s_prof_file.dat`) of a GOTM-WET model
directory in place, without needing an `aeme` object. Intended for a
GOTM-WET-only workflow where a user just wants to tweak initial
conditions, run the model, and load the output.

## Usage

``` r
set_gotm_init(
  path_gotm,
  temp = NULL,
  salt = NULL,
  gotm_file = file.path(path_gotm, "gotm.yaml")
)
```

## Arguments

- path_gotm:

  filepath; directory containing the GOTM-WET configuration

- temp:

  numeric; new initial water temperature profile. Recycled to the number
  of depths in `inputs/t_prof_file.dat`. `NULL` (default) leaves it
  unchanged.

- salt:

  numeric; new initial salinity profile, same recycling rule as `temp`,
  written to `inputs/s_prof_file.dat`. `NULL` (default) leaves it
  unchanged.

- gotm_file:

  filepath; path to the yaml file to edit. Defaults to `gotm.yaml` in
  `path_gotm`.

## Value

invisibly, the updated gotm yaml object

## Details

The existing profile depths in each `.dat` file are left unchanged –
`temp`/`salt` values are recycled (via
[`rep_len()`](https://rdrr.io/r/base/rep.html)) across however many
depths are already defined. The `gotm.yaml` surface SST seed
(`surface$sst$constant_value`) is updated to match the surface-most
`temp` value when `temp` is provided.

## Examples

``` r
if (FALSE) { # \dontrun{
set_gotm_init(path_gotm, temp = seq(20, 10, length.out = 10))
} # }
```
