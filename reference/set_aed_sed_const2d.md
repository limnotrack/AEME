# Setup AED aed_sed_const2d sediment model parameters

Setup AED aed_sed_const2d sediment model parameters

## Usage

``` r
set_aed_sed_const2d(
  aeme,
  path,
  lake_dir = NULL,
  baseline = c(fsed_oxy = -25, fsed_amm = 2, fsed_nit = 0.2, fsed_frp = 0.05)
)
```

## Arguments

- aeme:

  Aeme object.

- path:

  character; directory where input files are located. Defaults to the
  path stored in `aeme`, or the current working directory if not set.

- baseline:

  Named numeric vector of baseline fluxes at `ref_depth`. Must include
  `fsed_oxy`, `fsed_amm`, `fsed_nit`, `fsed_frp`.

## Value

Invisible NULL. Updates the aed.nml file in the glm_aed model directory.
