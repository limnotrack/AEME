# Set grid for GOTM model

Set grid for GOTM model

## Usage

``` r
set_gotm_grid(
  gotm,
  path_gotm = NULL,
  depth,
  method = 1,
  aeme = NULL,
  path = NULL,
  thickness_factor = 1,
  ddu = 0,
  ddl = 0
)
```

## Arguments

- gotm:

  list; GOTM model configuration

- path_gotm:

  character; path to GOTM model directory

- depth:

  numeric; depth of the lake

- method:

  0=equal by default with optional zooming, 1=prescribed relative
  fractions, 2=prescribed layer thicknesses

- aeme:

  Aeme object.

- path:

  character; directory where input files are located. Defaults to the
  path stored in `aeme`, or the current working directory if not set.

- thickness_factor:

  numeric; factor to multiply the thickness of the layers. Default is 1.

- ddu:

  numeric; Surface zooming (dimensionless; min=0.0; default=0.0)

- ddl:

  numeric; Bottom zooming (dimensionless; min=0.0; default=0.0)

## Value

list; GOTM model configuration
