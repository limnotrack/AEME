# Reset water balance parameters

This function resets the water balance parameters in the Aeme object.
This is useful if you want to start fresh with a new set of parameters
for example if you add/remove a inflow or change the meteorological
data.

## Usage

``` r
reset_wbal_param(aeme, model = NULL)
```

## Arguments

- aeme:

  Aeme object.

- model:

  character; model name(s) to reset parameters for (e.g. `"glm_aed"`).
  If `NULL` (default), all fitted parameters are cleared.

## Value

Aeme object with water balance parameters reset
