# Get water balance parameters

Retrieves the outflow parameters from an `Aeme` object, as set by
[`set_wbal_param`](https://limnotrack.com/reference/set_wbal_param.md).
See that function for details of how the parameters are used in the
outflow equation.

## Usage

``` r
get_wbal_param(aeme, model = NULL)
```

## Arguments

- aeme:

  Aeme object.

- model:

  character; model name(s) to fetch parameters for (e.g. `"glm_aed"`).
  If `NULL` (default), returns every fitted family as a named list.

## Value

If `model` resolves to a single evaporation family, a named numeric
vector with elements `C` (outflow coefficient) and `h_inv` (inversion
height, m). If `model` is `NULL`, a named list of such vectors, keyed by
family. `NULL` if no parameters have been set.

## Details

Parameters are stored per evaporation family, since `dy_cd`/`glm_aed`
share one fitted set and `gotm_wet`/`simstrat_aed2` each have their own
(see `calc_water_balance`). Use `model` to fetch a specific model's
parameters; omit it to get every family that has been fitted so far.

A typical use case is calibrating parameters over a period with observed
water level data, then transferring them to a period without:

1.  Build an `Aeme` object for a calibration period with observed water
    level data (e.g. 2010–2020) using
    [`build_aeme`](https://limnotrack.com/reference/build_aeme.md).

2.  Run the water balance and retrieve the fitted parameters:
    `wbal_param <- get_wbal_param(aeme, model = "glm_aed")`.

3.  Configure a new `Aeme` object for the target period without observed
    water levels (e.g. 2020–2024).

4.  Transfer the parameters with
    [`set_wbal_param`](https://limnotrack.com/reference/set_wbal_param.md):
    `aeme <- set_wbal_param(aeme, params = wbal_param, model = "glm_aed")`.

5.  Build the new object with
    [`build_aeme`](https://limnotrack.com/reference/build_aeme.md).

## See also

[`set_wbal_param`](https://limnotrack.com/reference/set_wbal_param.md),
[`reset_wbal_param`](https://limnotrack.com/reference/reset_wbal_param.md)
