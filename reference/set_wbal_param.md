# Set water balance parameters

Sets the outflow parameters used in the lake water balance. Outflow is
calculated at each timestep as:

## Usage

``` r
set_wbal_param(aeme, C, h_inv, params = NULL, model = NULL)
```

## Arguments

- aeme:

  Aeme object.

- C:

  numeric; outflow coefficient. Scales the magnitude of outflow when
  water level exceeds `h_inv`.

- h_inv:

  numeric; inversion height (m). The water level threshold below which
  outflow is zero.

- params:

  Optional named numeric vector with elements `"C"` and `"h_inv"`, as
  returned by
  [`get_wbal_param`](https://limnotrack.com/reference/get_wbal_param.md)
  for a single model. If supplied, overrides the individual `C` and
  `h_inv` arguments. Alternatively, a family-keyed list as returned by
  [`get_wbal_param`](https://limnotrack.com/reference/get_wbal_param.md)
  with no `model` – each entry is applied directly to its own family,
  and `model` is ignored.

- model:

  character; model name(s) to set parameters for (e.g. `"glm_aed"`). If
  `NULL` (default), the same values are applied to every evaporation
  family (`dy_cd`/`glm_aed`, `gotm_wet`, `simstrat_aed2`). Ignored if
  `params` is a family-keyed list.

## Value

An `Aeme` object with updated water balance parameters.

## Details

\$\$O_t = C \cdot \max(h_t - h\_{inv}, 0)^{1.5} \times 86400\$\$

where \\O_t\\ is outflow (m\\^3\\/day), \\h_t\\ is the simulated water
level (m), \\h\_{inv}\\ is the inversion height (m), \\C\\ is the
outflow coefficient, and 86400 converts seconds to days.

Parameters are stored per evaporation family, since `dy_cd`/`glm_aed`
share one fitted set and `gotm_wet`/`simstrat_aed2` each have their own
(see `calc_water_balance`). Use `model` to set a specific model's
parameters; omit it to apply the same values to every family (matching
the pre-per-family behaviour).
