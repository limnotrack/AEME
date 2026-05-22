# Set water balance parameters

Sets the outflow parameters used in the lake water balance. Outflow is
calculated at each timestep as:

## Usage

``` r
set_wbal_param(aeme, C, h_inv, params = NULL)
```

## Arguments

- aeme:

  aeme; object.

- C:

  numeric; outflow coefficient. Scales the magnitude of outflow when
  water level exceeds `h_inv`.

- h_inv:

  numeric; inversion height (m). The water level threshold below which
  outflow is zero.

- params:

  Optional named numeric vector with elements `"C"` and `"h_inv"`, as
  returned by
  [`get_wbal_param`](https://limnotrack.com/reference/get_wbal_param.md).
  If supplied, overrides the individual `C` and `h_inv` arguments.

## Value

An `Aeme` object with updated water balance parameters.

## Details

\$\$O_t = C \cdot \max(h_t - h\_{inv}, 0)^{1.5} \times 86400\$\$

where \\O_t\\ is outflow (m\\^3\\/day), \\h_t\\ is the simulated water
level (m), \\h\_{inv}\\ is the inversion height (m), \\C\\ is the
outflow coefficient, and 86400 converts seconds to days.
