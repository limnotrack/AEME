# Apply a suggested simulation period to an Aeme object

Thin wrapper over
[`set_time()`](https://limnotrack.com/reference/set_time.md) that takes
the object returned by
[`suggest_sim_period()`](https://limnotrack.com/reference/suggest_sim_period.md),
so the period that was reported is the period that gets set.

## Usage

``` r
set_sim_period(aeme, period, spin_up = NULL)
```

## Arguments

- aeme:

  Aeme object.

- period:

  An `aeme_sim_period` from
  [`suggest_sim_period()`](https://limnotrack.com/reference/suggest_sim_period.md).

- spin_up:

  Numeric. Override the period's spin-up. Default `NULL` keeps it.

## Value

The Aeme object with its time slot set.

## See also

[`suggest_sim_period()`](https://limnotrack.com/reference/suggest_sim_period.md),
[`set_time()`](https://limnotrack.com/reference/set_time.md)
