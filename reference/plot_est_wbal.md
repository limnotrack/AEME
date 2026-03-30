# Plot estimated water balance diagnostics from an aeme object.

Plot estimated water balance diagnostics from an aeme object.

## Usage

``` r
plot_est_wbal(aeme, model, time_axis = c("auto", "daily", "monthly", "annual"))
```

## Arguments

- aeme:

  aeme object.

- model:

  character; single model name. If missing, uses list_models().

- time_axis:

  one of `"auto"` (default), `"daily"`, `"monthly"`, or `"annual"`.

## Value

A patchwork object, or NULL if no water balance data available.
