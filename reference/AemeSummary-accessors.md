# Accessors for AemeSummary objects

[`lake()`](https://limnotrack.com/AEME/reference/lake.md),
[`time()`](https://limnotrack.com/AEME/reference/time.md),
[`observations()`](https://limnotrack.com/AEME/reference/observations.md),
[`output()`](https://limnotrack.com/AEME/reference/output.md) and
[`parameters()`](https://limnotrack.com/AEME/reference/parameters.md)
work on an `AemeSummary` as they do on an `Aeme`, returning the matching
slot. `forcing()`, `profiles()`, `models()` and `hypsograph()` are
specific to it.

## Usage

``` r
# S4 method for class 'AemeSummary'
lake(aeme)

# S4 method for class 'AemeSummary'
time(aeme)

# S4 method for class 'AemeSummary'
observations(aeme)

# S4 method for class 'AemeSummary'
output(aeme)

# S4 method for class 'AemeSummary'
parameters(aeme)

forcing(x)

# S4 method for class 'AemeSummary'
forcing(x)

profiles(x)

# S4 method for class 'AemeSummary'
profiles(x)

models(x)

# S4 method for class 'AemeSummary'
models(x)

hypsograph(x)

# S4 method for class 'AemeSummary'
hypsograph(x)
```

## Arguments

- aeme, x:

  An `AemeSummary` object.

## Value

The slot.
