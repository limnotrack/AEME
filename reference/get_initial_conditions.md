# Get the stored initial-conditions specification

Return the structured initial-conditions specification set by
[`set_initial_conditions()`](https://limnotrack.com/reference/set_initial_conditions.md),
or, for a single `model`, that model's resolved initial conditions (its
overrides merged over the generic defaults).

## Usage

``` r
get_initial_conditions(aeme, model = NULL)
```

## Arguments

- aeme:

  Aeme object.

- model:

  character(1); optional model name. If supplied, the resolved (merged)
  initial conditions for that model are returned instead of the full
  specification.

## Value

A list. When `model` is `NULL`, the stored specification (a list with a
`default` element and an element per model with overrides), or `NULL` if
none has been set. When `model` is supplied, a list with any of `depth`,
`profile` and `wq`.

## See also

[`set_initial_conditions()`](https://limnotrack.com/reference/set_initial_conditions.md)

## Examples

``` r
aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
aeme <- readRDS(aeme_file)
aeme <- set_initial_conditions(aeme, depth = 10)
#> ✔ Initial conditions updated.
get_initial_conditions(aeme)
#> $default
#> $default$depth
#> [1] 10
#> 
#> 
get_initial_conditions(aeme, model = "glm_aed")
#> $depth
#> [1] 10
#> 
```
