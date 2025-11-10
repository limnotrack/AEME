# Check AEME variable names

Check if the provided variable names are valid AEME variable names.

## Usage

``` r
check_aeme_vars(vars)
```

## Arguments

- vars:

  Character vector of variable names to check.

## Value

Invisibly returns TRUE if all variables are valid, otherwise throws an
error.

## Examples

``` r
check_aeme_vars("HYD_temp")
#> [1] "HYD_temp"
```
