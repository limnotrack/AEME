# Set time parameters for an Aeme object

Set time parameters for an Aeme object

## Usage

``` r
set_time(aeme, start, stop, spin_up)
```

## Arguments

- aeme:

  aeme; object.

- start, stop:

  Time in the format "YYYY-mm-dd" or "YYYY-mm-dd HH:MM" or "YYYY-mm-dd
  HH:MM:SS"

- spin_up:

  Spin-up time in days. Can be a single numeric value or a list with
  model names as names and numeric values as values.

## Value

Aeme object with time parameters set

## Examples

``` r
aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
aeme <- readRDS(aeme_file)
aeme <- set_time(aeme = aeme, start = "2020-01-01", stop = "2020-12-31",
                 spin_up = 35)
```
