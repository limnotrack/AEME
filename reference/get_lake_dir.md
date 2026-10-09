# Get the directory of the lake model setup

Get the directory of the lake model setup

## Usage

``` r
get_lake_dir(aeme, path)
```

## Arguments

- aeme:

  Aeme object.

- path:

  character; directory where input files are located. Defaults to the
  path stored in `aeme`, or the current working directory if not set.

## Value

character; the directory of the lake model setup

## Examples

``` r
aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
aeme <- readRDS(aeme_file)
path <- tempdir()
model <- c("glm_aed")
aeme <- build_aeme(aeme = aeme, model = model, path = path, ext_elev = 3)
#> Warning: ! `SIL_rsi`: SIL_rsi is constant across all rows -- this may be a placeholder
#>   value.
#> ℹ Check raw data or unit conversion for this variable.
#> 
#> ── Calculating water balance ──
#> 
#> Resolving water level
#>   ℹ Using observed water level
#> ! Missing values in observed water level
#> ℹ Estimating surface water temperature
#> ✔ Estimating surface water temperature [24ms]
#> 
#> Estimating lake water levels for glm_aed
#>   ℹ Optimizing parameters for water balance
#>   ✔ Optimization Complete: C = 0.341, h_inv = 23.478, Final RMSE = 0.1456
#> ℹ Correcting water balance using estimated outflows (method = 2).
#> 
#> ── Building GLM-AED for lake wainamu ──
#> 
#> ℹ Copied in GLM nml file (glm4.nml)
#> ℹ Copied in AED nml file and supporting files
#> ℹ Copied in GLM plots nml file
#> ! Forcing sed_heat_model from 2 to 1: sed_heat_model = 2 needs an active WQ
#>   module and `use_bgc` is FALSE.
#> ✔ GLM nml validation completed - no issues detected.
lake_dir <- get_lake_dir(aeme)
```
