# Write an AEME object to files

Write an AEME object to files

## Usage

``` r
write_aeme_to_files(aeme, path, include_output = FALSE)
```

## Arguments

- aeme:

  Aeme object.

- path:

  character; directory where input files are located. Defaults to the
  path stored in `aeme`, or the current working directory if not set.

- include_output:

  logical, include output files. Default is FALSE. The output files can
  be large and take up a lot of space.

## Value

A vector of file paths to the written files

## Examples

``` r
aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
aeme <- readRDS(aeme_file)
model <- "glm_aed"
path <- "test_write"
model_controls <- get_model_controls()
aeme <- build_aeme(path = path, aeme = aeme, model = model,
model_controls = model_controls, ext_elev = 5)
#> Warning: ! `SIL_rsi`: SIL_rsi is constant across all rows — this may be a placeholder
#>   value.
#> ℹ Check raw data or unit conversion for this variable.
#> ℹ Using observed water level.
#> ! Missing values in observed water level.
#> ℹ Correcting water balance using estimated outflows (method = 2).
#> ℹ Building GLM-AED for lake wainamu
#> Error in rename_modelvars(var_names, type_output = "glm_aed"): `input` must be a non-empty character vector.
aeme <- run_aeme(aeme = aeme, model = "glm_aed", path = path)
#> Error in run_aeme(aeme = aeme, model = "glm_aed", path = path): ✖ `model_controls` need to be provided to load model output.
write_aeme_to_files(aeme, path)
```
