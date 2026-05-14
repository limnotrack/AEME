# Write an AEME object to files

Write an AEME object to files

## Usage

``` r
write_aeme_to_files(aeme, path, include_output = FALSE)
```

## Arguments

- aeme:

  aeme; object.

- path:

  filepath; where input files are located relative to the current
  working directory.

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
#> ℹ All columns already match AEME standard variable names, skipping name
#>   guessing.
#> ℹ All columns already match AEME standard inflow variable names, skipping name
#>   guessing.
#> Warning: ! `SIL_rsi`: SIL_rsi is constant across all rows — this may be a placeholder
#>   value.
#> ℹ Check raw data or unit conversion for this variable.
#> ℹ Using observed water level.
#> ! Missing values in observed water level.
#> ℹ Correcting water balance using estimated outflows (method = 2).
#> ℹ Calculating lake level using lake depth and a sinisoidal function.
#> ℹ Building GLM-AED for lake wainamu
#> ✔ GLM nml validation completed - no issues detected.
aeme <- run_aeme(aeme = aeme, model = "glm_aed", path = path)
#> ℹ Running models... (Have you tried parallelizing?) [2026-05-14 04:35:03]
#> → GLM-AED running... [2026-05-14 04:35:03]
#> ✔ GLM-AED run successful! [2026-05-14 04:35:04]
#> ✔ Model run complete! [2026-05-14 04:35:04]
write_aeme_to_files(aeme, path)
```
