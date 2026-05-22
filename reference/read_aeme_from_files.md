# Read an AEME object from files

Read an AEME object from files

## Usage

``` r
read_aeme_from_files(path)
```

## Arguments

- path:

  Path to the directory containing the AEME files.

## Value

An AEME object populated with data from the files.

## Examples

``` r
aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
aeme <- readRDS(aeme_file)
path <- "test_write"
model_controls <- get_model_controls()
aeme <- build_aeme(path = path, aeme = aeme, model = "glm_aed",
model_controls = model_controls, ext_elev = 3)
#> ✔ Created missing directory: D:\a\AEME\AEME\docs\reference\test_write
#> Warning: ! `SIL_rsi`: SIL_rsi is constant across all rows — this may be a placeholder
#>   value.
#> ℹ Check raw data or unit conversion for this variable.
#> ℹ Using observed water level.
#> ! Missing values in observed water level.
#> ℹ Correcting water balance using estimated outflows (method = 2).
#> ℹ Building GLM-AED for lake wainamu
#> ℹ Copied in GLM nml file
#> ℹ Copied in AED nml file and supporting files
#> ✔ GLM nml validation completed - no issues detected.
aeme <- run_aeme(aeme = aeme, model = "glm_aed", path = path)
#> ℹ Running models... (Have you tried parallelizing?) [2026-05-22 05:23:10]
#> → GLM-AED running... [2026-05-22 05:23:11]
#> ✔ GLM-AED run successful! [2026-05-22 05:23:11]
#> ✔ Model run complete! [2026-05-22 05:23:11]
write_aeme_to_files(aeme, path)
aeme_path <- get_lake_dir(aeme = aeme, path = path)
aeme2 <- read_aeme_from_files(aeme_path)
```
