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
model_controls = model_controls)
#> Created missing directory: D:\a\AEME\AEME\docs\reference\test_write
#> ℹ Using observed water level
#> ! Missing values in observed water level
#> ℹ Insufficient water level observations. Using constant water level
#> ℹ Correcting water balance using estimated outflows (method = 2).
#> ℹ Calculating lake level using lake depth and a sinisoidal function.
#> ℹ Building GLM-AED2 for lake wainamu
#> ℹ Copied in GLM nml file
#> ✔ GLM nml validation completed - no issues detected.
aeme <- run_aeme(aeme = aeme, model = "glm_aed", path = path)
#> ℹ Running models... (Have you tried parallelizing?) [2025-12-10 20:20:40]
#> → GLM-AED running... [2025-12-10 20:20:40]
#> ✔ GLM-AED2 run successful! [2025-12-10 20:20:40]
#> ✔ Model run complete! [2025-12-10 20:20:40]
write_aeme_to_files(aeme, path)
aeme_path <- get_lake_dir(aeme = aeme, path = path)
aeme2 <- read_aeme_from_files(aeme_path)
```
