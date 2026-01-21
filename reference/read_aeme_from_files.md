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
#> ℹ No missing values in observed water level. Using observed water level
#> Parameters: C = 0.5 , h_inv = 22.9904 

#> Parameters: C = 0.5 , h_inv = 22.9904 
#> Parameters: C = 0.501 , h_inv = 22.9904 
#> Parameters: C = 0.499 , h_inv = 22.9904 
#> Parameters: C = 0.5 , h_inv = 22.9914 
#> Parameters: C = 0.5 , h_inv = 22.9894 
#> Parameters: C = 0.2661 , h_inv = 23.6504 
#> Parameters: C = 0.2671 , h_inv = 23.6504 
#> Parameters: C = 0.2651 , h_inv = 23.6504 
#> Parameters: C = 0.2661 , h_inv = 23.6504 
#> Parameters: C = 0.2661 , h_inv = 23.6494 
#> Parameters: C = 0.5 , h_inv = 22.9904 
#> Parameters: C = 0.501 , h_inv = 22.9904 
#> Parameters: C = 0.499 , h_inv = 22.9904 
#> Parameters: C = 0.5 , h_inv = 22.9914 
#> Parameters: C = 0.5 , h_inv = 22.9894 
#> Parameters: C = 0.3831 , h_inv = 23.3204 
#> Parameters: C = 0.3841 , h_inv = 23.3204 
#> Parameters: C = 0.3821 , h_inv = 23.3204 
#> Parameters: C = 0.3831 , h_inv = 23.3214 
#> Parameters: C = 0.3831 , h_inv = 23.3194 
#> Parameters: C = 0.3831 , h_inv = 23.3204 
#> Parameters: C = 0.3841 , h_inv = 23.3204 
#> Parameters: C = 0.3821 , h_inv = 23.3204 
#> Parameters: C = 0.3831 , h_inv = 23.3214 
#> Parameters: C = 0.3831 , h_inv = 23.3194 
#> Parameters: C = 0.3831 , h_inv = 23.3204 
#> Parameters: C = 0.3841 , h_inv = 23.3204 
#> Parameters: C = 0.3821 , h_inv = 23.3204 
#> Parameters: C = 0.3831 , h_inv = 23.3214 
#> Parameters: C = 0.3831 , h_inv = 23.3194 
#> Parameters: C = 0.3246 , h_inv = 23.4854 
#> Parameters: C = 0.3256 , h_inv = 23.4854 
#> Parameters: C = 0.3236 , h_inv = 23.4854 
#> Parameters: C = 0.3246 , h_inv = 23.4864 
#> Parameters: C = 0.3246 , h_inv = 23.4844 
#> Parameters: C = 0.3101 , h_inv = 23.5319 
#> Parameters: C = 0.3111 , h_inv = 23.5319 
#> Parameters: C = 0.3091 , h_inv = 23.5319 
#> Parameters: C = 0.3101 , h_inv = 23.5329 
#> Parameters: C = 0.3101 , h_inv = 23.5309 
#> Parameters: C = 0.313 , h_inv = 23.5213 
#> Parameters: C = 0.314 , h_inv = 23.5213 
#> Parameters: C = 0.312 , h_inv = 23.5213 
#> Parameters: C = 0.313 , h_inv = 23.5223 
#> Parameters: C = 0.313 , h_inv = 23.5203 
#> Parameters: C = 0.311 , h_inv = 23.5284 
#> Parameters: C = 0.312 , h_inv = 23.5284 
#> Parameters: C = 0.31 , h_inv = 23.5284 
#> Parameters: C = 0.311 , h_inv = 23.5294 
#> Parameters: C = 0.311 , h_inv = 23.5274 
#> Optimization Complete:
#>   Best C: 0.311
#>   Best h_inv: 23.5284
#>   Final RMSE: 0.0767

#> ℹ Correcting water balance using estimated outflows (method = 2).
#> ℹ Calculating lake level using lake depth and a sinisoidal function.
#> ℹ Building GLM-AED2 for lake wainamu
#> ℹ Copied in GLM nml file
#> ✔ GLM nml validation completed - no issues detected.
aeme <- run_aeme(aeme = aeme, model = "glm_aed", path = path)
#> ℹ Running models... (Have you tried parallelizing?) [2026-01-21 23:38:02]
#> → GLM-AED running... [2026-01-21 23:38:02]
#> ✔ GLM-AED run successful! [2026-01-21 23:38:02]
#> ✔ Model run complete! [2026-01-21 23:38:02]
write_aeme_to_files(aeme, path)
aeme_path <- get_lake_dir(aeme = aeme, path = path)
aeme2 <- read_aeme_from_files(aeme_path)
```
