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

## Examples

``` r
aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
aeme <- readRDS(aeme_file)
path <- "test_write"
model_controls <- get_model_controls()
aeme <- build_aeme(path = path, aeme = aeme, model = "glm_aed",
model_controls = model_controls)
#> ℹ Using observed water level
#> ℹ No missing values in observed water level. Using observed water level
#> ℹ Correcting water balance using estimated outflows (method = 2).
#> ℹ Calculating lake level using lake depth and a sinisoidal function.
#> ℹ Building GLM-AED2 for lake wainamu
#> ✔ GLM nml validation completed - no issues detected.
aeme <- run_aeme(aeme = aeme, model = "glm_aed", path = path)
#> ℹ Running models... (Have you tried parallelizing?) [2026-02-02 00:58:30]
#> → GLM-AED running... [2026-02-02 00:58:30]
#> ✔ GLM-AED run successful! [2026-02-02 00:58:31]
#> ✔ Model run complete! [2026-02-02 00:58:31]
write_aeme_to_files(aeme, path)
```
