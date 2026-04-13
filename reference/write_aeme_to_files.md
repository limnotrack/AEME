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
aeme <- build_aeme(path = path, aeme = aeme, model = model,
model_controls = model_controls, ext_elev = 5)
#> Error: object 'model' not found
aeme <- run_aeme(aeme = aeme, model = "glm_aed", path = path)
#> Error in run_aeme(aeme = aeme, model = "glm_aed", path = path): ✖ `model_controls` need to be provided to load model output.
write_aeme_to_files(aeme, path)
```
