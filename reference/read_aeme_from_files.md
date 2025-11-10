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
#> Error in "lapply(text, glue_cmd, .envir = .envir)": ! Could not evaluate cli `{}` expression: `path`.
#> Caused by error in `eval(expr, envir = envir)`:
#> ! object 'path' not found
aeme <- run_aeme(aeme = aeme, model = "glm_aed", path = path)
#> Error in run_aeme(aeme = aeme, model = "glm_aed", path = path): `model_controls` need to be provided to load model output.
write_aeme_to_files(aeme, path)
aeme_path <- get_lake_dir(aeme = aeme, path = path)
aeme2 <- read_aeme_from_files(aeme_path)
#> Error in check_model(model = model): `model` must be provided and not be empty.
```
