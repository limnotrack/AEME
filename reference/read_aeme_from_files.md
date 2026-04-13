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
#> Error in calc_water_balance(aeme_time = aeme_time, model = model, method = w_bal$method,     use = w_bal$use, hyps = hyps, inf = inf, outf = outf[["outflow"]],     level = level, init_elev = init_elev, init_temp = init_temp,     obs_lake = aeme_obs[["lake"]], obs_met = met, elevation = elev,     print_plots = FALSE, params = wbal_params, coeffs = coeffs): ! Observed water level values are outside the range of the hypsograph
#>   elevations.
#> ℹ Observed water level range: 23.4904 to 23.6874.
#> ℹ Hypsograph elevation range: 10.57 to 23.64.
aeme <- run_aeme(aeme = aeme, model = "glm_aed", path = path)
#> Error in run_aeme(aeme = aeme, model = "glm_aed", path = path): ✖ `model_controls` need to be provided to load model output.
write_aeme_to_files(aeme, path)
aeme_path <- get_lake_dir(aeme = aeme, path = path)
aeme2 <- read_aeme_from_files(aeme_path)
#> Error in check_model(model = model): `model` must be provided and not be empty.
```
