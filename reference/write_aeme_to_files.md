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
model <- "glm_aed"
path <- "test_write"
model_controls <- get_model_controls()
aeme <- build_aeme(path = path, aeme = aeme, model = model,
model_controls = model_controls, ext_elev = 5)
#> ℹ All columns already match AEME standard variable names, skipping name
#>   guessing.
#> ℹ MET_tmpair: values appear to be in the expected units, no conversion applied.
#> ℹ MET_tmpdew: values appear to be in the expected units, no conversion applied.
#> ℹ MET_radswd: values appear to be in the expected units, no conversion applied.
#> ℹ MET_radlwd: values appear to be in the expected units, no conversion applied.
#> ℹ MET_humrel: values appear to be in the expected units, no conversion applied.
#> ℹ MET_cldcvr: values appear to be in the expected units, no conversion applied.
#> ℹ MET_prsttn: values appear to be in the expected units, no conversion applied.
#> ℹ MET_prmslp: values appear to be in the expected units, no conversion applied.
#> ℹ MET_prvapr: values appear to be in the expected units, no conversion applied.
#> ℹ MET_wndspd: values appear to be in the expected units, no conversion applied.
#> ℹ MET_wnduvu: values appear to be in the expected units, no conversion applied.
#> ℹ MET_wnduvv: values appear to be in the expected units, no conversion applied.
#> ℹ MET_pprain: values appear to be in the expected units, no conversion applied.
#> ℹ MET_ppsnow: values appear to be in the expected units, no conversion applied.
#> ℹ All columns already match AEME standard inflow variable names, skipping name
#>   guessing.
#> ℹ `HYD_temp`: values appear to be in the expected units, no conversion applied.
#> ℹ `CHM_oxy`: values appear to be in the expected units, no conversion applied.
#> ℹ `NIT_amm`: values appear to be in the expected units, no conversion applied.
#> ℹ `NIT_nit`: values appear to be in the expected units, no conversion applied.
#> ℹ `NIT_don`: values appear to be in the expected units, no conversion applied.
#> ℹ `NIT_pon`: values appear to be in the expected units, no conversion applied.
#> ℹ `PHS_frp`: values appear to be in the expected units, no conversion applied.
#> ℹ `PHS_dop`: values appear to be in the expected units, no conversion applied.
#> ℹ `PHS_pop`: values appear to be in the expected units, no conversion applied.
#> ℹ `PHS_pip`: values appear to be in the expected units, no conversion applied.
#> ℹ `CAR_doc`: values appear to be in the expected units, no conversion applied.
#> ℹ `CAR_poc`: values appear to be in the expected units, no conversion applied.
#> ℹ `SIL_rsi`: values appear to be in the expected units, no conversion applied.
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
#> ℹ Running models... (Have you tried parallelizing?) [2026-04-29 02:57:05]
#> → GLM-AED running... [2026-04-29 02:57:05]
#> ✔ GLM-AED run successful! [2026-04-29 02:57:06]
#> ✔ Model run complete! [2026-04-29 02:57:06]
write_aeme_to_files(aeme, path)
```
