# Get the output variables from an AEME object

Get the output variables from an AEME object

## Usage

``` r
get_output_vars(aeme, model, ens_n = 1)
```

## Arguments

- aeme:

  aeme; object.

- model:

  vector; of models to be used. Can be \`dy_cd\`, \`glm_aed\`,
  \`gotm_wet\`.

- ens_n:

  numeric; ensemble number to allocate to model output which is loaded.
  Defaults to 1.

## Value

A character vector of the output variables

## Examples

``` r
aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
aeme <- readRDS(aeme_file)
path <- tempdir()
model_controls <- get_model_controls(use_bgc = TRUE)
model <- c("glm_aed", "gotm_wet")
aeme <- build_aeme(path = path, aeme = aeme, model = model,
                   model_controls = model_controls,
                   ext_elev = 5, use_bgc = TRUE)
#> ! Missing state variables in inflows:
#> ! ZOO_zoo1
#> ℹ Added default values for missing variables.
#> ℹ Using observed water level
#> ℹ No missing values in observed water level. Using observed water level
#> ℹ Correcting water balance using estimated outflows (method = 2).
#> ℹ Calculating lake level using lake depth and a sinisoidal function.
#> ℹ Building GLM-AED2 for lake wainamu
#> ℹ Copied in GLM nml file
#> ℹ Copied in AED nml file and supporting files
#> ℹ 15 replaced with 41.6285
#> ℹ Using default pH initialisation
#> ℹ 15 replaced with 16.6514
#> ℹ 225 replaced with 312.5
#> ℹ 2.25 replaced with 1.4279
#> ℹ 21 replaced with 21.4183
#> ℹ 6.96 replaced with 1.0709
#> ℹ 19.8 replaced with 7.1394
#> ℹ 0.008 replaced with 0.3229
#> ℹ 0.05 replaced with 0.3229
#> ℹ 0.05 replaced with 0.3229
#> ℹ PHY_cyano 2 replaced with 0.24022
#> ℹ PHY_diatom 10 replaced with 0.300275
#> ℹ PHY_green 0.04 replaced with 0.300275
#> ℹ 100 replaced with 1
#> ℹ Using default zooplankton initialisation
#> ℹ Building GOTM-WET model for lake wainamu
#> ℹ Copied in GOTM configuration files
#> ℹ instances/abiotic_water/initialization/sDDOMW 2.5 replaced with 0.5
#> ℹ instances/abiotic_water/initialization/sDPOMW 0.1 replaced with 0.2
#> ℹ instances/abiotic_water/initialization/sO2W 13 replaced with 10
#> ℹ instances/abiotic_water/initialization/sDIMW 4 replaced with 3
#> ℹ instances/abiotic_water/initialization/sNH4W 0.05 replaced with 0.02
#> ℹ instances/abiotic_water/initialization/sNDOMW 0.01 replaced with 0.3
#> ℹ instances/abiotic_water/initialization/sNO3W 0.5 replaced with 0.015
#> ℹ instances/abiotic_water/initialization/sNPOMW 0.01 replaced with 0.1
#> ℹ instances/abiotic_water/initialization/sPDOMW 0.001 replaced with 0.01
#> ℹ instances/abiotic_water/initialization/sPO4W 0.1 replaced with 0.01
#> ℹ instances/abiotic_water/initialization/sPPOMW 0.001 replaced with 0.01
#> ℹ Setting initial condition for instances/cyanobacteria/initialization/sDW: 0.1
#>   replaced with 0.2
#> ℹ Setting initial condition for instances/cyanobacteria/initialization/sNW:
#>   0.03 replaced with 0.03
#> ℹ Setting initial condition for instances/cyanobacteria/initialization/sPW:
#>   0.003 replaced with 0.0019
#> ℹ Setting initial condition for instances/diatoms/initialization/sDW: 0.2
#>   replaced with 0.25
#> ℹ Setting initial condition for instances/diatoms/initialization/sNW: 0.05
#>   replaced with 0.038
#> ℹ Setting initial condition for instances/diatoms/initialization/sPW: 0.005
#>   replaced with 0.0024
#> ℹ Setting initial condition for instances/greens/initialization/sDW: 0.1
#>   replaced with 0.1
#> ℹ Setting initial condition for instances/greens/initialization/sNW: 0.05
#>   replaced with 0.015
#> ℹ Setting initial condition for instances/greens/initialization/sPW: 0.001
#>   replaced with 0.00094
#> ℹ instances/abiotic_water/initialization/sSiO2W 3.5 replaced with 1
#> ℹ instances/cladocerans/initialization/sD 0.05 replaced with 1
#> ℹ Setting initial condition forinstances/cladocerans/initialization/sN: 0.0035
#>   replaced with 0.07
#> ℹ Setting initial condition forinstances/cladocerans/initialization/sP: 5e-04
#>   replaced with 0.01
#> [1] "Configuring GLM-AED totals..."
#> Warning: NAs introduced by coercion
#> Error: NA is not a .true. or .false.; conversion to TRUE or FALSE failed.
# Run models
aeme <- run_aeme(aeme = aeme, model = model, verbose = FALSE,
path = path, model_controls = model_controls,
parallel = TRUE, ncores = 2L)
#> Warning: NAs introduced by coercion
#> Error: NA is not a .true. or .false.; conversion to TRUE or FALSE failed.
get_output_vars(aeme, model)
#> NULL
```
