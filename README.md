
<!-- README.md is generated from README.Rmd. Please edit that file -->

# AEME <a href="https://limnotrack.github.io/AEME/"><img src="man/figures/logo.png" align="right" height="120" alt="AEME website" /></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/limnotrack/AEME/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/limnotrack/AEME/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://github.com/limnotrack/AEME/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/limnotrack/AEME/actions/workflows/pkgdown.yaml)
[![Codecov test
coverage](https://codecov.io/gh/limnotrack/AEME/branch/main/graph/badge.svg)](https://app.codecov.io/gh/limnotrack/AEME?branch=main)

<!-- badges: end -->

The Aquatic Ecosystem Model Ensemble (AEME) package allows you to setup
and run an ensemble of aquatic ecosystem models. The models are
[DYRESM-CAEDYM](https://github.com/AquaticEcoDynamics/dy-cd),
[GLM-AED](https://aquatic.science.uwa.edu.au/research/models/GLM/) and
[GOTM-WET](https://gitlab.com/wateritech-public/waterecosystemstool/wet).

## Development

This package was developed by [LimnoTrack](http://limnotrack.com/) as
part of the Lake Ecosystem Restoration New Zealand Modelling Platform
(LERNZmp) project.
<a href="http://limnotrack.com/"><img src="man/figures/limnotrack_border.jpg" alt="LimnoTrack website" align="right" height="60"/></a>

## Installation

You can install the development version of AEME from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("limnotrack/AEME")
```

## Example

This is a basic example which shows you how to build and run one of the
models in the ensemble:

``` r
library(AEME)
#> 
#> Attaching package: 'AEME'
#> The following object is masked from 'package:stats':
#> 
#>     time
## basic example code
tmpdir <- tempdir()
aeme_dir <- system.file("extdata/lake/", package = "AEME")
# Copy files from package into tempdir
file.copy(aeme_dir, tmpdir, recursive = TRUE)
#> [1] TRUE
path <- file.path(tmpdir, "lake")
aeme <- yaml_to_aeme(path = path, "aeme.yaml")
#> Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.3.1; sf_use_s2() is FALSE
#> Warning in aeme_constructor(lake = yaml$lake, time = yaml$time, configuration = yaml$configuration, : Lake area [152343 m2] is different to the area calculated from the lake
#> shape [152433.09 m2].
model_controls <- get_model_controls(use_bgc = TRUE)
model <- c("dy_cd", "glm_aed", "gotm_wet")
aeme <- build_aeme(path = path, aeme = aeme, model = model,
                            model_controls = model_controls,
                            ext_elev = 5, use_bgc = TRUE)
#> Building simulation for Wainamu [2024-09-18 03:18:48]
#> Missing state variables in inflows: PHY_crypt
#> Added default values for missing variables.
#> Using observed water level
#> Missing values in observed water level
#> Using constant water level
#> Correcting water balance using estimated outflows (method = 2).
#> Calculating lake level using lake depth and a sinisoidal function.
#> Building DYRESM-CAEDYM for lake wainamu
#> Copied in DYRESM par file
#> Writing DYRESM configuration
#> [1] "TEMPTURE SALINITY DO PO4 DOPL POPL PIP TP NH4 NO3 DONL PONL TN DOCL POCL SiO2 CYANO CHLOR CRYPT FDIAT TCHLA SSOL1"
#> Writing DYRESM control file
#> Building GLM3-AED2 model for lake wainamu
#> Copied in GLM nml file
#> Copied in AED nml file
#>    oxy_initial   = 625 replaced with 312.5
#>    frp_initial = 0.3229 replaced with 0.3229
#>      dop_initial  = 0.3229 replaced with 0.3229
#>      pop_initial  = 0.3229 replaced with 0.3229
#>    amm_initial = 1.4279 replaced with 1.4279
#>    nit_initial = 1.0709 replaced with 1.0709
#>      don_initial  = 21.4183 replaced with 21.4183
#>      pon_initial  = 7.1394 replaced with 7.1394
#>      doc_initial  = 41.6285 replaced with 41.6285
#>      poc_initial  = 16.6514 replaced with 16.6514
#>    rsi_initial = 1 replaced with 1
#> PHY_cyano 0.24022 replaced with 0.24022
#> PHY_green 0.300275 replaced with 0.300275
#> PHY_crypt  replaced with
#> PHY_diatom 0.300275 replaced with 0.300275
#>     ss_initial   = 3,3 replaced with 3,
#> Building GOTM-WET for lake wainamu
#> Copied all GOTM configuration files
#> instances/abiotic_water/initialization/sO2W 13 replaced with 10
#> instances/abiotic_water/initialization/sPO4W 0.1 replaced with 0.01
#> instances/abiotic_water/initialization/sPDOMW 0.001 replaced with 0.01
#> instances/abiotic_water/initialization/sPPOMW 0.001 replaced with 0.01
#> instances/abiotic_water/initialization/sNH4W 0.05 replaced with 0.02
#> instances/abiotic_water/initialization/sNO3W 0.5 replaced with 0.015
#> instances/abiotic_water/initialization/sNDOMW 0.01 replaced with 0.3
#> instances/abiotic_water/initialization/sNPOMW 0.01 replaced with 0.1
#> instances/abiotic_water/initialization/sDDOMW 2.5 replaced with 0.5
#> instances/abiotic_water/initialization/sDPOMW 0.1 replaced with 0.2
#> instances/abiotic_water/initialization/sSiO2W 3.5 replaced with 1
#> instances/cyanobacteria/initialization/sDW 0.1 replaced with 0.2
#> instances/cyanobacteria/initialization/sNW 0.03 replaced with 0.03
#> instances/cyanobacteria/initialization/sPW 0.003 replaced with 0.0019
#> instances/greens/initialization/sDW 0.1 replaced with 0.1
#> instances/greens/initialization/sNW 0.05 replaced with 0.015
#> instances/greens/initialization/sPW 0.001 replaced with 0.00094
#> instances/diatoms/initialization/sDW 0.2 replaced with 0.25
#> instances/diatoms/initialization/sNW 0.05 replaced with 0.038
#> instances/diatoms/initialization/sPW 0.005 replaced with 0.0024
#> instances/abiotic_water/initialization/sDIMW 4 replaced with 3
aeme <- run_aeme(aeme = aeme, model = model, verbose = FALSE, 
                      path = path, parallel = TRUE)
#> Running models in parallel... [2024-09-18 15:18:52]
#> Model run complete![2024-09-18 15:21:12]
#> Reading models in parallel... [2024-09-18 15:21:13]
#> Model reading complete![2024-09-18 15:21:16]
```

The model input and output is handled as it’s own S4 object of class
`aeme`. This allows for the standardisation and generalisation of
functions for this class alongside ensuring integrity and validity to
it’s structure.

``` r
class(aeme)
#> [1] "Aeme"
#> attr(,"package")
#> [1] "AEME"
```

This allows for easier handling of the model output data within our
structure and allows for condensed output to be printed to the console:

``` r
aeme
#>             AEME 
#> -------------------------------------------------------------------
#>   Lake
#> Wainamu (ID: 45819); Lat: -36.89; Lon: 174.47; Elev: 23.64m; Depth: 13.07m;
#> Area: 152343 m2; Shape file: Present
#> -------------------------------------------------------------------
#>   Time
#> Start: 2020-08-01; Stop: 2021-06-30; Time step: 3600
#>  Spin up (days): GLM: 2; GOTM: 1; DYRESM: 1
#> -------------------------------------------------------------------
#>   Configuration
#>     Model controls: Present
#>           Physical   |   Biogeochemical
#> DY-CD    : Present    |   Present
#> GLM-AED  : Present    |   Present
#> GOTM-WET : Present    |   Present
#> -------------------------------------------------------------------
#>   Observations
#> Lake: Present; Level: Present
#> -------------------------------------------------------------------
#>   Input
#> Inital profile: Present; Inital depth: 13.07m; Hypsograph: Present (n=44);
#> Meteo: Present; Use longwave: TRUE; Kw: 1.31
#> -------------------------------------------------------------------
#>   Inflows
#> Data: Present; Scaling factors: DY-CD: 1; GLM-AED: 1; GOTM-WET: 1
#> -------------------------------------------------------------------
#>   Outflows
#> Data: Present; Scaling factors: DY-CD: 1; GLM-AED: 1; GOTM-WET: 1
#> -------------------------------------------------------------------
#>   Water balance
#> Method: 2; Use: obs; Modelled: Absent; Water balance: Present
#> -------------------------------------------------------------------
#>   Parameters: 
#> Number of parameters: 0
#> -------------------------------------------------------------------
#>   Output: 
#> Number of ensembles: 1
#> DY-CD:    1
#> GLM-AED:  1
#> GOTM-WET: 1
```

Model data can be visualised easily using the `plot_output()` function:

``` r
p1 <- plot_output(aeme = aeme, model = model, var_sim = "HYD_temp")
p1
#> Warning: Removed 246 rows containing missing values or values outside the scale range
#> (`geom_col()`).
```

<img src="man/figures/README-plot_output-HYD_temp-1.png" width="100%" />

Also, visualising lake level plots.

``` r
p2 <- plot_output(aeme = aeme, model = model, var_sim = "LKE_lvlwtr",
                  facet = FALSE)
p2
```

<img src="man/figures/README-plot-output-HYD_wlev-1.png" width="100%" />
