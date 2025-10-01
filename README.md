
<!-- README.md is generated from README.Rmd. Please edit that file -->

# AEME <a href="https://limnotrack.github.io/AEME/"><img src="man/figures/logo.png" align="right" height="120" alt="AEME website" /></a>

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-green.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![R-CMD-check](https://github.com/limnotrack/AEME/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/limnotrack/AEME/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://github.com/limnotrack/AEME/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/limnotrack/AEME/actions/workflows/pkgdown.yaml)
[![Codecov test
coverage](https://codecov.io/gh/limnotrack/AEME/branch/main/graph/badge.svg)](https://app.codecov.io/gh/limnotrack/AEME?branch=main)
[![DOI](https://zenodo.org/badge/679273467.svg)](https://zenodo.org/badge/latestdoi/679273467)

<!-- badges: end -->

The Aquatic Ecosystem Model Ensemble (AEME) package allows you to setup
and run an ensemble of aquatic ecosystem models. The models are
[DYRESM-CAEDYM](https://github.com/AquaticEcoDynamics/dy-cd),
[GLM-AED](https://aquatic.science.uwa.edu.au/research/models/GLM/) and
[GOTM-WET](https://gitlab.com/wateritech-public/waterecosystemstool/wet).

## Development

This package was developed by [LimnoTrack](http://limnotrack.com/) as
part of the [Lake Ecosystem Research New Zealand Modelling
Platform](https://limnotrack.shinyapps.io/LERNZmp/) (LERNZmp) project.
<a href="http://limnotrack.com/"><img src="man/figures/limnotrack_border.jpg" alt="LimnoTrack website" align="right" height="50"/></a>

## Installation

You can install AEME from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("limnotrack/AEME")
```

Currently, AEME is only available for Windows users.

## Example

This is a basic example which shows you how to build and run one of the
models in the ensemble:

``` r
library(AEME)
## basic example code
tmpdir <- tempdir()
aeme_dir <- system.file("extdata/lake/", package = "AEME")
# Copy files from package into tempdir
file.copy(aeme_dir, tmpdir, recursive = TRUE)
path <- file.path(tmpdir, "lake")
aeme <- yaml_to_aeme(path = path, "aeme.yaml")
model_controls <- get_model_controls(use_bgc = TRUE)
model <- c("dy_cd", "glm_aed", "gotm_wet")
aeme <- build_aeme(path = path, aeme = aeme, model = model,
                            model_controls = model_controls,
                            ext_elev = 5, use_bgc = TRUE)
aeme <- run_aeme(aeme = aeme, model = model, verbose = FALSE, 
                      path = path, parallel = TRUE)
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
#> Area: 152343 m2
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
#> 
#> DY-CD:    1
#> GLM-AED:  1
#> GOTM-WET: 1
```

Model data can be visualised easily using the `plot_output()` function:

``` r
p1 <- plot_output(aeme = aeme, model = model, var_sim = "HYD_temp")
p1
```

<img src="man/figures/README-plot_output-HYD_temp-1.png" width="100%" />

Also, visualising lake level plots.

``` r
p2 <- plot_output(aeme = aeme, model = model, var_sim = "LKE_lvlwtr",
                  facet = FALSE)
p2
```

<img src="man/figures/README-plot-output-HYD_wlev-1.png" width="100%" />

## Documentation

We have a host of vignettes to help you get started with AEME:

- [Introduction to
  AEME](https://limnotrack.github.io/AEME/articles/intro-aeme.html)
- [Set up for your own
  lake](https://limnotrack.github.io/AEME/articles/getting-started.html)
- [Description of model
  inputs](https://limnotrack.github.io/AEME/articles/aeme-inputs.html)

## Extension

- [aemetools](https://limnotrack.github.io/AEME/articles/aeme-inputs.html) -
  For downloading meteorological data, calibration and sensitivity
  analysis.
- [bathytools](https://limnotrack.github.io/bathytools/) - For
  processing lake bathymetry data.
