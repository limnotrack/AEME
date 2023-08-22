
<!-- README.md is generated from README.Rmd. Please edit that file -->

# AEME

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/limnotrack/AEME/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/limnotrack/AEME/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://github.com/limnotrack/AEME/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/limnotrack/AEME/actions/workflows/pkgdown.yaml)
<!-- badges: end -->

The Aquatic Ecosystem Model Ensemble (AEME) package allows you to setup
and run an ensemble of aquatic ecosystem models. The models are
[DYRESM-CAEDYM](), [GLM-AED]() and [GOTM-WET]().

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
#> The legacy packages maptools, rgdal, and rgeos, underpinning the sp package,
#> which was just loaded, will retire in October 2023.
#> Please refer to R-spatial evolution reports for details, especially
#> https://r-spatial.org/r/2023/05/15/evolution4.html.
#> It may be desirable to make the sf package available;
#> package maintainers should consider adding sf to Suggests:.
#> The sp package is now running under evolution status 2
#>      (status 2 uses the sf package in place of rgdal)
#> Warning: replacing previous import 'stats::filter' by 'dplyr::filter' when
#> loading 'gotmtools'
#> Please note that 'maptools' will be retired during October 2023,
#> plan transition at your earliest convenience (see
#> https://r-spatial.org/r/2023/05/15/evolution4.html and earlier blogs
#> for guidance);some functionality will be moved to 'sp'.
#>  Checking rgeos availability: TRUE
## basic example code
tmpdir <- tempdir()
aeme_dir <- system.file("extdata/lake/", package = "AEME")
# Copy files from package into tempdir
file.copy(aeme_dir, tmpdir, recursive = TRUE)
#> [1] TRUE
dir <- file.path(tmpdir, "lake")
config <- configr::read.config(file.path(dir, "aeme.yaml"))
mod_ctrls <- read.csv(file.path(dir, "model_controls.csv"))
inf_factor = c("glm_aed" = 1)
outf_factor = c("glm_aed" = 1)
model <- c("glm_aed")
build_ensemble(dir = dir, config = config, model = model,
               mod_ctrls = mod_ctrls, inf_factor = inf_factor, ext_elev = 5,
               use_bgc = FALSE, use_lw = TRUE)
#> Building simulation for Wainamu [2023-08-22 13:52:16]
#> Reading layer `lake' from data source 
#>   `C:\Users\tadhg\AppData\Local\Temp\RtmpiiU6Yv\lake\data\lake.shp' 
#>   using driver `ESRI Shapefile'
#> Simple feature collection with 1 feature and 14 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 174.4645 ymin: -36.89195 xmax: 174.4757 ymax: -36.88648
#> Geodetic CRS:  WGS 84
#> Spherical geometry (s2) switched off
#> Spherical geometry (s2) switched on
#> Building GLM3-AED2 model for lake wainamu
#> Copied in GLM nml file
run_aeme(dir = dir, config = config, model = model, verbose = TRUE)
#> Running models... (Have you tried parallelizing?) [2023-08-22 13:52:17]
#> Model run complete![2023-08-22 13:52:17]
```
