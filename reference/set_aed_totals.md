# Setup AED aed_totals parameters

This function sets up the aed_totals parameters in the aed block of the
bgc section of the model config for glm_aed. It extracts the necessary
information from the aed_phyto_pars and aed blocks to determine which
variables to include in the totals for TN, TP and TOC, and their scaling
factors. It then updates the model config and writes it back to the
aed.nml file in the glm_aed model directory.

## Usage

``` r
set_aed_totals(aeme, path, lake_dir = NULL)
```

## Arguments

- aeme:

  aeme; object.

- path:

  filepath; where input files are located relative to the current
  working directory.

## Value

Aeme object with aed_totals parameters set in the model config
