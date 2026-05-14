# Model controls

A reference table for catchment nutrient and sediment yields for
calculating inflow nutrients in AEME.

## Usage

``` r
model_controls
```

## Format

### `model_controls`

A data frame with 65 rows and 6 columns:

- var_aeme:

  AEME variable

- simulate:

  Logical value to simulate the variable

- inf_default:

  Default value in inflows.

- initial_wc:

  Default value for initialising in the water column.

- initial_sed:

  Default value for initialising in the sediments.

- conversion_aed:

  Unit conversion for GLM-AED.

## Source

Package development.

## Author

Tadhg Moore, Chris McBride
