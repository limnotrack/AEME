# AEME Inputs

## Summary

Inputs for AEME are designed to similar to most standard data inputs
used in lake ecosystem modelling.

## Lake

Lake inputs include:

## Meteorological Data

Meteorological data requirements for lake modelling are similar to those
used in most lake ecosystem models. The following meteorological
variables are **required** for AEME:

- Air temperature
- Shortwave radiation
- Wind speed
- Rain
- Either relative humidity or dew point temperature
- Either mean sea level pressure or station pressure

The following meteorological variables can be **derived** from the
required variables:

- Dew point temperature
- Relative humidity
- Precipitation vapor pressure
- Wind u and v components
- Cloud cover
- Longwave radiation

## Inflows and Outflows

### Inflows

Inflows are important for lake hydrodynamics and water quality. Inflows
can be specified as a time series of flow rates (m3/s) and temperature
(C). They can also include nutrient and sediment concentrations if the
biogeochemistry model is being used.

One of the most important features of configuring an inflow is ensuring
that the variables are in the correct units and that the variable names
match those specified in the `key_naming` data frame.

## Parameters

Being able to specify the parameters of the model is crucial for the
simulation. Parameters can be stored in a data frame within the `aeme`
object. The parameters in this data frame are used to update the model
configuration files and/or update the meteorological data (e.g. scaling
wind factor) and/or update the inflow/outflow data (e.g. scaling inflow
factor).

``` r
utils::data("aeme_parameters")
aeme_parameters
```

``` r
parameters(aeme) <- aeme_parameters
```
