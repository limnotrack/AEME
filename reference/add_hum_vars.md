# Add humidity variables to meteorological data frame

Add humidity variables to meteorological data frame

## Usage

``` r
add_hum_vars(data, hum_method = 1)
```

## Arguments

- data:

  Data frame with meteorological data including columns: "hum" (humidity
  input), "airp" (air pressure, Pa), "sst" (sea surface temperature,
  degC or K), "airt" (air temperature, degC or K)

- hum_method:

  Method for humidity input: 1 = relative humidity ( 2 = wet bulb
  temperature (degC or K) 3 = dew point temperature (degC or K) 4 =
  specific humidity (kg/kg)

## Value

Data frame with added columns:

- es:

  Saturation vapour pressure at sea surface temperature (Pa)

- qs:

  Saturation specific humidity at sea surface temperature (kg/kg)

- ea:

  Actual vapour pressure (Pa)

- qa:

  Actual specific humidity (kg/kg)

- rhoa:

  Air density (kg/m3)
