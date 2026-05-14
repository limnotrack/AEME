# Calculate humidity-related variables using GOTM formulas

Calculate humidity-related variables using GOTM formulas

## Usage

``` r
calc_humidity_vars(
  hum_method,
  hum,
  airp,
  tw,
  ta,
  rgas = 287.05,
  kelvin = 273.15,
  const06 = 0.62198
)
```

## Arguments

- hum_method:

  Method for humidity input: 1 = relative humidity (%) 2 = wet bulb
  temperature (degC or K) 3 = dew point temperature (degC or K) 4 =
  specific humidity (kg/kg)

- hum:

  Humidity input (depends on method)

- airp:

  Air pressure (Pa)

- tw:

  Sea surface (water) temperature (degC or K)

- ta:

  Air temperature (degC or K)

- rgas:

  Gas constant for dry air (default 287.05 J/kg/K)

- kelvin:

  Value to convert degC to K (default 273.15)

- const06:

  Constant for specific humidity conversion (default 0.62198)

## Value

List with:

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
