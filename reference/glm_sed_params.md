# Generate GLM Sediment Parameters

Generate GLM Sediment Parameters

## Usage

``` r
glm_sed_params(
  n_zones = 1,
  zone_heights = c(10),
  sed_heat_Ksoil = 0.01,
  sed_temp_depth = 0.2,
  sed_temp_mean = c(12),
  sed_temp_amplitude = c(8),
  sed_temp_peak_doy = c(30),
  sed_reflectivity = c(0.01),
  sed_roughness = c(0.01),
  benthic_mode = 2
)
```

## Arguments

- n_zones:

  Number of sediment zones to simulate. Default is 1.

- zone_heights:

  Upper height of zone boundarys (m). Length must equal 'n_zones'. 0 is
  lake bottom. Default is c(10) for n_zones = 1.

- sed_heat_Ksoil:

  Heat conductivity of soil/sediment. Default is 0.01.

- sed_temp_depth:

  Depth of soil/sediment layer below the lake bottom, used for heat flux
  calculation. Default is 0.2.

- sed_temp_mean:

  Annual mean sediment temperature. Default is 12.

- sed_temp_amplitude:

  Amplitude of temperature variation experienced in the sediment over
  one year. Default is 8.

- sed_temp_peak_doy:

  Day of the year where the sediment temperature peaks. Default is 30.

- sed_reflectivity:

  Sediment reflectivity. Default is 0.01.

- sed_roughness:

  Sediment roughness. Default is 0.01.

- benthic_mode:

  Switch to configure which mode of benthic interaction to apply.
  Options are:

  - 0: Bottom layer only

  - 1: Bottom layer & layer flanks

  - 2: Sediment zones with individual properties (default)

## Value

Data frame of GLM sediment parameters

## Examples

``` r
# Generate sediment parameters for 1 zone
sed_params_1zone <- glm_sed_params(n_zones = 1)
print(sed_params_1zone)
#>      model     file                        name value    min    max group index
#> 1  glm_aed glm3.nml       sediment/benthic_mode  2.00  2.000  2.000  <NA>    NA
#> 2  glm_aed glm3.nml            sediment/n_zones  1.00  1.000  1.000  <NA>    NA
#> 3  glm_aed glm3.nml     sediment/sed_heat_Ksoil  0.01  0.005  0.015  <NA>     1
#> 4  glm_aed glm3.nml     sediment/sed_temp_depth  0.20  0.100  0.300  <NA>     1
#> 5  glm_aed glm3.nml      sediment/sed_temp_mean 12.00  6.000 18.000  <NA>     1
#> 6  glm_aed glm3.nml sediment/sed_temp_amplitude  8.00  4.000 12.000  <NA>     1
#> 7  glm_aed glm3.nml  sediment/sed_temp_peak_doy 30.00 15.000 45.000  <NA>     1
#> 8  glm_aed glm3.nml       sediment/zone_heights 10.00  5.000 15.000  <NA>     1
#> 9  glm_aed glm3.nml   sediment/sed_reflectivity  0.01  0.005  0.015  <NA>     1
#> 10 glm_aed glm3.nml      sediment/sed_roughness  0.01  0.005  0.015  <NA>     1
#>      module
#> 1  sediment
#> 2  sediment
#> 3  sediment
#> 4  sediment
#> 5  sediment
#> 6  sediment
#> 7  sediment
#> 8  sediment
#> 9  sediment
#> 10 sediment
#' # Generate sediment parameters for 3 zones
sed_params_3zones <- glm_sed_params(
  n_zones = 3,
  zone_heights = c(5, 15, 20),
  sed_temp_mean = c(10, 12, 14)
)
print(sed_params_3zones)
#>      model     file                        name value    min    max group index
#> 1  glm_aed glm3.nml       sediment/benthic_mode  2.00  2.000  2.000  <NA>    NA
#> 2  glm_aed glm3.nml            sediment/n_zones  3.00  3.000  3.000  <NA>    NA
#> 3  glm_aed glm3.nml     sediment/sed_heat_Ksoil  0.01  0.005  0.015  <NA>     1
#> 4  glm_aed glm3.nml     sediment/sed_heat_Ksoil  0.01  0.005  0.015  <NA>     2
#> 5  glm_aed glm3.nml     sediment/sed_heat_Ksoil  0.01  0.005  0.015  <NA>     3
#> 6  glm_aed glm3.nml     sediment/sed_temp_depth  0.20  0.100  0.300  <NA>     1
#> 7  glm_aed glm3.nml     sediment/sed_temp_depth  0.20  0.100  0.300  <NA>     2
#> 8  glm_aed glm3.nml     sediment/sed_temp_depth  0.20  0.100  0.300  <NA>     3
#> 9  glm_aed glm3.nml      sediment/sed_temp_mean 10.00  5.000 15.000  <NA>     1
#> 10 glm_aed glm3.nml      sediment/sed_temp_mean 12.00  6.000 18.000  <NA>     2
#> 11 glm_aed glm3.nml      sediment/sed_temp_mean 14.00  7.000 21.000  <NA>     3
#> 12 glm_aed glm3.nml sediment/sed_temp_amplitude  8.00  4.000 12.000  <NA>     1
#> 13 glm_aed glm3.nml sediment/sed_temp_amplitude  8.00  4.000 12.000  <NA>     2
#> 14 glm_aed glm3.nml sediment/sed_temp_amplitude  8.00  4.000 12.000  <NA>     3
#> 15 glm_aed glm3.nml  sediment/sed_temp_peak_doy 30.00 15.000 45.000  <NA>     1
#> 16 glm_aed glm3.nml  sediment/sed_temp_peak_doy 30.00 15.000 45.000  <NA>     2
#> 17 glm_aed glm3.nml  sediment/sed_temp_peak_doy 30.00 15.000 45.000  <NA>     3
#> 18 glm_aed glm3.nml       sediment/zone_heights  5.00  2.500  7.500  <NA>     1
#> 19 glm_aed glm3.nml       sediment/zone_heights 15.00  7.500 22.500  <NA>     2
#> 20 glm_aed glm3.nml       sediment/zone_heights 20.00 10.000 30.000  <NA>     3
#> 21 glm_aed glm3.nml   sediment/sed_reflectivity  0.01  0.005  0.015  <NA>     1
#> 22 glm_aed glm3.nml   sediment/sed_reflectivity  0.01  0.005  0.015  <NA>     2
#> 23 glm_aed glm3.nml   sediment/sed_reflectivity  0.01  0.005  0.015  <NA>     3
#> 24 glm_aed glm3.nml      sediment/sed_roughness  0.01  0.005  0.015  <NA>     1
#> 25 glm_aed glm3.nml      sediment/sed_roughness  0.01  0.005  0.015  <NA>     2
#> 26 glm_aed glm3.nml      sediment/sed_roughness  0.01  0.005  0.015  <NA>     3
#>      module
#> 1  sediment
#> 2  sediment
#> 3  sediment
#> 4  sediment
#> 5  sediment
#> 6  sediment
#> 7  sediment
#> 8  sediment
#> 9  sediment
#> 10 sediment
#> 11 sediment
#> 12 sediment
#> 13 sediment
#> 14 sediment
#> 15 sediment
#> 16 sediment
#> 17 sediment
#> 18 sediment
#> 19 sediment
#> 20 sediment
#> 21 sediment
#> 22 sediment
#> 23 sediment
#> 24 sediment
#> 25 sediment
#> 26 sediment
```
