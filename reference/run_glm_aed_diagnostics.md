# Run GLM-AED diagnostics

Run GLM-AED diagnostics

## Usage

``` r
run_glm_aed_diagnostics(
  aeme,
  model,
  groups = NULL,
  depth_collapse = "mean",
  plot = TRUE,
  use_bounds = TRUE,
  print_table = TRUE
)
```

## Arguments

- model:

  Model name. One of "gotm_wet", "glm_aed", or "dy_cd".

- groups:

  character vector selecting catalogue entries. Accepts:

  - catalogue entry names (e.g. "nitrogen_state"),

  - element codes ("O","N","P","Phy"),

  - types ("state","process"). Default NULL = all entries.

- depth_collapse:

  "mean", "surface" or "max" — reduce 3D variables

- plot:

  draw combined plots, grouped by element

- use_bounds:

  add dashed lines to plots showing expected bounds (from catalogue)

- print_table:

  print the kable summary

## Value

invisibly, list(summary, plots, data)

## Examples

``` r
aeme_dir <- system.file("extdata/lake/", package = "AEME")
# Copy files from package into tempdir
path <- tempdir()
aeme <- yaml_to_aeme(path = aeme_dir, "aeme.yaml")
vars_sim <- c("HYD_strat", "HYD_temp", "HYD_thmcln", "HYD_schstb", 
              "CHM_oxycln", "CHM_oxynal",
              "NIT_tn", "PHS_tp", "PHY_tchla")
model_controls <- get_model_controls(use_bgc = TRUE)
model_controls <- set_vars_sim(model_controls = model_controls,
                               vars_sim = vars_sim)
model <- c("glm_aed")
aeme <- build_aeme(path = path, aeme = aeme, model = model,
                   model_controls = model_controls,
                   ext_elev = 5, use_bgc = TRUE)
#> ℹ All columns already match AEME standard variable names, skipping name
#>   guessing.
#> ℹ All columns already match AEME standard inflow variable names, skipping name
#>   guessing.
#> Warning: ! `SIL_rsi`: SIL_rsi is constant across all rows — this may be a placeholder
#>   value.
#> ℹ Check raw data or unit conversion for this variable.
#> Warning: ! 1 missing state variable in `FWMT`:
#> ✖ `ZOO_zoo1 `
#> ℹ Filled 1 missing variable with default value from `model_controls`.
#> ℹ Using observed water level.
#> ! Missing values in observed water level.
#> ℹ Correcting water balance using estimated outflows (method = 2).
#> ℹ Calculating lake level using lake depth and a sinisoidal function.
#> ℹ Building GLM-AED for lake wainamu
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
#> ℹ PHY_cyano 10 replaced with 0.24022
#> ℹ PHY_diatom 8.4 replaced with 0.300275
#> ℹ PHY_green 0.04 replaced with 0.300275
#> ℹ 100 replaced with 1
#> ℹ Using default zooplankton initialisation
#> ✔ Updated GLM-AED models from: aed_sedflux, aed_oxygen, aed_silica,
#>   aed_nitrogen, aed_phosphorus, aed_organic_matter, aed_phytoplankton,
#>   aed_zooplankton, aed_macrophyte, aed_totals to: aed_sedflux, aed_oxygen,
#>   aed_silica, aed_nitrogen, aed_phosphorus, aed_organic_matter,
#>   aed_phytoplankton, aed_totals
#> ℹ Setting up AED aed_sed_const2d sediment zones: 2
#> 
#> Tier 2: zone-median summer concentrations used for adjustment:
#>         oxy   amm   nit   frp
#> Zone1 0.075 0.078 0.010 0.004
#> Zone2 7.160 0.005 0.001 0.002
#> Tier 2 adjustments applied: fsed_amm (2 zones, direct NH4); fsed_frp (2 zones, direct FRP)
#> 
#> === Sediment zone flux estimates (obs_adjusted) ===
#> n_zones: 2 | max lake depth: 13.07 m | ref_depth: 5 m
#> 
#>  zone height_lower_m height_upper_m depth_upper_m depth_lower_m mean_depth_m
#>     1           0.00           3.07            10          13.1         11.5
#>     2           3.07          19.00             0          10.0          5.0
#>  area_m2 area_frac fsed_oxy fsed_amm fsed_nit fsed_frp
#>    43957     0.289    -38.8    5.835     -0.4   0.1035
#>   108386     0.711    -19.4    0.512      0.1   0.0259
#> 
#> Lake-wide area-weighted average fluxes (for sanity check):
#>     oxy     amm     nit     frp 
#> -25.007   2.050  -0.044   0.048 
#> ✔ GLM nml validation completed - no issues detected.

aeme <- run_aeme(aeme)
#> ℹ Running models... (Have you tried parallelizing?) [2026-05-17 21:38:08]
#> → GLM-AED running... [2026-05-17 21:38:08]
#> ✔ GLM-AED run successful! [2026-05-17 21:38:11]
#> ✔ Model run complete! [2026-05-17 21:38:11]
out <- run_glm_aed_diagnostics(aeme = aeme)
#> Requesting 86 variables from model output...  (sediment zone _Z variables are
#> optional; missing ones are skipped)
#> 
#> === GLM-AED diagnostic summary ===
#> All diagnostics within expected ranges.
#> 
#> -- Full summary --
#> 
#> 
#> |group                    |variable        |label                       |      min|  median|    mean|     max|     sd|flag |
#> |:------------------------|:---------------|:---------------------------|--------:|-------:|-------:|-------:|------:|:----|
#> |oxygen_state             |OXY_oxy         |O2 (mmol/m3)                |  176.426| 233.781| 240.075| 322.492| 44.019|ok   |
#> |oxygen_state             |OXY_sat         |O2 saturation (%)           |   49.560|  73.746|  75.338|  95.715|  8.425|ok   |
#> |oxygen_fluxes            |OXY_oxy_atm     |Atm O2 flux (mmol/m2/d)     |  -14.134|  11.722|  25.827| 427.104| 44.832|ok   |
#> |oxygen_fluxes            |OXY_oxy_atmv    |Atm O2 flux (vol)           |   -1.688|   0.900|   2.576|  55.752|  5.371|ok   |
#> |oxygen_fluxes            |OXY_oxy_dsf     |SWI O2 flux (mmol/m2/d)     |  -22.248| -14.172| -14.588|  -9.977|  2.428|ok   |
#> |oxygen_fluxes            |OXY_oxy_dsfv    |SOD (vol)                   |   -3.227|  -1.904|  -1.940|  -1.277|  0.390|ok   |
#> |nitrogen_state           |NIT_amm         |NH4 (mmol N/m3)             |    0.000|   0.004|   0.024|   0.111|  0.032|ok   |
#> |nitrogen_state           |NIT_n2o         |N2O (mmol N/m3)             |    0.011|   0.056|   0.253|   1.105|  0.334|ok   |
#> |nitrogen_state           |NIT_nit         |NO3 (mmol N/m3)             |    2.422|   3.877|   3.774|   4.530|  0.444|ok   |
#> |nitrogen_state           |NIT_no2         |NO2 (mmol N/m3)             |    0.000|   0.014|   0.038|   0.143|  0.044|ok   |
#> |nitrogen_organic         |OGM_don         |DON (mmol N/m3)             |    0.371|   0.878|   2.455|  21.164|  3.850|ok   |
#> |nitrogen_organic         |OGM_donr        |Refractory DON              |    0.527|   1.315|   1.787|   8.946|  1.534|ok   |
#> |nitrogen_organic         |OGM_pon         |PON (mmol N/m3)             |    2.652|   3.217|   3.423|   7.086|  0.671|ok   |
#> |nitrogen_transformations |NIT_anammox     |Anammox                     |    0.000|   0.000|   0.001|   0.005|  0.001|ok   |
#> |nitrogen_transformations |NIT_denit       |Denitrification             |    0.000|   0.000|   0.000|   0.000|  0.000|ok   |
#> |nitrogen_transformations |NIT_dnra        |DNRA                        |    0.000|   0.000|   0.000|   0.000|  0.000|ok   |
#> |nitrogen_transformations |NIT_n2oprod     |N2O production              |    0.000|   0.002|   0.007|   0.063|  0.012|ok   |
#> |nitrogen_transformations |NIT_nitrif      |Nitrification               |    0.000|   0.071|   0.110|   0.926|  0.151|ok   |
#> |nitrogen_sediment_flux   |NIT_amm_dsf     |NH4 SWI flux                |    0.135|   0.532|   0.701|   1.532|  0.514|ok   |
#> |nitrogen_sediment_flux   |NIT_n2o_atm     |N2O atm flux                |    0.000|   0.002|   0.018|   0.782|  0.062|ok   |
#> |nitrogen_sediment_flux   |NIT_n2o_dsf     |N2O SWI flux                |    0.000|   0.000|   0.000|   0.000|  0.000|ok   |
#> |nitrogen_sediment_flux   |NIT_nit_dsf     |NO3 SWI flux                |   -0.032|   0.003|   0.011|   0.055|  0.030|ok   |
#> |nitrogen_sediment_flux   |NIT_no2_dsf     |NO2 SWI flux                |    0.000|   0.000|   0.000|   0.000|  0.000|ok   |
#> |phosphorus_state         |OGM_dop         |DOP                         |    0.004|   0.012|   0.036|   0.319|  0.058|ok   |
#> |phosphorus_state         |OGM_dopr        |Refractory DOP              |    0.009|   0.022|   0.030|   0.149|  0.026|ok   |
#> |phosphorus_state         |OGM_pop         |POP                         |    0.086|   0.111|   0.125|   0.321|  0.036|ok   |
#> |phosphorus_state         |PHS_frp         |FRP (mmol P/m3)             |    0.000|   0.001|   0.002|   0.010|  0.002|ok   |
#> |phosphorus_fluxes        |OGM_dop_min     |DOP mineralisation          |    0.000|   0.000|   0.000|   0.002|  0.000|ok   |
#> |phosphorus_fluxes        |OGM_dop_swi     |DOP SWI flux                |    0.000|   0.000|   0.000|   0.000|  0.000|ok   |
#> |phosphorus_fluxes        |OGM_pop_res     |POP resuspension            |    0.000|   0.000|   0.000|   0.000|  0.000|ok   |
#> |phosphorus_fluxes        |OGM_pop_swi     |POP SWI flux                |   -0.093|  -0.028|  -0.032|  -0.021|  0.010|ok   |
#> |phosphorus_fluxes        |PHS_frp_dsf     |FRP SWI flux                |    0.001|   0.003|   0.008|   0.025|  0.009|ok   |
#> |phyto_biomass            |PHY_cyano       |Cyanobacteria               |    0.030|   0.031|   0.112|   0.504|  0.136|ok   |
#> |phyto_biomass            |PHY_diatom      |Diatoms                     |    0.030|   0.030|   1.618|  19.594|  4.002|ok   |
#> |phyto_biomass            |PHY_green       |Greens                      |    0.305|  19.228|  22.932|  57.277| 17.140|ok   |
#> |phyto_biomass            |PHY_tchla       |Total chl-a (ug/L)          |    0.224|   5.995|   6.937|  17.144|  4.213|ok   |
#> |phyto_biomass            |PHY_tphy        |Total phyto (mmol C/m3)     |    0.786|  20.077|  23.145|  57.151| 14.030|ok   |
#> |phyto_stoichiometry      |PHY_cyano_NtoP  |Cyano N:P                   |   11.499|  48.238|  45.319|  70.930| 14.623|ok   |
#> |phyto_stoichiometry      |PHY_diatom_NtoP |Diatom N:P                  |   11.447|  53.348|  48.752|  71.049| 14.894|ok   |
#> |phyto_stoichiometry      |PHY_green_NtoP  |Green N:P                   |   11.551|  44.797|  42.067|  66.727| 13.218|ok   |
#> |phyto_limitation         |PHY_cyano_fI    |Cyano fI                    |    0.014|   0.243|   0.232|   0.328|  0.060|ok   |
#> |phyto_limitation         |PHY_cyano_fNit  |Cyano fN                    |    0.634|   0.987|   0.950|   0.997|  0.059|ok   |
#> |phyto_limitation         |PHY_cyano_fPho  |Cyano fP                    |    0.000|   0.244|   0.315|   0.997|  0.292|ok   |
#> |phyto_limitation         |PHY_cyano_fT    |Cyano fT                    |    0.439|   0.863|   0.826|   1.058|  0.157|ok   |
#> |phyto_limitation         |PHY_diatom_fI   |Diatom fI                   |    0.034|   0.321|   0.307|   0.407|  0.065|ok   |
#> |phyto_limitation         |PHY_diatom_fNit |Diatom fN                   |    0.631|   0.978|   0.947|   0.997|  0.058|ok   |
#> |phyto_limitation         |PHY_diatom_fPho |Diatom fP                   |    0.000|   0.159|   0.246|   0.994|  0.292|ok   |
#> |phyto_limitation         |PHY_diatom_fT   |Diatom fT                   |    0.638|   1.000|   0.958|   1.000|  0.059|ok   |
#> |phyto_limitation         |PHY_green_fI    |Green fI                    |    0.014|   0.243|   0.232|   0.328|  0.060|ok   |
#> |phyto_limitation         |PHY_green_fNit  |Green fN                    |    0.637|   0.991|   0.955|   1.000|  0.058|ok   |
#> |phyto_limitation         |PHY_green_fPho  |Green fP                    |    0.039|   0.422|   0.440|   0.998|  0.258|ok   |
#> |phyto_limitation         |PHY_green_fT    |Green fT                    |    0.638|   1.000|   0.958|   1.000|  0.059|ok   |
#> |phyto_fluxes             |PHY_gpp         |GPP                         |    0.121|   0.985|   1.452|   6.976|  1.260|ok   |
#> |phyto_fluxes             |PHY_ncp         |NCP                         |   -0.057|   0.828|   1.233|   6.299|  1.153|ok   |
#> |phyto_fluxes             |PHY_set         |Sedimentation               |  -13.444|  -0.873|  -1.623|  -0.180|  2.277|ok   |
#> |phyto_fluxes             |PHY_upt_nh4     |NH4 uptake                  |    0.005|   0.037|   0.056|   0.262|  0.049|ok   |
#> |phyto_fluxes             |PHY_upt_no3     |NO3 uptake                  |    0.000|   0.000|   0.000|   0.000|  0.000|ok   |
#> |phyto_fluxes             |PHY_upt_po4     |PO4 uptake                  |    0.000|   0.000|   0.001|   0.011|  0.002|ok   |
#> |sedflux_oxygen_Z         |OXY_oxy_atm_Z   |Atm O2 flux (per zone)      |    0.000|   0.000|   0.000|   0.000|  0.000|ok   |
#> |sedflux_oxygen_Z         |OXY_oxy_dsf_Z   |SWI O2 exchange (per zone)  |  -34.720| -15.532| -14.740|  -0.025|  7.057|ok   |
#> |sedflux_oxygen_Z         |SDF_Fsed_oxy_Z  |SDF O2 flux (per zone)      |  -38.800| -29.100| -29.100| -19.400|  9.707|ok   |
#> |sedflux_nitrogen_Z       |NIT_amm_dsf_Z   |NH4 SWI flux (per zone)     |    0.034|   0.249|   1.176|   5.206|  1.666|ok   |
#> |sedflux_nitrogen_Z       |NIT_n2o_atm_Z   |N2O atm flux (per zone)     |    0.000|   0.000|   0.000|   0.000|  0.000|ok   |
#> |sedflux_nitrogen_Z       |NIT_n2o_dsf_Z   |N2O SWI flux (per zone)     |    0.000|   0.000|   0.000|   0.000|  0.000|ok   |
#> |sedflux_nitrogen_Z       |NIT_nit_dsf_Z   |NO3 SWI flux (per zone)     |   -0.306|   0.018|  -0.029|   0.079|  0.113|ok   |
#> |sedflux_nitrogen_Z       |NIT_no2_dsf_Z   |NO2 SWI flux (per zone)     |    0.000|   0.000|   0.000|   0.000|  0.000|ok   |
#> |sedflux_nitrogen_Z       |SDF_Fsed_amm_Z  |SDF NH4 flux (per zone)     |    0.512|   3.173|   3.173|   5.835|  2.663|ok   |
#> |sedflux_nitrogen_Z       |SDF_Fsed_nit_Z  |SDF NO3 flux (per zone)     |   -0.400|  -0.150|  -0.150|   0.100|  0.250|ok   |
#> |sedflux_phosphorus_Z     |OGM_doc_swi_Z   |DOC SWI flux (per zone)     |    0.014|   0.028|   0.037|   0.093|  0.024|ok   |
#> |sedflux_phosphorus_Z     |OGM_don_swi_Z   |DON SWI flux (per zone)     |    0.000|   0.000|   0.000|   0.000|  0.000|ok   |
#> |sedflux_phosphorus_Z     |OGM_dop_swi_Z   |DOP SWI flux (per zone)     |    0.000|   0.000|   0.000|   0.000|  0.000|ok   |
#> |sedflux_phosphorus_Z     |OGM_poc_swi_Z   |POC SWI flux (per zone)     |   -9.994|  -3.126|  -4.232|  -0.983|  2.381|ok   |
#> |sedflux_phosphorus_Z     |OGM_pon_swi_Z   |PON SWI flux (per zone)     |   -2.576|  -0.789|  -0.714|  -0.259|  0.392|ok   |
#> |sedflux_phosphorus_Z     |OGM_pop_swi_Z   |POP SWI flux (per zone)     |   -0.117|  -0.030|  -0.026|  -0.008|  0.015|ok   |
#> |sedflux_phosphorus_Z     |PHS_frp_dsf_Z   |FRP SWI flux (per zone)     |    0.000|   0.001|   0.013|   0.089|  0.025|ok   |
#> |sedflux_phosphorus_Z     |SDF_Fsed_frp_Z  |SDF FRP flux (per zone)     |    0.026|   0.065|   0.065|   0.104|  0.039|ok   |
#> |sedflux_organic_Z        |OGM_poc_res_Z   |POC resuspension (per zone) |    0.000|   0.000|   0.000|   0.000|  0.000|ok   |
#> |sedflux_organic_Z        |OGM_pon_res_Z   |PON resuspension (per zone) |    0.000|   0.000|   0.000|   0.000|  0.000|ok   |
#> |sedflux_organic_Z        |OGM_pop_res_Z   |POP resuspension (per zone) |    0.000|   0.000|   0.000|   0.000|  0.000|ok   |
#> |sedflux_organic_Z        |OGM_toc_sed_Z   |TOC sed mass (per zone)     |    0.000|   0.000|   0.000|   0.000|  0.000|ok   |
#> |sedflux_organic_Z        |OGM_ton_sed_Z   |TON sed mass (per zone)     |    0.000|   0.000|   0.000|   0.000|  0.000|ok   |
#> |sedflux_organic_Z        |OGM_top_sed_Z   |TOP sed mass (per zone)     |    0.000|   0.000|   0.000|   0.000|  0.000|ok   |
#> |sedflux_organic_Z        |PHY_phy_swi_c_Z |Phyto SWI C (per zone)      | -133.755|  -5.456| -10.621|  -0.468| 18.218|ok   |
#> |sedflux_organic_Z        |PHY_phy_swi_n_Z |Phyto SWI N (per zone)      |   -9.043|  -0.380|  -0.731|  -0.033|  1.235|ok   |
#> |sedflux_organic_Z        |PHY_phy_swi_p_Z |Phyto SWI P (per zone)      |   -0.328|  -0.009|  -0.023|  -0.002|  0.045|ok   |
#> |sedflux_silica_Z         |SIL_dsf_rsi_Z   |Si SWI flux (per zone)      |    0.002|   0.005|   0.072|   0.866|  0.183|ok   |




```
