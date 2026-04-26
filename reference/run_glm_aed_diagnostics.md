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
  print_table = TRUE
)
```

## Arguments

- model:

  Model name. One of "gotm_wet", "glm_aed", or "dy_cd".

- groups:

  character vector selecting catalogue entries. Accepts: - catalogue
  entry names (e.g. "nitrogen_state"), - element codes
  ("O","N","P","Phy"), - types ("state","process"). Default NULL = all
  entries.

- depth_collapse:

  "mean", "surface" or "max" — reduce 3D variables

- plot:

  draw combined plots, grouped by element

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
#> ℹ MET_tmpair: values appear to be in the expected units, no conversion applied.
#> ℹ MET_tmpdew: values appear to be in the expected units, no conversion applied.
#> ℹ MET_radswd: values appear to be in the expected units, no conversion applied.
#> ℹ MET_radlwd: values appear to be in the expected units, no conversion applied.
#> ℹ MET_humrel: values appear to be in the expected units, no conversion applied.
#> ℹ MET_cldcvr: values appear to be in the expected units, no conversion applied.
#> ℹ MET_prsttn: values appear to be in the expected units, no conversion applied.
#> ℹ MET_prmslp: values appear to be in the expected units, no conversion applied.
#> ℹ MET_prvapr: values appear to be in the expected units, no conversion applied.
#> ℹ MET_wndspd: values appear to be in the expected units, no conversion applied.
#> ℹ MET_wnduvu: values appear to be in the expected units, no conversion applied.
#> ℹ MET_wnduvv: values appear to be in the expected units, no conversion applied.
#> ℹ MET_pprain: values appear to be in the expected units, no conversion applied.
#> ℹ MET_ppsnow: values appear to be in the expected units, no conversion applied.
#> ! Missing state variables in inflows:
#> ! ZOO_zoo1
#> ℹ Added default values for missing variables.
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
#> ✔ GLM nml validation completed - no issues detected.

aeme <- run_aeme(aeme)
#> ℹ Running models... (Have you tried parallelizing?) [2026-04-26 22:37:21]
#> → GLM-AED running... [2026-04-26 22:37:21]
#> ✔ GLM-AED run successful! [2026-04-26 22:37:23]
#> ✔ Model run complete! [2026-04-26 22:37:23]
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
#> |oxygen_state             |OXY_oxy         |O2 (mmol/m3)                |  158.439| 228.830| 234.104| 322.116| 50.287|ok   |
#> |oxygen_state             |OXY_sat         |O2 saturation (%)           |   49.561|  71.764|  73.457|  95.588| 10.511|ok   |
#> |oxygen_fluxes            |OXY_oxy_atm     |Atm O2 flux (mmol/m2/d)     |  -11.033|  15.360|  32.286| 428.343| 48.208|ok   |
#> |oxygen_fluxes            |OXY_oxy_atmv    |Atm O2 flux (vol)           |   -1.316|   1.417|   3.197|  55.914|  5.783|ok   |
#> |oxygen_fluxes            |OXY_oxy_dsf     |SWI O2 flux (mmol/m2/d)     |  -22.414| -15.979| -15.901| -10.096|  2.305|ok   |
#> |oxygen_fluxes            |OXY_oxy_dsfv    |SOD (vol)                   |   -3.070|  -1.961|  -1.998|  -1.314|  0.304|ok   |
#> |nitrogen_state           |NIT_amm         |NH4 (mmol N/m3)             |    0.000|   0.000|   0.009|   0.043|  0.013|ok   |
#> |nitrogen_state           |NIT_n2o         |N2O (mmol N/m3)             |    0.011|   0.034|   0.108|   0.402|  0.133|ok   |
#> |nitrogen_state           |NIT_nit         |NO3 (mmol N/m3)             |    2.423|   4.851|   4.735|   6.346|  1.016|ok   |
#> |nitrogen_state           |NIT_no2         |NO2 (mmol N/m3)             |    0.000|   0.002|   0.016|   0.065|  0.020|ok   |
#> |nitrogen_organic         |OGM_don         |DON (mmol N/m3)             |    0.361|   0.900|   2.461|  21.164|  3.847|ok   |
#> |nitrogen_organic         |OGM_donr        |Refractory DON              |    0.527|   1.315|   1.787|   8.946|  1.534|ok   |
#> |nitrogen_organic         |OGM_pon         |PON (mmol N/m3)             |    2.652|   3.299|   3.440|   7.086|  0.636|ok   |
#> |nitrogen_transformations |NIT_anammox     |Anammox                     |    0.000|   0.000|   0.000|   0.001|  0.000|ok   |
#> |nitrogen_transformations |NIT_denit       |Denitrification             |    0.000|   0.000|   0.000|   0.000|  0.000|ok   |
#> |nitrogen_transformations |NIT_dnra        |DNRA                        |    0.000|   0.000|   0.000|   0.000|  0.000|ok   |
#> |nitrogen_transformations |NIT_n2oprod     |N2O production              |    0.000|   0.000|   0.003|   0.032|  0.006|ok   |
#> |nitrogen_transformations |NIT_nitrif      |Nitrification               |    0.000|   0.006|   0.026|   0.231|  0.040|ok   |
#> |nitrogen_sediment_flux   |NIT_amm_dsf     |NH4 SWI flux                |    0.136|   0.293|   0.411|   0.822|  0.248|ok   |
#> |nitrogen_sediment_flux   |NIT_n2o_atm     |N2O atm flux                |    0.000|   0.003|   0.011|   0.332|  0.031|ok   |
#> |nitrogen_sediment_flux   |NIT_n2o_dsf     |N2O SWI flux                |    0.000|   0.000|   0.000|   0.000|  0.000|ok   |
#> |nitrogen_sediment_flux   |NIT_nit_dsf     |NO3 SWI flux                |    0.072|   0.105|   0.108|   0.153|  0.014|ok   |
#> |nitrogen_sediment_flux   |NIT_no2_dsf     |NO2 SWI flux                |    0.000|   0.000|   0.000|   0.000|  0.000|ok   |
#> |phosphorus_state         |OGM_dop         |DOP                         |    0.004|   0.013|   0.036|   0.319|  0.058|ok   |
#> |phosphorus_state         |OGM_dopr        |Refractory DOP              |    0.009|   0.022|   0.030|   0.149|  0.026|ok   |
#> |phosphorus_state         |OGM_pop         |POP                         |    0.082|   0.111|   0.124|   0.321|  0.036|ok   |
#> |phosphorus_state         |PHS_frp         |FRP (mmol P/m3)             |    0.000|   0.000|   0.002|   0.010|  0.002|ok   |
#> |phosphorus_fluxes        |OGM_dop_min     |DOP mineralisation          |    0.000|   0.000|   0.000|   0.002|  0.000|ok   |
#> |phosphorus_fluxes        |OGM_dop_swi     |DOP SWI flux                |    0.000|   0.000|   0.000|   0.000|  0.000|ok   |
#> |phosphorus_fluxes        |OGM_pop_res     |POP resuspension            |    0.000|   0.000|   0.000|   0.000|  0.000|ok   |
#> |phosphorus_fluxes        |OGM_pop_swi     |POP SWI flux                |   -0.093|  -0.028|  -0.031|  -0.021|  0.010|ok   |
#> |phosphorus_fluxes        |PHS_frp_dsf     |FRP SWI flux                |    0.001|   0.001|   0.005|   0.014|  0.005|ok   |
#> |phyto_biomass            |PHY_cyano       |Cyanobacteria               |    0.030|   0.031|   0.112|   0.505|  0.137|ok   |
#> |phyto_biomass            |PHY_diatom      |Diatoms                     |    0.030|   0.030|   1.632|  19.728|  4.027|ok   |
#> |phyto_biomass            |PHY_green       |Greens                      |    0.305|  20.858|  21.658|  47.534| 14.379|ok   |
#> |phyto_biomass            |PHY_tchla       |Total chl-a (ug/L)          |    0.224|   6.400|   6.609|  14.227|  3.450|ok   |
#> |phyto_biomass            |PHY_tphy        |Total phyto (mmol C/m3)     |    0.786|  21.338|  22.053|  47.430| 11.487|ok   |
#> |phyto_stoichiometry      |PHY_cyano_NtoP  |Cyano N:P                   |   11.499|  48.934|  46.057|  71.051| 14.623|ok   |
#> |phyto_stoichiometry      |PHY_diatom_NtoP |Diatom N:P                  |   11.447|  54.350|  49.859|  71.448| 14.895|ok   |
#> |phyto_stoichiometry      |PHY_green_NtoP  |Green N:P                   |   11.551|  45.041|  42.600|  66.380| 13.187|ok   |
#> |phyto_limitation         |PHY_cyano_fI    |Cyano fI                    |    0.011|   0.242|   0.232|   0.326|  0.059|ok   |
#> |phyto_limitation         |PHY_cyano_fNit  |Cyano fN                    |    0.659|   0.988|   0.951|   0.997|  0.057|ok   |
#> |phyto_limitation         |PHY_cyano_fPho  |Cyano fP                    |    0.000|   0.213|   0.296|   0.997|  0.295|ok   |
#> |phyto_limitation         |PHY_cyano_fT    |Cyano fT                    |    0.469|   0.863|   0.826|   1.057|  0.157|ok   |
#> |phyto_limitation         |PHY_diatom_fI   |Diatom fI                   |    0.027|   0.321|   0.308|   0.405|  0.063|ok   |
#> |phyto_limitation         |PHY_diatom_fNit |Diatom fN                   |    0.659|   0.978|   0.949|   0.997|  0.057|ok   |
#> |phyto_limitation         |PHY_diatom_fPho |Diatom fP                   |    0.000|   0.127|   0.233|   0.994|  0.297|ok   |
#> |phyto_limitation         |PHY_diatom_fT   |Diatom fT                   |    0.664|   1.000|   0.958|   1.000|  0.058|ok   |
#> |phyto_limitation         |PHY_green_fI    |Green fI                    |    0.011|   0.242|   0.232|   0.326|  0.059|ok   |
#> |phyto_limitation         |PHY_green_fNit  |Green fN                    |    0.664|   0.991|   0.955|   1.000|  0.057|ok   |
#> |phyto_limitation         |PHY_green_fPho  |Green fP                    |    0.046|   0.412|   0.431|   0.998|  0.259|ok   |
#> |phyto_limitation         |PHY_green_fT    |Green fT                    |    0.664|   1.000|   0.958|   1.000|  0.058|ok   |
#> |phyto_fluxes             |PHY_gpp         |GPP                         |    0.139|   1.040|   1.398|   7.004|  1.168|ok   |
#> |phyto_fluxes             |PHY_ncp         |NCP                         |   -0.036|   0.878|   1.184|   6.327|  1.057|ok   |
#> |phyto_fluxes             |PHY_set         |Sedimentation               |  -13.534|  -0.865|  -1.594|  -0.180|  2.286|ok   |
#> |phyto_fluxes             |PHY_upt_nh4     |NH4 uptake                  |    0.005|   0.039|   0.054|   0.263|  0.045|ok   |
#> |phyto_fluxes             |PHY_upt_no3     |NO3 uptake                  |    0.000|   0.000|   0.000|   0.000|  0.000|ok   |
#> |phyto_fluxes             |PHY_upt_po4     |PO4 uptake                  |    0.000|   0.000|   0.001|   0.011|  0.002|ok   |
#> |sedflux_oxygen_Z         |OXY_oxy_atm_Z   |Atm O2 flux (per zone)      |    0.000|   0.000|   0.000|   0.000|  0.000|ok   |
#> |sedflux_oxygen_Z         |OXY_oxy_dsf_Z   |SWI O2 exchange (per zone)  |  -23.577| -14.113| -14.037|   0.000|  6.935|ok   |
#> |sedflux_oxygen_Z         |SDF_Fsed_oxy_Z  |SDF O2 flux (per zone)      |  -25.000| -25.000| -25.000| -25.000|  0.000|ok   |
#> |sedflux_nitrogen_Z       |NIT_amm_dsf_Z   |NH4 SWI flux (per zone)     |    0.132|   0.287|   0.520|   1.841|  0.524|ok   |
#> |sedflux_nitrogen_Z       |NIT_n2o_atm_Z   |N2O atm flux (per zone)     |    0.000|   0.000|   0.000|   0.000|  0.000|ok   |
#> |sedflux_nitrogen_Z       |NIT_n2o_dsf_Z   |N2O SWI flux (per zone)     |    0.000|   0.000|   0.000|   0.000|  0.000|ok   |
#> |sedflux_nitrogen_Z       |NIT_nit_dsf_Z   |NO3 SWI flux (per zone)     |    0.000|   0.104|   0.095|   0.156|  0.048|ok   |
#> |sedflux_nitrogen_Z       |NIT_no2_dsf_Z   |NO2 SWI flux (per zone)     |    0.000|   0.000|   0.000|   0.000|  0.000|ok   |
#> |sedflux_nitrogen_Z       |SDF_Fsed_amm_Z  |SDF NH4 flux (per zone)     |    2.000|   2.000|   2.000|   2.000|  0.000|ok   |
#> |sedflux_nitrogen_Z       |SDF_Fsed_nit_Z  |SDF NO3 flux (per zone)     |    0.200|   0.200|   0.200|   0.200|  0.000|ok   |
#> |sedflux_phosphorus_Z     |OGM_doc_swi_Z   |DOC SWI flux (per zone)     |    0.014|   0.027|   0.037|   0.094|  0.024|ok   |
#> |sedflux_phosphorus_Z     |OGM_don_swi_Z   |DON SWI flux (per zone)     |    0.000|   0.000|   0.000|   0.000|  0.000|ok   |
#> |sedflux_phosphorus_Z     |OGM_dop_swi_Z   |DOP SWI flux (per zone)     |    0.000|   0.000|   0.000|   0.000|  0.000|ok   |
#> |sedflux_phosphorus_Z     |OGM_poc_swi_Z   |POC SWI flux (per zone)     |  -10.082|  -3.085|  -4.250|  -0.983|  2.375|ok   |
#> |sedflux_phosphorus_Z     |OGM_pon_swi_Z   |PON SWI flux (per zone)     |   -2.576|  -0.805|  -0.717|  -0.259|  0.390|ok   |
#> |sedflux_phosphorus_Z     |OGM_pop_swi_Z   |POP SWI flux (per zone)     |   -0.117|  -0.029|  -0.026|  -0.008|  0.015|ok   |
#> |sedflux_phosphorus_Z     |PHS_frp_dsf_Z   |FRP SWI flux (per zone)     |    0.001|   0.001|   0.007|   0.044|  0.013|ok   |
#> |sedflux_phosphorus_Z     |SDF_Fsed_frp_Z  |SDF FRP flux (per zone)     |    0.050|   0.050|   0.050|   0.050|  0.000|ok   |
#> |sedflux_organic_Z        |OGM_poc_res_Z   |POC resuspension (per zone) |    0.000|   0.000|   0.000|   0.000|  0.000|ok   |
#> |sedflux_organic_Z        |OGM_pon_res_Z   |PON resuspension (per zone) |    0.000|   0.000|   0.000|   0.000|  0.000|ok   |
#> |sedflux_organic_Z        |OGM_pop_res_Z   |POP resuspension (per zone) |    0.000|   0.000|   0.000|   0.000|  0.000|ok   |
#> |sedflux_organic_Z        |OGM_toc_sed_Z   |TOC sed mass (per zone)     |    0.000|   0.000|   0.000|   0.000|  0.000|ok   |
#> |sedflux_organic_Z        |OGM_ton_sed_Z   |TON sed mass (per zone)     |    0.000|   0.000|   0.000|   0.000|  0.000|ok   |
#> |sedflux_organic_Z        |OGM_top_sed_Z   |TOP sed mass (per zone)     |    0.000|   0.000|   0.000|   0.000|  0.000|ok   |
#> |sedflux_organic_Z        |PHY_phy_swi_c_Z |Phyto SWI C (per zone)      | -134.660|  -5.163| -10.390|  -0.468| 18.226|ok   |
#> |sedflux_organic_Z        |PHY_phy_swi_n_Z |Phyto SWI N (per zone)      |   -9.103|  -0.361|  -0.715|  -0.033|  1.235|ok   |
#> |sedflux_organic_Z        |PHY_phy_swi_p_Z |Phyto SWI P (per zone)      |   -0.329|  -0.009|  -0.022|  -0.002|  0.045|ok   |
#> |sedflux_silica_Z         |SIL_dsf_rsi_Z   |Si SWI flux (per zone)      |    0.002|   0.004|   0.102|   0.927|  0.249|ok   |




```
