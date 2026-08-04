# Run GLM-AED diagnostics

Run GLM-AED diagnostics

## Usage

``` r
run_glm_aed_diagnostics(
  aeme,
  model,
  groups = NULL,
  depth_collapse = "mean",
  plot = FALSE,
  use_bounds = TRUE,
  print_table = TRUE
)
```

## Arguments

- aeme:

  Aeme object.

- model:

  Model name. One of "gotm_wet", "glm_aed", or "dy_cd".

- groups:

  character vector selecting catalogue entries. Accepts:

  - catalogue entry names (e.g. "nitrogen_state"),

  - element codes ("O","N","P","Phy"),

  - types ("state","process"). Default NULL = all entries.

- depth_collapse:

  "mean", "surface" or "max" – reduce 3D variables

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
#> Warning: ! `SIL_rsi`: SIL_rsi is constant across all rows -- this may be a placeholder
#>   value.
#> ℹ Check raw data or unit conversion for this variable.
#> 
#> ── Calculating water balance ──
#> 
#> Resolving water level
#>   ℹ Using observed water level
#> ! Missing values in observed water level
#> ℹ Estimating surface water temperature
#> ✔ Estimating surface water temperature [8ms]
#> 
#> Estimating lake water levels for glm_aed
#>   ℹ Optimizing parameters for water balance
#>   ✔ Optimization Complete: C = 0.3343, h_inv = 23.4915, Final RMSE = 0.1431
#> ℹ Correcting water balance using estimated outflows (method = 2).
#> 
#> ── Building GLM-AED for lake wainamu ──
#> 
#> ℹ Copied in GLM nml file
#> ℹ Copied in AED nml file and supporting files
#> ℹ Copied in GLM plots nml file
#> ℹ CAR_doc: 15 replaced with 41.6285
#> ℹ CAR_poc: 15 replaced with 16.6514
#> ℹ CHM_oxy: 225 replaced with 312.5
#> ℹ NIT_amm: 2.25 replaced with 1.4279
#> ℹ NIT_don: 21 replaced with 21.4183
#> ℹ NIT_nit: 6.96 replaced with 1.0709
#> ℹ NIT_pon: 19.8 replaced with 7.1394
#> ℹ PHS_dop: 0.008 replaced with 0.3229
#> ℹ PHS_frp: 0.05 replaced with 0.3229
#> ℹ PHS_pop: 0.05 replaced with 0.3229
#> ℹ PHY_cyano: 10 replaced with 0.24022
#> ℹ PHY_diatom: 8.4 replaced with 0.300275
#> ℹ PHY_green: 0.04 replaced with 0.300275
#> ℹ SIL_rsi: 100 replaced with 1
#> ℹ Setting up AED aed_sed_const2d sediment zones: 2
#> ℹ Tier 2: zone-median summer concentrations used for adjustment:
#> ┌─────┬───────────┬────────────┬────────────┬────────────┐
#> │ zone│ O2 (mg/L) │ NH4 (mg/L) │ NO3 (mg/L) │ FRP (mg/L) │
#> ├─────┼───────────┼────────────┼────────────┼────────────┤
#> │Zone1│ 0.075     │ 0.078      │ 0.01       │ 0.004      │
#> │Zone2│ 7.16      │ 0.005      │ 0.001      │ 0.002      │
#> └─────┴───────────┴────────────┴────────────┴────────────┘
#> ℹ Tier 2 adjustments applied: fsed_amm (2 zones, direct NH4); fsed_frp (2
#>   zones, direct FRP)
#> ── Sediment zone flux estimates (obs_adjusted) ─────────────────────────────────
#> n_zones: 2 | max lake depth: 13.07 m | ref_depth: 5 m
#> ┌────┬───────────┬───────────┬───────────┬───────────┬──────────┬─────────┬─────────┬─────┬─────┬────┬──────┐
#> │Zone│H lower (m)│H upper (m)│D upper (m)│D lower (m)│Mean D (m)│Area (m2)│Area frac│ O2  │ NH4 │ NO3│ FRP  │
#> ├────┼───────────┼───────────┼───────────┼───────────┼──────────┼─────────┼─────────┼─────┼─────┼────┼──────┤
#> │   1│    0      │ 3.07      │   10      │ 13.1      │ 11.5     │ 4.4e+04 │ 0.289   │-38.8│ 5.83│-0.4│ 0.103│
#> │   2│ 3.07      │   19      │    0      │   10      │    5     │ 1.08e+05│ 0.711   │-19.4│0.512│ 0.1│0.0259│
#> └────┴───────────┴───────────┴───────────┴───────────┴──────────┴─────────┴─────────┴─────┴─────┴────┴──────┘
#> 
#> ── Lake-wide area-weighted average fluxes ──────────────────────────────────────
#> ┌──────────────┬───────────────┬───────────────┬───────────────┐
#> │O2 (mmol/m2/d)│NH4 (mmol/m2/d)│NO3 (mmol/m2/d)│FRP (mmol/m2/d)│
#> ├──────────────┼───────────────┼───────────────┼───────────────┤
#> │ -25.007      │ 2.05          │ -0.044        │ 0.048         │
#> └──────────────┴───────────────┴───────────────┴───────────────┘
#> ✔ GLM nml validation completed - no issues detected.

aeme <- run_aeme(aeme)
#> ℹ Running models... (Have you tried parallelizing?) [2026-08-04 18:48:48]
#> → GLM-AED running... [2026-08-04 18:48:48]
#> ✔ GLM-AED run successful! [2026-08-04 18:48:50]
#> ✔ Model run complete! [2026-08-04 18:48:50]
out <- run_glm_aed_diagnostics(aeme = aeme)
#> Requesting 86 variables from model output...  (sediment zone _Z variables are
#> optional; missing ones are skipped)
#> ── GLM-AED diagnostic summary ──────────────────────────────────────────────────
#> ! 2 flagged variables
#> ┌───────────────────┬──────────────┬───────────────────────┬─────┬──────┬────┬────┬────┬────────────┐
#> │ group             │ variable     │ label                 │ min │median│mean│ max│ sd │ flag       │
#> ├───────────────────┼──────────────┼───────────────────────┼─────┼──────┼────┼────┼────┼────────────┤
#> │ oxygen_fluxes     │ OXY_oxy_atm  │Atm O2 flux (mmol/m2/d)│-13.6│ 6.84 │20.5│ 606│53.8│OUT OF RANGE│
#> │phyto_stoichiometry│PHY_green_NtoP│ Green N:P             │ 29.1│   47 │49.3│ 109│  11│OUT OF RANGE│
#> └───────────────────┴──────────────┴───────────────────────┴─────┴──────┴────┴────┴────┴────────────┘
#> ── Full summary ────────────────────────────────────────────────────────────────
#> ┌────────────────────────┬───────────────┬───────────────────────────┬────────┬─────────┬─────────┬─────────┬────────┬────────────┐
#> │ group                  │ variable      │ label                     │ min    │ median  │ mean    │ max     │ sd     │ flag       │
#> ├────────────────────────┼───────────────┼───────────────────────────┼────────┼─────────┼─────────┼─────────┼────────┼────────────┤
#> │ oxygen_state           │ OXY_oxy       │ O2 (mmol/m3)              │ 92.1   │  242    │  247    │  323    │ 45.9   │ ok         │
#> │ oxygen_state           │ OXY_sat       │ O2 saturation (%)         │ 25.1   │ 81.4    │ 81.5    │ 97.8    │ 9.92   │ ok         │
#> │ oxygen_fluxes          │ OXY_oxy_atm   │ Atm O2 flux (mmol/m2/d)   │ -13.6  │ 6.84    │ 20.5    │  606    │ 53.8   │OUT OF RANGE│
#> │ oxygen_fluxes          │ OXY_oxy_atmv  │ Atm O2 flux (vol)         │ -1.66  │ 0.977   │ 2.57    │ 79.2    │ 6.95   │ ok         │
#> │ oxygen_fluxes          │ OXY_oxy_dsf   │ SWI O2 flux (mmol/m2/d)   │ -22.5  │ -14.4   │ -14.9   │ -8.73   │ 2.39   │ ok         │
#> │ oxygen_fluxes          │ OXY_oxy_dsfv  │ SOD (vol)                 │ -2.58  │ -0.823  │ -0.865  │ -0.0338 │ 0.565  │ ok         │
#> │ nitrogen_state         │ NIT_amm       │ NH4 (mmol N/m3)           │1.19e-05│ 0.00303 │ 0.0202  │ 0.0983  │ 0.0274 │ ok         │
#> │ nitrogen_state         │ NIT_n2o       │ N2O (mmol N/m3)           │ 0.0109 │ 0.0473  │ 0.217   │ 0.943   │ 0.285  │ ok         │
#> │ nitrogen_state         │ NIT_nit       │ NO3 (mmol N/m3)           │ 0.00772│ 1.18    │ 1.32    │ 2.63    │ 0.692  │ ok         │
#> │ nitrogen_state         │ NIT_no2       │ NO2 (mmol N/m3)           │    0   │ 0.02    │ 0.0405  │ 0.141   │ 0.041  │ ok         │
#> │ nitrogen_organic       │ OGM_don       │ DON (mmol N/m3)           │ 0.00485│ 0.0865  │ 0.127   │ 0.282   │ 0.0753 │ ok         │
#> │ nitrogen_organic       │ OGM_donr      │ Refractory DON            │ 0.529  │ 1.32    │ 1.79    │ 8.95    │ 1.53   │ ok         │
#> │ nitrogen_organic       │ OGM_pon       │ PON (mmol N/m3)           │ 0.00273│ 0.592   │ 0.945   │ 2.58    │ 0.867  │ ok         │
#> │nitrogen_transformations│ NIT_anammox   │ Anammox                   │    0   │ 3.81e-05│ 0.000914│ 0.00425 │ 0.0013 │ ok         │
#> │nitrogen_transformations│ NIT_denit     │ Denitrification           │    0   │    0    │    0    │    0    │    0   │ ok         │
#> │nitrogen_transformations│ NIT_dnra      │ DNRA                      │    0   │    0    │    0    │    0    │    0   │ ok         │
#> │nitrogen_transformations│ NIT_n2oprod   │ N2O production            │    0   │ 0.00259 │ 0.00726 │ 0.0584  │ 0.0108 │ ok         │
#> │nitrogen_transformations│ NIT_nitrif    │ Nitrification             │    0   │ 0.16    │ 0.241   │ 3.47    │ 0.357  │ ok         │
#> │ nitrogen_sediment_flux │ NIT_amm_dsf   │ NH4 SWI flux              │ 0.138  │ 0.502   │ 0.686   │ 1.57    │ 0.512  │ ok         │
#> │ nitrogen_sediment_flux │ NIT_n2o_atm   │ N2O atm flux              │-0.00133│ 0.00106 │ 0.0139  │ 0.701   │ 0.0523 │ ok         │
#> │ nitrogen_sediment_flux │ NIT_n2o_dsf   │ N2O SWI flux              │    0   │    0    │    0    │    0    │    0   │ ok         │
#> │ nitrogen_sediment_flux │ NIT_nit_dsf   │ NO3 SWI flux              │ -0.0348│ -0.00083│ 0.00766 │ 0.0526  │ 0.0305 │ ok         │
#> │ nitrogen_sediment_flux │ NIT_no2_dsf   │ NO2 SWI flux              │    0   │    0    │    0    │    0    │    0   │ ok         │
#> │ phosphorus_state       │ OGM_dop       │ DOP                       │ 0.0003 │ 0.00117 │ 0.00165 │ 0.0033  │0.000932│ ok         │
#> │ phosphorus_state       │ OGM_dopr      │ Refractory DOP            │ 0.00883│ 0.022   │ 0.0298  │ 0.149   │ 0.0256 │ ok         │
#> │ phosphorus_state       │ OGM_pop       │ POP                       │0.000407│ 0.0161  │ 0.025   │ 0.0624  │ 0.0207 │ ok         │
#> │ phosphorus_state       │ PHS_frp       │ FRP (mmol P/m3)           │1.19e-05│ 0.0003  │ 0.00129 │ 0.00571 │ 0.00159│ ok         │
#> │ phosphorus_fluxes      │ OGM_dop_min   │ DOP mineralisation        │    0   │    0    │ 1.07e-07│ 2.14e-05│1.41e-06│ ok         │
#> │ phosphorus_fluxes      │ OGM_dop_swi   │ DOP SWI flux              │    0   │    0    │    0    │    0    │    0   │ ok         │
#> │ phosphorus_fluxes      │ OGM_pop_res   │ POP resuspension          │    0   │    0    │    0    │    0    │    0   │ ok         │
#> │ phosphorus_fluxes      │ OGM_pop_swi   │ POP SWI flux              │-0.00228│-0.000473│-0.000747│-1.09e-05│0.000615│ ok         │
#> │ phosphorus_fluxes      │ PHS_frp_dsf   │ FRP SWI flux              │0.000617│ 0.00239 │ 0.00677 │ 0.0247  │ 0.0077 │ ok         │
#> │ phyto_biomass          │ PHY_cyano     │ Cyanobacteria             │ 0.0301 │ 0.0309  │ 0.11    │ 0.947   │ 0.168  │ ok         │
#> │ phyto_biomass          │ PHY_diatom    │ Diatoms                   │ 0.03   │ 0.0303  │ 0.0946  │ 0.911   │ 0.157  │ ok         │
#> │ phyto_biomass          │ PHY_green     │ Greens                    │ 0.85   │ 11.7    │ 17.9    │ 49.8    │ 15.6   │ ok         │
#> │ phyto_biomass          │ PHY_tchla     │ Total chl-a (ug/L)        │ 0.572  │ 3.52    │ 5.42    │   15    │ 4.66   │ ok         │
#> │ phyto_biomass          │ PHY_tphy      │ Total phyto (mmol C/m3)   │ 2.02   │ 11.8    │ 18.1    │   50    │ 15.5   │ ok         │
#> │ phyto_stoichiometry    │ PHY_cyano_NtoP│ Cyano N:P                 │ 26.2   │ 50.6    │ 53.2    │ 99.4    │ 10.8   │ ok         │
#> │ phyto_stoichiometry    │PHY_diatom_NtoP│ Diatom N:P                │ 30.4   │ 55.9    │ 56.8    │ 83.1    │  9.2   │ ok         │
#> │ phyto_stoichiometry    │ PHY_green_NtoP│ Green N:P                 │ 29.1   │   47    │ 49.3    │  109    │   11   │OUT OF RANGE│
#> │ phyto_limitation       │ PHY_cyano_fI  │ Cyano fI                  │ 0.0863 │ 0.268   │ 0.253   │ 0.342   │ 0.061  │ ok         │
#> │ phyto_limitation       │ PHY_cyano_fNit│ Cyano fN                  │    0   │ 0.985   │ 0.976   │ 0.998   │ 0.0753 │ ok         │
#> │ phyto_limitation       │ PHY_cyano_fPho│ Cyano fP                  │    0   │ 0.223   │ 0.214   │ 0.625   │ 0.156  │ ok         │
#> │ phyto_limitation       │ PHY_cyano_fT  │ Cyano fT                  │ 0.56   │ 0.907   │ 0.863   │ 1.06    │ 0.146  │ ok         │
#> │ phyto_limitation       │ PHY_diatom_fI │ Diatom fI                 │ 0.152  │ 0.353   │ 0.333   │ 0.425   │ 0.0653 │ ok         │
#> │ phyto_limitation       │PHY_diatom_fNit│ Diatom fN                 │ 0.00806│ 0.972   │ 0.966   │ 0.999   │ 0.0595 │ ok         │
#> │ phyto_limitation       │PHY_diatom_fPho│ Diatom fP                 │    0   │ 0.124   │ 0.124   │ 0.576   │ 0.125  │ ok         │
#> │ phyto_limitation       │ PHY_diatom_fT │ Diatom fT                 │    1   │    1    │    1    │    1    │    0   │ ok         │
#> │ phyto_limitation       │ PHY_green_fI  │ Green fI                  │ 0.0863 │ 0.268   │ 0.253   │ 0.342   │ 0.061  │ ok         │
#> │ phyto_limitation       │ PHY_green_fNit│ Green fN                  │ 0.0142 │ 0.997   │ 0.992   │    1    │ 0.0583 │ ok         │
#> │ phyto_limitation       │ PHY_green_fPho│ Green fP                  │    0   │ 0.404   │ 0.37    │ 0.719   │ 0.17   │ ok         │
#> │ phyto_limitation       │ PHY_green_fT  │ Green fT                  │    1   │    1    │    1    │    1    │    0   │ ok         │
#> │ phyto_fluxes           │ PHY_gpp       │ GPP                       │    0   │ 0.629   │ 0.879   │ 3.74    │ 0.785  │ ok         │
#> │ phyto_fluxes           │ PHY_ncp       │ NCP                       │ -0.0659│  0.5    │ 0.758   │ 3.47    │ 0.732  │ ok         │
#> │ phyto_fluxes           │ PHY_set       │ Sedimentation             │ -1.82  │ -0.402  │ -0.594  │ -0.147  │ 0.449  │ ok         │
#> │ phyto_fluxes           │ PHY_upt_nh4   │ NH4 uptake                │ 0.00579│ 0.0253  │ 0.035   │ 0.146   │ 0.0311 │ ok         │
#> │ phyto_fluxes           │ PHY_upt_no3   │ NO3 uptake                │    0   │    0    │ 7.11e-09│ 2.38e-06│ 1.3e-07│ ok         │
#> │ phyto_fluxes           │ PHY_upt_po4   │ PO4 uptake                │    0   │ 0.000264│ 0.000499│ 0.0106  │0.000858│ ok         │
#> │ sedflux_oxygen_Z       │ OXY_oxy_atm_Z │ Atm O2 flux (per zone)    │    0   │    0    │    0    │    0    │    0   │ ok         │
#> │ sedflux_oxygen_Z       │ OXY_oxy_dsf_Z │ SWI O2 exchange (per zone)│ -34.7  │  -16    │ -15.2   │ -0.694  │ 6.66   │ ok         │
#> │ sedflux_oxygen_Z       │ SDF_Fsed_oxy_Z│ SDF O2 flux (per zone)    │ -38.8  │ -29.1   │ -29.1   │ -19.4   │ 9.71   │ ok         │
#> │ sedflux_nitrogen_Z     │ NIT_amm_dsf_Z │ NH4 SWI flux (per zone)   │ 0.0338 │ 0.248   │ 1.12    │ 5.13    │  1.6   │ ok         │
#> │ sedflux_nitrogen_Z     │ NIT_n2o_atm_Z │ N2O atm flux (per zone)   │    0   │    0    │    0    │    0    │    0   │ ok         │
#> │ sedflux_nitrogen_Z     │ NIT_n2o_dsf_Z │ N2O SWI flux (per zone)   │    0   │    0    │    0    │    0    │    0   │ ok         │
#> │ sedflux_nitrogen_Z     │ NIT_nit_dsf_Z │ NO3 SWI flux (per zone)   │ -0.306 │ 0.0127  │ -0.0316 │ 0.0791  │ 0.115  │ ok         │
#> │ sedflux_nitrogen_Z     │ NIT_no2_dsf_Z │ NO2 SWI flux (per zone)   │    0   │    0    │    0    │    0    │    0   │ ok         │
#> │ sedflux_nitrogen_Z     │ SDF_Fsed_amm_Z│ SDF NH4 flux (per zone)   │ 0.512  │ 3.17    │ 3.17    │ 5.83    │ 2.66   │ ok         │
#> │ sedflux_nitrogen_Z     │ SDF_Fsed_nit_Z│ SDF NO3 flux (per zone)   │ -0.4   │ -0.15   │ -0.15   │  0.1    │ 0.25   │ ok         │
#> │ sedflux_phosphorus_Z   │ OGM_doc_swi_Z │ DOC SWI flux (per zone)   │ 0.0143 │ 0.0267  │ 0.0359  │ 0.0927  │ 0.0233 │ ok         │
#> │ sedflux_phosphorus_Z   │ OGM_don_swi_Z │ DON SWI flux (per zone)   │    0   │    0    │    0    │    0    │    0   │ ok         │
#> │ sedflux_phosphorus_Z   │ OGM_dop_swi_Z │ DOP SWI flux (per zone)   │    0   │    0    │    0    │    0    │    0   │ ok         │
#> │ sedflux_phosphorus_Z   │ OGM_poc_swi_Z │ POC SWI flux (per zone)   │ -1.97  │ -0.00222│ -0.288  │    0    │ 0.477  │ ok         │
#> │ sedflux_phosphorus_Z   │ OGM_pon_swi_Z │ PON SWI flux (per zone)   │ -0.314 │-0.000136│ -0.0435 │    0    │ 0.0755 │ ok         │
#> │ sedflux_phosphorus_Z   │ OGM_pop_swi_Z │ POP SWI flux (per zone)   │-0.00779│-1.87e-05│ -0.00127│    0    │ 0.00195│ ok         │
#> │ sedflux_phosphorus_Z   │ PHS_frp_dsf_Z │ FRP SWI flux (per zone)   │0.000325│ 0.00116 │ 0.0111  │ 0.0824  │ 0.021  │ ok         │
#> │ sedflux_phosphorus_Z   │ SDF_Fsed_frp_Z│ SDF FRP flux (per zone)   │ 0.0259 │ 0.0647  │ 0.0647  │ 0.103   │ 0.0388 │ ok         │
#> │ sedflux_organic_Z      │ OGM_poc_res_Z │POC resuspension (per zone)│    0   │    0    │    0    │    0    │    0   │ ok         │
#> │ sedflux_organic_Z      │ OGM_pon_res_Z │PON resuspension (per zone)│    0   │    0    │    0    │    0    │    0   │ ok         │
#> │ sedflux_organic_Z      │ OGM_pop_res_Z │POP resuspension (per zone)│    0   │    0    │    0    │    0    │    0   │ ok         │
#> │ sedflux_organic_Z      │ OGM_toc_sed_Z │ TOC sed mass (per zone)   │    0   │    0    │    0    │    0    │    0   │ ok         │
#> │ sedflux_organic_Z      │ OGM_ton_sed_Z │ TON sed mass (per zone)   │    0   │    0    │    0    │    0    │    0   │ ok         │
#> │ sedflux_organic_Z      │ OGM_top_sed_Z │ TOP sed mass (per zone)   │    0   │    0    │    0    │    0    │    0   │ ok         │
#> │ sedflux_organic_Z      │PHY_phy_swi_c_Z│ Phyto SWI C (per zone)    │ -6.22  │ -0.154  │ -0.751  │    0    │ 1.36   │ ok         │
#> │ sedflux_organic_Z      │PHY_phy_swi_n_Z│ Phyto SWI N (per zone)    │ -0.435 │ -0.0108 │ -0.0523 │    0    │ 0.0952 │ ok         │
#> │ sedflux_organic_Z      │PHY_phy_swi_p_Z│ Phyto SWI P (per zone)    │-0.00931│-0.000461│ -0.00154│    0    │ 0.00202│ ok         │
#> │ sedflux_silica_Z       │ SIL_dsf_rsi_Z │ Si SWI flux (per zone)    │ 0.00233│ 0.00422 │ 0.0418  │ 0.594   │ 0.0977 │ ok         │
#> └────────────────────────┴───────────────┴───────────────────────────┴────────┴─────────┴─────────┴─────────┴────────┴────────────┘
```
