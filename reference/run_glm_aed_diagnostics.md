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
#> ℹ Detected regular timestep: 1 day(s).
#> ℹ Detected regular timestep: 1 day(s).
#> Warning: ! `SIL_rsi`: SIL_rsi is constant across all rows — this may be a placeholder
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
#> ✔ Updated GLM-AED models from: aed_sedflux, aed_oxygen, aed_silica,
#>   aed_nitrogen, aed_phosphorus, aed_organic_matter, aed_phytoplankton,
#>   aed_zooplankton, aed_macrophyte, aed_totals to: aed_sedflux, aed_oxygen,
#>   aed_silica, aed_nitrogen, aed_phosphorus, aed_organic_matter,
#>   aed_phytoplankton, aed_totals
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
#> ℹ Running models... (Have you tried parallelizing?) [2026-06-08 23:43:10]
#> → GLM-AED running... [2026-06-08 23:43:10]
#> ✔ GLM-AED run successful! [2026-06-08 23:43:12]
#> ✔ Model run complete! [2026-06-08 23:43:12]
out <- run_glm_aed_diagnostics(aeme = aeme)
#> Requesting 86 variables from model output...  (sediment zone _Z variables are
#> optional; missing ones are skipped)
#> ── GLM-AED diagnostic summary ──────────────────────────────────────────────────
#> ! 2 flagged variables
#> ┌───────────────────┬──────────────┬───────────────────────┬─────┬──────┬────┬────┬────┬────────────┐
#> │ group             │ variable     │ label                 │ min │median│mean│ max│ sd │ flag       │
#> ├───────────────────┼──────────────┼───────────────────────┼─────┼──────┼────┼────┼────┼────────────┤
#> │ oxygen_fluxes     │ OXY_oxy_atm  │Atm O2 flux (mmol/m2/d)│-13.6│ 7.07 │20.5│ 606│53.8│OUT OF RANGE│
#> │phyto_stoichiometry│PHY_green_NtoP│ Green N:P             │ 27.9│ 45.7 │  47│ 109│  10│OUT OF RANGE│
#> └───────────────────┴──────────────┴───────────────────────┴─────┴──────┴────┴────┴────┴────────────┘
#> ── Full summary ────────────────────────────────────────────────────────────────
#> ┌────────────────────────┬───────────────┬───────────────────────────┬────────┬────────┬────────┬─────────┬────────┬────────────┐
#> │ group                  │ variable      │ label                     │ min    │ median │ mean   │ max     │ sd     │ flag       │
#> ├────────────────────────┼───────────────┼───────────────────────────┼────────┼────────┼────────┼─────────┼────────┼────────────┤
#> │ oxygen_state           │ OXY_oxy       │ O2 (mmol/m3)              │ 92.1   │  242   │  247   │  323    │ 45.9   │ ok         │
#> │ oxygen_state           │ OXY_sat       │ O2 saturation (%)         │ 25.1   │ 76.5   │ 77.7   │ 97.7    │ 9.17   │ ok         │
#> │ oxygen_fluxes          │ OXY_oxy_atm   │ Atm O2 flux (mmol/m2/d)   │ -13.6  │ 7.07   │ 20.5   │  606    │ 53.8   │OUT OF RANGE│
#> │ oxygen_fluxes          │ OXY_oxy_atmv  │ Atm O2 flux (vol)         │ -1.66  │ 0.474  │ 2.13   │ 79.2    │ 6.82   │ ok         │
#> │ oxygen_fluxes          │ OXY_oxy_dsf   │ SWI O2 flux (mmol/m2/d)   │ -22.2  │ -14.4  │ -14.9  │ -8.65   │ 2.35   │ ok         │
#> │ oxygen_fluxes          │ OXY_oxy_dsfv  │ SOD (vol)                 │ -3.23  │ -1.95  │ -1.99  │ -1.18   │ 0.37   │ ok         │
#> │ nitrogen_state         │ NIT_amm       │ NH4 (mmol N/m3)           │1.19e-05│ 0.00303│ 0.0202 │ 0.0983  │ 0.0274 │ ok         │
#> │ nitrogen_state         │ NIT_n2o       │ N2O (mmol N/m3)           │ 0.0109 │ 0.0473 │ 0.216  │ 0.943   │ 0.285  │ ok         │
#> │ nitrogen_state         │ NIT_nit       │ NO3 (mmol N/m3)           │ 0.00772│ 1.19   │ 1.32   │ 2.62    │ 0.689  │ ok         │
#> │ nitrogen_state         │ NIT_no2       │ NO2 (mmol N/m3)           │    0   │ 0.0196 │ 0.0401 │ 0.141   │ 0.0413 │ ok         │
#> │ nitrogen_organic       │ OGM_don       │ DON (mmol N/m3)           │ 0.00485│ 0.0865 │ 0.127  │ 0.283   │ 0.0754 │ ok         │
#> │ nitrogen_organic       │ OGM_donr      │ Refractory DON            │ 0.528  │ 1.32   │ 1.79   │ 8.95    │ 1.53   │ ok         │
#> │ nitrogen_organic       │ OGM_pon       │ PON (mmol N/m3)           │ 0.00273│ 0.592  │ 0.945  │ 2.58    │ 0.867  │ ok         │
#> │nitrogen_transformations│ NIT_anammox   │ Anammox                   │    0   │ 5e-05  │0.000957│ 0.00497 │ 0.00135│ ok         │
#> │nitrogen_transformations│ NIT_denit     │ Denitrification           │    0   │    0   │    0   │    0    │    0   │ ok         │
#> │nitrogen_transformations│ NIT_dnra      │ DNRA                      │    0   │    0   │    0   │    0    │    0   │ ok         │
#> │nitrogen_transformations│ NIT_n2oprod   │ N2O production            │    0   │ 0.00228│ 0.00721│ 0.0588  │ 0.0108 │ ok         │
#> │nitrogen_transformations│ NIT_nitrif    │ Nitrification             │    0   │ 0.155  │ 0.203  │ 0.919   │ 0.187  │ ok         │
#> │ nitrogen_sediment_flux │ NIT_amm_dsf   │ NH4 SWI flux              │ 0.135  │ 0.485  │ 0.667  │ 1.52    │ 0.497  │ ok         │
#> │ nitrogen_sediment_flux │ NIT_n2o_atm   │ N2O atm flux              │-0.00133│ 0.00108│ 0.0139 │ 0.702   │ 0.0523 │ ok         │
#> │ nitrogen_sediment_flux │ NIT_n2o_dsf   │ N2O SWI flux              │    0   │    0   │    0   │    0    │    0   │ ok         │
#> │ nitrogen_sediment_flux │ NIT_nit_dsf   │ NO3 SWI flux              │ -0.0323│0.000856│ 0.00961│ 0.0538  │ 0.0297 │ ok         │
#> │ nitrogen_sediment_flux │ NIT_no2_dsf   │ NO2 SWI flux              │    0   │    0   │    0   │    0    │    0   │ ok         │
#> │ phosphorus_state       │ OGM_dop       │ DOP                       │ 0.0003 │ 0.00117│ 0.00165│ 0.0033  │0.000934│ ok         │
#> │ phosphorus_state       │ OGM_dopr      │ Refractory DOP            │ 0.00883│ 0.022  │ 0.0298 │ 0.149   │ 0.0256 │ ok         │
#> │ phosphorus_state       │ OGM_pop       │ POP                       │0.000407│ 0.0161 │ 0.025  │ 0.0624  │ 0.0207 │ ok         │
#> │ phosphorus_state       │ PHS_frp       │ FRP (mmol P/m3)           │1.19e-05│ 0.0003 │ 0.00129│ 0.00571 │ 0.00159│ ok         │
#> │ phosphorus_fluxes      │ OGM_dop_min   │ DOP mineralisation        │    0   │    0   │7.82e-08│ 1.67e-05│1.05e-06│ ok         │
#> │ phosphorus_fluxes      │ OGM_dop_swi   │ DOP SWI flux              │    0   │    0   │    0   │    0    │    0   │ ok         │
#> │ phosphorus_fluxes      │ OGM_pop_res   │ POP resuspension          │    0   │    0   │    0   │    0    │    0   │ ok         │
#> │ phosphorus_fluxes      │ OGM_pop_swi   │ POP SWI flux              │ -0.0186│-0.00392│-0.00631│-0.000105│ 0.00535│ ok         │
#> │ phosphorus_fluxes      │ PHS_frp_dsf   │ FRP SWI flux              │0.000606│ 0.00232│ 0.00658│ 0.0237  │ 0.00746│ ok         │
#> │ phyto_biomass          │ PHY_cyano     │ Cyanobacteria             │ 0.0301 │ 0.0309 │ 0.11   │ 0.947   │ 0.168  │ ok         │
#> │ phyto_biomass          │ PHY_diatom    │ Diatoms                   │ 0.0301 │ 0.0303 │ 0.0945 │ 0.911   │ 0.156  │ ok         │
#> │ phyto_biomass          │ PHY_green     │ Greens                    │ 0.85   │ 11.7   │ 17.9   │ 49.8    │ 15.7   │ ok         │
#> │ phyto_biomass          │ PHY_tchla     │ Total chl-a (ug/L)        │ 0.524  │ 3.46   │ 5.05   │ 14.9    │ 4.19   │ ok         │
#> │ phyto_biomass          │ PHY_tphy      │ Total phyto (mmol C/m3)   │ 1.85   │ 11.5   │ 16.8   │ 49.8    │   14   │ ok         │
#> │ phyto_stoichiometry    │ PHY_cyano_NtoP│ Cyano N:P                 │ 26.2   │ 49.4   │ 50.6   │ 99.5    │  9.5   │ ok         │
#> │ phyto_stoichiometry    │PHY_diatom_NtoP│ Diatom N:P                │ 29.9   │ 54.9   │ 54.1   │ 82.8    │ 8.25   │ ok         │
#> │ phyto_stoichiometry    │ PHY_green_NtoP│ Green N:P                 │ 27.9   │ 45.7   │   47   │  109    │   10   │OUT OF RANGE│
#> │ phyto_limitation       │ PHY_cyano_fI  │ Cyano fI                  │ 0.011  │ 0.248  │ 0.237  │ 0.342   │ 0.0626 │ ok         │
#> │ phyto_limitation       │ PHY_cyano_fNit│ Cyano fN                  │    0   │ 0.961  │ 0.936  │ 0.998   │ 0.0909 │ ok         │
#> │ phyto_limitation       │ PHY_cyano_fPho│ Cyano fP                  │    0   │ 0.223  │ 0.21   │ 0.615   │ 0.154  │ ok         │
#> │ phyto_limitation       │ PHY_cyano_fT  │ Cyano fT                  │ 0.462  │ 0.866  │ 0.828  │ 1.06    │ 0.157  │ ok         │
#> │ phyto_limitation       │ PHY_diatom_fI │ Diatom fI                 │ 0.0263 │ 0.325  │ 0.314  │ 0.425   │ 0.0677 │ ok         │
#> │ phyto_limitation       │PHY_diatom_fNit│ Diatom fN                 │ 0.00811│ 0.948  │ 0.927  │ 0.998   │ 0.0766 │ ok         │
#> │ phyto_limitation       │PHY_diatom_fPho│ Diatom fP                 │    0   │ 0.124  │ 0.125  │ 0.586   │ 0.121  │ ok         │
#> │ phyto_limitation       │ PHY_diatom_fT │ Diatom fT                 │ 0.664  │    1   │ 0.96   │    1    │ 0.0572 │ ok         │
#> │ phyto_limitation       │ PHY_green_fI  │ Green fI                  │ 0.011  │ 0.248  │ 0.237  │ 0.342   │ 0.0626 │ ok         │
#> │ phyto_limitation       │ PHY_green_fNit│ Green fN                  │ 0.0142 │ 0.992  │ 0.952  │    1    │ 0.079  │ ok         │
#> │ phyto_limitation       │ PHY_green_fPho│ Green fP                  │    0   │ 0.399  │ 0.362  │ 0.727   │ 0.172  │ ok         │
#> │ phyto_limitation       │ PHY_green_fT  │ Green fT                  │ 0.664  │    1   │ 0.96   │    1    │ 0.0572 │ ok         │
#> │ phyto_fluxes           │ PHY_gpp       │ GPP                       │    0   │ 0.571  │ 0.849  │ 4.24    │ 0.786  │ ok         │
#> │ phyto_fluxes           │ PHY_ncp       │ NCP                       │ -0.0659│ 0.459  │ 0.735  │    4    │ 0.737  │ ok         │
#> │ phyto_fluxes           │ PHY_set       │ Sedimentation             │ -1.71  │ -0.376 │ -0.563 │ -0.147  │ 0.425  │ ok         │
#> │ phyto_fluxes           │ PHY_upt_nh4   │ NH4 uptake                │ 0.00536│ 0.0226 │ 0.0335 │ 0.16    │ 0.0306 │ ok         │
#> │ phyto_fluxes           │ PHY_upt_no3   │ NO3 uptake                │    0   │    0   │7.11e-09│ 2.38e-06│ 1.3e-07│ ok         │
#> │ phyto_fluxes           │ PHY_upt_po4   │ PO4 uptake                │    0   │ 0.00026│0.000389│ 0.00614 │0.000568│ ok         │
#> │ sedflux_oxygen_Z       │ OXY_oxy_atm_Z │ Atm O2 flux (per zone)    │    0   │    0   │    0   │    0    │    0   │ ok         │
#> │ sedflux_oxygen_Z       │ OXY_oxy_dsf_Z │ SWI O2 exchange (per zone)│ -34.7  │  -16   │ -15.2  │ -0.694  │ 6.66   │ ok         │
#> │ sedflux_oxygen_Z       │ SDF_Fsed_oxy_Z│ SDF O2 flux (per zone)    │ -38.8  │ -29.1  │ -29.1  │ -19.4   │ 9.71   │ ok         │
#> │ sedflux_nitrogen_Z     │ NIT_amm_dsf_Z │ NH4 SWI flux (per zone)   │ 0.0338 │ 0.248  │ 1.12   │ 5.14    │  1.6   │ ok         │
#> │ sedflux_nitrogen_Z     │ NIT_n2o_atm_Z │ N2O atm flux (per zone)   │    0   │    0   │    0   │    0    │    0   │ ok         │
#> │ sedflux_nitrogen_Z     │ NIT_n2o_dsf_Z │ N2O SWI flux (per zone)   │    0   │    0   │    0   │    0    │    0   │ ok         │
#> │ sedflux_nitrogen_Z     │ NIT_nit_dsf_Z │ NO3 SWI flux (per zone)   │ -0.306 │ 0.0127 │ -0.0316│ 0.0791  │ 0.115  │ ok         │
#> │ sedflux_nitrogen_Z     │ NIT_no2_dsf_Z │ NO2 SWI flux (per zone)   │    0   │    0   │    0   │    0    │    0   │ ok         │
#> │ sedflux_nitrogen_Z     │ SDF_Fsed_amm_Z│ SDF NH4 flux (per zone)   │ 0.512  │ 3.17   │ 3.17   │ 5.83    │ 2.66   │ ok         │
#> │ sedflux_nitrogen_Z     │ SDF_Fsed_nit_Z│ SDF NO3 flux (per zone)   │ -0.4   │ -0.15  │ -0.15  │  0.1    │ 0.25   │ ok         │
#> │ sedflux_phosphorus_Z   │ OGM_doc_swi_Z │ DOC SWI flux (per zone)   │ 0.0143 │ 0.0267 │ 0.0359 │ 0.0928  │ 0.0233 │ ok         │
#> │ sedflux_phosphorus_Z   │ OGM_don_swi_Z │ DON SWI flux (per zone)   │    0   │    0   │    0   │    0    │    0   │ ok         │
#> │ sedflux_phosphorus_Z   │ OGM_dop_swi_Z │ DOP SWI flux (per zone)   │    0   │    0   │    0   │    0    │    0   │ ok         │
#> │ sedflux_phosphorus_Z   │ OGM_poc_swi_Z │ POC SWI flux (per zone)   │ -6.15  │ -0.642 │ -1.27  │ -0.00444│ 1.44   │ ok         │
#> │ sedflux_phosphorus_Z   │ OGM_pon_swi_Z │ PON SWI flux (per zone)   │ -0.985 │ -0.0925│ -0.193 │-0.000271│ 0.231  │ ok         │
#> │ sedflux_phosphorus_Z   │ OGM_pop_swi_Z │ POP SWI flux (per zone)   │ -0.0236│-0.00288│-0.00517│-3.74e-05│ 0.00559│ ok         │
#> │ sedflux_phosphorus_Z   │ PHS_frp_dsf_Z │ FRP SWI flux (per zone)   │0.000325│ 0.00116│ 0.0111 │ 0.0824  │ 0.021  │ ok         │
#> │ sedflux_phosphorus_Z   │ SDF_Fsed_frp_Z│ SDF FRP flux (per zone)   │ 0.0259 │ 0.0647 │ 0.0647 │ 0.103   │ 0.0388 │ ok         │
#> │ sedflux_organic_Z      │ OGM_poc_res_Z │POC resuspension (per zone)│    0   │    0   │    0   │    0    │    0   │ ok         │
#> │ sedflux_organic_Z      │ OGM_pon_res_Z │PON resuspension (per zone)│    0   │    0   │    0   │    0    │    0   │ ok         │
#> │ sedflux_organic_Z      │ OGM_pop_res_Z │POP resuspension (per zone)│    0   │    0   │    0   │    0    │    0   │ ok         │
#> │ sedflux_organic_Z      │ OGM_toc_sed_Z │ TOC sed mass (per zone)   │    0   │    0   │    0   │    0    │    0   │ ok         │
#> │ sedflux_organic_Z      │ OGM_ton_sed_Z │ TON sed mass (per zone)   │    0   │    0   │    0   │    0    │    0   │ ok         │
#> │ sedflux_organic_Z      │ OGM_top_sed_Z │ TOP sed mass (per zone)   │    0   │    0   │    0   │    0    │    0   │ ok         │
#> │ sedflux_organic_Z      │PHY_phy_swi_c_Z│ Phyto SWI C (per zone)    │ -18.1  │ -2.45  │ -3.83  │ -0.308  │ 4.21   │ ok         │
#> │ sedflux_organic_Z      │PHY_phy_swi_n_Z│ Phyto SWI N (per zone)    │ -1.26  │ -0.17  │ -0.266 │ -0.0215 │ 0.293  │ ok         │
#> │ sedflux_organic_Z      │PHY_phy_swi_p_Z│ Phyto SWI P (per zone)    │ -0.0228│-0.00402│-0.00584│-0.000991│ 0.00502│ ok         │
#> │ sedflux_silica_Z       │ SIL_dsf_rsi_Z │ Si SWI flux (per zone)    │ 0.00233│ 0.00423│ 0.0418 │ 0.593   │ 0.0978 │ ok         │
#> └────────────────────────┴───────────────┴───────────────────────────┴────────┴────────┴────────┴─────────┴────────┴────────────┘
```
