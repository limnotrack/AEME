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
#> Warning: ! `SIL_rsi`: SIL_rsi is constant across all rows — this may be a placeholder
#>   value.
#> ℹ Check raw data or unit conversion for this variable.
#> ℹ Using observed water level.
#> ! Missing values in observed water level.
#> ℹ Correcting water balance using estimated outflows (method = 2).
#> ℹ Building GLM-AED for lake wainamu
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
#> ℹ Running models... (Have you tried parallelizing?) [2026-05-25 03:53:41]
#> → GLM-AED running... [2026-05-25 03:53:41]
#> ✔ GLM-AED run successful! [2026-05-25 03:53:43]
#> ✔ Model run complete! [2026-05-25 03:53:43]
out <- run_glm_aed_diagnostics(aeme = aeme)
#> Requesting 86 variables from model output...  (sediment zone _Z variables are
#> optional; missing ones are skipped)
#> ── GLM-AED diagnostic summary ──────────────────────────────────────────────────
#> ! 2 flagged variables
#> ┌───────────────────┬──────────────┬───────────────────────┬─────┬──────┬────┬────┬────┬────────────┐
#> │ group             │ variable     │ label                 │ min │median│mean│ max│ sd │ flag       │
#> ├───────────────────┼──────────────┼───────────────────────┼─────┼──────┼────┼────┼────┼────────────┤
#> │ oxygen_fluxes     │ OXY_oxy_atm  │Atm O2 flux (mmol/m2/d)│-13.6│ 6.82 │20.4│ 606│53.8│OUT OF RANGE│
#> │phyto_stoichiometry│PHY_green_NtoP│ Green N:P             │ 27.2│   46 │  47│ 109│10.1│OUT OF RANGE│
#> └───────────────────┴──────────────┴───────────────────────┴─────┴──────┴────┴────┴────┴────────────┘
#> ── Full summary ────────────────────────────────────────────────────────────────
#> ┌────────────────────────┬───────────────┬───────────────────────────┬────────┬────────┬────────┬─────────┬────────┬────────────┐
#> │ group                  │ variable      │ label                     │ min    │ median │ mean   │ max     │ sd     │ flag       │
#> ├────────────────────────┼───────────────┼───────────────────────────┼────────┼────────┼────────┼─────────┼────────┼────────────┤
#> │ oxygen_state           │ OXY_oxy       │ O2 (mmol/m3)              │   92   │  242   │  247   │  323    │   46   │ ok         │
#> │ oxygen_state           │ OXY_sat       │ O2 saturation (%)         │ 25.1   │ 76.1   │ 77.8   │ 97.8    │  9.4   │ ok         │
#> │ oxygen_fluxes          │ OXY_oxy_atm   │ Atm O2 flux (mmol/m2/d)   │ -13.6  │ 6.82   │ 20.4   │  606    │ 53.8   │OUT OF RANGE│
#> │ oxygen_fluxes          │ OXY_oxy_atmv  │ Atm O2 flux (vol)         │ -1.66  │ 0.536  │ 2.15   │ 79.2    │ 6.77   │ ok         │
#> │ oxygen_fluxes          │ OXY_oxy_dsf   │ SWI O2 flux (mmol/m2/d)   │ -22.3  │ -14.4  │ -14.9  │ -8.65   │ 2.35   │ ok         │
#> │ oxygen_fluxes          │ OXY_oxy_dsfv  │ SOD (vol)                 │ -3.23  │ -1.95  │ -1.99  │ -1.17   │ 0.376  │ ok         │
#> │ nitrogen_state         │ NIT_amm       │ NH4 (mmol N/m3)           │1.19e-05│ 0.00336│ 0.021  │ 0.0995  │ 0.0285 │ ok         │
#> │ nitrogen_state         │ NIT_n2o       │ N2O (mmol N/m3)           │ 0.0109 │ 0.0455 │ 0.216  │ 0.939   │ 0.284  │ ok         │
#> │ nitrogen_state         │ NIT_nit       │ NO3 (mmol N/m3)           │ 0.00772│ 1.19   │ 1.32   │ 2.63    │ 0.692  │ ok         │
#> │ nitrogen_state         │ NIT_no2       │ NO2 (mmol N/m3)           │    0   │ 0.0198 │ 0.0402 │ 0.139   │ 0.0406 │ ok         │
#> │ nitrogen_organic       │ OGM_don       │ DON (mmol N/m3)           │ 0.00484│ 0.0865 │ 0.127  │ 0.283   │ 0.0755 │ ok         │
#> │ nitrogen_organic       │ OGM_donr      │ Refractory DON            │ 0.528  │ 1.32   │ 1.79   │ 8.95    │ 1.53   │ ok         │
#> │ nitrogen_organic       │ OGM_pon       │ PON (mmol N/m3)           │ 0.00273│ 0.592  │ 0.947  │ 2.58    │ 0.869  │ ok         │
#> │nitrogen_transformations│ NIT_anammox   │ Anammox                   │    0   │5.48e-05│0.000981│ 0.00493 │ 0.00137│ ok         │
#> │nitrogen_transformations│ NIT_denit     │ Denitrification           │    0   │    0   │    0   │    0    │    0   │ ok         │
#> │nitrogen_transformations│ NIT_dnra      │ DNRA                      │    0   │    0   │    0   │    0    │    0   │ ok         │
#> │nitrogen_transformations│ NIT_n2oprod   │ N2O production            │    0   │ 0.00217│ 0.00723│ 0.0589  │ 0.0109 │ ok         │
#> │nitrogen_transformations│ NIT_nitrif    │ Nitrification             │    0   │ 0.155  │ 0.204  │ 0.921   │ 0.186  │ ok         │
#> │ nitrogen_sediment_flux │ NIT_amm_dsf   │ NH4 SWI flux              │ 0.135  │ 0.497  │ 0.67   │ 1.52    │ 0.498  │ ok         │
#> │ nitrogen_sediment_flux │ NIT_n2o_atm   │ N2O atm flux              │-0.00132│ 0.00105│ 0.0139 │ 0.698   │ 0.0524 │ ok         │
#> │ nitrogen_sediment_flux │ NIT_n2o_dsf   │ N2O SWI flux              │    0   │    0   │    0   │    0    │    0   │ ok         │
#> │ nitrogen_sediment_flux │ NIT_nit_dsf   │ NO3 SWI flux              │ -0.0324│ 0.0018 │ 0.00977│ 0.0542  │ 0.0298 │ ok         │
#> │ nitrogen_sediment_flux │ NIT_no2_dsf   │ NO2 SWI flux              │    0   │    0   │    0   │    0    │    0   │ ok         │
#> │ phosphorus_state       │ OGM_dop       │ DOP                       │ 0.0003 │ 0.00117│ 0.00166│ 0.00331 │0.000936│ ok         │
#> │ phosphorus_state       │ OGM_dopr      │ Refractory DOP            │ 0.00883│ 0.0219 │ 0.0298 │ 0.149   │ 0.0256 │ ok         │
#> │ phosphorus_state       │ OGM_pop       │ POP                       │0.000407│ 0.0161 │ 0.025  │ 0.0626  │ 0.0208 │ ok         │
#> │ phosphorus_state       │ PHS_frp       │ FRP (mmol P/m3)           │1.19e-05│ 0.00031│ 0.00132│ 0.0059  │ 0.00163│ ok         │
#> │ phosphorus_fluxes      │ OGM_dop_min   │ DOP mineralisation        │    0   │    0   │7.11e-08│ 1.67e-05│ 9.9e-07│ ok         │
#> │ phosphorus_fluxes      │ OGM_dop_swi   │ DOP SWI flux              │    0   │    0   │    0   │    0    │    0   │ ok         │
#> │ phosphorus_fluxes      │ OGM_pop_res   │ POP resuspension          │    0   │    0   │    0   │    0    │    0   │ ok         │
#> │ phosphorus_fluxes      │ OGM_pop_swi   │ POP SWI flux              │ -0.0173│-0.00384│-0.00634│-0.000105│ 0.0054 │ ok         │
#> │ phosphorus_fluxes      │ PHS_frp_dsf   │ FRP SWI flux              │0.000606│ 0.00239│ 0.00664│ 0.0243  │ 0.00753│ ok         │
#> │ phyto_biomass          │ PHY_cyano     │ Cyanobacteria             │ 0.0301 │ 0.0309 │ 0.11   │ 0.947   │ 0.168  │ ok         │
#> │ phyto_biomass          │ PHY_diatom    │ Diatoms                   │ 0.0301 │ 0.0303 │ 0.0945 │ 0.911   │ 0.156  │ ok         │
#> │ phyto_biomass          │ PHY_green     │ Greens                    │ 0.85   │ 11.7   │ 17.9   │ 50.1    │ 15.7   │ ok         │
#> │ phyto_biomass          │ PHY_tchla     │ Total chl-a (ug/L)        │ 0.535  │ 3.35   │ 5.06   │   15    │ 4.22   │ ok         │
#> │ phyto_biomass          │ PHY_tphy      │ Total phyto (mmol C/m3)   │ 1.89   │ 11.2   │ 16.9   │   50    │ 14.1   │ ok         │
#> │ phyto_stoichiometry    │ PHY_cyano_NtoP│ Cyano N:P                 │ 26.2   │ 49.3   │ 50.6   │ 99.5    │  9.6   │ ok         │
#> │ phyto_stoichiometry    │PHY_diatom_NtoP│ Diatom N:P                │   30   │   55   │ 54.2   │ 82.8    │ 8.18   │ ok         │
#> │ phyto_stoichiometry    │ PHY_green_NtoP│ Green N:P                 │ 27.2   │   46   │   47   │  109    │ 10.1   │OUT OF RANGE│
#> │ phyto_limitation       │ PHY_cyano_fI  │ Cyano fI                  │ 0.0188 │ 0.251  │ 0.237  │ 0.342   │ 0.0633 │ ok         │
#> │ phyto_limitation       │ PHY_cyano_fNit│ Cyano fN                  │    0   │ 0.965  │ 0.937  │ 0.998   │ 0.0915 │ ok         │
#> │ phyto_limitation       │ PHY_cyano_fPho│ Cyano fP                  │    0   │ 0.225  │ 0.211  │ 0.614   │ 0.155  │ ok         │
#> │ phyto_limitation       │ PHY_cyano_fT  │ Cyano fT                  │ 0.411  │ 0.854  │ 0.828  │ 1.06    │ 0.156  │ ok         │
#> │ phyto_limitation       │ PHY_diatom_fI │ Diatom fI                 │ 0.044  │ 0.328  │ 0.314  │ 0.424   │ 0.0682 │ ok         │
#> │ phyto_limitation       │PHY_diatom_fNit│ Diatom fN                 │ 0.00811│ 0.947  │ 0.928  │ 0.998   │ 0.0776 │ ok         │
#> │ phyto_limitation       │PHY_diatom_fPho│ Diatom fP                 │    0   │ 0.12   │ 0.126  │ 0.585   │ 0.124  │ ok         │
#> │ phyto_limitation       │ PHY_diatom_fT │ Diatom fT                 │ 0.613  │    1   │ 0.96   │    1    │ 0.0582 │ ok         │
#> │ phyto_limitation       │ PHY_green_fI  │ Green fI                  │ 0.0188 │ 0.251  │ 0.237  │ 0.342   │ 0.0633 │ ok         │
#> │ phyto_limitation       │ PHY_green_fNit│ Green fN                  │ 0.0142 │ 0.992  │ 0.952  │    1    │ 0.0797 │ ok         │
#> │ phyto_limitation       │ PHY_green_fPho│ Green fP                  │    0   │ 0.403  │ 0.362  │ 0.726   │ 0.173  │ ok         │
#> │ phyto_limitation       │ PHY_green_fT  │ Green fT                  │ 0.613  │    1   │ 0.96   │    1    │ 0.0582 │ ok         │
#> │ phyto_fluxes           │ PHY_gpp       │ GPP                       │    0   │ 0.588  │ 0.852  │ 3.94    │ 0.792  │ ok         │
#> │ phyto_fluxes           │ PHY_ncp       │ NCP                       │ -0.0659│ 0.478  │ 0.737  │ 3.74    │ 0.743  │ ok         │
#> │ phyto_fluxes           │ PHY_set       │ Sedimentation             │ -1.81  │ -0.383 │ -0.568 │ -0.147  │ 0.433  │ ok         │
#> │ phyto_fluxes           │ PHY_upt_nh4   │ NH4 uptake                │ 0.00552│ 0.0231 │ 0.0336 │ 0.145   │ 0.0308 │ ok         │
#> │ phyto_fluxes           │ PHY_upt_no3   │ NO3 uptake                │    0   │    0   │7.11e-09│ 2.38e-06│ 1.3e-07│ ok         │
#> │ phyto_fluxes           │ PHY_upt_po4   │ PO4 uptake                │    0   │0.000262│0.000391│ 0.00635 │0.000576│ ok         │
#> │ sedflux_oxygen_Z       │ OXY_oxy_atm_Z │ Atm O2 flux (per zone)    │    0   │    0   │    0   │    0    │    0   │ ok         │
#> │ sedflux_oxygen_Z       │ OXY_oxy_dsf_Z │ SWI O2 exchange (per zone)│ -34.7  │  -16   │ -15.2  │ -0.285  │ 6.68   │ ok         │
#> │ sedflux_oxygen_Z       │ SDF_Fsed_oxy_Z│ SDF O2 flux (per zone)    │ -38.8  │ -29.1  │ -29.1  │ -19.4   │ 9.71   │ ok         │
#> │ sedflux_nitrogen_Z     │ NIT_amm_dsf_Z │ NH4 SWI flux (per zone)   │ 0.0338 │ 0.248  │ 1.12   │ 5.13    │  1.6   │ ok         │
#> │ sedflux_nitrogen_Z     │ NIT_n2o_atm_Z │ N2O atm flux (per zone)   │    0   │    0   │    0   │    0    │    0   │ ok         │
#> │ sedflux_nitrogen_Z     │ NIT_n2o_dsf_Z │ N2O SWI flux (per zone)   │    0   │    0   │    0   │    0    │    0   │ ok         │
#> │ sedflux_nitrogen_Z     │ NIT_nit_dsf_Z │ NO3 SWI flux (per zone)   │ -0.306 │ 0.0135 │ -0.0313│ 0.0791  │ 0.115  │ ok         │
#> │ sedflux_nitrogen_Z     │ NIT_no2_dsf_Z │ NO2 SWI flux (per zone)   │    0   │    0   │    0   │    0    │    0   │ ok         │
#> │ sedflux_nitrogen_Z     │ SDF_Fsed_amm_Z│ SDF NH4 flux (per zone)   │ 0.512  │ 3.17   │ 3.17   │ 5.83    │ 2.66   │ ok         │
#> │ sedflux_nitrogen_Z     │ SDF_Fsed_nit_Z│ SDF NO3 flux (per zone)   │ -0.4   │ -0.15  │ -0.15  │  0.1    │ 0.25   │ ok         │
#> │ sedflux_phosphorus_Z   │ OGM_doc_swi_Z │ DOC SWI flux (per zone)   │ 0.0143 │ 0.0266 │ 0.036  │ 0.0927  │ 0.0234 │ ok         │
#> │ sedflux_phosphorus_Z   │ OGM_don_swi_Z │ DON SWI flux (per zone)   │    0   │    0   │    0   │    0    │    0   │ ok         │
#> │ sedflux_phosphorus_Z   │ OGM_dop_swi_Z │ DOP SWI flux (per zone)   │    0   │    0   │    0   │    0    │    0   │ ok         │
#> │ sedflux_phosphorus_Z   │ OGM_poc_swi_Z │ POC SWI flux (per zone)   │ -5.96  │ -0.628 │ -1.28  │ -0.00444│ 1.45   │ ok         │
#> │ sedflux_phosphorus_Z   │ OGM_pon_swi_Z │ PON SWI flux (per zone)   │ -0.951 │ -0.0906│ -0.194 │-0.000271│ 0.233  │ ok         │
#> │ sedflux_phosphorus_Z   │ OGM_pop_swi_Z │ POP SWI flux (per zone)   │ -0.0218│-0.00283│-0.00519│-3.74e-05│ 0.00564│ ok         │
#> │ sedflux_phosphorus_Z   │ PHS_frp_dsf_Z │ FRP SWI flux (per zone)   │0.000325│ 0.00116│ 0.0112 │ 0.0866  │ 0.0212 │ ok         │
#> │ sedflux_phosphorus_Z   │ SDF_Fsed_frp_Z│ SDF FRP flux (per zone)   │ 0.0259 │ 0.0647 │ 0.0647 │ 0.103   │ 0.0388 │ ok         │
#> │ sedflux_organic_Z      │ OGM_poc_res_Z │POC resuspension (per zone)│    0   │    0   │    0   │    0    │    0   │ ok         │
#> │ sedflux_organic_Z      │ OGM_pon_res_Z │PON resuspension (per zone)│    0   │    0   │    0   │    0    │    0   │ ok         │
#> │ sedflux_organic_Z      │ OGM_pop_res_Z │POP resuspension (per zone)│    0   │    0   │    0   │    0    │    0   │ ok         │
#> │ sedflux_organic_Z      │ OGM_toc_sed_Z │ TOC sed mass (per zone)   │    0   │    0   │    0   │    0    │    0   │ ok         │
#> │ sedflux_organic_Z      │ OGM_ton_sed_Z │ TON sed mass (per zone)   │    0   │    0   │    0   │    0    │    0   │ ok         │
#> │ sedflux_organic_Z      │ OGM_top_sed_Z │ TOP sed mass (per zone)   │    0   │    0   │    0   │    0    │    0   │ ok         │
#> │ sedflux_organic_Z      │PHY_phy_swi_c_Z│ Phyto SWI C (per zone)    │ -18.5  │ -2.41  │ -3.86  │ -0.307  │ 4.27   │ ok         │
#> │ sedflux_organic_Z      │PHY_phy_swi_n_Z│ Phyto SWI N (per zone)    │ -1.28  │ -0.166 │ -0.268 │ -0.0215 │ 0.297  │ ok         │
#> │ sedflux_organic_Z      │PHY_phy_swi_p_Z│ Phyto SWI P (per zone)    │ -0.0219│-0.00394│-0.00588│-0.000999│ 0.00506│ ok         │
#> │ sedflux_silica_Z       │ SIL_dsf_rsi_Z │ Si SWI flux (per zone)    │ 0.00233│ 0.00421│ 0.0435 │ 0.755   │ 0.104  │ ok         │
#> └────────────────────────┴───────────────┴───────────────────────────┴────────┴────────┴────────┴─────────┴────────┴────────────┘




```
