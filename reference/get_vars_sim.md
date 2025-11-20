# Get all variables to be simulated, including those that are derived from others.

Get all variables to be simulated, including those that are derived from
others.

## Usage

``` r
get_vars_sim(vars_sim, aeme, model_controls)
```

## Arguments

- vars_sim:

  vector of variable names to be simulated. If NULL, the variables are
  taken from model_controls where simulate == TRUE.

- aeme:

  aeme; object.

- model_controls:

  dataframe; of configuration loaded from "model_controls.csv".

## Value

vector of variable names

## Examples

``` r
data("model_controls", package = "AEME")
get_vars_sim(model_controls)
#>       var_aeme simulate inf_default initial_wc initial_sed conversion_aed
#> 1     HYD_flow    FALSE          NA         NA          NA     1.00000000
#> 2     HYD_temp     TRUE       15.00     11.000          NA     1.00000000
#> 3     HYD_dens    FALSE          NA         NA          NA     1.00000000
#> 4     HYD_dens     TRUE          NA         NA          NA             NA
#> 5    HYD_strat     TRUE          NA         NA          NA             NA
#> 6   HYD_thmcln     TRUE          NA         NA          NA             NA
#> 7   HYD_schstb     TRUE          NA         NA          NA             NA
#> 8   HYD_ctrbuy     TRUE          NA         NA          NA             NA
#> 9   HYD_epidep     TRUE          NA         NA          NA             NA
#> 10  HYD_hypdep     TRUE          NA         NA          NA             NA
#> 11     RAD_par    FALSE          NA         NA          NA     1.00000000
#> 12    RAD_extc    FALSE          NA         NA          NA     1.00000000
#> 13  RAD_secchi    FALSE          NA         NA          NA     1.00000000
#> 14    LKE_tlic     TRUE          NA         NA          NA             NA
#> 15    LKE_tlin     TRUE          NA         NA          NA             NA
#> 16    LKE_tlip     TRUE          NA         NA          NA             NA
#> 17   LKE_tlise     TRUE          NA         NA          NA             NA
#> 18    LKE_tli3     TRUE          NA         NA          NA             NA
#> 19    LKE_tli4     TRUE          NA         NA          NA             NA
#> 20    CHM_salt     TRUE        0.00      0.000       0e+00     1.00000000
#> 21     CHM_oxy     TRUE       10.00     10.000       1e+01     0.03200000
#> 22  CHM_oxysat    FALSE          NA         NA          NA             NA
#> 23  CHM_oxycln     TRUE          NA         NA          NA             NA
#> 24  CHM_oxyepi     TRUE          NA         NA          NA             NA
#> 25  CHM_oxymet     TRUE          NA         NA          NA             NA
#> 26  CHM_oxyhyp     TRUE          NA         NA          NA             NA
#> 27  CHM_oxymom     TRUE          NA         NA          NA             NA
#> 28  CHM_oxynal     TRUE          NA         NA          NA             NA
#> 29     PHS_frp     TRUE        0.00      0.010       1e+06     0.03097376
#> 30     PHS_dop     TRUE        0.00      0.010       1e+06     0.03097376
#> 31    PHS_dopr    FALSE        0.00         NA          NA     0.03097376
#> 32     PHS_pop     TRUE        0.00      0.010       1e-04     0.03097376
#> 33    PHS_popr    FALSE        0.00         NA          NA     0.03097376
#> 34     PHS_pip     TRUE        0.00      0.002       5e-03     0.03097376
#> 35      PHS_tp     TRUE          NA         NA          NA     0.03097376
#> 36     NIT_amm     TRUE        0.05      0.020       1e+06     0.01400670
#> 37     NIT_nit     TRUE        0.20      0.015       1e+06     0.01400670
#> 38     NIT_don     TRUE        0.00      0.300       1e+06     0.01400670
#> 39    NIT_donr    FALSE        0.00         NA          NA     0.01400670
#> 40     NIT_pon     TRUE        0.00      0.100       1e-03     0.01400670
#> 41    NIT_ponr    FALSE        0.00         NA          NA     0.01400670
#> 42     NIT_pin    FALSE          NA      0.010       1e-03     0.01400670
#> 43      NIT_tn     TRUE          NA         NA          NA     0.01400670
#> 44     CAR_dic    FALSE       10.00      2.000       1e+06     0.01201100
#> 45     CAR_doc     TRUE        0.00      0.500       1e+06     0.01201100
#> 46    CAR_docr    FALSE        0.00         NA       1e+06     0.01201100
#> 47     CAR_poc     TRUE        0.00      0.200       1e-01     0.01201100
#> 48    CAR_pocr    FALSE        0.00         NA          NA     0.01201100
#> 49      CAR_ph    FALSE        0.00         NA          NA     1.00000000
#> 50     CAR_ch4    FALSE          NA         NA          NA     1.00000000
#> 51     SIL_rsi     TRUE        0.00      1.000       1e+07     1.00000000
#> 52     BAC_bac    FALSE        0.00         NA          NA     1.00000000
#> 53   PHY_dinof    FALSE        0.10      1.000       0e+00     1.00000000
#> 54   PHY_cyano     TRUE        0.10      1.000       0e+00     1.00000000
#> 55   PHY_nodul    FALSE        0.10      1.000       0e+00     1.00000000
#> 56   PHY_green     TRUE        0.10      1.000       0e+00     1.00000000
#> 57   PHY_crypt    FALSE        0.10      1.000       0e+00     1.00000000
#> 58   PHY_mdiat    FALSE        0.10      1.000       0e+00     1.00000000
#> 59  PHY_diatom     TRUE        0.10      1.000       0e+00     1.00000000
#> 60   PHY_tchla     TRUE          NA         NA          NA     1.00000000
#> 61     NCS_ss1     TRUE        5.00      3.000       3e-01     1.00000000
#> 62     NCS_ss2    FALSE        5.00      3.000       3e-01     1.00000000
#> 63     NCS_ss3    FALSE        5.00         NA          NA     1.00000000
#> 64     NCS_ss4    FALSE        5.00         NA          NA     1.00000000
#> 65     NCS_ss5    FALSE        5.00         NA          NA     1.00000000
#> 66     NCS_ss6    FALSE        5.00         NA          NA     1.00000000
#> 67     NCS_iss    FALSE          NA         NA          NA     1.00000000
#> 68     NCS_tss    FALSE          NA         NA          NA     1.00000000
#> 69    ZOO_zoo1     TRUE        0.10      1.000       0e+00     1.00000000
#> 70    ZOO_zoo2    FALSE        0.10         NA          NA     1.00000000
#> 71    ZOO_zoo3    FALSE        0.10         NA          NA     1.00000000
#> 72    ZOO_zoo4    FALSE        0.10         NA          NA     1.00000000
#> 73    ZOO_zoo5    FALSE        0.10         NA          NA     1.00000000
#> 74   FSH_fish1    FALSE        0.00      1.000          NA     1.00000000
#> 75   FSH_fish2    FALSE        0.00         NA          NA     1.00000000
#> 76   FSH_fish3    FALSE        0.00         NA          NA     1.00000000
#> 77   FSH_jelly    FALSE        0.00         NA          NA     1.00000000
#> 78 MAC_macalg1    FALSE        0.00         NA          NA     1.00000000
#> 79 MAC_macalg2    FALSE        0.00         NA          NA     1.00000000
#> 80 MAC_macalg3    FALSE        0.00         NA          NA     1.00000000
#> 81 MAC_macalg4    FALSE        0.00         NA          NA     1.00000000
#> 82   CLM_clam1    FALSE        0.00         NA          NA     1.00000000
#> 83   CLM_clam2    FALSE        0.00         NA          NA     1.00000000
#> 84   CLM_clam3    FALSE        0.00         NA          NA     1.00000000
#> 85     TRC_col    FALSE        0.00      0.000       0e+00     1.00000000
get_vars_sim("HYD_thmcln")
#> [1] "HYD_temp"   "HYD_thmcln"
```
