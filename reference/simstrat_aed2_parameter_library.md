# Simstrat-AED2 parameter library

A comprehensive reference table of parameters used in the Simstrat-AED2
configuration, including their default (template) values, units, and a
brief description of each parameter's role.

## Usage

``` r
simstrat_aed2_parameter_library
```

## Format

A data frame with columns:

- module:

  The model module the parameter belongs to.

- group:

  A functional grouping of the parameter within the module.

- parameter:

  The parameter's name as it appears in the configuration file
  (`simstrat.par` or `aed2.nml`).

- label:

  A short human-readable label for the parameter.

- symbol:

  A mathematical symbol for the parameter, where applicable.

- description:

  A description of the parameter's role in the model.

- units:

  The units of the parameter.

- default:

  The default (template) value of the parameter.

- source:

  A URL for further information about the parameter.

The physical (hydrodynamic) parameters configure Simstrat's seiche
(internal wave) energy, wind drag, bottom drag, surface heat fluxes,
light absorption, and ice/snow dynamics – described in Table 1 of the
Simstrat User Manual. Default values are those in AEME's bundled
`inst/extdata/simstrat_aed2/simstrat.par` template.

The AED2 (Aquatic EcoDynamics) parameters configure the biogeochemical
modules coupled to Simstrat: oxygen, carbon (dissolved inorganic carbon,
pH, methane), silica, inorganic nitrogen, inorganic phosphorus, organic
matter, phytoplankton, and zooplankton. Simstrat-AED2 and GLM-AED couple
to the same underlying AED2 library, so most module/parameter
descriptions here are shared with
[`glm_aed_parameter_library`](https://limnotrack.com/reference/glm_aed_parameter_library.md)
(only the module name prefix differs: `"aed2_"` here vs. `"aed_"` there,
matching each model's actual configuration file). The carbon module has
no AED (v1) analogue and is documented directly from AEME's bundled
`aed2.nml` template.

As with `glm_aed_parameter_library`, `aed2_phytoplankton` and
`aed2_zooplankton` parameters listed under groups such as `"growth"`,
`"light"`, `"nitrogen"`, `"phosphorus"`, and `"silica"` are per-group
parameters specified in the separate
`aed2_phyto_pars.nml`/`aed2_zoop_pars.nml` files rather than directly in
`aed2.nml`.

## Source

Simstrat User Manual and source code:
<https://github.com/Eawag-AppliedSystemAnalysis/Simstrat>

AED Science Manual: <https://aquaticecodynamics.github.io/aed-science/>

Goudsmit, G-H., Burchard, H., Peeters, F., and Wuest, A. (2002).
Application of k-epsilon turbulence models to enclosed basins: The role
of internal seiches. *Journal of Geophysical Research: Oceans*,
107(C12), 23-1–23-13.

## Examples

``` r
# Load the parameter library
data(simstrat_aed2_parameter_library)

# View the structure
str(simstrat_aed2_parameter_library)
#> tibble [286 × 9] (S3: tbl_df/tbl/data.frame)
#>  $ module     : chr [1:286] "simstrat" "simstrat" "simstrat" "simstrat" ...
#>  $ group      : chr [1:286] "site" "site" "seiche" "seiche" ...
#>  $ parameter  : chr [1:286] "lat" "p_air" "a_seiche" "a_seiche_w" ...
#>  $ label      : chr [1:286] "Latitude" "Air pressure" "Seiche parameter" "Seiche parameter (winter)" ...
#>  $ symbol     : chr [1:286] "lat" "p_air" "a_seiche" "a_seiche^w" ...
#>  $ description: chr [1:286] "Latitude of the site (used for Coriolis force and albedo calculation)" "Long-term average air pressure at the site" "Fraction of wind energy transferred to seiche (internal wave) energy" "Fraction of wind energy transferred to seiche energy in winter (used instead of a_seiche when N2 < strat_sumr, "| __truncated__ ...
#>  $ units      : chr [1:286] "deg" "mbar" "-" "-" ...
#>  $ default    : chr [1:286] "45" "965" "0.00424" "0" ...
#>  $ source     : chr [1:286] "https://github.com/Eawag-AppliedSystemAnalysis/Simstrat" "https://github.com/Eawag-AppliedSystemAnalysis/Simstrat" "https://github.com/Eawag-AppliedSystemAnalysis/Simstrat" "https://github.com/Eawag-AppliedSystemAnalysis/Simstrat" ...

# List all unique modules
unique(simstrat_aed2_parameter_library$module)
#> [1] "simstrat"            "aed2_carbon"         "aed2_oxygen"        
#> [4] "aed2_nitrogen"       "aed2_phosphorus"     "aed2_silica"        
#> [7] "aed2_phytoplankton"  "aed2_organic_matter" "aed2_zooplankton"   

# Filter to the physical (hydrodynamic) parameters
simstrat_aed2_parameter_library[simstrat_aed2_parameter_library$module ==
                                "simstrat", ]
#> # A tibble: 23 × 9
#>    module   group  parameter  label      symbol description units default source
#>    <chr>    <chr>  <chr>      <chr>      <chr>  <chr>       <chr> <chr>   <chr> 
#>  1 simstrat site   lat        Latitude   lat    Latitude o… deg   45      https…
#>  2 simstrat site   p_air      Air press… p_air  Long-term … mbar  965     https…
#>  3 simstrat seiche a_seiche   Seiche pa… a_sei… Fraction o… -     0.00424 https…
#>  4 simstrat seiche a_seiche_w Seiche pa… a_sei… Fraction o… -     0       https…
#>  5 simstrat seiche strat_sumr Stratific… N2_th… Brunt-Vais… 1/s2  0       https…
#>  6 simstrat seiche q_nn       Seiche en… q_NN   Fit parame… -     1.1     https…
#>  7 simstrat seiche seiche_ini Initial s… E_sei… Initial to… J     0       https…
#>  8 simstrat wind   f_wind     Filtered … f_wind Fit parame… -     1.2193  https…
#>  9 simstrat wind   c10        Wind drag… C_10   Wind drag … -     1       https…
#> 10 simstrat mixing cd         Bottom dr… C_D    Bottom dra… -     0.002   https…
#> # ℹ 13 more rows

# Count parameters per module
table(simstrat_aed2_parameter_library$module)
#> 
#>         aed2_carbon       aed2_nitrogen aed2_organic_matter         aed2_oxygen 
#>                  19                  27                  60                   9 
#>     aed2_phosphorus  aed2_phytoplankton         aed2_silica    aed2_zooplankton 
#>                  15                  95                   7                  31 
#>            simstrat 
#>                  23 
```
