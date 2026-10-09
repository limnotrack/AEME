# Convert aeme.yaml file to list

**\[deprecated\]**

## Usage

``` r
yaml_to_aeme(path, file)
```

## Arguments

- path:

  directory where aeme.yaml file is located. Can be used instead of
  `file` argument.

- file:

  filepath; to aeme.yaml file. Can be used instead of `path` and `file`
  arguments.

## Value

aeme object

## Details

`yaml_to_aeme()` is soft-deprecated in favour of
[`aeme_constructor()`](https://limnotrack.com/reference/aeme_constructor.md)
(build an `Aeme` object from your own lake data, with full validation)
or [`new_aeme()`](https://limnotrack.com/reference/new_aeme.md) (a quick
placeholder object to populate incrementally). It still works and will
keep working, but new code should prefer those instead of hand-editing a
YAML file.

## See also

[`aeme_constructor()`](https://limnotrack.com/reference/aeme_constructor.md),
[`new_aeme()`](https://limnotrack.com/reference/new_aeme.md)

## Examples

``` r
aeme_yaml <- system.file("extdata/lake/aeme.yaml", package = "AEME")
aeme <- yaml_to_aeme(file = aeme_yaml)
aeme
#> 
#> ── AEME v0.4.0 ─────────────────────────────────────────────────────────────────
#> 
#> ── Lake ──
#> 
#> Wainamu (ID: LID45819)
#> • Lat: -36.89; Lon: 174.47
#> • Elev: 23.64m; Depth: 13.07m; Area: 152343 m2
#> 
#> ── Time ──
#> 
#> • Start: 2020-08-01 00:00:00; Stop: 2021-06-30 00:00:00; Time step: 3600 s;
#>   Output step: 86400 s
#> • Timezone: UTC (timestamps stored UTC)
#> • Spin up (days): GLM: 2; GOTM: 1; DYRESM: 1; Simstrat: 2
#> 
#> ── Configuration ──
#> 
#> • Model: glm_aed
#> • Path: D:/a/AEME/AEME/docs/reference
#> • Model controls: Present
#> • Use biogeochemical model: No
#> ┌ Model Configuration ─────────────────────────────────────────┐
#> │       Model              Physical         Biogeochemical     │
#> │ ---                                                          │
#> │       DY-CD               Absent              Absent         │
#> │      GLM-AED              Absent              Absent         │
#> │      GOTM-WET             Absent              Absent         │
#> │   SIMSTRAT-AED2           Absent              Absent         │
#> │    SIMSTRAT-AED           Absent              Absent         │
#> └──────────────────────────────────────────────────────────────┘
#> 
#> ── Observations ──
#> 
#> • Lake: Present; Level: Present
#> 
#> ── Input ──
#> 
#> • Initial profile: Absent; Initial depth: 13.07m
#> • Hypsograph: Present (n=132)
#> • Meteo: Present; Use longwave: TRUE; Kw: 1.31
#> 
#> ── Inflows ──
#> 
#> • Number of inflows: 1; Names: FWMT
#> • Scaling factors: DY-CD: 1; GLM-AED: 1; GOTM-WET: 1; Simstrat-AED2: 1
#> 
#> ── Outflows ──
#> 
#> • Number of outflows: 1; Names: outflow; Elevations: -1
#> • Scaling factors: DY-CD: 1; GLM-AED: 1; GOTM-WET: 1; Simstrat-AED2: 1
#> 
#> ── Water Balance ──
#> 
#> • Method: 2; Use: obs
#> • Modelled: Absent; Water balance: Absent
#> 
#> ── Parameters ──
#> 
#> • Number of parameters: 0
#> 
#> ── Output ──
#> 
#> • DY-CD: 0
#> • GLM-AED: 0
#> • GOTM-WET: 0
#> • SIMSTRAT-AED2: 0
#> • SIMSTRAT-AED: 0
#> • Variables: 0
#> None
```
