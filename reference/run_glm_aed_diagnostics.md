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
#> 
#> ── Calculating water balance ──
#> 
#> Resolving water level
#>   ℹ Using observed water level
#> ! Missing values in observed water level
#> ℹ Estimating surface water temperature
#> ✔ Estimating surface water temperature [7ms]
#> 
#> Error in "lapply(text, glue_cmd, .envir = .envir)": ! Could not evaluate cli `{}` expression: `model`.
#> Caused by error in `eval(expr, envir = envir)`:
#> ! object 'model' not found

aeme <- run_aeme(aeme)
#> Error in run_aeme(aeme): ✖ Simulation folder does not exist
#>   D:\a\AEME\AEME\docs\reference/LID45819_wainamu
out <- run_glm_aed_diagnostics(aeme = aeme)
#> Error in check_path(lake_dir, must_exist = TRUE): ! Directory does not exist:
#>   D:\a\AEME\AEME\docs\reference\LID45819_wainamu.
#> ℹ Make sure to run `build_aeme()` before calling this function.
```
