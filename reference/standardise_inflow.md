# Standardise inflow variable names and units for AEME

Attempts to match column names in an inflow data frame to AEME standard
variable names using
[`guess_aeme_vars()`](https://limnotrack.com/reference/guess_aeme_vars.md),
then detects the likely input units of each variable from its values and
converts to the units expected by the package (GLM-AED conventions).

## Usage

``` r
standardise_inflow(
  inflow,
  model_controls = NULL,
  inf_vars = NULL,
  aeme_time = NULL,
  inflow_name = "inflow",
  model = NULL,
  pot_inf_vars = NULL,
  verbose = TRUE
)
```

## Arguments

- inflow:

  data.frame; inflow data with a `time` or `Date` column and one or more
  inflow variable columns.

- model_controls:

  data.frame; model controls table containing at minimum columns
  `var_aeme` and `inf_default`. Used to fill missing state variables
  with default values. If `NULL` (default), the missing-variable fill
  step is skipped.

- inf_vars:

  character; vector of required inflow state variable names (e.g. from
  `model_controls$var_aeme`). Variables in `inf_vars` that are absent
  from `inflow` will be filled with values from
  `model_controls$inf_default`. Ignored when `model_controls` is `NULL`.

- aeme_time:

  named list or object; passed directly to `check_time()`. If `NULL`
  (default), the time-coverage check is skipped.

- inflow_name:

  character(1); human-readable label for this inflow stream, used in
  `check_time()` messages (e.g. `"inflow-tributary_1"`). Ignored when
  `aeme_time` is `NULL`.

- model:

  character(1); model identifier passed to `check_time()`. Ignored when
  `aeme_time` is `NULL`.

- pot_inf_vars:

  character; column allowlist used for the final
  [`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html)
  step. Defaults to `NULL`, which retains all columns. Typical value:
  `c("time", "HYD_flow", inf_vars, "model")`.

- verbose:

  logical; if `TRUE` (default), emit `cli_inform` messages describing
  each detected unit conversion applied.

## Value

The input data frame with column names remapped to AEME standard names,
values converted to AEME standard units, missing state variables filled
with defaults, time coverage validated, and columns trimmed to
`pot_inf_vars` (when supplied). Columns that could not be matched are
retained unchanged with a warning. A warning is also emitted if required
variables (`flow`, `temp`, `salt`) are absent after renaming.

## AEME standard inflow variables and units

|                         |               |          |              |
|-------------------------|---------------|----------|--------------|
| **Variable**            | **Name**      | **Unit** | **Required** |
| Flow                    | `flow`        | m³/day   | Yes          |
| Water temperature       | `temp`        | °C       | Yes          |
| Salinity                | `salt`        | PSU      | Yes          |
| Dissolved oxygen        | `OXY_oxy`     | mmol/m³  | No           |
| Phosphate-P             | `PHS_frp`     | mmol/m³  | No           |
| Dissolved organic P     | `OGM_dop`     | mmol/m³  | No           |
| Particulate organic P   | `OGM_pop`     | mmol/m³  | No           |
| Particulate inorganic P | `PHS_frp_ads` | mmol/m³  | No           |
| Ammoniacal nitrogen     | `NIT_amm`     | mmol/m³  | No           |
| Nitrate-N               | `NIT_nit`     | mmol/m³  | No           |
| Dissolved organic N     | `OGM_don`     | mmol/m³  | No           |
| Particulate organic N   | `OGM_pon`     | mmol/m³  | No           |
| Dissolved organic C     | `OGM_doc`     | mmol/m³  | No           |
| Particulate organic C   | `OGM_poc`     | mmol/m³  | No           |
| Dissolved inorganic C   | `CAR_dic`     | mmol/m³  | No           |
| Silica                  | `SIL_rsi`     | mmol/m³  | No           |
| Suspended solids 1      | `NCS_ss1`     | g/m³     | No           |
| Suspended solids 2      | `NCS_ss2`     | g/m³     | No           |
| pH                      | `CAR_pH`      | \-       | No           |

## Unit detection logic

Nutrient concentrations (N, P, C fractions) are expected in mmol/m³.
Detection thresholds are based on comparison of observed value ranges
against the example inflow data distributed with AEME:

- **N fractions** (`NIT_amm`, `NIT_nit`, `OGM_don`, `OGM_pon`): mmol/m³
  typically 0-10. Median \> 20 assumed to be mg/L and divided by the
  molar mass of N (14.007 g/mol).

- **P fractions** (`PHS_frp`, `OGM_dop`, `OGM_pop`, `PHS_frp_ads`):
  mmol/m³ typically 0-5. Median \> 10 assumed to be mg/L and divided by
  the molar mass of P (30.974 g/mol).

- **C fractions** (`OGM_doc`, `OGM_poc`): mmol/m³ typically 0-100.
  Median \> 100 assumed to be mg/L and divided by the molar mass of C
  (12.011 g/mol).

- **Dissolved inorganic C** (`CAR_dic`): mmol/m³ typically 500-1000.
  Median \> 2000 assumed to be mg/L.

- **Dissolved oxygen** (`OXY_oxy`): mmol/m³ typically 200-400. Median \<
  50 assumed to be mg/L and multiplied by 1000/32 (molar mass of O2).

- **Silica** (`SIL_rsi`): mmol/m³ typically 1-50. Median \> 100 assumed
  to be mg/L and divided by molar mass of SiO2 (60.084).

- **Temperature** (`temp`): °C expected. Median \> 100 assumed to be
  Kelvin.
