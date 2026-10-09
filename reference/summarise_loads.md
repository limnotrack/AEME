# Summarise lake loading from inflows

Calculates the discharge and constituent loads (e.g. nitrogen,
phosphorus, carbon) entering the lake via its inflows, expressed as both
totals over the simulation period and annual averages. Useful for
getting a quick overview of how much water and nutrient mass is being
delivered to the lake, and by which inflow.

## Usage

``` r
summarise_loads(aeme, inflow_vars = NULL, by_inflow = TRUE)
```

## Arguments

- aeme:

  Aeme object.

- inflow_vars:

  character; vector of AEME inflow concentration variable names to
  calculate loads for (e.g. `c("NIT_tn", "PHS_tp")`). If `NULL`
  (default), all recognised mass-concentration variables present in the
  inflow data are used.

- by_inflow:

  logical; if `TRUE` (default), totals are broken down by `inflow_id` as
  well as summed across all inflows (labelled `"all"`). If `FALSE`, only
  the combined total across all inflows is returned.

## Value

A data frame with columns:

- `inflow_id`: the inflow identifier, or `"all"` for the sum across all
  inflows.

- `var_aeme`: variable name (`"HYD_flow"` for discharge, or the AEME
  concentration variable name for a load).

- `name_text`: human-readable variable name.

- `metric`: `"discharge"` or `"load"`.

- `unit`: unit of the `total` and `annual_average` columns (`"m3"` for
  discharge, `"kg"` for loads).

- `total`: total discharge/load summed over the full inflow record.

- `annual_average`: `total` divided by the number of years spanned by
  the inflow record.

- `n_years`: number of years spanned by the inflow record, used to
  calculate `annual_average`.

## Details

Discharge is summed directly from the `HYD_flow` column (m3/day).
Constituent loads are calculated as `HYD_flow * concentration` for each
recognised mass-concentration variable (units `g/m^3` in `key_naming`,
e.g. `NIT_tn`, `NIT_amm`, `PHS_tp`, `PHS_frp`, `CAR_doc`, `CAR_poc`)
present in the inflow data, summed over time and converted from g to kg.

## Examples

``` r
if (FALSE) { # \dontrun{
summarise_loads(aeme)
summarise_loads(aeme, inflow_vars = c("NIT_tn", "PHS_tp"))
} # }
```
