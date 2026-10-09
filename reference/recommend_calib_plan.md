# Turn a diagnosis into concrete calibration suggestions

Rule-based and deliberately conservative: it names the parameters and
the reasoning, it does not build or launch a calibration run itself.
Meant to be read, not blindly executed - a stage design still has to
reconcile these suggestions against every other lake being calibrated in
the same run.

## Usage

``` r
recommend_calib_plan(diag, param_table = NULL)
```

## Arguments

- diag:

  an `aeme_diag` object from
  [`diag_aeme`](https://limnotrack.com/reference/diag_aeme.md).

- param_table:

  data frame with columns `name` (parameter name) and `vars`
  (list-column; each element a character vector of the AEME `var_sim`
  names that parameter is linked to), used to look up which calibration
  parameters are linked to a biased nutrient-budget variable. `NULL`
  (default) skips that lookup.

## Value

data frame with columns `issue`, `severity`
(`"low"`/`"medium"`/`"high"`), `evidence`, `recommendation`, `params`
(comma-joined parameter names, where known).
