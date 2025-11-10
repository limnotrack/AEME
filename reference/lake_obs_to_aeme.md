# Format lake observation data to AEME format

Converts a data frame of lake observations into the format required for
AEME. The function maps variable names and units to AEME standards using
provided lookup tables. It also ensures that required columns are
present and correctly named.

## Usage

``` r
lake_obs_to_aeme(
  data,
  depth_col_name,
  datetime_col_name,
  var_col_name,
  value_col_name,
  lake_id_col,
  var_map
)
```

## Arguments

- data:

  data frame containing lake observations

- depth_col_name:

  column name for depth (m). If missing, the function will attempt to
  infer it.

- datetime_col_name:

  column name for date/time. If missing, the function will attempt to
  infer it.

- var_col_name:

  column name for variable names. If missing, the function will attempt
  to infer it.

- value_col_name:

  column name for variable values. If missing, the function will attempt
  to infer it.

- lake_id_col:

  column name for lake identifier. If missing, the function assumes all
  data is for a single lake.

- var_map:

  data frame with columns "var_aeme", "name", and "unit" for mapping
  variable names and units to AEME standards. "var_aeme" is the defined
  AEME variable name, "name" is the name used in the input data, and
  "unit" is the unit used in the input data.

## Value

A data frame formatted for AEME with columns "Date", "var_aeme",
"depth_from", "depth_to", and "value".
