# Get model output file

Get model output file

## Usage

``` r
get_model_outfile(aeme = NULL, model, path = NULL, lake_dir, all = FALSE)
```

## Arguments

- aeme:

  Aeme object.

- model:

  character vector; models to use. One or more of `"dy_cd"`,
  `"glm_aed"`, `"gotm_wet"`, `"simstrat_aed2"`. Defaults to all models
  if not found in `aeme`.

- path:

  Directory to search for the model output. If `aeme` is also provided,
  `path` is the root combined with `aeme` to compute the lake's
  directory (as in
  [`get_lake_dir()`](https://limnotrack.com/reference/get_lake_dir.md))
  – omit it to use `aeme`'s own stored path. If `aeme` is not provided,
  `path` is searched directly, and can be either an ensemble root or a
  single model's own directory.

- lake_dir:

  **\[deprecated\]** Use `path` instead of `lake_dir`

- all:

  logical; a model run can produce more than one output file (e.g.
  GLM-AED's netCDF plus its `csv_lake`/`csv_point`/mass-balance CSVs, or
  GOTM's `output.nc` plus `output_daily.nc`). When `FALSE` (the
  default), only the primary file per model is returned – the netCDF
  entry named `"output"` when there is one, otherwise the first (only)
  file. Set to `TRUE` to get every file the model's resolver found.

## Value

list of model output files.
