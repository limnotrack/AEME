# Encode and decode full parameter names

A calibratable parameter is identified by its `group`, `name` and
`index` (the columns of
[`parameters()`](https://limnotrack.com/AEME/reference/parameters.md)).
These helpers turn that triple into a single string – the *full name*,
`group/name[index]` – and back. Calibration and sensitivity tools use
the full name as a key, for example in PEST++ control files and results
tables, where a parameter has to be one token.

The format is fixed because the strings are stored: a missing group or
index is written as `NA`, so `NA`/`inflow`/`1` encodes to
`"NA/inflow[1]"` and `NA`/`ce`/`NA` to `"NA/ce[NA]"`.

Decoding splits at the **first** `/` and the **last** `[...]`. The group
therefore cannot itself contain a `/`, and a group literally called
`"NA"` decodes as a missing group. A name may contain `/` (for example
`"mixing/coef_mix_conv"`) and `[`.

## Usage

``` r
encode_param(group, name, index)

decode_param_full(name_full)

decode_param(name_full)
```

## Arguments

- group:

  character; the parameter group (`NA` for none).

- name:

  character; the parameter name.

- index:

  integer; the index of the parameter within a vector (`NA` for none).

- name_full:

  character; full parameter names in the format `group/name[index]`, as
  made by `encode_param()`.

## Value

`encode_param()` returns a character vector of full names, recycled like
[`paste0()`](https://rdrr.io/r/base/paste.html); `character(0)` when
`group`, `name` and `index` are all empty.

`decode_param_full()` returns a data.frame with columns `group`, `name`
and `index`, one row per element of `name_full`.

`decode_param()` returns the `name` part only. Elements that are not in
the `group/name[index]` format (and `NA`) decode to `NA`.

## Examples

``` r
nm <- encode_param(group = c("light", NA), name = c("Kw", "inflow"),
                   index = c(1L, NA))
nm
#> [1] "light/Kw[1]"   "NA/inflow[NA]"
decode_param(nm)
#> [1] "Kw"     "inflow"
decode_param_full(nm)
#>   group   name index
#> 1 light     Kw     1
#> 2  <NA> inflow    NA
```
