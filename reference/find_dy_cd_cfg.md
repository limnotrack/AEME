# Detect the DYRESM-CAEDYM configuration file within a directory

A DYRESM-CAEDYM model directory holds a family of files that all share a
single `<lakename>` prefix (`<lakename>.cfg`, `.con`, `.stg`, `.inf`,
`.wdr`, `.met`, `.pro`, `.int`). The `<lakename>` is not knowable from
the directory name alone, so these helpers recover it from the `.stg`
file that is always present (the same approach
[`run_dy_cd()`](https://limnotrack.com/reference/run_dy_cd.md) itself
uses), and return the full path to the requested companion file.

## Usage

``` r
find_dy_cd_cfg(dir, must_exist = TRUE)
```

## Arguments

- dir:

  character; the `dy_cd` model directory (not searched recursively).

- must_exist:

  logical; abort if no `.stg` file is found. Default `TRUE`.

## Value

character; full path to the matching file, or `NA_character_` if
`must_exist = FALSE` and no `.stg` file was found.

## Details

`find_dy_cd_cfg()` returns the `.cfg` file (DYRESM-CAEDYM's top-level
configuration), analogous to
[`find_glm_nml()`](https://limnotrack.com/reference/find_glm_nml.md) for
GLM-AED.

## Examples

``` r
if (FALSE) { # \dontrun{
find_dy_cd_cfg(path_dy)
} # }
```
