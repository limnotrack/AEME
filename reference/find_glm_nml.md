# Detect the GLM hydrodynamic nml file within a directory

GLM's hydrodynamic namelist file has historically been named `glm3.nml`,
but newer GLM releases may write e.g. `glm4.nml` instead. AEME treats
any file matching `glm<version>.nml` in the top level of a `glm_aed`
model directory as *the* hydrodynamic nml, so the rest of the package
works the same regardless of which GLM version produced it. If more than
one such file is present, the choice between them is resolved by
.preferred_glm_major_version() (see Details).

## Usage

``` r
find_glm_nml(dir, must_exist = TRUE)
```

## Arguments

- dir:

  character; the `glm_aed` model directory (not searched recursively –
  AED's own nml files live one level down in `aed/`, so they never
  collide with this pattern).

- must_exist:

  logical; abort if none is found. Default `TRUE`.

## Value

character; full path to the matching file, or `NA_character_` if
`must_exist = FALSE` and none was found.

## Details

When multiple `glm<version>.nml` files are found in the same directory
(e.g. a leftover `glm3.nml` alongside a newer `glm4.nml`), the one to
use is chosen in priority order:

1.  The GLM version pinned via the `AEME.glm_version` option (set by
    [`install_glm_aed()`](https://limnotrack.com/reference/install_glm_aed.md),
    or by the caller directly).

2.  Whichever GLM version is actually installed locally (see
    [`glm_exe_path()`](https://limnotrack.com/reference/glm_exe_path.md)),
    checked directly rather than trusting session state.

3.  If neither can be determined, the highest version number among the
    files present (e.g. `glm4.nml` over `glm3.nml`).

A message reports which file was picked and why, since an unused
`glm<version>.nml` sitting in the directory is otherwise easy to miss.

## Examples

``` r
glm_dir <- file.path(tempdir(), "glm_aed")
dir.create(glm_dir, showWarnings = FALSE)
file.create(file.path(glm_dir, "glm3.nml"))
#> [1] TRUE
find_glm_nml(glm_dir)
#> [1] "C:\\Users\\RUNNER~1\\AppData\\Local\\Temp\\RtmpCSvjTS/glm_aed/glm3.nml"
```
