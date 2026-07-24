# Locate an installed GLM executable

Returns the path to a GLM executable previously installed with
[`install_glm_aed()`](https://limnotrack.com/reference/install_glm_aed.md).
Errors if that version isn't installed for this platform yet.

## Usage

``` r
glm_exe_path(version = getOption("AEME.glm_version", NULL), os = NULL)
```

## Arguments

- version:

  Character. The GLM version to locate, e.g. `"3.9.108"`.

- os:

  Character. One of `"windows"`, `"macos"`, or `"linux"`. Defaults to
  the current platform.

## Value

Character. The file path to the `glm`/`glm.exe` executable.

## Examples

``` r
if (FALSE) { # \dontrun{
install_glm_aed(version = "3.9.108")
glm_exe_path(version = "3.9.108")
} # }
```
