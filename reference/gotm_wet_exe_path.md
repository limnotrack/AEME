# Locate an installed GOTM-WET executable

Returns the path to a GOTM-WET executable previously installed with
[`install_gotm_wet()`](https://limnotrack.com/reference/install_gotm_wet.md).
Errors if that version isn't installed for this platform yet. Mirrors
[`glm_exe_path()`](https://limnotrack.com/reference/glm_exe_path.md).

## Usage

``` r
gotm_wet_exe_path(version = getOption("AEME.gotm_version", NULL), os = NULL)
```

## Arguments

- version:

  Character. The GOTM-WET version to locate, e.g. `"2023.2.0"`.

- os:

  Character. One of `"windows"`, `"macos"`, or `"linux"`. Defaults to
  the current platform.

## Value

Character. The file path to the `gotm`/`gotm.exe` executable.

## Examples

``` r
if (FALSE) { # \dontrun{
install_gotm_wet(version = "2023.2.0")
gotm_wet_exe_path(version = "2023.2.0")
} # }
```
