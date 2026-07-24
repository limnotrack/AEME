# Locate an installed Simstrat-AED2 executable

Returns the path to a Simstrat-AED2 executable previously installed with
[`install_simstrat_aed2()`](https://limnotrack.com/reference/install_simstrat_aed2.md).
Errors if that version isn't installed for this platform yet. Mirrors
[`glm_exe_path()`](https://limnotrack.com/reference/glm_exe_path.md);
unlike GLM, if no `version` is given this falls back to the binary
bundled with the package (see `inst/extbin/simstrat_aed2/`) rather than
an installed-version lookup, since that's what
[`run_aeme()`](https://limnotrack.com/reference/run_aeme.md) itself
resolves to by default.

## Usage

``` r
simstrat_aed2_exe_path(
  version = getOption("AEME.simstrat_version", NULL),
  os = NULL
)
```

## Arguments

- version:

  Character. The Simstrat-AED2 version to locate, e.g. `"3.0.4"`. If
  `NULL` (the default), returns the binary bundled with the package
  instead of one installed via
  [`install_simstrat_aed2()`](https://limnotrack.com/reference/install_simstrat_aed2.md).

- os:

  Character. One of `"windows"`, `"macos"`, or `"linux"`. Defaults to
  the current platform.

## Value

Character. The file path to the `simstrat`/`simstrat.exe` executable.

## Examples

``` r
if (FALSE) { # \dontrun{
install_simstrat_aed2(version = "3.0.4")
simstrat_aed2_exe_path(version = "3.0.4")
} # }
```
