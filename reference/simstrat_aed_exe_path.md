# Locate an installed Simstrat-AED executable

Returns the path to a Simstrat-AED executable previously installed with
[`install_simstrat_aed()`](https://limnotrack.com/reference/install_simstrat_aed.md).
Errors if that version isn't installed for this platform yet. Mirrors
[`simstrat_aed2_exe_path()`](https://limnotrack.com/reference/simstrat_aed2_exe_path.md);
there is no bundled fallback – Simstrat-AED binaries are only ever
obtained via
[`install_simstrat_aed()`](https://limnotrack.com/reference/install_simstrat_aed.md).

## Usage

``` r
simstrat_aed_exe_path(
  version = getOption("AEME.simstrat_aed_version", NULL),
  os = NULL
)
```

## Arguments

- version:

  Character. The Simstrat-AED version to locate, e.g. `"3.0.4"`. If
  `NULL` (the default), resolves to whichever version is currently
  installed (see
  [`install_simstrat_aed()`](https://limnotrack.com/reference/install_simstrat_aed.md)),
  following the same resolution order
  [`run_aeme()`](https://limnotrack.com/reference/run_aeme.md) itself
  uses.

- os:

  Character. One of `"windows"`, `"macos"`, or `"linux"`. Defaults to
  the current platform.

## Value

Character. The file path to the `simstrat`/`simstrat.exe` executable.

## Examples

``` r
if (FALSE) { # \dontrun{
install_simstrat_aed(version = "3.0.4")
simstrat_aed_exe_path(version = "3.0.4")
} # }
```
