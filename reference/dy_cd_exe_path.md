# Locate an installed DYRESM-CAEDYM executable

Returns the path to `dycd.exe` previously installed with
[`install_dy_cd()`](https://limnotrack.com/reference/install_dy_cd.md) –
its three companion tools (`createDYref.exe`, `createDYsim.exe`,
`extractDYinfo.exe`) sit alongside it in the same directory. Errors if
that version isn't installed for this platform yet. Mirrors
[`glm_exe_path()`](https://limnotrack.com/reference/glm_exe_path.md).

## Usage

``` r
dy_cd_exe_path(version = getOption("AEME.dyresm_version", NULL), os = NULL)
```

## Arguments

- version:

  Character. The DYRESM-CAEDYM version to locate, e.g. `"5.0.0"`.

- os:

  Character. One of `"windows"`, `"macos"`, or `"linux"`. Defaults to
  the current platform.

## Value

Character. The file path to `dycd.exe`.

## Examples

``` r
if (FALSE) { # \dontrun{
install_dy_cd(version = "5.0.0")
dy_cd_exe_path(version = "5.0.0")
} # }
```
