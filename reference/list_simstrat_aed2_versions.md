# List available Simstrat-AED2 versions

Scans all releases of `repo` and returns every Simstrat-AED2 version
that has a binary attached, along with which AEME release (package
version) and platform each one belongs to. Mirrors
[`list_glm_versions()`](https://limnotrack.com/reference/list_glm_versions.md).

## Usage

``` r
list_simstrat_aed2_versions(repo = "limnotrack/AEME", os = NULL)
```

## Arguments

- repo:

  Character. The `"owner/repo"` GitHub repository to search. Defaults to
  `"limnotrack/AEME"`.

- os:

  Character or `NULL`. If supplied, one of `"windows"`, `"macos"`, or
  `"linux"`, restricting results to that platform. If `NULL` (the
  default), all platforms are included.

## Value

A data frame with one row per available (release, platform, version)
combination, with columns `package_release`, `os`, and
`simstrat_version`. Returns a zero-row data frame (with a message) if
nothing is found.

## Examples

``` r
if (FALSE) { # \dontrun{
list_simstrat_aed2_versions()
list_simstrat_aed2_versions(os = "windows")
} # }
```
