# List available GLM versions

Scans all releases of `repo` and returns every GLM version that has a
binary attached, along with which AEME release (package version) and
platform each one belongs to.

## Usage

``` r
list_glm_versions(repo = "limnotrack/AEME", os = NULL)
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
combination, with columns `package_release`, `os`, and `glm_version`.
Returns a zero-row data frame (with a message) if nothing is found.

## Examples

``` r
if (FALSE) { # \dontrun{
list_glm_versions()
list_glm_versions(os = "windows")
} # }
```
