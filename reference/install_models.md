# Install the latest available binary for every AEME model

Convenience wrapper around
[`install_glm_aed()`](https://limnotrack.com/reference/install_glm_aed.md),
[`install_gotm_wet()`](https://limnotrack.com/reference/install_gotm_wet.md),
[`install_dy_cd()`](https://limnotrack.com/reference/install_dy_cd.md),
and
[`install_simstrat_aed2()`](https://limnotrack.com/reference/install_simstrat_aed2.md)
that installs the latest version of each, for whichever models actually
have a binary published for the current platform. A model with no
release asset for this OS (or no release assets published at all yet,
e.g. GOTM-WET/DYRESM-CAEDYM before their first upload) is reported and
skipped rather than aborting the whole call - one missing model
shouldn't block installing the others.

## Usage

``` r
install_models(
  model = NULL,
  os = NULL,
  repo = "limnotrack/AEME",
  force = FALSE,
  quiet = FALSE
)
```

## Arguments

- model:

  Character vector of model names to install, in either display
  (`"GLM-AED"`) or code (`"glm_aed"`) form. Defaults to every model AEME
  knows about (see
  [`list_models()`](https://limnotrack.com/reference/list_models.md)).

- os:

  Character. One of `"windows"`, `"macos"`, or `"linux"`. Defaults to
  the platform R is currently running on.

- repo:

  Character. The `"owner/repo"` GitHub repository release assets are
  attached to. Defaults to `"limnotrack/AEME"`.

- force:

  Logical. If `FALSE` (the default), a model already installed at the
  resolved "latest" version is left alone. Set to `TRUE` to re-download
  and reinstall every model regardless.

- quiet:

  Logical. If `TRUE`, suppresses the per-model progress messages from
  each installer (the final summary is still printed unless silenced
  separately - see Value).

## Value

Invisibly, a named character vector with one entry per requested model:
the installed executable path on success, or `NA` for any model that was
skipped or failed.

## See also

[`install_glm_aed()`](https://limnotrack.com/reference/install_glm_aed.md),
[`install_gotm_wet()`](https://limnotrack.com/reference/install_gotm_wet.md),
[`install_dy_cd()`](https://limnotrack.com/reference/install_dy_cd.md),
[`install_simstrat_aed2()`](https://limnotrack.com/reference/install_simstrat_aed2.md)
to install a single model with more control (specific version, etc.).

## Examples

``` r
if (FALSE) { # \dontrun{
install_models()
install_models(model = c("glm_aed", "simstrat_aed2"))
install_models(force = TRUE)
} # }
```
