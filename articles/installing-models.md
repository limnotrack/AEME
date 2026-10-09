# Installing Model Binaries

## Introduction

Since AEME v0.4.0, the package does not bundle model executables with
the R package – GLM-AED, GOTM-WET, DYRESM-CAEDYM and Simstrat-AED2 are
downloaded separately, once, after installing AEME itself. This vignette
covers:

1.  Which models are available on which platform
2.  Installing everything with one call, or one model at a time
3.  Checking what’s installed, and picking a specific version
4.  Where AEME looks for an installed model, if you need to know

If you already have models installed (or are working through
[`vignette("intro-aeme")`](https://limnotrack.com/articles/intro-aeme.md)
in an environment where this has been done for you), skip ahead to
[`vignette("intro-aeme")`](https://limnotrack.com/articles/intro-aeme.md).

## Platform availability

Not every model builds on every operating system:

| Model         | Windows | macOS | Linux |
|---------------|---------|-------|-------|
| DYRESM-CAEDYM | Yes     | No    | No    |
| GLM-AED       | Yes     | Yes   | Yes   |
| GOTM-WET      | Yes     | No    | No    |
| Simstrat-AED2 | Yes     | Yes   | Yes   |

[`install_models()`](https://limnotrack.com/reference/install_models.md)
installs whatever is available for the platform it’s run on; asking for
a model that doesn’t build there raises an error naming what is
available instead of failing silently.

## Installing everything

The simplest path – one call installs the latest release of every model
available for your platform:

``` r

library(AEME)
install_models()
```

This is safe to re-run: by default, a model already installed at the
resolved “latest” version is left untouched (`force = FALSE`).

## Installing one model at a time

Each model also has its own installer, for when you only need one:

``` r

install_glm_aed()
install_gotm_wet()
install_dy_cd()
install_simstrat_aed2()
```

[`install_models()`](https://limnotrack.com/reference/install_models.md)
accepts a `model` argument if you’d rather filter the all-in-one call
instead of calling installers individually:

``` r

install_models(model = c("glm_aed", "simstrat_aed2"))
```

## Picking a specific version

By default, every installer resolves to the latest available release. To
see what’s available – for example, to pin a version for
reproducibility, or to match a model version a collaborator is using –
list versions before installing:

``` r

list_glm_versions()
list_gotm_wet_versions()
list_dy_cd_versions()
list_simstrat_aed2_versions()
list_simstrat_aed_versions()
```

``` r

list_glm_versions()
#> ⠙ 3 items, page 1 | 3ms
#>   package_release      os glm_version
#> 1          v0.4.0   linux       3.3.5
#> 4          v0.4.0   macos       3.3.5
#> 7          v0.4.0 windows       3.3.5
#> 3          v0.4.0   linux       4.0.0
#> 6          v0.4.0   macos       4.0.0
#> 9          v0.4.0 windows       4.0.0
#> 2          v0.4.0   linux      4.0.0+
#> 5          v0.4.0   macos      4.0.0+
#> 8          v0.4.0 windows      4.0.0+
```

``` r

install_glm_aed(version = "4.0.0")
```

## Checking what’s installed

``` r

get_model_version(model = "glm_aed")
```

[`list_models()`](https://limnotrack.com/reference/list_models.md)
returns every model code AEME knows about, independent of what’s
actually installed – useful as the input to functions like
`install_models(model = ...)` that expect the short code (`"glm_aed"`)
rather than the display name (`"GLM-AED"`):

``` r

library(AEME)
list_models()
#>   DYRESM-CAEDYM         GLM-AED        GOTM-WET   SIMSTRAT-AED2    SIMSTRAT-AED 
#>         "dy_cd"       "glm_aed"      "gotm_wet" "simstrat_aed2"  "simstrat_aed"
```

## Where installed models live

Installers place binaries under a per-user AEME data directory (not
inside the R package library, so a package reinstall doesn’t wipe them).
You shouldn’t normally need to know the exact path –
[`run_aeme()`](https://limnotrack.com/reference/run_aeme.md) and the
model-specific `run_*()` wrappers find it automatically – but if a build
step reports a missing executable,
[`get_model_version()`](https://limnotrack.com/reference/get_model_version.md)
and the `*_exe_path()` family
([`glm_exe_path()`](https://limnotrack.com/reference/glm_exe_path.md),
[`gotm_wet_exe_path()`](https://limnotrack.com/reference/gotm_wet_exe_path.md),
[`dy_cd_exe_path()`](https://limnotrack.com/reference/dy_cd_exe_path.md),
[`simstrat_aed2_exe_path()`](https://limnotrack.com/reference/simstrat_aed2_exe_path.md))
resolve exactly which binary, if any, AEME will use.

``` r

exe_path <- glm_exe_path()
exe_path
#> [1] "C:\\Users\\runneradmin\\AppData\\Roaming/R/data/R/AEME/windows/4.0.0+/glm.exe"
```

## Which model should I pick?

Well, it depends on what you want to do. The model-specific vignettes
cover the differences in detail, but in brief:

- GLM-AED is the most widely used and supported, and is the default in
  AEME. It is suitable for a wide range of lakes and reservoirs, and has
  a large user community.

- Simstrat-AED2/AED is a model that is also suitable for a wide range of
  lakes, it is particularly well-suited for deep, stratified lakes. It
  is less widely used than GLM-AED, but has a strong user community and
  is actively maintained. The Simstrat-AED model is an experimental
  version of the model, coupled with the recently (Augusu 2026) released
  AED API. It has only been developed within the Limnotrack branch, but
  may be of interest to researchers looking to explore new features and
  capabilities.

- GOTM-WET is a more specialized model, suitable for certain types of
  lakes and reservoirs. It is less widely used than GLM-AED, but may be
  more appropriate for certain applications.

- DYRESM-CAEDYM is a one-dimensional model that is suitable for certain
  types of lakes and reservoirs. However, it is quite old and is not
  actively maintained, so it is not recommended for new projects.

## Next steps

With a model installed,
[`vignette("intro-aeme")`](https://limnotrack.com/articles/intro-aeme.md)
walks through the `Aeme` object model and builds your first ensemble
member.
