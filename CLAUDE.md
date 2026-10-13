# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## What this is

AEME (Aquatic Ecosystem Model Ensemble) is an R package (roxygen2
markdown, testthat 3e, `Language: en-GB`) that builds inputs for, runs,
and post-processes four 1D lake models behind one interface:
DYRESM-CAEDYM (`dy_cd`), GLM-AED (`glm_aed`), GOTM-WET (`gotm_wet`) and
Simstrat-AED2 (`simstrat_aed2`). Model executables are **not bundled**;
they are fetched with
[`install_models()`](https://limnotrack.com/AEME/reference/install_models.md)
(per-model `install_*.R`).

## Commands

Run from the repo root in R:

``` r

devtools::load_all()                       # load for development
devtools::document()                       # regenerate man/ and NAMESPACE (roxygen2 8.x)
devtools::test()                           # full suite (slow: builds and runs models)
devtools::test(filter = "build")           # one test file -> tests/testthat/test-build.R
testthat::test_file("tests/testthat/test-build.R")
devtools::check()                          # R CMD check (CI: windows-latest, release)
AEME::check_AEME_pkg()                     # end-to-end smoke test on the bundled example lake
pkgdown::build_site()                      # docs site (_pkgdown.yml controls the reference index)
```

- `tests/testthat/setup.R` sets
  `options(AEME.inform = FALSE, ncore = 2L)` and pre-installs models
  once. `helper-glm.R` provides `skip_if_models_unavailable()`,
  `skip_if_no_glm()` and a model-subsetting helper; use these in new
  tests that need a model binary rather than adding OS checks.
- Platform support: only `glm_aed` and `simstrat_aed2` run on
  macOS/Linux; `dy_cd` and `gotm_wet` are Windows-only (see
  `.windows_only_models` in the test helper and `.detect_os()` in the
  package).
- `Suggests` includes GitHub-only packages (`aemetools`, `bathytools`,
  `ltapi`; see `Remotes`). Parameter-sampling/calibration/sensitivity
  lives in `aemetools`, bathymetry processing in `bathytools`; don’t
  re-add that here (some helpers were recently moved *into* AEME from
  aemetools, see `NEWS.md`).

## Architecture

The central object is the S4 class `Aeme` (`R/Aeme.R`, ~1800 lines:
class, validity, accessors/setters, `show`). Its slots are `lake`,
`time`, `configuration`, `observations`, `input`, `inflows`, `outflows`,
`water_balance`, `output`, `parameters`. Accessor functions
([`lake()`](https://limnotrack.com/AEME/reference/lake.md),
[`time()`](https://limnotrack.com/AEME/reference/time.md),
[`output()`](https://limnotrack.com/AEME/reference/output.md), …) and
`validObject()` in the setters enforce structure, so go through
accessors instead of `@` where possible.

Typical pipeline (see README example):

1.  **Load** – `yaml_to_aeme(path, "aeme.yaml")` reads a lake
    description (plus observation/met/inflow/hypsograph files) into an
    `Aeme` object. Example lakes are in `inst/extdata/` (`lake`,
    `gotm_wet`, …) and `tests/testthat/LID45819_wainamu/`.
2.  **Build** –
    [`build_aeme()`](https://limnotrack.com/AEME/reference/build_aeme.md)
    (`R/build_aeme.R`) creates per-model config in `path/<model>/` via
    the model-specific builders `build_glm.R`, `build_dycd.R`,
    `build_gotm.R`, `build_simstrat.R`. These call the many `set_*`
    functions (`set_glm_*`, `set_gotm_*`, `set_dy_cd_*`,
    `set_simstrat_*`, plus model-agnostic `set_inflows`, `set_time`,
    `set_initial_conditions`, …) and the writers (`write_nml.R`,
    `write_yaml.R`, `make_dy_*.R`, `write_simstrat_nc.R`).
    Model-specific file formats are handled by `read_nml`/`nml_helpers`,
    `yaml` for GOTM, plain text/cfg for DYRESM, JSON-like par for
    Simstrat.
3.  **Parameters** –
    [`input_model_parameters()`](https://limnotrack.com/AEME/reference/input_model_parameters.md)
    / `apply_parameters*.R` map AEME’s generic parameter table (the
    `parameters` slot; columns defined in `param_colnames.R`) onto each
    model’s native parameter names. Parameter names/lookup tables are
    generated from `data-raw/*.R` + CSVs into `data/` (`sysdata`/lazy
    data); edit the CSV and rerun the corresponding `data-raw` script
    rather than editing `.rda` files. Parameters are encoded as
    `group/name[index]`
    ([`encode_param()`](https://limnotrack.com/AEME/reference/param_names.md)/[`decode_param()`](https://limnotrack.com/AEME/reference/param_names.md));
    zone-ratio/offset parameters are expanded in
    [`input_model_parameters()`](https://limnotrack.com/AEME/reference/input_model_parameters.md).
4.  **Run** –
    [`run_aeme()`](https://limnotrack.com/AEME/reference/run_aeme.md)
    (`R/run_aeme.R`) executes each model’s binary via `processx`
    (optionally `parallel`), then
    [`load_output()`](https://limnotrack.com/AEME/reference/load_output.md)
    reads model-native output back into the `output` slot.
    [`run_aeme_ensemble()`](https://limnotrack.com/AEME/reference/run_aeme_ensemble.md)
    runs one model across many parameter sets, storing results as
    `output(aeme)$ens_001...`.
    [`write_aeme_parquet()`](https://limnotrack.com/AEME/reference/write_aeme_parquet.md)
    / `write_output_daily_nc.R` export results.
5.  **Analyse/plot** – `plot_*.R`, `summarise_aeme.R`,
    [`get_var()`](https://limnotrack.com/AEME/reference/get_var.md),
    `calc_*.R` (met/heat-flux/water-balance helpers).

Cross-model variable mapping is central: AEME uses its own variable
names (e.g. `HYD_temp`, `HYD_wlev`; see `lookup_aeme_vars`,
`guess_aeme_vars`, `key_naming.csv`, `generate_var_map_code.R`) which
are translated to each model’s native names on both write
(`set_output_vars`, `set_vars_sim`) and read (`load_output`). When
adding a variable or parameter you typically need to touch the lookup
CSV/data-raw script and the per-model set/load code.

`model_controls`
([`get_model_controls()`](https://limnotrack.com/AEME/reference/get_model_controls.md),
`model_controls_class.R`, `set_model_controls.R`) toggles
biogeochemistry and per-model options; `use_bgc` switches between
physical-only and AED/WET/CAEDYM runs.

## Conventions and gotchas

- User-facing messages use `cli`; informational messages are gated on
  `getOption("AEME.inform")`. Model versions can be pinned with options
  such as `AEME.glm_version`, `AEME.gotm_version`,
  `AEME.simstrat_version`, `AEME.simstrat_aed_version`.
- `R/import-standalone-purrr.R` is a vendored standalone file; don’t
  edit it by hand.
- `man/` and `NAMESPACE` are generated; edit roxygen comments in `R/`
  and run `document()`. `README.md` is generated from `README.Rmd`.
- Add a `NEWS.md` entry (top section is the in-development version) for
  user-visible changes.
- Working branch is `dev`; PRs target `dev`/`main`. The repo root
  contains many scratch/output directories (`aeme_tst`, `output`,
  `reprex*`, `test_write`, `models`, etc.) and a stray `nul` file; these
  are not package source (check `.Rbuildignore`).
