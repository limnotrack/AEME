# Run AEME models

These functions provide a unified interface for running external
hydrodynamic or biogeochemical lake models from within R. Each function
moves into the appropriate simulation directory, executes the model's
command-line binary, prints progress messages, and reports whether the
model run completed successfully. The functions are intended to be
called for their side effects: they do not return model results
directly, but instead produce model output files in the simulation
folder.

## Usage

``` r
run_dy_cd(
  sim_folder,
  verbose = FALSE,
  debug = FALSE,
  args = character(),
  timeout = Inf,
  version = getOption("AEME.dyresm_version", default = NULL)
)

run_glm_aed(
  sim_folder,
  verbose = FALSE,
  debug = FALSE,
  args = character(),
  timeout = Inf,
  version = getOption("AEME.glm_version", default = NULL)
)

run_gotm_wet(
  sim_folder,
  verbose = FALSE,
  debug = FALSE,
  args = character(),
  timeout = Inf,
  version = getOption("AEME.gotm_version", default = NULL)
)

run_simstrat_aed2(
  sim_folder,
  verbose = FALSE,
  debug = FALSE,
  args = character(),
  timeout = Inf
)
```

## Arguments

- sim_folder:

  the directory where simulation files are contained

- verbose:

  Logical: Should output of model be shown

- debug:

  Logical; save debug file. DYRESM only.

- args:

  character vector of additional command-line arguments to pass to the
  model executable. Currently only used for GLM-AED. Options are:
  "–xdisp" to plot the model output using the plots.nml settings.

- timeout:

  timeout in seconds, ignored if 0. This is a limit for the elapsed time
  running `command` in a separate process. Fractions of seconds are
  ignored.

- version:

  character; specific version of the model to run. If not provided, the
  default version bundled with the package will be used. For GLM-AED and
  GOTM-WET, this can also be set via the `AEME.glm_version` or
  `AEME.gotm_version` options, respectively. For DYRESM-CAEDYM, use
  `AEME.dyresm_version`. Currently, only GLM-AED support version
  selection; GOTM-WET and DYRESM-CAEDYM always uses the bundled version.

## Value

Invisibly returns `NULL`.
