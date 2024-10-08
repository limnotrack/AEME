% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_aeme.R
\name{run_aeme}
\alias{run_aeme}
\title{Run aquatic model ensemble}
\usage{
run_aeme(
  aeme,
  model,
  return = TRUE,
  ens_n = 1,
  model_controls = NULL,
  nlev = NULL,
  verbose = FALSE,
  debug = FALSE,
  timeout = 0,
  parallel = FALSE,
  ncores,
  check_output = FALSE,
  path = "."
)
}
\arguments{
\item{aeme}{aeme; object.}

\item{model}{vector; of models to be used. Can be `dy_cd`, `glm_aed`,
`gotm_wet`.}

\item{return}{logical; return model output within an `aeme` object? Defaults
to TRUE.}

\item{ens_n}{numeric; ensemble number to allocate to model output which is
loaded. Defaults to 1.}

\item{model_controls}{dataframe; of configuration loaded from
"model_controls.csv".}

\item{nlev}{numeric; number of levels to return in model output. If NULL,
calculates number of levels based on the `model_layer_structure`.}

\item{verbose}{logical; print model output to console. Defaults to FALSE.}

\item{debug}{logical; write debug log (Only DYRESM). Defaults to FALSE.}

\item{timeout}{timeout in seconds, ignored if 0.  This is a limit for the
    elapsed time running \code{command} in a separate process.   Fractions
    of seconds are ignored.}

\item{parallel}{logical; run models in parallel. Defaults to FALSE.}

\item{ncores}{integer; number of cores to use for parallelization. Defaults
to `min(c(detectCores() - 1, length(model)))`.}

\item{check_output}{logical; check model output after running? Defaults to
FALSE.}

\item{path}{filepath; where input files are located relative to `config`.}
}
\value{
an `aeme` object with model output loaded.
}
\description{
Run aquatic model ensemble
}
\examples{
aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
aeme <- readRDS(aeme_file)
path <- tempdir()
model_controls <- get_model_controls()
model <- c("glm_aed")
aeme <- build_aeme(path = path, aeme = aeme, model = model,
model_controls = model_controls, ext_elev = 5)
aeme <- run_aeme(aeme = aeme, model = model, path = path)
plot_output(aeme, model = model)
}
