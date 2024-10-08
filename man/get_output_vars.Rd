% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_output_vars.R
\name{get_output_vars}
\alias{get_output_vars}
\title{Get the output variables from an AEME object}
\usage{
get_output_vars(aeme, model, ens_n = 1)
}
\arguments{
\item{aeme}{aeme; object.}

\item{model}{vector; of models to be used. Can be `dy_cd`, `glm_aed`,
`gotm_wet`.}

\item{ens_n}{numeric; ensemble number to allocate to model output which is
loaded. Defaults to 1.}
}
\value{
A character vector of the output variables
}
\description{
Get the output variables from an AEME object
}
\examples{
aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
aeme <- readRDS(aeme_file)
path <- tempdir()
model_controls <- get_model_controls(use_bgc = TRUE)
model <- c("glm_aed", "gotm_wet")
aeme <- build_aeme(path = path, aeme = aeme, model = model,
                   model_controls = model_controls,
                   ext_elev = 5, use_bgc = TRUE)
# Run models
aeme <- run_aeme(aeme = aeme, model = model, verbose = FALSE,
path = path, model_controls = model_controls,
parallel = TRUE, ncores = 2L)
get_output_vars(aeme, model)
}
