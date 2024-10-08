% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_model_output.R
\name{check_model_output}
\alias{check_model_output}
\title{Check model output}
\usage{
check_model_output(path, aeme, model)
}
\arguments{
\item{path}{filepath; where input files are located relative to `config`.}

\item{aeme}{aeme; object.}

\item{model}{vector; of models to be used. Can be `dy_cd`, `glm_aed`,
`gotm_wet`.}
}
\value{
logical; if model output passes checks or not.
}
\description{
Check model output
}
