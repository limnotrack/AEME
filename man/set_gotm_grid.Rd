% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/set_gotm_grid.R
\name{set_gotm_grid}
\alias{set_gotm_grid}
\title{Set grid for GOTM model}
\usage{
set_gotm_grid(
  gotm,
  path_gotm = NULL,
  depth,
  method = 1,
  aeme = NULL,
  path = NULL,
  thickness_factor = 1,
  ddu = 0,
  ddl = 0
)
}
\arguments{
\item{gotm}{list; GOTM model configuration}

\item{depth}{numeric; depth of the lake}

\item{method}{0=equal by default with optional zooming, 1=prescribed relative
fractions, 2=prescribed layer thicknesses}

\item{aeme}{aeme; object.}

\item{path}{filepath; where input files are located relative to `config`.}

\item{thickness_factor}{numeric; factor to multiply the thickness of the
layers. Default is 1.}

\item{ddu}{numeric; Surface zooming [dimensionless; min=0.0; default=0.0]}

\item{ddl}{numeric; Bottom zooming [dimensionless; min=0.0; default=0.0]}
}
\value{
list; GOTM model configuration
}
\description{
Set grid for GOTM model
}
