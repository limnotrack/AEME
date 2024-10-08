% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_hypsograph.R
\name{generate_hypsograph}
\alias{generate_hypsograph}
\title{Generate a hypsograph curve}
\usage{
generate_hypsograph(
  aeme = NULL,
  max_depth,
  surface_area,
  volume_development = 1.5,
  elev = NULL,
  z_range = 0.2,
  ext_elev = 0
)
}
\arguments{
\item{aeme}{An AEME object. If not provided, then max_depth and surface_area
must be provided.}

\item{max_depth}{The maximum depth of the lake.}

\item{surface_area}{The surface area of the lake.}

\item{volume_development}{The volume development factor.}

\item{elev}{The elevation of the lake. If not provided, it is assumed to be
the same as the maximum depth.}

\item{z_range}{numeric; 0-1, representing fraction of hypsograph to be used
for extrapolation. Default is 0.2, which uses the top 20% of the hypsograph
for extrapolation.}

\item{ext_elev}{numeric; metres to extend the hypograph by.}
}
\value{
A data frame with columns for elevation, depth, and area.
}
\description{
This function generates a hypsograph curve for a lake based on the maximum
depth, surface area, and volume development factor.
}
