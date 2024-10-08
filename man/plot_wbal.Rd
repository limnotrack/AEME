% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_wbal.R
\name{plot_wbal}
\alias{plot_wbal}
\title{Plot water balance components}
\usage{
plot_wbal(aeme, cumulative = FALSE)
}
\arguments{
\item{aeme}{aeme; object.}

\item{cumulative}{logical; plot cumulative sum of variable}
}
\value{
ggplot2 object
}
\description{
Plot water balance components for a given model including
evaporation volume, lake precipitation, lake inflow, and lake outflow.
}
