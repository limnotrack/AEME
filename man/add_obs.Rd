% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_obs.R
\name{add_obs}
\alias{add_obs}
\title{Add observations to Aeme object}
\usage{
add_obs(aeme, lake = NULL, level = NULL)
}
\arguments{
\item{aeme}{aeme; object.}

\item{lake}{data frame with columns "Date", "var_aeme", "depth_from",
"depth_to" and "value". If NULL, no observations are added.}

\item{level}{data frame with columns "Date", "var_aeme" and "value". If NULL,
no observations are added.}
}
\value{
Aeme object with observations added
}
\description{
Add observations to Aeme object
}
