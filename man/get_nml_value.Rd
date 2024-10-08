% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_nml_value.R
\name{get_nml_value}
\alias{get_nml_value}
\title{gets a nml value according to an arg_name}
\usage{
get_nml_value(glm_nml = NA, arg_name, nml_file = NA, ...)
}
\arguments{
\item{glm_nml}{a nml (a list) for GLM config}

\item{arg_name}{a string representing a valid field in glm_nml}

\item{nml_file}{a string with the path to the GLM glm2.nml file or
\code{'template'} for loading the GLM template nml file with GLM3r (default)}

\item{\dots}{additional arguments passed to \code{get_block}, such as warn=TRUE}
}
\value{
arg_val value for the valid field in glm_nml specified by \code{arg_name}
}
\description{
This function returns an nml value according to the arg_name nml list for GLM.
}
\seealso{
\link{read_nml}, \link{set_nml}
}
\author{
Jordan S. Read
}
