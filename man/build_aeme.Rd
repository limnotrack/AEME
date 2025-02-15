% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/build_aeme.R
\name{build_aeme}
\alias{build_aeme}
\title{Build model configuration directories}
\usage{
build_aeme(
  aeme = NULL,
  config = NULL,
  model = c("dy_cd", "glm_aed", "gotm_wet"),
  model_controls = NULL,
  inf_factor = c(glm_aed = 1, dy_cd = 1, gotm_wet = 1),
  outf_factor = c(glm_aed = 1, dy_cd = 1, gotm_wet = 1),
  ext_elev = 0,
  use_bgc = FALSE,
  print = TRUE,
  calc_wbal = TRUE,
  wb_method = 2,
  calc_wlev = TRUE,
  use_aeme = FALSE,
  coeffs = NULL,
  hum_type = 3,
  est_swr_hr = TRUE,
  path = "."
)
}
\arguments{
\item{aeme}{aeme; object.}

\item{config}{list; loaded via `config <- yaml::read_yaml("aeme.yaml")`}

\item{model}{vector; of models to be used. Can be `dy_cd`, `glm_aed`,
`gotm_wet`.}

\item{model_controls}{dataframe; of configuration loaded from
"model_controls.csv".}

\item{inf_factor}{vector; containing numeric factor to multiple the inflows.
Needs to be named according to the model.}

\item{outf_factor}{vector; containing numeric factor to multiple the
outflows. Needs to be named according to the model.}

\item{ext_elev}{numeric; metres to extend the hypograph by.}

\item{use_bgc}{logical; switch to use the biogeochemical model.}

\item{print}{logical; print messages. Default = TRUE.}

\item{calc_wbal}{logical; calculate water balance. Default = TRUE.}

\item{wb_method}{numeric; method to use for calculating water balance. Must be
1 (no inflows or outflows) or 2 (outflows calculated) or 3 (inflows and
outflows calculated). Default = 2}

\item{calc_wlev}{logical; calculate water level.}

\item{use_aeme}{logical; use AEME object to generate model confiuration
files.}

\item{coeffs}{numeric vector of length two; to be used to estimate surface
water temperature for estimating evaporation. Defaults to NULL. If water
temperature observations are included in `aeme` object, then it will use
those to build a linear relationship between air temperature and water
temperature. Otherwise. it uses the simple estimation
 \eqn{temp_water = 5 + 0.75 * temp_air} from Stefan & Preud'homme, 2007:
 www.doi.org/10.1111/j.1752-1688.1993.tb01502.x}

\item{hum_type}{numeric; GOTM humidity metric [1=relative humidity (%),
2=wet-bulb temperature, 3=dew point temperature, 4=specific humidity (kg/kg)]
Default = 3.}

\item{est_swr_hr}{logical; estimate hourly shortwave radiation from daily
values. Default = TRUE.}

\item{path}{filepath; where input files are located relative to `config`.}
}
\value{
builds the model ensemble configuration.

aeme object
}
\description{
Configure an ensemble of lake model simulations from basic set of inputs.
}
\examples{
tmpdir <- tempdir()
aeme_dir <- system.file("extdata/lake/", package = "AEME")
# Copy files from package into tempdir
file.copy(aeme_dir, tmpdir, recursive = TRUE)
path <- file.path(tmpdir, "lake")
aeme <- yaml_to_aeme(path = path, "aeme.yaml")
model_controls <- get_model_controls()
inf_factor = c("glm_aed" = 1)
outf_factor = c("glm_aed" = 1)
model <- c("glm_aed")
build_aeme(path = path, aeme = aeme, model = model,
               model_controls = model_controls, inf_factor = inf_factor, ext_elev = 5,
               use_bgc = FALSE)
}
