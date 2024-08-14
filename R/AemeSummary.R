#' S4 Class representing summarised AEME data
#'
#' This class represents data summarising an Aeme object.
#' @title Aeme Class
#' @name Aeme
#' @aliases Aeme-class
#' @slot lake A list representing lake information. \itemize{
#'  \item \code{\bold{name}}: character; lake name.
#'  \item \code{\bold{id}}: character; lake identifier.
#'  \item \code{\bold{latitude}}: numeric; lake latitude.
#'  \item \code{\bold{longitude}}: numeric; lake longitude.
#'  \item \code{\bold{elevation}}: numeric; lake elevation.
#'  \item \code{shape}: sf; lake shape.
#'  \item \code{\bold{depth}}: numeric; lake depth.
#'  \item \code{\bold{area}}: numeric; lake area. Calculated from shape if not
#'  provided.
#'  }
#' @slot time A list representing time information. \itemize{
#' \item \code{\bold{start}}: character; start date.
#' \item \code{\bold{stop}}: character; end date.
#' \item \code{\bold{timestep}}: numeric; time step.
#' \item \code{\bold{spin_up}}: list; spin up information for each model
#' }
#' @slot configuration A list representing each model's configuration. \itemize{
#' \item \code{model_controls}: dataframe; Model controls for simulation.
#' \item \code{dy_cd}: list; DYRESM-CAEDYM configuration.
#' \item \code{glm_aed}: list; GLM-AED configuration.
#' \item \code{gotm_wet}: list; GOTM-WET configuration.
#' }
#' @slot observations A list representing observation information. \itemize{
#' \item \code{lake}: dataframe; lake observations.
#' \item \code{level}: dataframe; lake level observations.
#' }
#' @slot input A list representing input information. \itemize{
#' \item \code{init_profile}: dataframe; initial temperature profile (if none
#' use NULL or leave empty; if empty/NULL, the observations file will be used).
#' \item \code{\bold{init_depth}}: numeric; initial height of lake surface relative to
#' the bottom (m).
#' \item \code{\bold{hypsograph}}: dataframe; hypsograph.
#' \item \code{\bold{meteo}}: dataframe; meteorological data.
#' \item \code{\bold{use_lw}}: logical; use longwave radiation.
#' \item \code{\bold{Kw}}: numeric; light extinction coefficient (m-1).
#' }
#' @slot inflows A list representing inflows information. \itemize{
#' \item \code{data}: named list of inflow dataframes.
#' \item \code{factor}: named list; inflow factors for each model.
#' }
#' @slot outflows A list representing outflows information. \itemize{
#' \item \code{data}: named list of outflow dataframes.
#' \item \code{factor}: named list; outflow factors for each model.
#' \item \code{lvl}: numeric; height of lake level outflow.
#' }
#' @slot water_balance A list representing water balance information. \itemize{
#' \item\code{\bold{method}}: integer; Method for calculating water balance.
#' 1 = none, 2 = outflows, 3 = inflows and outflows.
#' \item\code{\bold{use}}: character; Can be 'obs' or 'mod'. Use observations
#'  or modelled data for water balance.
#' \item{\code{data}}: list of dataframe for water balance.
#' }
#' @slot output A list representing output information. \itemize{
#' \item \code{dy_cd}: list; DYRESM-CAEDYM output.
#' \item \code{glm_aed}: list; GLM-AED output.
#' \item \code{gotm_wet}: list; GOTM-WET output.
#' }
#' @slot parameters A dataframe representing model parameters.
#' @export

setClass("AemeSummary",
         representation(
           aeme = "Aeme"
         )
)
