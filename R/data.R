#' Model variable naming
#'
#' A reference table for variable names between the models in AEME.
#'
#' @format ## `key_naming`
#' A data frame with 79 rows and 8 columns:
#' \describe{
#'   \item{name}{AEME variable name}
#'   \item{dy_cd}{DYRESM variable name}
#'   \item{glm_aed}{GLM variable name}
#'   \item{gotm_wet}{GOTM variable name}
#'   \item{gotm_fabm}{GOTM-FABM variable name containing key-value names}
#'   \item{name_full}{String variable name}
#'   \item{name_parse}{Variable name for parsing}
#'   \item{conversion_aed}{Unit conversion for GLM-AED}
#' }
#' @source Package development.
"key_naming"

#' Catchment yields
#'
#' A reference table for catchment nutrient and sediment yields for calculating
#' inflow nutrients in AEME.
#'
#' @format ## `catchment_yields`
#' A data frame with 4 rows and 4 columns:
#' \describe{
#'   \item{Type}{Type of catchment}
#'   \item{TN_kg/ha/y}{Total nitrogen yield per hectare per year.}
#'   \item{TP_kg/ha/y}{Total phosphorus yield per hectare per year.}
#'   \item{TSS_kg/ha/y}{Total suspended sediment yield per hectare per year.}
#' }
#' @author Chris McBride
#' @source Package development.
"catchment_yields"

#' Model controls
#'
#' A reference table for catchment nutrient and sediment yields for calculating
#' inflow nutrients in AEME.
#'
#' @format ## `model_controls`
#' A data frame with 65 rows and 6 columns:
#' \describe{
#'   \item{var_aeme}{AEME variable}
#'   \item{simulate}{Logical value to simulate the variable}
#'   \item{inf_default}{Default value in inflows.}
#'   \item{initial_wc}{Default value for initialising in the water column.}
#'   \item{initial_sed}{Default value for initialising in the sediments.}
#'   \item{conversion_aed}{Unit conversion for GLM-AED.}
#' }
#' @author Tadhg Moore, Chris McBride
#' @source Package development.
"model_controls"
