#' ERA5 reference table
#'
#' A reference table for variable names between ERA5, LER and AEME.
#'
#' @format ## `era5_ref_table`
#' A data frame with 9 rows and 6 columns:
#' \describe{
#'   \item{variable}{Variable name}
#'   \item{era5}{ERA5 variable names}
#'   \item{nc}{ERA nc variable names}
#'   \item{ler}{LakeEnsemblR variable names}
#'   \item{aeme}{AEME variable names}
#'   \item{nc}{ERA nc variable names}
#' }
#' @source Package development.
"era5_ref_table"

#' Model variable naming
#'
#' A reference table for variable names between the models in AEME.
#'
#' @format ## `key_naming`
#' A data frame with 79 rows and 7 columns:
#' \describe{
#'   \item{name}{AEME variable name}
#'   \item{dy_cd}{DYRESM variable name}
#'   \item{glm_aed}{GLM variable name}
#'   \item{gotm_wet}{GOTM variable name}
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
