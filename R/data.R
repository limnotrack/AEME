#' Model variable naming
#'
#' A reference table for variable names between the models in AEME.
#'
#' @format ## `key_naming`
#' A data frame with 126 rows and 13 columns:
#' \describe{
#'   \item{name}{AEME variable name}
#'   \item{dy_cd}{DYRESM variable name}
#'   \item{glm_aed}{GLM variable name}
#'   \item{gotm_wet}{GOTM variable name}
#'   \item{gotm_fabm}{GOTM-FABM variable name containing key-value names}
#'   \item{name_text}{Regular text string variable name}
#'   \item{name_full}{String variable name}
#'   \item{name_parse}{Variable name for parsing}
#'   \item{units}{Units of the variable}
#'   \item{conversion_aed}{Unit conversion for GLM-AED}
#'   \item{default}{Default value for the variable}
#'   \item{derived}{Logical value if the variable is derived}
#'   \item{derived_from}{Variable name that the variable is derived from}
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

#' Example dataframe used for calibrating AEME models.
#'
#' An example dataframe used for inputting and calibrating AEME models.
#'
#' @format ## `aeme_parameters`
#' A data frame with 17 rows and 6 columns:
#' \describe{
#'   \item{model}{Model for the parameter}
#'   \item{file}{File in which the parameter is stored}
#'   \item{name}{Name of the parameter}
#'   \item{value}{Value of the parameter}
#'   \item{min}{Minimum range of the parameter}
#'   \item{max}{Maximum range of the parameter}
#' }
#' @source Package development.
"aeme_parameters"

#' Example dataframe used for calibrating the biogeochemistry in the AEME
#' models.
#'
#' An example dataframe used for inputting and calibrating AEME models.
#'
#' @format ## `aeme_parameters_bgc`
#' A data frame with 30 rows and 7 columns:
#' \describe{
#'   \item{model}{Model for the parameter}
#'   \item{file}{File in which the parameter is stored}
#'   \item{name}{Name of the parameter}
#'   \item{value}{Value of the parameter}
#'   \item{min}{Minimum range of the parameter}
#'   \item{max}{Maximum range of the parameter}
#'   \item{var}{Maximum range of the parameter}
#' }
#' @source Package development.
"aeme_parameters_bgc"

#' Example dataframe used for calibrating the biogeochemistry in the GOTM-WET
#' model.
#'
#' All the parameters within the fabm.yaml file. This includes three
#' phytoplankton groups (greens, cyanobacteria an diatoms), one zooplankton
#' group (cladocerans). This has the values in the default file and 25 % parameter
#' ranges for sensitivity analysis.
#'
#' @format ## `gotm_wet_parameters`
#' A data frame with 182 rows and 7 columns:
#' \describe{
#'   \item{model}{Model for the parameter}
#'   \item{file}{File in which the parameter is stored}
#'   \item{name}{Name of the parameter}
#'   \item{value}{Value of the parameter}
#'   \item{min}{Minimum range of the parameter}
#'   \item{max}{Maximum range of the parameter}
#'   \item{var}{Maximum range of the parameter}
#' }
#' @source Package development.
"gotm_wet_parameters"

#' Example dataframe used for calibrating the biogeochemistry in the GOTM-WET
#' model.
#'
#' All the parameters within the fabm.yaml file. This includes three
#' phytoplankton groups (greens, cyanobacteria an diatoms), one zooplankton
#' group (cladocerans). This has the values in the default file and 25 % parameter
#' ranges for sensitivity analysis.
#'
#' @format ## `glm_aed_parameters`
#' A data frame with 253 rows and 7 columns:
#' \describe{
#'   \item{model}{Model for the parameter}
#'   \item{file}{File in which the parameter is stored}
#'   \item{name}{Name of the parameter}
#'   \item{value}{Value of the parameter}
#'   \item{min}{Minimum range of the parameter}
#'   \item{max}{Maximum range of the parameter}
#'   \item{var}{Maximum range of the parameter}
#' }
#' @source Package development.
"glm_aed_parameters"

#' Reference data frame for model layer structure.
#'
#' The reference used to structure model layers within the different models.
#' For the gotm_wet model, this is used to estimate the fractions
#' at different depths. Whereas for the glm_aed and dy_cd models, this is used
#' to define the min and max width of the layers.
#'
#' @format ## `model_layer_structure`
#' A data frame with 191 rows and 3 columns:
#' \describe{
#'   \item{zi}{Interface depth (m)}
#'   \item{h}{Layer thickness (m)}
#'   \item{z}{Layer depth (m)}
#' }
#' @source Package development.
"model_layer_structure"
