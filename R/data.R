#' Model variable naming
#'
#' A reference table for variable names between the models in AEME.
#'
#' @format ## `key_naming`
#' A data frame with 127 rows and 13 columns:
#' \describe{
#'   \item{name}{AEME variable name}
#'   \item{dy_cd}{DYRESM variable name}
#'   \item{glm_aed}{GLM variable name}
#'   \item{gotm_wet}{GOTM variable name}
#'   \item{gotm_fabm}{GOTM-FABM variable name containing key-value names}
#'   \item{name_text}{Regular text string variable name}
#'   \item{name_full}{String variable name}
#'   \item{name_parse}{Variable name for parsing}
#'   \item{units}{Units of the variable. As defined using the
#'    \href{https://cran.r-project.org/package=units}{\pkg{units}} package.}
#'   \item{conversion_aed}{Unit conversion for GLM-AED}
#'   \item{default}{Default value for the variable}
#'   \item{derived}{Logical value if the variable is derived}
#'   \item{derived_from}{Variable name that the variable is derived from}
#'   \item{min}{Minimum value for the variable}
#'   \item{max}{Maximum value for the variable}
#'   \item{keywords}{Keywords associated with the variable}
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
#'   \item{group}{Phytoplankton Group for the parameter, only applies to
#'   phytoplankton parameters}
#'   \item{index}{Index for the parameter in the model file, only used for
#'   parameters that have multiple values in a vector such as
#'    "sediment/sed_temp_mean" in GLM-AED}
#'   \item{module}{Module for the parameter in the model, useful to help
#'   identify parameters} 
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
#'   \item{group}{Phytoplankton Group for the parameter, only applies to
#'   phytoplankton parameters}
#'   \item{index}{Index for the parameter in the model file, only used for
#'   parameters that have multiple values in a vector such as
#'    "sediment/sed_temp_mean" in GLM-AED}
#'   \item{module}{Module for the parameter in the model, useful to help
#'   identify parameters} 
#' }
#' @source Package development.
"aeme_parameters_bgc"

#' Example dataset of parameters for the DYRESM-CAEDYM model
#'
#' This dataset contains all parameters defined in the `.par` and `.cfg` file.
#' The values represent defaults from the standard configuration file, with 
#' ±25% parameter ranges for use in sensitivity analysis or model calibration.
#'
#' @format A data frame with 253 rows and 9 columns:
#' \describe{
#'   \item{model}{Model to which the parameter belongs}
#'   \item{file}{File in which the parameter is stored}
#'   \item{name}{Name of the parameter}
#'   \item{value}{Default value of the parameter}
#'   \item{min}{Minimum range of the parameter}
#'   \item{max}{Maximum range of the parameter}
#'   \item{group}{Phytoplankton group for the parameter; only applies to
#'     phytoplankton parameters}
#'   \item{index}{Index for parameters with multiple values in a vector (e.g.,
#'     `"sediment/sed_temp_mean"` in GLM-AED)}
#'   \item{module}{Model module associated with the parameter, useful for
#'     identifying functional groupings}
#' }
#'
#' @source Created during package development.
"dy_cd_parameters"


#' Example dataset of parameters for the GOTM-WET model
#'
#' All the parameters within the `gotm.yaml` and `fabm.yaml` file. 
#' This includes three phytoplankton groups (greens, cyanobacteria an diatoms),
#' one zooplankton group (cladocerans). This has the values in the default file 
#' and 25 % parameter ranges for sensitivity analysis or model calibration.
#'
#' @format A data frame with 628 rows and 14 columns:
#' \describe{
#'   \item{model}{Model for the parameter}
#'   \item{file}{File in which the parameter is stored}
#'   \item{name}{Name of the parameter}
#'   \item{value}{Value of the parameter}
#'   \item{min}{Minimum range of the parameter}
#'   \item{max}{Maximum range of the parameter}
#'   \item{group}{Phytoplankton Group for the parameter, only applies to
#'   phytoplankton parameters}
#'   \item{index}{Index for the parameter in the model file, only used for
#'   parameters that have multiple values in a vector such as
#'    "sediment/sed_temp_mean" in GLM-AED}
#'   \item{module}{Module for the parameter in the model, useful to help
#'   identify parameters} 
#'   \item{par}{Short name for the parameter}
#'   \item{logical}{Logical value if the parameter is boolean}
#'   \item{logical_val}{Value if the parameter is boolean}
#'   \item{char}{Logical value if the parameter is character}
#'   \item{char_val}{Value if the parameter is character}
#' }
#' @source Package development.
"gotm_wet_parameters"

#' Example dataframe used for calibrating the biogeochemistry in the GLM-AED
#' model.
#'
#' All the parameters within the AED.nml file. This includes three
#' phytoplankton groups (greens, cyanobacteria an diatoms), one zooplankton
#' group (cladocerans). This has the values in the default file and 25 % parameter
#' ranges for sensitivity analysis.
#'
#' @format `glm_aed_parameters`
#' A data frame with 253 rows and 7 columns:
#' \describe{
#'   \item{model}{Model for the parameter}
#'   \item{file}{File in which the parameter is stored}
#'   \item{name}{Name of the parameter}
#'   \item{value}{Value of the parameter}
#'   \item{min}{Minimum range of the parameter}
#'   \item{max}{Maximum range of the parameter}
#'   \item{group}{Phytoplankton Group for the parameter, only applies to
#'   phytoplankton parameters}
#'   \item{index}{Index for the parameter in the model file, only used for
#'   parameters that have multiple values in a vector such as
#'    "sediment/sed_temp_mean" in GLM-AED}
#'   \item{module}{Module for the parameter in the model, useful to help
#'   identify parameters} 
#'   \item{par}{Short name for the parameter}
#'   \item{logical}{Logical value if the parameter is boolean}
#'   \item{logical_val}{Value if the parameter is boolean}
#'   \item{char}{Logical value if the parameter is character}
#'   \item{char_val}{Value if the parameter is character}
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
#' @format `model_layer_structure`
#' A data frame with 191 rows and 3 columns:
#' \describe{
#'   \item{zi}{Interface depth (m)}
#'   \item{h}{Layer thickness (m)}
#'   \item{z}{Layer depth (m)}
#'   \item{n}{Layer number}
#' }
#' @source Package development.
"model_layer_structure"

#' Example dataframe of parameters for phytoplankton in the GLM-AED model.
#' 
#' This dataset contains all parameters related to phytoplankton in the 
#' aed_phyto_pars.csv file. 
#' 
#' This includes three phytoplankton groups (greens, cyanobacteria an diatoms).
#' 
#' @format `aed_phyto_pars`
#' A data frame with 564 rows and 5 columns:
#' \describe{
#'   \item{parameter_name}{Name of the parameter in AED}
#'   \item{group}{Phytoplankton group for the parameter}
#'   \item{value}{Default value of the parameter}
#'   \item{description}{Description of the parameter}
#'   \item{var_sim}{AEME variable names associated with the parameter, multiple
#'    variables are separated by a '|' symbol}
#' }
"aed_phyto_pars"
