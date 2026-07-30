#' Model variable naming
#'
#' A reference table for variable names between the models in AEME.
#'
#' @format ## `key_naming`
#' A data frame with 130 rows and 18 columns:
#' \describe{
#'   \item{var_aeme}{AEME variable name}
#'   \item{dy_cd}{DYRESM variable name}
#'   \item{glm_aed}{GLM variable name}
#'   \item{glm_aed2}{GLM-AED2 variable name}
#'   \item{simstrat_aed2}{Simstrat-AED2 variable name}
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

#' Example dataframe used for calibrating the Simstrat-AED2 model.
#'
#' Physical (hydrodynamic) parameters from the Simstrat `.par` file and a
#' subset of biogeochemical initial-concentration parameters from the
#' `aed2.nml` file (oxygen, carbon, silica, nitrogen, phosphorus, and organic
#' matter modules). This has the values in the default template files and
#' 50 % parameter ranges for sensitivity analysis.
#'
#' @format `simstrat_aed2_parameters`
#' A data frame with columns:
#' \describe{
#'   \item{model}{Model for the parameter}
#'   \item{file}{File in which the parameter is stored}
#'   \item{name}{Name of the parameter}
#'   \item{value}{Value of the parameter}
#'   \item{min}{Minimum range of the parameter}
#'   \item{max}{Maximum range of the parameter}
#'   \item{module}{Whether the parameter belongs to the "hydrodynamic" or
#'   "bgc" (biogeochemical) part of the model}
#'   \item{par}{Short parameter name}
#' }
#' @source Package development.
"simstrat_aed2_parameters"

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

#' Parameter library for the GLM-AED model.
#'
#' This dataset contains most parameters within the glm3.nml and the aed.nml
#' file. This includes 452 GLM parameters across 24 namelist blocks (e.g.,
#' \code{&glm_setup}, \code{&mixing}, \code{&morphometry}, \code{&meteorology},
#' \code{&inflow}, \code{&outflow}) and 452 AED parameters across 24 modules
#' (e.g., \code{aed_oxygen}, \code{aed_nitrogen}, \code{aed_phosphorus},
#' \code{aed_silica}, \code{aed_phytoplankton}, \code{aed_sedflux},
#' \code{aed_seddiagenesis}), for a combined total of 452 rows.
#'
#' Parameters are organised by \code{module} (the namelist block or AED module
#' they belong to) and optionally by \code{group} (a sub-category describing
#' the process context, such as \code{"nitrification"},
#' \code{"sediment_flux"}, or \code{"radiation"}).
#'
#' @format A data frame with 452 rows and 9 variables:
#' \describe{
#'   \item{module}{Character. The configuration block (GLM) or module (AED)
#'     the parameter belongs to. GLM modules correspond to Fortran namelist
#'     blocks in glm3.nml (e.g., \code{"glm_setup"}, \code{"mixing"},
#'     \code{"morphometry"}, \code{"time"}, \code{"output"},
#'     \code{"init_profiles"}, \code{"light"}, \code{"bird_model"},
#'     \code{"sediment"}, \code{"snowice"}, \code{"meteorology"},
#'     \code{"inflow"}, \code{"outflow"}, \code{"wq_setup"}). AED modules
#'     correspond to namelist blocks in aed.nml (e.g., \code{"aed_oxygen"},
#'     \code{"aed_nitrogen"}, \code{"aed_phosphorus"}, \code{"aed_silica"},
#'     \code{"aed_phytoplankton"}, \code{"aed_sedflux"},
#'     \code{"aed_seddiagenesis"}).}
#'   \item{group}{Character. An optional sub-category grouping related
#'     parameters by process or function within a module. For example,
#'     \code{"nitrification"}, \code{"denitrification"}, and
#'     \code{"sediment_flux"} within the \code{aed_nitrogen} module; or
#'     \code{"radiation"}, \code{"wind"}, and \code{"rainfall"} within the
#'     \code{meteorology} module. Empty string (\code{""}) where no
#'     sub-grouping applies.}
#'   \item{parameter}{Character. The parameter name as used in the
#'     configuration file (e.g., \code{"max_layers"}, \code{"Fsed_oxy"},
#'     \code{"R_nitrif"}).}
#'   \item{label}{Character. A short, human-readable label for the parameter
#'     (e.g., \code{"Layers"}, \code{"Sediment O2 flux"},
#'     \code{"Nitrification rate"}).}
#'   \item{symbol}{Character. The mathematical symbol used in the model
#'     documentation and equations, rendered as plain text (e.g.,
#'     \code{"N_MAX"}, \code{"F_sed^oxy"}, \code{"theta_nitrif"}).
#'     Empty string where no symbol is defined.}
#'   \item{description}{Character. A description of the parameter, its role
#'     in the model, and any relevant context.}
#'   \item{units}{Character. The units of the parameter. Uses \code{"-"} for
#'     dimensionless quantities, \code{"string"} for text parameters, and
#'     \code{"boolean"} for logical switches. Physical units follow standard
#'     abbreviations (e.g., \code{"m"}, \code{"mmol O2/m3"},
#'     \code{"mmol N/m2/d"}, \code{"/d"}).}
#'   \item{default}{Character. The default value of the parameter as
#'     documented, or an empty string (\code{""}) where no default is
#'     specified in the source documentation.}
#'   \item{source}{Character. URL of the documentation page from which the
#'     parameter information was extracted. GLM parameters are sourced from
#'     \url{https://aquatic.science.uwa.edu.au/research/models/GLM/configuration.html}
#'     and AED parameters from individual chapter pages of the AED Science
#'     Manual at \url{https://aquaticecodynamics.github.io/aed-science/}.}
#' }
#'
#' @details
#' The GLM (General Lake Model) parameters configure the hydrodynamic model,
#' including layer structure, mixing, morphometry, meteorological forcing,
#' boundary conditions (inflows and outflows), light penetration, ice/snow
#' dynamics, and sediment heat exchange.
#'
#' The AED (Aquatic EcoDynamics) parameters configure the water quality and
#' biogeochemical modules that couple to GLM. The modules included in this
#' dataset cover dissolved oxygen, inorganic nitrogen (ammonium, nitrate,
#' nitrification, denitrification), inorganic phosphorus (phosphate,
#' adsorption), silica, phytoplankton (growth, light, nutrient uptake,
#' respiration, mortality, settling), and sediment biogeochemistry (static
#' sediment fluxes via \code{aed_sedflux} and dynamic diagenesis via
#' \code{aed_seddiagenesis}).
#'
#' Note that \code{aed_phytoplankton} parameters listed under groups such as
#' \code{"growth"}, \code{"light"}, \code{"nitrogen"}, \code{"phosphorus"},
#' and \code{"silica"} are per-phytoplankton-group parameters typically
#' specified in a separate \code{phyto_data} namelist file rather than
#' directly in \code{aed.nml}.
#'
#' @source
#' GLM configuration reference:
#' \url{https://aquatic.science.uwa.edu.au/research/models/GLM/configuration.html}
#'
#' AED Science Manual:
#' \url{https://aquaticecodynamics.github.io/aed-science/}
#'
#' Hipsey, M.R., Bruce, L.C., Boon, C., Busch, B., Carey, C.C., Hamilton,
#' D.P., Hanson, P.C., Read, J.S., de Sousa, E., Weber, M. and Winslow, L.A.
#' (2019). A General Lake Model (GLM 3.0) for linking with high-frequency
#' sensor data from the Global Lake Ecological Observatory Network (GLEON).
#' \emph{Geoscientific Model Development}, 12, 473--523.
#' \doi{10.5194/gmd-12-473-2019}
#'
#' Hipsey, M.R. (Ed.) (2022). Modelling Aquatic Eco-Dynamics: Overview of the
#' AED modular simulation platform. Zenodo.
#' \doi{10.5281/zenodo.6516222}
#'
#' @examples
#' # Load the parameter library
#' data(glm_aed_parameter_library)
#'
#' # View the structure
#' str(glm_aed_parameter_library)
#'
#' # List all unique modules
#' unique(glm_aed_parameter_library$module)
#'
#' # Filter to a specific module
#' glm_aed_parameter_library[glm_aed_parameter_library$module == "aed_oxygen", ]
#'
#' # Get all phytoplankton growth parameters
#' glm_aed_parameter_library[glm_aed_parameter_library$module == "aed_phytoplankton" &
#'                           glm_aed_parameter_library$group == "growth", ]
#'
#' # Count parameters per module
#' table(glm_aed_parameter_library$module)
#'
"glm_aed_parameter_library"

#' Simstrat-AED2 parameter library
#'
#' A comprehensive reference table of parameters used in the Simstrat-AED2
#' configuration, including their default (template) values, units, and a
#' brief description of each parameter's role.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{module}{The model module the parameter belongs to.}
#'   \item{group}{A functional grouping of the parameter within the module.}
#'   \item{parameter}{The parameter's name as it appears in the configuration
#'   file (\code{simstrat.par} or \code{aed2.nml}).}
#'   \item{label}{A short human-readable label for the parameter.}
#'   \item{symbol}{A mathematical symbol for the parameter, where applicable.}
#'   \item{description}{A description of the parameter's role in the model.}
#'   \item{units}{The units of the parameter.}
#'   \item{default}{The default (template) value of the parameter.}
#'   \item{source}{A URL for further information about the parameter.}
#' }
#'
#' The physical (hydrodynamic) parameters configure Simstrat's seiche
#' (internal wave) energy, wind drag, bottom drag, surface heat fluxes,
#' light absorption, and ice/snow dynamics -- described in Table 1 of the
#' Simstrat User Manual. Default values are those in AEME's bundled
#' \code{inst/extdata/simstrat_aed2/simstrat.par} template.
#'
#' The AED2 (Aquatic EcoDynamics) parameters configure the biogeochemical
#' modules coupled to Simstrat: oxygen, carbon (dissolved inorganic carbon,
#' pH, methane), silica, inorganic nitrogen, inorganic phosphorus, organic
#' matter, phytoplankton, and zooplankton. Simstrat-AED2 and GLM-AED couple
#' to the same underlying AED2 library, so most module/parameter
#' descriptions here are shared with \code{\link{glm_aed_parameter_library}}
#' (only the module name prefix differs: \code{"aed2_"} here vs.
#' \code{"aed_"} there, matching each model's actual configuration file).
#' The carbon module has no AED (v1) analogue and is documented directly from
#' AEME's bundled \code{aed2.nml} template.
#'
#' As with \code{glm_aed_parameter_library}, \code{aed2_phytoplankton} and
#' \code{aed2_zooplankton} parameters listed under groups such as
#' \code{"growth"}, \code{"light"}, \code{"nitrogen"}, \code{"phosphorus"},
#' and \code{"silica"} are per-group parameters specified in the separate
#' \code{aed2_phyto_pars.nml}/\code{aed2_zoop_pars.nml} files rather than
#' directly in \code{aed2.nml}.
#'
#' @source
#' Simstrat User Manual and source code:
#' \url{https://github.com/Eawag-AppliedSystemAnalysis/Simstrat}
#'
#' AED Science Manual:
#' \url{https://aquaticecodynamics.github.io/aed-science/}
#'
#' Goudsmit, G-H., Burchard, H., Peeters, F., and Wuest, A. (2002).
#' Application of k-epsilon turbulence models to enclosed basins: The role of
#' internal seiches. \emph{Journal of Geophysical Research: Oceans}, 107(C12),
#' 23-1--23-13.
#'
#' @examples
#' # Load the parameter library
#' data(simstrat_aed2_parameter_library)
#'
#' # View the structure
#' str(simstrat_aed2_parameter_library)
#'
#' # List all unique modules
#' unique(simstrat_aed2_parameter_library$module)
#'
#' # Filter to the physical (hydrodynamic) parameters
#' simstrat_aed2_parameter_library[simstrat_aed2_parameter_library$module ==
#'                                 "simstrat", ]
#'
#' # Count parameters per module
#' table(simstrat_aed2_parameter_library$module)
#'
"simstrat_aed2_parameter_library"
