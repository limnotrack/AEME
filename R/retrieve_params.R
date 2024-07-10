#' Retrieve parameters for each model
#'
#' @inheritParams build_aeme
#' @param module character; vector of modules to retrieve parameters for.
#' @param par character; vector of parameters to retrieve.
#'
#' @return dataframe; of parameters filtered by model, module and parameter.
#' @export
#'
#' @importFrom utils data
#' @importFrom dplyr bind_rows filter
#'

retrieve_params <- function(model, module, name, par) {

  accept_model <- c("dy_cd", "glm_aed", "gotm_wet")

  if (missing(model)) {
    model <- accept_model
  } else {
    if (any(!model %in% accept_model)) {
      stop("Model not found! Please check the spelling of the model. \n",
           "Possible models are: ", paste0(accept_model, collapse = ", "))
    }
  }

  # Load parameters and combine
  utils::data("gotm_wet_parameters", package = "AEME", envir = environment())
  utils::data("glm_aed_parameters", package = "AEME", envir = environment())
  all_param <- dplyr::bind_rows(gotm_wet_parameters, glm_aed_parameters)
  accept_module <- unique(all_param$module)

  if (missing(module)) {
    module <- c(".*")
  } else {
    if (any(!module %in% accept_module)) {
      stop("Module not found! Please check the spelling of the module. \n",
           "Possible modules are: ", paste0(accept_module, collapse = ", "))
    }
  }
  if (missing(par)) {
    par <- c(".*")
  }
  if (missing(name)) {
    name <- c(".*")
  }

  sel_model <- model
  sel_module <- paste0(module, collapse = "|")
  sel_name <- paste0(name, collapse = "|")
  sel_par <- paste0(par, collapse = "|")

  all_param |>
    dplyr::filter(module == "light")

  sum(grepl(sel_par, all_param$par))

  # Filter parameters
  sel_param <- all_param |>
    dplyr::filter(model %in% sel_model,
                  grepl(sel_module, module),
                  grepl(sel_name, name),
                  grepl(sel_par, par)
                  )

  # model_pars <- all_param |>
  #   dplyr::filter(model %in% sel_model)
  # module_pars <- all_param |>
  #   dplyr::filter(grepl(sel_module, module))
  # name_pars <- all_param |>
  #   dplyr::filter(grepl(sel_name, name))
  # par_pars <- all_param |>
  #   dplyr::filter(grepl(sel_par, par))

  if (nrow(sel_param) == 0) {
    pot_pars <- agrep(sel_par, all_param$par, value = TRUE)
    stop("No parameters found! Please check the spelling of the parameters. \n",
         "Possible parameters are: ", paste0(pot_pars, collapse = ", "))
  }

  return(sel_param)
}
