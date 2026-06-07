#' Make parameters dataframe for AEME
#'
#' @inheritParams build_aeme
#' @param module character; vector of modules to retrieve parameters for.
#' @param file character; vector of file names to retrieve.
#' @param name character; vector of parameter names to retrieve.
#' @param par character; vector of parameters to retrieve.
#'
#' @return dataframe; of parameters filtered by model, module and parameter.
#' @export
#'
#' @importFrom utils data
#' @importFrom dplyr bind_rows filter
#'

get_aeme_parameters <- function(model, file, module, name, par) {

  accept_model <- c("dy_cd", "glm_aed", "gotm_wet")

  if (missing(model)) {
    model <- accept_model
  } else {
    model <- check_model(model = model)
  }

  # Load parameters and combine
  data("dy_cd_parameters", package = "AEME", envir = environment())
  data("gotm_wet_parameters", package = "AEME", envir = environment())
  data("glm_aed_parameters", package = "AEME", envir = environment())
  all_param <- dplyr::bind_rows(dy_cd_parameters, gotm_wet_parameters, 
                                glm_aed_parameters)
  accept_module <- unique(all_param$module)

  if (missing(module)) {
    module <- c(".*")
  } else {
    if (any(!module %in% accept_module)) {
      cli::cli_abort("Module not found! Please check the spelling of the module,
           Possible modules are: {.val {accept_module}}")
    }
  }
  if (missing(par)) {
    par <- c(".*")
  }
  if (missing(name)) {
    name <- c(".*")
  }
  if (missing(file)) {
    file <- c(".*")
  } else {
    accept_file <- unique(all_param$file)
    if (any(!file %in% accept_file)) {
      cli::cli_abort("File not found! Please check the spelling of the file,
           Possible files are: {.val {accept_file}}")
    }
  }

  sel_model <- model
  sel_module <- paste0(module, collapse = "|")
  sel_name <- paste0(name, collapse = "|")
  sel_par <- paste0(par, collapse = "|")
  sel_file <- paste0(file, collapse = "|")

  # Filter parameters
  sel_param <- all_param |>
    dplyr::mutate(
      par = dplyr::if_else(is.na(par), "", par)
    ) |> 
    dplyr::filter(model %in% sel_model,
                  grepl(sel_module, module),
                  grepl(sel_name, name),
                  grepl(sel_par, par),
                  grepl(sel_file, file)
                  ) |> 
    dplyr::mutate(
      par = dplyr::if_else(par == "", NA, par)
    )

  if (nrow(sel_param) == 0) {
    pot_pars <- agrep(sel_par, all_param$par, value = TRUE)
    cli::cli_abort("No parameters found! Please check the spelling of the input,
         Possible parameters are: {.val {pot_pars}}")
  }

  return(sel_param)
}
