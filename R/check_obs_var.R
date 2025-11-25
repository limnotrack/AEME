#' Check if a variable is present in the observations for the model time period
#'
#' @inheritParams get_var
#'
#' @export
#'
#' @return A list with two elements:
#' \itemize{
#' \item \code{obs}: A data frame with the following columns:
#' \itemize{
#' \item \code{model}: Model name
#' \item \code{var_aeme}: Variable name
#' \item \code{n}: Number of observations
#' }
#' \item \code{vars_present}: A character vector of variables present in the
#' observations
#' }
#'
#' @importFrom dplyr filter bind_rows pull
#' @importFrom lubridate ddays
#' @importFrom cli cli_alert_danger cli_text cli_alert_success
#'

check_obs_var <- function(aeme, var_sim) {
  aeme    <- check_aeme(aeme)
  var_sim <- check_aeme_vars(var_sim)
  
  obs     <- observations(aeme)
  tme     <- time(aeme)
  cfg     <- configuration(aeme)
  
  # Get only the models defined in list_models()
  valid_models <- list_models()
  model <- intersect(names(cfg), valid_models)
  
  # Keep only models that have a hydrodynamic component
  model <- model[sapply(model, function(m) !is.null(cfg[[m]][["hydrodynamic"]]))]
  model  <- check_model(model)
  
  out <- lapply(model, function(m) {
    start <- as.Date(tme$start) - lubridate::ddays(tme$spin_up[[m]])
    stop  <- as.Date(tme$stop)
    
    obs_lake <- dplyr::filter(obs$lake, Date >= start & Date <= stop)
    
    d <- lapply(var_sim, function(v) {
      n_obs <- sum(obs_lake$var_aeme == v, na.rm = TRUE)
      data.frame(model = m, var_aeme = v, n = n_obs, stringsAsFactors = FALSE)
    }) |>
      dplyr::bind_rows()
    
    d
  }) |>
    dplyr::bind_rows()
  
  vars_present <- out |>
    dplyr::filter(n > 0) |>
    dplyr::pull(var_aeme) |>
    unique()
  
  # --- Report missing variables ---
  out_missing <- out |>
    dplyr::filter(n == 0)
  
  if (nrow(out_missing) > 0) {
    cli::cli_alert_danger("Some variables are missing in observations:")
    for (i in seq_len(nrow(out_missing))) {
      cli::cli_text("{.emph {out_missing$model[i]}}: Variable {.var {out_missing$var_aeme[i]}} has 0 observations")
    }
  } else {
    cli::cli_alert_success("All requested variables are present in observations for all models.")
  }
  
  list(obs = out, vars_present = vars_present)
}
