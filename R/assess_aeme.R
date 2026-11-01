#' Assess model performance
#'
#' @inheritParams build_aeme
#' @inheritParams plot_output
#'
#' @return Data frame with model performance statistics for each model and
#' variable. These include:
#' \itemize{
#' \item{bias}{ - Bias}
#' \item{mae}{ - Mean absolute error}
#' \item{rmse}{ - Root mean square error}
#' \item{nmae}{ - Normalised mean absolute error}
#' \item{nse}{ - Nash-Sutcliffe efficiency}
#' \item{d2}{ - Index of agreement model skill score Willmott index}
#' \item{r}{ - Pearson correlation coefficient}
#' \item{rs}{ - Spearman correlation coefficient}
#' \item{r2}{ - R-squared value from linear model}
#' \item{B}{ - Bardsley coefficient}
#' \item{n}{ - number of observations}
#' }
#'
#' @importFrom dplyr group_by summarise mutate n case_when where across
#' @importFrom dplyr relocate filter left_join select last_col bind_rows 
#' @importFrom dplyr case_when
#' @importFrom stats cor cor.test lm
#'
#' @export
#'

assess_aeme <- function(aeme, model, var_sim) {
  
  # Check aeme is Aeme object
  aeme <- check_aeme(aeme)
  # Check model is valid
  if (missing(model)) {
    model <- list_models(aeme = aeme)
  } else {
    model <- check_model(model = model)
  }
  if (missing(var_sim)) {
    var_sim <- get_mod_obs_vars(aeme = aeme, model = model) |> 
      dplyr::pull(var_aeme)
    if (length(var_sim) == 0) {
      cli::cli_alert_warning(c("No variables found in model output.",
                       "Make sure to set {.code simulate = TRUE} in the 
                       {.code model_controls} for selected variables when 
                       executing {.code run_aeme}. Use the {.code set_vars_sim()} 
                       to set variables to simulate."))
      return(NULL)
    }
  }
  # Check model is in aeme
  var_sim <- check_aeme_vars(var_sim, aeme = aeme)
  
  data("key_naming", package = "AEME", envir = environment())
  var_name <- key_naming |> 
    dplyr::select(var_aeme, name_text)
  
  # Extract observations
  obs <- observations(aeme)
  
  # Loop through variables, extract model statistics, bind to dataframe and
  # return
  out <- lapply(var_sim, \(v) {

    # Extract variable from aeme
    df <- get_var(aeme = aeme, model = model, var_sim = v, use_obs = TRUE) |>
      dplyr::filter(is.finite(value))
    
    if (nrow(df) == 0) {
      return(NULL)
    }
    
    # Calculate statistics for each model
    df |>
      # dplyr::mutate(Residual = sim - obs) |>
      dplyr::mutate(var_aeme = var_sim) |>
      dplyr::group_by(Model, var_aeme) |>
      dplyr::summarise(
        bias = bias(obs, value),
        mae = mae(obs, value),
        rmse = rmse(obs, value),
        nmae = nmae(obs, value),
        nse = nse(obs, value),
        kge = kge(obs, value),
        d2 = d2(obs, value),
        r = r_pearson(obs, value),
        rs = r_spearman(obs, value),
        B = bardsley(obs, value),
        n = dplyr::n(),
        obs_na = sum(is.na(obs)),
        sim_na = sum(is.na(value)),
        .groups = "drop"
      ) |>
      as.data.frame() |> 
      # Round all columns with with numeric values to 3 decimal places
      dplyr::mutate(
        dplyr::across(bias:B, \(x) ifelse(is.na(x), NA, signif(x, 3))),
      ) |>
      dplyr::relocate(c(n, obs_na, sim_na), .after = dplyr::last_col())
    
  }) |>
    dplyr::bind_rows()  # Bind list of data frames into one data frame and return
  
  if (nrow(out) > 0) {
    out <- out |> 
      dplyr::left_join(var_name, by = "var_aeme") |> 
      dplyr::relocate(c(name_text), .after = Model) |> 
      dplyr::arrange(Model, var_aeme)
  }
  
  return(out)
}

#' @rdname assess_aeme
#' @aliases assess_model
#' @export
assess_model <- assess_aeme
