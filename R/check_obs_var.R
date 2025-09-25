#' Check if a variable is present in the observations for the model time period
#'
#' @inheritParams build_aeme
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

check_obs_var <- function(aeme, var_sim) {
  var_sim <- check_aeme_vars(var_sim)
  obs <- observations(aeme)
  tme <- time(aeme)
  cfg <- configuration(aeme)
  models <- c("dy_cd", "glm_aed", "gotm_wet")
  model <- sapply(models, \(m) {
    !is.null(cfg[[m]]$hydrodynamic)
  })
  model <- names(model)[model]

  out <- lapply(model, \(m) {
    start <- as.Date(tme$start) - tme$spin_up[[m]]
    stop <- as.Date(tme$stop)
    obs_lake <- obs$lake |>
      dplyr::filter(
        Date >= start & Date <= stop
      )
    d <- lapply(var_sim, \(v) {
      n_obs <- sum(obs_lake$var_aeme == v)
      message(paste0(m, ": ", v, ": ", n_obs, " observations"))
      data.frame(model = m, var_aeme = v, n = n_obs)
    }) |>
      dplyr::bind_rows()
    d
  }) |>
    dplyr::bind_rows()
  vars_present <- out |>
    dplyr::filter(n > 0) |>
    dplyr::pull(var_aeme) |>
    unique()

  return(list(obs = out, vars_present = vars_present))
}
