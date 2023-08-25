#' Set the simulation duration for glm
#'
#' @param date_range date vector of length 2
#' @param glm_nml
#'
#' @return GLM nml list object with updated dates
#' @noRd

daterange_GLM <-  function(date_range, glm_nml) {

  arg_list <- list(timefmt = 2, start = paste0(date_range[1]," 00:00:00"),
                   stop = paste0(date_range[2]," 00:00:00"))

  glm_nml <- set_nml(glm_nml = glm_nml, arg_list = arg_list)
  return(glm_nml)
}
