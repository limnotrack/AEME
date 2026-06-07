#' Get AED sed_const2d parameters
#'
#' @inheritParams build_aeme
#'
#' @returns Data frame with AED sed_const2d parameters
#' @export
#'

get_aed_sed_const2d_param <- function(aeme, path, lake_dir = NULL) {
  n_zones <- get_glm_sed_zones(aeme = aeme, path = path, lake_dir = lake_dir)
  
  sed_param <- get_aeme_parameters(model = "glm_aed", file = "aed.nml", 
                                   module = "sed_const2d")
  
  sed_param_no_idx <- sed_param |> 
    dplyr::filter(is.na(index)) |> 
    dplyr::mutate(
      value = dplyr::case_when(
        grepl("aed_sed_const2d/n_zones", name) ~ n_zones, .default = value
      ),
      min = dplyr::case_when(
        grepl("aed_sed_const2d/n_zones", name) ~ n_zones, .default = min
      ),
      max = dplyr::case_when(
        grepl("aed_sed_const2d/n_zones", name) ~ n_zones, .default = max
      )
    )
  
  sed_param_idx <- sed_param |> 
    dplyr::filter(!is.na(index))
  
  # duplicate chunk for each zone
  sed_param_expanded <- lapply(1:n_zones, function(z) {
    sed_param_idx |> 
      dplyr::mutate(
        value = dplyr::case_when(
          grepl("aed_sed_const2d/active_zones", name) ~ z, .default = value
        ),
        min = dplyr::case_when(
          grepl("aed_sed_const2d/active_zones", name) ~ z, .default = min
        ),
        max = dplyr::case_when(
          grepl("aed_sed_const2d/active_zones", name) ~ z, .default = max
        ),
        index = index + (z - 1) * max(sed_param_idx$index),
                    zone = z)
  }) |> 
    dplyr::bind_rows() |> 
    dplyr::select(-zone)
  
  sed_param_final <- dplyr::bind_rows(sed_param_no_idx, sed_param_expanded) |> 
    dplyr::arrange(name, index)
  return(sed_param_final)
} 
