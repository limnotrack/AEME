#' Generate GLM Sediment Parameters
#'
#' @param n_zones Number of sediment zones to simulate. Default is 1.
#' @param sed_heat_Ksoil Heat conductivity of soil/sediment. Default is 0.01.
#' @param sed_temp_depth Depth of soil/sediment layer below the lake bottom, 
#' used for heat flux calculation. Default is 0.2.
#' @param sed_temp_mean Annual mean sediment temperature. Default is 12.
#' @param sed_temp_amplitude Amplitude of temperature variation experienced in 
#' the sediment over one year. Default is 8.
#' @param sed_temp_peak_doy Day of the year where the sediment temperature 
#' peaks. Default is 30.
#' @param benthic_mode Switch to configure which mode of benthic interaction to
#'  apply. Options are:
#'   \itemize{
#'    \item 0: Bottom layer only
#'    \item 1: Bottom layer & layer flanks
#'    \item 2: Sediment zones with individual properties (default)
#'   }
#' @param zone_heights Upper height of zone boundarys (m). Length must equal 
#' 'n_zones'. 0 is lake bottom. Default is c(10) for n_zones = 1.
#' @param sed_reflectivity Sediment reflectivity. Default is 0.01.
#' @param sed_roughness Sediment roughness. Default is 0.01.
#'
#' @returns Data frame of GLM sediment parameters
#' @export
#'
#' @examples
#' # Generate sediment parameters for 1 zone
#' sed_params_1zone <- generate_glm_sed_params(n_zones = 1)
#' print(sed_params_1zone)
#' #' # Generate sediment parameters for 3 zones
#' sed_params_3zones <- generate_glm_sed_params(
#'   n_zones = 3,
#'   zone_heights = c(5, 15, 20),
#'   sed_temp_mean = c(10, 12, 14)
#' )
#' print(sed_params_3zones)

glm_sed_params <- function(n_zones = 1,
                           zone_heights = c(10),
                           sed_heat_Ksoil = 0.01,
                           sed_temp_depth = 0.2,
                           sed_temp_mean = c(12),
                           sed_temp_amplitude = c(8),
                           sed_temp_peak_doy = c(30),
                           sed_reflectivity = c(0.01),
                           sed_roughness = c(0.01),
                           benthic_mode = 2) {
  # --- Checks ---
  if (length(zone_heights) != n_zones)
    stop("Length of 'zone_heights' must equal 'n_zones'.", call. = FALSE)
  if (any(diff(zone_heights) <= 0))
    stop("'zone_heights' must be strictly increasing.", call. = FALSE)
  
  # Helper: force length to n_zones
  force_len <- function(x, n) rep_len(x, n)
  
  sed_heat_Ksoil     <- force_len(sed_heat_Ksoil, n_zones)
  sed_temp_depth     <- force_len(sed_temp_depth, n_zones)
  sed_temp_mean      <- force_len(sed_temp_mean, n_zones)
  sed_temp_amplitude <- force_len(sed_temp_amplitude, n_zones)
  sed_temp_peak_doy  <- force_len(sed_temp_peak_doy, n_zones)
  zone_heights       <- force_len(zone_heights, n_zones)
  sed_reflectivity   <- force_len(sed_reflectivity, n_zones)
  sed_roughness      <- force_len(sed_roughness, n_zones)
  
  # Helper: calculate min/max
  min_max <- function(val, special = NULL) {
    if (!is.null(special)) {
      min_val <- special$min
      max_val <- special$max
    } else if (is.numeric(val)) {
      min_val <- val * 0.5
      max_val <- val * 1.5
    } else {
      min_val <- NA_real_
      max_val <- NA_real_
    }
    return(list(min = min_val, max = max_val))
  }
  
  # Zone-dependent parameters
  zone_param_names <- c("sediment/sed_heat_Ksoil",
                        "sediment/sed_temp_depth",
                        "sediment/sed_temp_mean",
                        "sediment/sed_temp_amplitude",
                        "sediment/sed_temp_peak_doy",
                        "sediment/zone_heights",
                        "sediment/sed_reflectivity",
                        "sediment/sed_roughness")
  
  values_list <- list(sed_heat_Ksoil,
                      sed_temp_depth,
                      sed_temp_mean,
                      sed_temp_amplitude,
                      sed_temp_peak_doy,
                      zone_heights,
                      sed_reflectivity,
                      sed_roughness)
  
  # Flatten values
  values_vec <- unlist(values_list, use.names = FALSE)
  name_vec <- rep(zone_param_names, each = n_zones)
  index_vec <- rep(seq_len(n_zones), times = length(zone_param_names))
  
  # Calculate min/max
  min_vec <- numeric(length(values_vec))
  max_vec <- numeric(length(values_vec))
  
  for (i in seq_along(values_vec)) {
    name_i <- name_vec[i]
    val <- values_vec[i]
    
    if (name_i == "sediment/sed_temp_peak_doy") {
      min_vec[i] <- max(1, floor(val * 0.5))
      max_vec[i] <- ceiling(val * 1.5)
    } else {
      min_vec[i] <- val * 0.5
      max_vec[i] <- val * 1.5
    }
  }
  
  multi_params <- data.frame(
    model = "glm_aed",
    file  = "glm3.nml",
    name  = name_vec,
    value = values_vec,
    min   = min_vec,
    max   = max_vec,
    group = "sediment",
    index = index_vec,
    stringsAsFactors = FALSE
  )
  
  # Global parameters: benthic_mode and n_zones
  single_params <- data.frame(
    model = "glm_aed",
    file  = "glm3.nml",
    name  = c("sediment/benthic_mode", "sediment/n_zones"),
    value = c(benthic_mode, n_zones),
    min   = c(benthic_mode, n_zones),
    max   = c(benthic_mode, n_zones),
    group = "sediment",
    index = c(NA_integer_, NA_integer_),
    stringsAsFactors = FALSE
  )
  
  params <- rbind(single_params, multi_params)
  return(params)
}
