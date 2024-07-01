#' Write a lake bathymetry to a GLM simulation
#'
#' @inheritParams set_nml
#' @param lakename string; name of lake
#' @param bathy data.frame; containing hypsograph
#' @param gps vector of length 2; containing latitude and longitude
#' @param crest numeric; height of crest of lake
#' @param dims_lake numeric vector of length 2; containing basin length and
#' width.
#' @param update_sediment Logical; update the sediment block in the nml object?
#'
#' @return updated nml object
#' @noRd

make_stgGLM <- function(glm_nml, lakename, bathy, gps, dims_lake, crest,
                        update_sediment = TRUE) {

  bathy_glm <- bathy |>
    dplyr::arrange(elev)

  # find the area at the surface height
  len <- dims_lake[1]
  wid <- dims_lake[2]

  if (max(bathy_glm[, 1]) < 3) {
    max_layer_thick <- 0.3
  } else {
    max_layer_thick <- 0.8
  }
  if (max(bathy_glm[, 1]) < 50) {
    max_layers <- 500
  } else {
    max_layers <- 1000
  }

  arg_list <- list(max_layers = max_layers, min_layer_vol = 0.025,
                   min_layer_thick = 0.1,
                   max_layer_thick = max_layer_thick,
                   crest_elev = crest,
                   density_model = 1, non_avg = TRUE,
                   lake_name = lakename, latitude = round(gps[1], 3),
                   longitude = round(gps[2], 3), base_elev = min(bathy_glm$elev),
                   bsn_len = len, bsn_wid = wid, bsn_vals = nrow(bathy_glm),
                   H = round(bathy_glm$elev, 2), A = round(bathy_glm$area, 0))

  glm_nml <- set_nml(glm_nml = glm_nml, arg_list = arg_list)

  if (update_sediment) {
    depth <- bathy$elev[(nrow(bathy)-1)] - min(bathy$elev)
    if(depth < 5) {
      sed_zones <- c(depth)
    } else {
      sed_zones <- c(round(depth / 2, 2), depth)
    }

    sediment <- list(
      sed_heat_Ksoil = rep(1.2, length(sed_zones)),
      sed_temp_depth = rep(0.2, length(sed_zones)),
      sed_temp_mean = rep(10, length(sed_zones)),
      sed_temp_amplitude = rep(4, length(sed_zones)),
      sed_temp_peak_doy = rep(10, length(sed_zones)),
      benthic_mode = 2,
      n_zones = length(sed_zones),
      zone_heights = sed_zones,
      sed_reflectivity = rep(0.1, length(sed_zones)),
      sed_roughness = rep(0.1, length(sed_zones))
    )
    glm_nml[["sediment"]] <- sediment
  }

  return(glm_nml)
}
