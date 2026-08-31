#' Write a lake bathymetry to a GLM simulation
#'
#' @inheritParams set_nml
#' @param lakename string; name of lake
#' @param bathy data.frame; containing hypsograph
#' @param lat numeric; latitude of lake
#' @param lon numeric; longitude of lake
#' @param crest numeric; height of crest of lake
#' @param dims_lake numeric vector of length 2; containing basin length and
#' width.
#' @param update_sediment Logical; update the sediment block in the nml object?
#' @param use_bgc Logical; is the biogeochemistry (AED) library active? When
#'   `FALSE`, `sed_heat_model` is forced to 1 because GLM's dynamic
#'   soil-temperature solver (`sed_heat_model = 2`) requires an active WQ
#'   module and aborts without one.
#' @param obs_temp data.frame or `NULL`; observed water-column temperature
#'   profiles in the long AEME format. When supplied, per-zone
#'   sediment-temperature parameters are derived from it via `calc_sed_temp()`.
#' @param nml_file character; name of the GLM nml file, forwarded to
#'   `calc_sed_temp()` for the `file` column of its AEME parameter table.
#'
#' @return updated nml object
#' @noRd

make_stg_glm <- function(glm_nml, lakename, bathy, lat, lon, dims_lake, crest,
                        update_sediment = TRUE, use_bgc = TRUE,
                        obs_temp = NULL, nml_file = "glm4.nml") {

  bathy_glm <- bathy |>
    dplyr::arrange(elev)

  # find the area at the surface height
  len <- dims_lake[1]
  wid <- dims_lake[2]

  max_depth <- max(bathy_glm$elev) - min(bathy_glm$elev)
  sub_layers <- get_model_layers(depth = max_depth)
  min_layer_thick <- min(sub_layers$h)
  max_layer_thick <- max(sub_layers$h)
  max_layers <- ceiling(max_depth / min_layer_thick) + 10

  arg_list <- list(max_layers = max_layers,
                   min_layer_vol = 0.025,
                   min_layer_thick = min_layer_thick,
                   max_layer_thick = max_layer_thick,
                   crest_elev = crest,
                   density_model = 1, non_avg = TRUE,
                   lake_name = lakename, latitude = lat,
                   longitude = lon,
                   base_elev = min(bathy_glm$elev),
                   bsn_len = len, bsn_wid = wid, bsn_vals = nrow(bathy_glm),
                   H = bathy_glm$elev, A = bathy_glm$area)

  glm_nml <- set_nml(glm_nml = glm_nml, arg_list = arg_list)

  if (update_sediment) {
    sed_zones <- estimate_sed_zones(hypsograph = bathy)
    n_zones <- length(sed_zones)

    # Per-zone sediment-temperature cycle (zone 1 = deepest). When observed
    # water-column temperatures are supplied, fit an annual harmonic per zone
    # via calc_sed_temp(); otherwise fall back to generic defaults.
    sed_temp <- list(
      mean      = rep(10, n_zones),
      amplitude = rep(4, n_zones),
      peak_doy  = rep(10L, n_zones)
    )
    if (!is.null(obs_temp) && nrow(obs_temp) > 0) {
      est <- tryCatch(
        calc_sed_temp(obs_temp = obs_temp, sed_zones = sed_zones,
                      max_depth = max_depth, hypsograph = bathy,
                      nml_file = nml_file, output = "nml", verbose = FALSE),
        error = function(e) {
          cli_inform_safe(c(
            "!" = "Could not estimate sediment temperatures from observations \\
                   ({conditionMessage(e)}); using defaults."
          ))
          NULL
        }
      )
      if (!is.null(est)) {
        sed_temp$mean      <- est$sed_temp_mean
        sed_temp$amplitude <- est$sed_temp_amplitude
        sed_temp$peak_doy  <- est$sed_temp_peak_doy
      }
    }

    # Zone geometry and per-zone parameters AEME derives from the bathymetry.
    managed <- list(
      sed_heat_Ksoil = rep(1.2, n_zones),
      sed_temp_depth = rep(0.2, n_zones),
      sed_temp_mean = sed_temp$mean,
      sed_temp_amplitude = sed_temp$amplitude,
      sed_temp_peak_doy = sed_temp$peak_doy,
      benthic_mode = 2,
      n_zones = n_zones,
      zone_heights = sed_zones,
      sed_reflectivity = rep(0.1, n_zones),
      sed_roughness = rep(0.1, n_zones)
    )

    # Merge rather than replace: keep any other keys already in the &sediment
    # block. In particular this preserves the expanded GLMv4 soil-column heat
    # model settings (sed_heat_model, n_sed_layers, sed_layer_depth, sed_vwc,
    # sed_spinup_days, sed_deep_temp, ...) that a glm4.nml template carries.
    sediment <- glm_nml[["sediment"]]
    if (is.null(sediment)) sediment <- list()

    # sed_heat_model = 2 (dynamic soil-column solver, zZSoilTemp) is provided
    # by the WQ library, so GLM aborts with it enabled when no WQ module is
    # active. Fall back to the analytical model (1) when biogeochemistry is
    # off. A glm4.nml template ships with sed_heat_model = 2.
    heat_model <- suppressWarnings(as.numeric(sediment[["sed_heat_model"]]))
    if (!isTRUE(use_bgc) && length(heat_model) == 1 && !is.na(heat_model) &&
        heat_model != 1) {
      cli_inform_safe(c(
        "!" = "Forcing {.field sed_heat_model} from {heat_model} to 1: \\
               {.field sed_heat_model = 2} needs an active WQ module and \\
               {.arg use_bgc} is {.val {FALSE}}."
      ))
      sediment[["sed_heat_model"]] <- 1
      heat_model <- 1
    }

    # Under the dynamic soil-column heat model (sed_heat_model = 2) GLM reads
    # sed_heat_Ksoil / sed_temp_depth as scalars, not per-zone vectors, so
    # leave whatever the template set rather than expanding them (mirrors the
    # length check in check_glm_nml()).
    if (length(heat_model) == 1 && !is.na(heat_model) && heat_model != 1) {
      managed[["sed_heat_Ksoil"]] <- NULL
      managed[["sed_temp_depth"]] <- NULL
    }

    sediment[names(managed)] <- managed
    glm_nml[["sediment"]] <- sediment
  }

  return(glm_nml)
}
