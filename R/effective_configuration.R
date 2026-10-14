#' Model configuration with the model parameters applied
#'
#' `configuration(aeme)` holds each model's configuration as built, before
#' any model parameters are applied, and `parameters(aeme)` is the table of
#' updates (e.g. calibrated values) applied on top of it when the model files
#' are written. `effective_configuration()` combines the two: it returns the
#' configuration the model files will contain, without changing `aeme`.
#'
#' Only parameters that set values in a model's configuration files are
#' applied. Parameters that scale the meteorology, inflows or outflows
#' (`file` of `"met"`, `"inf"` or `"wdr"`) act on the boundary-condition data
#' rather than the configuration, so are not reflected here. Parameters for a
#' file a model's configuration does not have (e.g. bgc parameters for a model
#' built without bgc) are skipped with a warning.
#'
#' @inheritParams build_aeme
#' @param model character vector; models to return. Defaults to all models in
#'   `aeme`.
#'
#' @return A list with the same structure as `configuration(aeme)`, with the
#'   parameters applied to each model's `hydrodynamic` and `bgc` elements.
#' @export
#'
#' @seealso [parameters()], [configuration()], [write_configuration()]
#'
#' @examples
#' \dontrun{
#' eff <- effective_configuration(aeme, model = "glm_aed")
#' eff$glm_aed$hydrodynamic$light$Kw
#' }

effective_configuration <- function(aeme, model) {
  aeme <- check_aeme(aeme)
  model <- if (missing(model)) list_models(aeme) else check_model(model)
  cfg <- configuration(aeme)
  param <- usable_parameters(aeme)
  if (is.null(param)) return(cfg)

  for (m in intersect(model, unique(param$model))) {
    if (is.null(cfg[[m]])) next
    p_m <- param[param$model == m, , drop = FALSE]
    if (!any(!p_m$file %in% c("met", "inf", "wdr"))) next
    cfg[[m]] <- apply_parameters(cfg[[m]], p_m, m, strict = FALSE)$config
  }
  cfg
}
