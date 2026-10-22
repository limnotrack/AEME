#' Parameters for scaling or setting inflow variables
#'
#' Builds parameter rows that change an inflow variable such as DOC during
#' calibration. A multiplier (`file = "inf"`) scales the variable in every
#' inflow that has it; a constant (`file = "inf_const"`) replaces it with a
#' single value. Flow is scaled with the existing `inflow` parameter in
#' [aeme_parameters].
#'
#' Scaling the flow also scales the load of every variable, but leaves its
#' concentration unchanged. These rows change the concentration independently.
#' Constants are in the units of the inflow data held in the Aeme object (e.g.
#' g/m3 for `CAR_doc`); unit conversions to each model happen afterwards.
#'
#' @param name character; inflow variable(s), e.g. `"CAR_doc"`.
#' @param value numeric; starting value (multiplier or constant).
#' @param min,max numeric; calibration bounds.
#' @param model character; model(s) to create rows for. Default is all five
#'   models.
#' @param constant logical; `TRUE` for constants (`inf_const`), `FALSE`
#'   (default) for multipliers (`inf`).
#'
#' @return data.frame with the columns of [aeme_parameters] that
#'   [input_model_parameters()] requires, to row-bind onto a parameter table.
#' @seealso [input_model_parameters()], [zone_ratio_param()]
#' @examples
#' # Scale DOC concentrations in the inflows by 0.5 to 2
#' inflow_param("CAR_doc", value = 1, min = 0.5, max = 2, model = "glm_aed")
#'
#' # Try a constant DOC concentration of 5 g/m3
#' inflow_param("CAR_doc", value = 5, min = 1, max = 15, model = "glm_aed",
#'              constant = TRUE)
#'
#' \dontrun{
#' # Run GLM-AED with the DOC in the inflows doubled and compare with the
#' # unscaled run. Inflow DOC only affects models that use inflow
#' # biogeochemistry (GLM-AED); GOTM-WET reads flow, temperature and salinity.
#' aeme_file <- system.file("extdata", "aeme.rds", package = "AEME")
#' aeme <- readRDS(aeme_file)
#' path <- tempdir()
#' model_controls <- get_model_controls(use_bgc = TRUE)
#' aeme <- build_aeme(path = path, aeme = aeme, model = "glm_aed",
#'                    model_controls = model_controls, ext_elev = 3,
#'                    use_bgc = TRUE)
#' base <- run_aeme(aeme, model = "glm_aed", path = path)
#'
#' # Write the scaled inflow files into the model directory and run again
#' p <- inflow_param("CAR_doc", value = 2, min = 0.5, max = 4,
#'                   model = "glm_aed")
#' aeme <- input_model_parameters(aeme, model = "glm_aed", param = p,
#'                                path = path)
#' scaled <- run_aeme(aeme, model = "glm_aed", path = path)
#'
#' # Surface DOC for both runs
#' doc <- rbind(
#'   transform(get_var(base, "glm_aed", "CHM_doc", depth = 0), run = "base"),
#'   transform(get_var(scaled, "glm_aed", "CHM_doc", depth = 0),
#'             run = "DOC x2")
#' )
#' boxplot(value ~ run, data = doc, ylab = "Surface DOC")
#' }
#' @export
inflow_param <- function(name, value = 1, min = 0.5, max = 2,
                         model = c("dy_cd", "glm_aed", "gotm_wet",
                                   "simstrat_aed2", "simstrat_aed"),
                         constant = FALSE) {
  stopifnot(is.character(name), is.numeric(value), is.numeric(min),
            is.numeric(max), is.logical(constant), length(constant) == 1)
  if (any(min > max) || any(value < min | value > max)) {
    stop("'value' must be between 'min' and 'max'.")
  }
  df <- expand.grid(model = model, name = name, stringsAsFactors = FALSE)
  n <- nrow(df)
  tibble::tibble(
    model = df$model, file = if (constant) "inf_const" else "inf",
    name = df$name, value = rep_len(value, n), min = rep_len(min, n),
    max = rep_len(max, n), group = NA_character_, index = NA_real_
  )
}
