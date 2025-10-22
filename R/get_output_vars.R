#' Get the output variables from an AEME object
#'
#' @inheritParams build_aeme
#' @inheritParams run_aeme
#'
#' @return A character vector of the output variables
#' @export
#'
#' @examples
#' aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
#' aeme <- readRDS(aeme_file)
#' path <- tempdir()
#' model_controls <- get_model_controls(use_bgc = TRUE)
#' model <- c("glm_aed", "gotm_wet")
#' aeme <- build_aeme(path = path, aeme = aeme, model = model,
#'                    model_controls = model_controls,
#'                    ext_elev = 5, use_bgc = TRUE)
#' # Run models
#' aeme <- run_aeme(aeme = aeme, model = model, verbose = FALSE,
#' path = path, model_controls = model_controls,
#' parallel = TRUE, ncores = 2L)
#' get_output_vars(aeme, model)

get_output_vars <- function(aeme, model, ens_n = 1) {

  outp <- output(aeme)
  if (missing(model)) {
    model <- list_models(aeme)
  }
  if (outp$n_members == 0) return()
  ens_lab <- format_ens_label(ens_n = ens_n)
  out <- outp[[ens_lab]]

  # Loop through the variables and get the ones that are not all -99
  out_vars <- c()
  for (m in model) {
    mod_vars <- names(out[[m]])
    for (i in 1:length(mod_vars)) {
      v <- out[[m]][[mod_vars[i]]]
      v[is.na(v)] <- -99
      if (!all(v == -99)) {
        out_vars <- c(out_vars, mod_vars[i])
      }
    }
  }
  out_vars <- unique(out_vars)
  utils::data("key_naming", package = "AEME")
  out_var_names <- key_naming$name_text[match(out_vars, key_naming$name)]
  nmes <- setNames(out_vars, out_var_names)
  nmes <- nmes[!is.na(nmes)]
  nmes <- nmes[!is.na(names(nmes)) & names(nmes) != ""]
  nmes <- nmes[-1]

  # order variables with target variables first
  tgt_vars <- c("HYD_temp", "HYD_thmcln", "CHM_oxy", "PHY_tchla",
                "NIT_tn", "PHS_tp")

  nmes <- nmes[order(match(nmes, tgt_vars))]

  return(nmes)
}
