#' Get a configuration value from the aeme object. If the key is not present, 
#' return the default value.
#'
#' @inheritParams build_aeme
#' @param key The name of the configuration value to retrieve.
#' @param default The default value to return if the key is not present in the
#' configuration. If NULL, the default value from config_defaults() will be used.
#' If the key is not present in config_defaults(), NULL will be returned.
#'
#' @returns The value of the configuration key, or the default value if the key 
#' is not present in the configuration.
#' @export

get_config_value <- function(aeme, key, default = NULL) {
  cfg_dflt <- config_defaults()
  
  aeme <- check_aeme(aeme)
  cfg <- aeme |>
    configuration()
  
  value <- cfg[[key]]
  if (!is.null(value)) return(value)
  if (!is.null(default)) return(default)
  cfg_dflt[[key]]  # NULL if key doesn't exist, which is fine
}
