#' View AEME variables
#'
#' @param group character string to filter variables by group. Default is "all".
#' Options include "HYD" (hydrodynamic), "LKE" (Lake), "RAD" (Radiation),
#' "CHM" (Chemistry), "PHS" (Phosphorus), "NIT" (Nitrogen), "CAR" (Carbon),
#'  "SIL" (Silica), "MET" (Meteorologica), "ZOO" (Zooplankton),
#'  "PHY" (Phytoplankton),
#'
#' @importFrom utils data
#' @importFrom dplyr select rename filter
#'
#' @returns A data frame with AEME variables
#' @export
#'
#' @examples
#' view_aeme_vars()
#' view_aeme_vars("HYD")
#' view_aeme_vars("CHM")
#' view_aeme_vars("PHY")

view_aeme_vars <- function(group = "all") {
  # Load Rdata
  utils::data("key_naming", package = "AEME", envir = environment())

  df <- key_naming |>
    dplyr::select(name, name_text, units) |>
    dplyr::rename(var_aeme = name, text = name_text, units = units)
  if (group != "all") {
    df <- df |>
      dplyr::filter(grepl(group, var_aeme))
  }
  return(df)
}
