#' Get meteorological data from Aeme object
#'
#' @inheritParams build_aeme
#'
#' @returns Meteorological data frame or NULL if not present.
#' @export

get_met <- function(aeme) {
  inp <- aeme |> 
    check_aeme() |> 
    input()
  met <- inp[["meteo"]]
  if (is.null(met)) {
    warning("No met found in Aeme object. Returning NULL.")
    return(NULL)
  }
  return(met)
}
