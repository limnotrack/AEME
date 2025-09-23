#' Get hypsograph from Aeme object
#'
#' @inheritParams build_aeme
#'
#' @returns Hypsograph data frame or NULL if not present
#' @export
#'

get_hypsograph <- function(aeme) {
  inp <- aeme |> 
    check_aeme() |> 
    input()
  hypsograph <- inp[["hypsograph"]]
  if (is.null(hypsograph)) {
    warning("No hypsograph found in Aeme object. Returning NULL.")
    return(NULL)
  }
  return(hypsograph)
}
