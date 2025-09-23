#' Get lake data from Aeme object
#'
#' @inheritParams build_aeme
#'
#' @returns Lake data frame or NULL if not present.
#' @export

get_lake <- function(aeme) {
  lke <- aeme |> 
    check_aeme() |> 
    lake() |> 
    as.data.frame()

  return(lke)
}
