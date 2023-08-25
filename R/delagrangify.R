#' Interpolate outputs of glm or dyresm into a water column with a fixed
#' number of even slices (as per gotm)
#'
#' @param elevs data.frame; of elevations corresponding to values
#' @param values data.frame; of values corresponding to elevs
#' @param nlev numeric; number of depths to output
#'
#' @return data.frame with data on regular grid
#' @noRd

delagrangify <- function(elevs, values, nlev) {

  out <- lapply(1:ncol(elevs), \(e) {

    elvs  <- elevs[!is.na(elevs[, e]), e]
    vals <- values[!is.na(values[, e]), e]
    if(length(vals) == 0) {
      return(rep(NA, nlev))
    } else if(length(vals) == 1) {
      return(rep(vals, nlev))
    }

    layer_mids <- elvs + c(diff(elvs)/2, -1* tail(elvs,1)/2)
    values_re <- c(vals[1], vals, tail(vals,1))
    elevs_re <- c(head(elvs, 1), layer_mids, 0)

    elevs_out <- seq(from = max(elvs) / nlev, to = max(elvs), by = max(elvs)/nlev)
    values_out <- approx(x = elevs_re, y = values_re, xout = elevs_out)$y
    return(values_out)
  })

  res <- data.frame(do.call(cbind, out))
  return(res)
}
