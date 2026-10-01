#' Read Simstrat bathymetry file
#'
#' @param file Path to Simstrat `Bathymetry.dat` file
#'
#' @returns Data frame with columns "elev" and "area". Depths in
#' `Bathymetry.dat` are already relative to the lake surface (0 at the
#' surface, negative going down; see \code{\link{make_stg_simstrat}}), so
#' they are used directly as `elev`.
#' @noRd
read_simstrat_hyps <- function(file) {

  lines <- readLines(file)

  out <- lines[-1] |>
    (\(x) paste(x, collapse = "\n"))() |>
    (\(x) read.table(
      text = x,
      col.names = c("elev", "area"),
      colClasses = c("numeric", "numeric")
    ))()

  out
}
