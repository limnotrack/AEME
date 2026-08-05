#' Create a new, minimal Aeme object
#'
#' Returns a valid, fully-populated `Aeme` object built from placeholder
#' values, intended as a starting point for a new lake configuration.
#' Unlike [aeme_constructor()], which requires real lake, time, and input
#' data and aborts without it, `new_aeme()` fills in sensible defaults so
#' you get a live object back immediately, ready to be built up
#' incrementally with the slot setters (e.g. `lake<-`, `time<-`, `input<-`)
#' and helpers such as [add_hypsograph()], [add_met()], and [add_inflows()].
#'
#' @param name character; lake name (alphanumeric only). Default `"newlake"`.
#' @param id character; lake identifier (alphanumeric only). Default
#'   `"0001"`.
#' @param latitude numeric; lake latitude, in \eqn{[-90, 90]}. Default `0`.
#' @param longitude numeric; lake longitude, in \eqn{[-180, 180]}. Default
#'   `0`.
#' @param elevation numeric; lake surface elevation above sea level (m).
#'   Default `100`.
#' @param depth numeric; lake depth (m). Default `10`.
#' @param area numeric; lake surface area (m^2). Default `1e5`.
#' @param start character, Date, or POSIXct; simulation start date. Default
#'   one year before `stop`.
#' @param stop character, Date, or POSIXct; simulation stop date. Default
#'   today.
#' @param time_step numeric; time step in seconds. Default `3600` (1 hour).
#' @param Kw numeric; light extinction coefficient (m^-1). Default `1`.
#'
#' @return A valid `Aeme` object populated with placeholder values.
#'
#' @seealso [aeme_constructor()] for building an `Aeme` object from real
#'   lake data with full validation.
#'
#' @export
#'
#' @examples
#' aeme <- new_aeme()
#'
#' aeme <- new_aeme(name = "mylake", id = "001", latitude = -37.8,
#'                  longitude = 175.3, elevation = 30, depth = 15,
#'                  area = 2.5e5)
new_aeme <- function(name = "newlake",
                     id = "0001",
                     latitude = 0,
                     longitude = 0,
                     elevation = 100,
                     depth = 10,
                     area = 1e5,
                     start = Sys.Date() - 365,
                     stop = Sys.Date(),
                     time_step = 3600,
                     Kw = 1) {

  lake <- list(
    name      = name,
    id        = id,
    latitude  = latitude,
    longitude = longitude,
    elevation = elevation,
    depth     = depth,
    area      = area
  )

  time <- list(
    start     = as.POSIXct(start, tz = "UTC"),
    stop      = as.POSIXct(stop, tz = "UTC"),
    time_step = time_step
  )

  input <- list(
    init_depth = depth,
    use_lw     = TRUE,
    Kw         = Kw
  )

  aeme_constructor(lake = lake, time = time, input = input)
}
