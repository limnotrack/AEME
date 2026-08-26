#' Show the observation variables present in an Aeme object
#'
#' Summarises the observations stored in an Aeme object (both the lake profile
#' observations and the water level observations), reporting one row per
#' observed variable along with how much data is available for it.
#'
#' @inheritParams build_aeme
#' @param time_filter logical; if TRUE, only observations within the simulation
#' period (see [time()]) are considered. Default is FALSE.
#'
#' @returns A data frame with one row per observation variable and the columns:
#' \itemize{
#'   \item \code{var_aeme}: variable name in the AEME format
#'   \item \code{name_text}: display name of the variable (from \code{key_naming})
#'   \item \code{source}: which observation slot the variable comes from
#'   (\code{"lake"} or \code{"level"})
#'   \item \code{n}: number of observations
#'   \item \code{n_dates}: number of unique dates
#'   \item \code{n_depths}: number of unique depths (NA for water level)
#'   \item \code{date_start}, \code{date_stop}: first and last observation date
#' }
#' Returns \code{NULL} if the Aeme object contains no observations.
#'
#' @seealso [list_obs_vars()] for a named vector of the variable names,
#' [get_mod_obs_vars()] for the variables shared with model output.
#'
#' @importFrom dplyr filter mutate group_by summarise arrange bind_rows n
#'
#' @export
#'
#' @examples
#' aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
#' aeme <- readRDS(aeme_file)
#' get_obs_vars(aeme)

get_obs_vars <- function(aeme, time_filter = FALSE) {

  aeme <- check_aeme(aeme)
  obs <- observations(aeme)

  lake <- obs$lake
  level <- obs$level

  if (is.null(lake) && is.null(level)) {
    return(NULL)
  }

  if (time_filter) {
    tme <- time(aeme)
    start <- as.Date(tme$start)
    stop <- as.Date(tme$stop)
    if (!is.null(lake)) {
      lake <- lake |>
        dplyr::filter(as.Date(Date) >= start & as.Date(Date) <= stop)
    }
    if (!is.null(level)) {
      level <- level |>
        dplyr::filter(as.Date(Date) >= start & as.Date(Date) <= stop)
    }
  }

  summarise_slot <- function(df, source) {
    if (is.null(df) || nrow(df) == 0) {
      return(NULL)
    }
    if (!("depth_from" %in% names(df))) df$depth_from <- NA_real_
    if (!("depth_to" %in% names(df))) df$depth_to <- NA_real_
    df |>
      dplyr::mutate(depth_mid = (depth_from + depth_to) / 2) |>
      dplyr::filter(!is.na(var_aeme)) |>
      dplyr::group_by(var_aeme) |>
      dplyr::summarise(
        source = source,
        n = dplyr::n(),
        n_dates = length(unique(Date)),
        n_depths = length(unique(depth_mid[!is.na(depth_mid)])),
        date_start = min(as.Date(Date)),
        date_stop = max(as.Date(Date)),
        .groups = "drop"
      )
  }

  out <- dplyr::bind_rows(
    summarise_slot(lake, "lake"),
    summarise_slot(level, "level")
  )

  if (is.null(out) || nrow(out) == 0) {
    return(NULL)
  }

  data("key_naming", package = "AEME", envir = environment())
  out$name_text <- key_naming$name_text[match(out$var_aeme,
                                              key_naming$var_aeme)]

  out |>
    dplyr::mutate(n_depths = ifelse(n_depths == 0, NA_integer_, n_depths)) |>
    dplyr::select(var_aeme, name_text, source, n, n_dates, n_depths,
                  date_start, date_stop) |>
    dplyr::arrange(source, var_aeme)
}
