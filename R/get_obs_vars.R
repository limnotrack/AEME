#' Get observation variable names
#'
#' @inheritParams build_aeme
#'
#' @return Observation variable names vector
#'
#' @export
#'
#' @importFrom utils data
#' @importFrom dplyr bind_rows filter pull
#'
#' @examples
#' aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
#' aeme <- readRDS(aeme_file)
#' get_obs_vars(aeme)

get_obs_vars <- function(aeme) {
  utils::data("key_naming", package = "AEME")
  aeme_time <- AEME::time(aeme)

  obs_vars <- AEME::observations(aeme) |>
    dplyr::bind_rows()

  if (nrow(obs_vars) == 0) {
    return()
  }

  obs_vars <- obs_vars |>
    dplyr::filter(Date >= aeme_time$start & Date <= aeme_time$stop)

  if (nrow(obs_vars) == 0) {
    return()
  }

  obs_vars <- obs_vars |>
    dplyr::filter(!is.na(var_aeme)) |>
    dplyr::pull(var_aeme) |>
    unique()
  if (length(obs_vars) == 0) {
    return()
  }
  name <- key_naming$name_text[match(obs_vars, key_naming$name)]
  idx <- !is.na(name)
  setNames(obs_vars[idx], name[idx])
}
