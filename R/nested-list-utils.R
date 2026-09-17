#' Get a value from a nested list by dot-separated path
#'
#' Shared by the Simstrat (JSON) and GOTM-WET (YAML) thin param wrappers,
#' whose config files are nested lists rather than GLM's flat nml blocks.
#'
#' @param x list; parsed config (from [jsonlite::fromJSON()] or
#' [yaml::read_yaml()]).
#' @param path character; dot-separated key path, e.g. `"Simulation.Reference year"`.
#'
#' @return the value at `path`.
#' @noRd
.nested_list_get <- function(x, path) {
  keys <- strsplit(path, ".", fixed = TRUE)[[1]]
  for (k in keys) {
    if (!is.list(x) || !k %in% names(x)) {
      cli::cli_abort("Parameter path {.val {path}} not found.")
    }
    x <- x[[k]]
  }
  x
}

#' Set a value in a nested list by dot-separated path
#'
#' @inheritParams .nested_list_get
#' @param value replacement value. Must be the same type (logical,
#' character, numeric) as the current value at `path`.
#'
#' @return `x`, with the value at `path` replaced.
#' @noRd
.nested_list_set <- function(x, path, value) {
  keys <- strsplit(path, ".", fixed = TRUE)[[1]]

  curr <- .nested_list_get(x, path)
  type_error <- paste0("Parameter {.val {path}} must be the same type as its current value")
  if (is.logical(curr) && !is.logical(value)) {
    cli::cli_abort(paste0(type_error, " (logical)."))
  } else if (is.character(curr) && !is.character(value)) {
    cli::cli_abort(paste0(type_error, " (character)."))
  } else if (is.numeric(curr) && !is.numeric(value)) {
    cli::cli_abort(paste0(type_error, " (numeric)."))
  }

  .assign_nested <- function(lst, keys, value) {
    if (length(keys) == 1) {
      lst[[keys[1]]] <- value
      return(lst)
    }
    lst[[keys[1]]] <- .assign_nested(lst[[keys[1]]], keys[-1], value)
    lst
  }
  .assign_nested(x, keys, value)
}
