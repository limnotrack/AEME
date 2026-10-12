#' Encode and decode full parameter names
#'
#' @description
#' A calibratable parameter is identified by its `group`, `name` and `index`
#' (the columns of [parameters()]). These helpers turn that triple into a single
#' string -- the *full name*, `group/name[index]` -- and back. Calibration and
#' sensitivity tools use the full name as a key, for example in PEST++ control
#' files and results tables, where a parameter has to be one token.
#'
#' The format is fixed because the strings are stored: a missing group or index
#' is written as `NA`, so `NA`/`inflow`/`1` encodes to `"NA/inflow[1]"` and
#' `NA`/`ce`/`NA` to `"NA/ce[NA]"`.
#'
#' Decoding splits at the **first** `/` and the **last** `[...]`. The group
#' therefore cannot itself contain a `/`, and a group literally called `"NA"`
#' decodes as a missing group. A name may contain `/` (for example
#' `"mixing/coef_mix_conv"`) and `[`.
#'
#' @param group character; the parameter group (`NA` for none).
#' @param name character; the parameter name.
#' @param index integer; the index of the parameter within a vector (`NA` for
#'   none).
#'
#' @return `encode_param()` returns a character vector of full names, recycled
#'   like [paste0()]; `character(0)` when `group`, `name` and `index` are all
#'   empty.
#'
#'   `decode_param_full()` returns a data.frame with columns `group`, `name` and
#'   `index`, one row per element of `name_full`.
#'
#'   `decode_param()` returns the `name` part only. Elements that are not in the
#'   `group/name[index]` format (and `NA`) decode to `NA`.
#' @name param_names
#' @examples
#' nm <- encode_param(group = c("light", NA), name = c("Kw", "inflow"),
#'                    index = c(1L, NA))
#' nm
#' decode_param(nm)
#' decode_param_full(nm)
NULL

#' @rdname param_names
#' @export
encode_param <- function(group, name, index) {
  if (length(group) == 0 && length(name) == 0 && length(index) == 0) {
    return(character(0))
  }
  paste0(group, "/", name, "[", index, "]")
}

#' @rdname param_names
#' @param name_full character; full parameter names in the format
#'   `group/name[index]`, as made by `encode_param()`.
#' @export
decode_param_full <- function(name_full) {
  name_full <- as.character(name_full)
  pat <- "^([^/]*)/(.*)\\[([^\\[\\]]*)\\]$"
  ok <- grepl(pat, name_full, perl = TRUE)

  part <- function(i) {
    out <- rep(NA_character_, length(name_full))
    out[ok] <- sub(pat, i, name_full[ok], perl = TRUE)
    out
  }

  group <- part("\\1")
  group[group %in% "NA"] <- NA_character_
  raw_index <- part("\\3")

  data.frame(
    group = group,
    name = part("\\2"),
    index = suppressWarnings(as.integer(raw_index)),
    stringsAsFactors = FALSE
  )
}

#' @rdname param_names
#' @export
decode_param <- function(name_full) {
  decode_param_full(name_full)$name
}
