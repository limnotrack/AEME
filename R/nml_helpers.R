#' Convert a list to an nml object
#'
#' @param list_obj A named list of named lists representing GLM nml blocks.
#' @return An object of class \code{nml}, invisibly.
#' @keywords internal
#' @noRd
.nml <- function(list_obj) {
  # Basic structural check: must be a named list of lists
  if (!is.list(list_obj)) {
    cli::cli_abort(
      c("{.arg list_obj} must be a {.cls list}.",
        "x" = "Got {.cls {class(list_obj)}}."),
      class = "nml_error_constructor"
    )
  }
  if (length(list_obj) == 0) {
    cli::cli_warn(
      c("!" = "Creating an empty {.cls nml} object.",
        "i" = "At least one named block is expected."),
      class = "nml_warn_empty"
    )
  }
  if (!is.null(names(list_obj)) && any(nchar(names(list_obj)) == 0)) {
    cli::cli_abort(
      c("All blocks in {.arg list_obj} must be named.",
        "x" = "One or more blocks have empty names."),
      class = "nml_error_constructor"
    )
  }
  if (!all(sapply(list_obj, is.list))) {
    bad <- names(list_obj)[!sapply(list_obj, is.list)]
    cli::cli_abort(
      c("Each block in an {.cls nml} object must be a named {.cls list}.",
        "x" = "Block{?s} {.val {bad}} {?is/are} not a list."),
      class = "nml_error_constructor"
    )
  }
  nml <- list_obj
  class(nml) <- "nml"
  invisible(nml)
}


# Boolean conversion helpers

#' Convert GLM logical strings to R logicals
#'
#' @param values Character vector containing \code{.true.} or \code{.false.}
#'   strings.
#' @return A logical vector.
#' @keywords internal
#' @noRd
from.glm_boolean <- function(values) {
  logicals <- sapply(values, FUN = function(x) {
    if (is.na(x) || x == "NA") {
      return(NA)
    }
    if (!isTRUE(grepl(".true.", x) || grepl(".false.", x))) {
      cli::cli_abort(
        c("{.val {x}} is not a GLM logical.",
          "x" = "Expected {.code .true.} or {.code .false.}.",
          "i" = "Check that the nml file uses GLM-format logicals."),
        class = "nml_error_boolean"
      )
    }
    isTRUE(grepl(".true.", x))
  })
  as.logical(logicals)
}


#' Convert R logicals to GLM logical strings
#'
#' @param values A logical vector.
#' @return A character vector of \code{.true.} / \code{.false.} strings.
#' @noRd
to.glm_boolean <- function(values) {
  result <- character(length(values))
  result[values]  <- ".true."
  result[!values] <- ".false."
  result
}


# Line parser

#' Parse a single line from a GLM nml file into a named list entry
#'
#' @param textLine string; line from GLM nml file.
#' @param lineNum numeric; line number in file.
#' @param blckName string; block name this line belongs to.
#' @param coerce logical; attempt to coerce NA values using neighbouring
#'   non-NA values. Default \code{TRUE}.
#' @return A length-1 named list: \code{list(param_name = param_value)}.
#' @keywords internal
#' @noRd
buildVal <- function(textLine, lineNum, blckName, coerce = TRUE) {
  
  # Strip inline comments
  textLine <- strsplit(textLine, "!")[[1]][1]
  
  if (!any(grep("=", textLine))) {
    cli::cli_abort(
      c("Hanging lines are not allowed in {.file .nml} files.",
        "x" = "Line {.val {lineNum}} in block {.code &{blckName}} has no {.code =}.",
        "i" = "Offending text: {.val {trimws(textLine)}}"),
      class = "nml_error_parse_hanging"
    )
  }
  
  params <- strsplit(textLine, "=")
  parNm  <- trimws(params[[1]][1])   # trim whitespace from parameter name
  parVl  <- params[[1]][2]
  
  if (is.na(parVl)) {
    cli::cli_abort(
      c("Empty value after {.code {trimws(textLine)}} on line {.val {lineNum}}.",
        "i" = "Check whether the value continues on the next line - hanging",
        " " = "values are not supported."),
      class = "nml_error_parse_empty"
    )
  }
  
  # Special case: date string - detect by position of ':' separators.
  # BUG FIX: original code had nchar(parVl > 17), which compares the logical
  # TRUE/FALSE to 17 via nchar(), always returning 4 or 5. Correct form is
  # nchar(parVl) > 17.
  if (nchar(parVl) > 17 && substr(parVl, 14, 14) == ":" &&
      substr(parVl, 17, 17) == ":") {
    parVl <- paste0(substr(parVl, 1, 11), " ", substr(parVl, 12, nchar(parVl)))
  }
  
  # Determine value type and parse accordingly.
  # trimws() is applied in every branch - the right-hand side of "param = value"
  # always carries a leading space after splitting on "=", and quote-stripping
  # alone does not remove it.
  if (any(grep("'", parVl))) {
    parVl <- trimws(gsub("'", "", parVl))
    if (any(grep(",", parVl))) {
      parVl <- trimws(unlist(strsplit(parVl, ",")))
    }
  } else if (any(grep("\"", parVl))) {
    parVl <- trimws(gsub("'", "", gsub("\"", "", parVl)))
    if (any(grep(",", parVl))) {
      parVl <- trimws(unlist(strsplit(parVl, ",")))
    }
  } else if (isTRUE(grepl(".true.", parVl) || grepl(".false.", parVl))) {
    logicals <- trimws(unlist(strsplit(parVl, ",")))
    parVl <- from.glm_boolean(logicals)
  } else if (any(grep(",", parVl))) {
    parVl <- as.numeric(trimws(unlist(strsplit(parVl, ","))))
  } else {
    parVl <- as.numeric(trimws(parVl))
  }
  
  # NA coercion handling
  if (coerce && any(is.na(parVl))) {
    non_na <- parVl[!is.na(parVl)]
    if (length(non_na) > 0) {
      # Recoverable: fill NAs with first non-NA value, but warn the user
      cli::cli_warn(
        c("!" = "Coercion produced {sum(is.na(parVl))} NA(s) for parameter
          {.val {parNm}} on line {.val {lineNum}} in block {.val {blckName}}.",
          "i" = "Filling with first non-NA value: {.val {non_na[1]}}."),
        class = "nml_warn_coercion_partial"
      )
      parVl[is.na(parVl)] <- non_na[1]
    } else {
      # All NA - inform (not warn) since we return NA and let caller decide
      cli::cli_inform(
        c("!" = "Coercion produced all NAs for parameter {.val {parNm}} on
          line {.val {lineNum}} in block {.val {blckName}}.",
          "i" = "Returning {.code NA}. Check the raw value in the {.file .nml} file."),
        class = "nml_inform_coercion_all_na"
      )
      parVl <- rep(NA, length(parVl))
    }
  }
  
  lineVal <- list(parVl)
  names(lineVal) <- parNm
  lineVal
}


# Block / argument lookup

#' Find which block(s) an argument belongs to in an nml object
#'
#' @param nml An nml object.
#' @param argName string; argument name to search for.
#' @return Integer index (or indices) of the matching block(s).
#' @noRd
findBlck <- function(nml, argName) {
  
  if (!is.character(argName)) {
    cli::cli_abort(
      c("{.arg argName} must be a {.cls character} string.",
        "x" = "Got {.cls {class(argName)}}."),
      class = "nml_error_findblck"
    )
  }
  
  blockNames <- names(nml)
  blckI <- which(sapply(seq_along(blockNames), function(i) {
    any(argName %in% names(nml[[i]]))
  }))
  
  if (length(blckI) == 0) {
    # Collect all available parameter names to give a useful suggestion
    all_params <- unlist(lapply(nml, names))
    cli::cli_abort(
      c("Parameter {.val {argName}} not found in any nml block.",
        "i" = "Available parameters: {.val {all_params}}."),
      class = "nml_error_param_not_found"
    )
  }
  
  blckI
}


#' Extract the argument name from a block::arg_name string
#'
#' @param arg_name string; optionally prefixed with \code{block::}.
#' @return The bare argument name.
#' @noRd
get_arg_name <- function(arg_name) {
  arg_split <- strsplit(arg_name, "::")[[1]]
  if (length(arg_split) > 1) {
    return(arg_split[2])
  }
  arg_name
}


#' Resolve the block index for a given argument
#'
#' Supports \code{block::arg_name} syntax for explicit block targeting.
#'
#' @param glm_nml An nml object.
#' @param arg_name string; argument name, optionally prefixed with block name.
#' @param warn logical; emit a warning when the argument appears in multiple
#'   blocks. Default \code{TRUE}.
#' @return A single integer block index.
#' @noRd
get_block <- function(glm_nml, arg_name, warn = TRUE) {
  
  arg_split <- strsplit(arg_name, "::")[[1]]
  
  if (length(arg_split) > 1) {
    blck     <- arg_split[1]
    arg_name <- arg_split[2]
  } else {
    blck <- findBlck(glm_nml, arg_name)
  }
  
  if (length(blck) > 1) {
    block_names <- names(glm_nml[blck])
    if (warn) {
      cli::cli_warn(
        c("{.val {arg_name}} was found in multiple blocks:
          {.val {paste(block_names, collapse = ' & ')}}.",
          "i" = "Returning the first match ({.val {block_names[1]}}).",
          "i" = "Use {.code {block_names[1]}::{arg_name}} for an explicit match."),
        class = "nml_warn_ambiguous_param"
      )
    }
    blck <- blck[1]
  }
  
  blck
}


# nml setters

#' Set a list of parameters in an nml object
#'
#' @inheritParams set_nml
#' @param arg_list A named list of parameter name-value pairs.
#' @return An updated nml object.
#' @noRd
setnmlList <- function(glm_nml, arg_list) {
  
  if (!is.list(arg_list)) {
    cli::cli_abort(
      c("{.arg arg_list} must be a named {.cls list}.",
        "x" = "Got {.cls {class(arg_list)}}."),
      class = "nml_error_setnmllist"
    )
  }
  if (length(arg_list) == 0 || length(names(arg_list)) == 0 ||
      any(nchar(names(arg_list)) == 0)) {
    cli::cli_abort(
      c("{.arg arg_list} must be a non-empty named {.cls list}.",
        "x" = "All entries must have non-empty names."),
      class = "nml_error_setnmllist"
    )
  }
  
  for (nm in names(arg_list)) {
    glm_nml <- set_nml(glm_nml, arg_name = nm, arg_val = arg_list[[nm]])
  }
  
  glm_nml
}


# File helpers

#' Check whether a file path refers to a GLM nml file
#'
#' @param nml_file File path.
#' @return \code{TRUE} if the extension is \code{.nml}, otherwise \code{FALSE}.
#' @noRd
#' @importFrom utils tail
is_nml_file <- function(nml_file) {
  ext <- tail(strsplit(nml_file, "\\.")[[1]], 1)
  identical(ext, "nml")
}


#' Return non-ASCII characters found in a file
#'
#' @param file File path.
#' @return Character vector of lines containing non-ASCII characters,
#'   or \code{character(0)} if none.
#' @noRd
#' @importFrom utils capture.output
#' @importFrom tools showNonASCIIfile
what_ascii <- function(file) {
  capture.output(tools::showNonASCIIfile(file))
}


#' Check whether a file contains only ASCII characters
#'
#' @param file File path.
#' @return \code{TRUE} if all characters are ASCII, \code{FALSE} otherwise.
#' @noRd
ascii_only <- function(file) {
  length(what_ascii(file)) == 0
}


# S3 methods

#' Print an nml object
#'
#' Renders the nml object in GLM nml file format.
#'
#' @param x An nml object.
#' @param ... Further arguments (currently unused).
#' @return \code{x}, invisibly.
#' @export
print.nml <- function(x, ...) {
  
  if (!inherits(x, "nml") || !is.list(x)) {
    cli::cli_abort(
      c("{.arg x} must be an {.cls nml} object.",
        "x" = "Got {.cls {class(x)}}."),
      class = "nml_error_print"
    )
  }
  
  for (i in seq_along(x)) {
    blckNm   <- names(x)[i]
    blckList <- x[[i]]
    
    cat("&", blckNm, "\n", sep = "")
    
    for (j in seq_along(blckList)) {
      param_name  <- names(blckList)[j]
      param_value <- blckList[[j]]
      
      cat("   ", param_name, " = ", sep = "")
      
      if (length(param_value) > 1) {
        if (is.logical(param_value)) {
          writer <- paste(to.glm_boolean(param_value), collapse = ", ")
        } else if (is.character(param_value)) {
          writer <- paste0("'", paste(param_value, collapse = "','"), "'")
        } else {
          writer <- paste(param_value, collapse = ", ")
        }
      } else if (is.character(param_value)) {
        parts  <- strsplit(param_value, ",")[[1]]
        writer <- paste0("'", paste(parts, collapse = "','"), "'")
      } else if (is.logical(param_value)) {
        writer <- to.glm_boolean(param_value)
      } else {
        writer <- param_value
      }
      
      cat(writer, "\n", sep = "")
    }
    
    cat("/\n")
  }
  
  invisible(x)
}


#' Summarise an nml object
#'
#' Currently delegates to \code{\link{print.nml}}.
#'
#' @param object An nml object.
#' @param ... Further arguments passed to \code{print.nml}.
#' @return \code{object}, invisibly.
#' @export
summary.nml <- function(object, ...) {
  print(object, ...)
}
