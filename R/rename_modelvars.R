#' Rename variables among the models and controlled vocabulary
#'
#' @param input character; vector of variable names to be renamed
#' @param type_input character; column name in `key_naming` representing the
#'   input type. Defaults to `"var_aeme"`.
#' @param type_output character; column name in `key_naming` representing the
#'   desired output type. Defaults to `"name_parse"`.
#' @param verbose logical; if `TRUE`, prints a formatted table of old-to-new
#'   name mappings to the console. Defaults to `FALSE`.
#' @param warn_unmatched logical; if `TRUE`, emits a warning (rather than an
#'   error) for unmatched names and returns `NA` for those entries. Defaults
#'   to `FALSE` (error on any unmatched name).
#' @param passthrough logical; if `TRUE`, unmatched names that look like AED
#'   variables (`PFX_name`, i.e. 2-4 capitals, an underscore, then a name) are
#'   returned unchanged with a warning instead of an error. Intended for
#'   modules still in development that are not yet in `key_naming`, when the
#'   input name already is the target model's name (e.g. `type_output =
#'   "glm_aed"`). Names that don't look like AED variables still error.
#'   Defaults to `FALSE`.
#'
#' @return A character vector of renamed variables, in the same order as
#'   `input`. Unmatched entries are `NA` when `warn_unmatched = TRUE`.
#' @noRd
#' @importFrom cli cli_abort cli_warn cli_bullets cli_text
rename_modelvars <- function(input,
                             type_input     = "var_aeme",
                             type_output    = "name_parse",
                             verbose        = FALSE,
                             warn_unmatched = FALSE,
                             passthrough    = FALSE) {
  
  # --- Input validation -------------------------------------------------------
  if (!is.character(input) || length(input) == 0L) {
    cli::cli_abort("{.arg input} must be a non-empty character vector.")
  }
  if (!is.character(type_input) || length(type_input) != 1L) {
    cli::cli_abort("{.arg type_input} must be a single string.")
  }
  if (!is.character(type_output) || length(type_output) != 1L) {
    cli::cli_abort("{.arg type_output} must be a single string.")
  }
  if (!is.logical(verbose) || length(verbose) != 1L) {
    cli::cli_abort("{.arg verbose} must be a single logical value.")
  }
  if (!is.logical(warn_unmatched) || length(warn_unmatched) != 1L) {
    cli::cli_abort("{.arg warn_unmatched} must be a single logical value.")
  }
  if (!is.logical(passthrough) || length(passthrough) != 1L) {
    cli::cli_abort("{.arg passthrough} must be a single logical value.")
  }
  type_input <- ifelse(type_input == "name", "var_aeme", type_input)
  type_output <- ifelse(type_output == "name", "var_aeme", type_output)
  
  # --- Load lookup table ------------------------------------------------------
  env <- new.env(parent = emptyenv())
  data("key_naming", package = "AEME", envir = env)
  key <- env$key_naming
  
  if (!is.data.frame(key)) {
    cli::cli_abort(
      c("{.var key_naming} from package {.pkg AEME} is not a data frame.",
        "i" = "The package data may be corrupt.")
    )
  }
  
  # Validate column names against the lookup table
  missing_cols <- setdiff(c(type_input, type_output), names(key))
  if (length(missing_cols) > 0L) {
    cli::cli_abort(
      c("Column(s) not found in {.var key_naming}:",
        "x" = "Missing: {.val {missing_cols}}",
        "i" = "Available columns: {.val {names(key)}}")
    )
  }
  
  # --- Match and remap --------------------------------------------------------
  idx       <- match(input, key[[type_input]])
  names_new <- key[[type_output]][idx]
  
  # --- Pass through AED-looking names missing from key_naming -----------------
  if (passthrough) {
    pass <- is.na(idx) & grepl("^[A-Z]{2,4}_[A-Za-z0-9_]+$", input)
    if (any(pass)) {
      cli::cli_warn(c(
        "Not in {.var key_naming}; passed through unchanged: {.val {input[pass]}}.",
        "i" = "No unit conversion is applied. Add to \
               {.file data-raw/key_naming.csv} for full support."))
      names_new[pass] <- input[pass]
      idx[pass] <- 0L  # mark as handled
    }
  }

  # --- Handle unmatched entries -----------------------------------------------
  unmatched <- input[is.na(idx)]
  
  if (length(unmatched) > 0L) {
    msg <- c(
      "{length(unmatched)} unmatched name{?s} in {.arg input}:",
      setNames(paste("{.val", unmatched, "}"), rep("x", length(unmatched))),
      "i" = "Check spelling and whether {.arg type_input = {.val {type_input}}} \\
             is the correct column."
    )
    if (warn_unmatched) {
      cli::cli_warn(msg)
    } else {
      cli::cli_abort(msg)
    }
  }
  
  # --- Verbose output ---------------------------------------------------------
  if (verbose) {
    matched <- !is.na(idx)
    cli::cli_text("{.strong {type_input}} {cli::symbol$arrow_right} {.strong {type_output}}")
    bullets <- setNames(
      sprintf("{.val %s} %s {.val %s}", input[matched], cli::symbol$arrow_right, names_new[matched]),
      rep("v", sum(matched))
    )
    cli::cli_bullets(bullets)
  }
  
  return(names_new)
}
