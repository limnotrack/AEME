#' Internal helpers for the structure of the `output` slot
#'
#' The `output` slot is a list holding `n_members` and one `ens_NNN` element per
#' ensemble member; each member is a list named by model. These helpers are the
#' single place that knows about that layout.
#' @name output_helpers
#' @noRd
NULL

# Model codes that can have output, in the order they are stored
.aeme_model_codes <- c("dy_cd", "glm_aed", "gotm_wet", "simstrat_aed2",
                       "simstrat_aed")

#' The empty `output` list: no members.
#' @noRd
.empty_output <- function() {
  list(n_members = 0L)
}

#' Names of the ensemble members in an `output` list (`ens_001`, ...).
#' @param outp the `output` list.
#' @noRd
.output_members <- function(outp) {
  nms <- names(outp)
  nms[grepl("^ens_[0-9]+$", nms)]
}

#' Number of ensemble members actually present in an `output` list.
#' @noRd
.count_members <- function(outp) {
  length(.output_members(outp))
}

#' Store one ensemble member in an `output` list and refresh `n_members`.
#'
#' @param outp the `output` list.
#' @param ens_n integer; member number.
#' @param mods named list of model outputs (any of `.aeme_model_codes`);
#'   unknown names are ignored and absent models are stored as `NULL`.
#' @return the updated `output` list.
#' @noRd
.set_member <- function(outp, ens_n, mods) {
  member <- lapply(.aeme_model_codes, function(m) mods[[m]])
  names(member) <- .aeme_model_codes
  outp[[format_ens_label(ens_n = ens_n)]] <- member
  outp$n_members <- .count_members(outp)
  outp
}

#' Get one model's output for one ensemble member, with clear errors.
#'
#' @param aeme `Aeme` object.
#' @param model character; a single model code.
#' @param ens_n integer; member number.
#' @param null_ok logical; return `NULL` instead of an error when that model has
#'   no output in the member.
#' @return the `aeme_output` list for that model and member.
#' @noRd
.get_member_output <- function(aeme, model, ens_n = 1, null_ok = FALSE) {
  outp <- output(aeme)
  n <- .count_members(outp)
  if (n == 0) {
    cli::cli_abort(c("{.arg aeme} has no model output.",
                     "i" = "Run a model with {.fn run_aeme} or load it with
                            {.fn load_output}."),
                   class = "aeme_error_no_output")
  }
  lab <- format_ens_label(ens_n = ens_n)
  if (!lab %in% names(outp)) {
    cli::cli_abort(c("Ensemble member {.val {ens_n}} not found.",
                     "i" = "{.arg aeme} has {n} member{?s}."),
                   class = "aeme_error_no_member")
  }
  out <- outp[[lab]][[model]]
  if (is.null(out) && !null_ok) {
    have <- names(Filter(Negate(is.null), outp[[lab]]))
    cli::cli_abort(c("No output for model {.val {model}} in member {.val {ens_n}}.",
                     "i" = "Models with output: {.val {have}}."),
                   class = "aeme_error_no_model_output")
  }
  out
}
