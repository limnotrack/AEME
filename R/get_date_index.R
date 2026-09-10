#' Get date index for each model in the AEME object
#'
#' @inheritParams get_var
#' @param path,lake_dir optional; root path or resolved lake directory used to
#'   locate each model's output file. When supplied (and the file exists), the
#'   reconstructed positional index is trimmed to the number of records the
#'   file actually holds, so a stale cadence (e.g. an hourly
#'   `output_time_step` against a run still written daily) is caught here
#'   rather than silently emptying the output downstream. When omitted, the
#'   index is the pure `aeme_time_axis()` reconstruction, exactly as before.
#'
#' @returns A list with date index for each model
#' @export
#'
get_date_index <- function(aeme, model, remove_spin_up = TRUE, path = NULL,
                           lake_dir = NULL, daily_mean = FALSE) {
  withr::local_locale(c("LC_TIME" = "C"))
  withr::local_timezone("UTC")
  aeme_time <- time(aeme)
  if (missing(model)) {
    model <- list_models(aeme)
  } else {
    model <- check_model(model = model)
  }
  date_index <- lapply(model, \(m) {
    idx <- aeme_time_axis(aeme_time = aeme_time, model = m, which = "output",
                          remove_spin_up = remove_spin_up,
                          daily = daily_mean)[["index"]]
    n_rec <- .model_output_nrec(aeme = aeme, model = m, path = path,
                                lake_dir = lake_dir,
                                daily_mean = daily_mean)
    if (!is.na(n_rec) && length(idx)) {
      if (max(idx) > n_rec) {
        dropped <- sum(idx > n_rec)
        cli::cli_warn(c(
          "!" = "{.val {m}} output holds {n_rec} record{?s} but the
                 reconstructed output axis has {length(idx)} step{?s}; dropping
                 the {dropped} step{?s} past the end of the file.",
          "i" = "Was {.val {m}} rebuilt and re-run after changing
                 {.field output_time_step}?"
        ))
        idx <- idx[idx <= n_rec]
      } else if (!remove_spin_up && n_rec - max(idx) > 1) {
        cli::cli_warn(c(
          "!" = "{.val {m}} output holds {n_rec} record{?s} but the
                 reconstructed output axis only reaches step {max(idx)};
                 {n_rec - max(idx)} trailing record{?s} will be ignored.",
          "i" = "The stored {.field output_time_step} may be coarser than the
                 cadence {.val {m}} was actually run at."
        ))
      }
    }
    idx
  })
  names(date_index) <- model
  return(date_index)
}

#' Number of time records in a model's output file, or `NA` if it can't be read
#' @param daily_mean logical; count records in the daily-mean
#'   `output_daily.nc` instead of the raw output file.
#' @noRd
.model_output_nrec <- function(aeme, model, path = NULL, lake_dir = NULL,
                               daily_mean = FALSE) {
  if (is.null(path) && is.null(lake_dir)) return(NA_integer_)
  out <- tryCatch({
    of <- if (!is.null(lake_dir)) {
      get_model_outfile(path = lake_dir, model = model)[[model]]
    } else {
      get_model_outfile(aeme = aeme, model = model, path = path)[[model]]
    }
    # Prefer the "output" entry (GOTM also returns "output_daily"); fall back
    # to the first match.
    if (!is.null(names(of)) && "output" %in% names(of)) of <- of["output"]
    of <- of[nzchar(of) & file.exists(of)]
    if (length(of) == 0) return(NA_integer_)
    if (isTRUE(daily_mean)) {
      # The daily-mean companion sits next to the raw output file.
      dof <- .output_daily_path(of[[1]])
      if (!is.null(dof) && file.exists(dof)) of <- stats::setNames(dof, "output_daily")
    }
    nc <- ncdf4::nc_open(of[[1]])
    on.exit(ncdf4::nc_close(nc))
    tvar <- if ("time" %in% names(nc$var) || "time" %in% names(nc$dim)) {
      "time"
    } else if (model == "dy_cd") {
      "dyresmTime"
    } else {
      NA_character_
    }
    if (is.na(tvar)) return(NA_integer_)
    length(ncdf4::ncvar_get(nc, tvar))
  }, error = function(e) NA_integer_)
  as.integer(out)
}
