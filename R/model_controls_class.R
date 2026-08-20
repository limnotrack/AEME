#' Tag a data frame as AEME model controls
#'
#' Adds the `model_controls` class so it prints as a compact, readable
#' summary instead of the default data.frame dump, which is hard to scan
#' for the ~80+ rows a typical `model_controls` table has. The class is a
#' thin wrapper -- `model_controls` objects are still ordinary data frames
#' for `dplyr`, `rbind()`, `write.csv()`, etc.
#'
#' @param df a model controls data frame
#'
#' @return `df` with the `model_controls` class prepended
#' @noRd
new_model_controls <- function(df) {
  stopifnot(is.data.frame(df))
  if (!inherits(df, "model_controls")) {
    class(df) <- c("model_controls", class(df))
  }
  df
}

#' Print a model controls table
#'
#' @param x a `model_controls` object (see [get_model_controls()]).
#' @param all logical; show every variable, including ones not set to
#'   simulate. Default `FALSE` shows only the simulated ones, which is
#'   almost always what you want to check.
#' @param ... unused.
#'
#' @returns `x`, invisibly.
#' @export
print.model_controls <- function(x, all = FALSE, ...) {
  is_true_vec <- function(v) !is.na(v) & v
  sim <- is_true_vec(x$simulate)
  n_sim <- sum(sim)

  df <- x[order(!sim, x$var_aeme), , drop = FALSE]
  if (!all) {
    df <- df[is_true_vec(df$simulate), , drop = FALSE]
  }

  cat(sprintf("<model_controls> %d/%d variables simulated\n", n_sim, nrow(x)))
  if (!all && n_sim < nrow(x)) {
    cat(sprintf(
      "  (%d not-simulated variable(s) hidden -- print(x, all = TRUE) to show all)\n",
      nrow(x) - n_sim
    ))
  }
  if (nrow(df) == 0) {
    cat("  (no variables to show)\n")
    return(invisible(x))
  }

  num_cols <- intersect(
    c("inf_default", "initial_wc", "initial_sed", "conversion_aed"),
    names(df)
  )
  fmt <- df
  for (col in num_cols) {
    v <- fmt[[col]]
    fmt[[col]] <- ifelse(
      is.na(v), "-",
      format(round(v, 3), trim = TRUE, scientific = FALSE)
    )
  }
  fmt$simulate <- ifelse(is_true_vec(df$simulate), "yes", "no")

  print.data.frame(fmt, row.names = FALSE)
  invisible(x)
}
