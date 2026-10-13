#' Skill scores for an AEME ensemble
#'
#' @description
#' Verifies an ensemble against the lake observations. For every observation
#' that falls on a date (and depth) the ensemble covers, the members' values at
#' that point are compared with the observed value, and the comparisons are
#' summarised per model, variable and depth, and over all depths.
#'
#' The scores are
#'
#' * `coverage` - fraction of observations inside the central `conf_int`
#'   interval of the members. A reliable ensemble covers about `conf_int`;
#'   much less means it is over-confident. With few members the interval is
#'   estimated from empirical quantiles and under-covers even for a reliable
#'   ensemble: an `n`-member ensemble covers about
#'   `(n - 1) * conf_int / (n + 1)`, roughly 80 % for a 95 % interval with 12
#'   members, so judge `coverage` against that and not against `conf_int` alone;
#' * `bias` - mean of (ensemble mean - observation);
#' * `rmse` - root mean square error of the ensemble mean;
#' * `spread` - root of the mean ensemble variance, i.e. the typical standard
#'   deviation among members;
#' * `spread_skill` - `spread / rmse`. About 1 for a reliable ensemble, below 1
#'   when the ensemble is under-dispersed (over-confident) and above 1 when it is
#'   over-dispersed;
#' * `crps` - mean continuous ranked probability score, which rewards an ensemble
#'   that is both accurate and sharp. It has the units of the variable and lower
#'   is better; for a one-member ensemble it reduces to the absolute error.
#'
#' Each observation is one case, so two observations at the same date and depth
#' count twice. Cases where fewer than two members have a value are dropped.
#'
#' @param x an [ensemble_summary()] result, or an `Aeme` object carrying
#'   ensemble output. The summary must have been built with
#'   `keep_members = TRUE` and observations (`add_obs = TRUE`), and for a
#'   depth-resolved variable with `depths`, because members are only comparable
#'   with an observation at a common depth.
#' @param conf_int numeric; central interval used for `coverage`. Default 0.95.
#' @param ... passed to [ensemble_summary()] when `x` is an `Aeme` object, for
#'   example `model`, `vars_sim` and `depths`. Ignored for a summary.
#'
#' @return A data.frame with one row per model, variable and depth
#'   (`scope = "depth"`), plus one pooled row per model and variable over all
#'   depths (`scope = "overall"`, `depth = NA`). A variable without a depth
#'   dimension has only the pooled row. Columns are `Model`, `var_sim`, `depth`,
#'   `scope`, `n_obs`, `coverage`, `bias`, `rmse`, `spread`, `spread_skill` and
#'   `crps`. The central interval used is in `attr(, "conf_int")`.
#' @seealso [ensemble_summary()], [plot_ensemble()], [run_aeme_ensemble()]
#' @export
#'
#' @examples
#' \dontrun{
#' s <- ensemble_summary(aeme, model = "glm_aed", vars_sim = "HYD_temp",
#'                       depths = c(1, 5, 10))
#' score_ensemble(s)
#'
#' # Or straight from an Aeme object with ensemble output
#' score_ensemble(aeme, model = "glm_aed", vars_sim = "HYD_temp",
#'                depths = c(1, 5, 10))
#' }
score_ensemble <- function(x, conf_int = 0.95, ...) {

  if (!is.numeric(conf_int) || length(conf_int) != 1 || is.na(conf_int) ||
      conf_int <= 0 || conf_int >= 1) {
    cli::cli_abort("{.arg conf_int} must be a single number between 0 and 1.")
  }

  if (!inherits(x, "aeme_ensemble_summary")) {
    x <- ensemble_summary(aeme = x, ...)
  }

  if (is.null(x$members)) {
    cli::cli_abort(c(
      "Scoring needs the per-member values.",
      "i" = "Rebuild {.fn ensemble_summary} with {.code keep_members = TRUE}."))
  }
  if (is.null(x$obs) || nrow(x$obs) == 0) {
    cli::cli_abort(c(
      "There are no observations to score against.",
      "i" = "Build {.fn ensemble_summary} with {.code add_obs = TRUE} for a
             variable that has lake observations in the simulation period."))
  }
  no_depths <- intersect(attr(x, "vars_2d"),
                         unique(x$members$var_sim[is.na(x$members$depth)]))
  if (length(no_depths)) {
    cli::cli_abort(c(
      "{.val {no_depths}} {?is/are} depth-resolved but the summary pools its
       layers.",
      "i" = "Depths are scaled to each member's water level and so differ
             between members, which then cannot be compared with an
             observation. Rebuild {.fn ensemble_summary} with
             {.arg depths}."))
  }

  cases <- ens_cases(x, conf_int)
  if (nrow(cases) == 0) {
    cli::cli_abort(c(
      "No observation coincides with two or more ensemble members.",
      "i" = "Check that the observations fall within the simulation period
             and, for depth-resolved variables, within {.code obs_tol} of
             {.arg depths}."))
  }

  by_depth <- cases[!is.na(cases$depth), , drop = FALSE]
  scores <- rbind(
    if (nrow(by_depth)) score_cases(by_depth, c("Model", "var_sim", "depth"),
                                    "depth"),
    score_cases(cases, c("Model", "var_sim"), "overall")
  )
  scores <- scores[order(scores$Model, scores$var_sim,
                         scores$scope == "overall", scores$depth), ,
                   drop = FALSE]
  rownames(scores) <- NULL

  attr(scores, "conf_int") <- conf_int
  scores
}

#' Pair each observation with the members' values at that date and depth
#'
#' Returns one row per observation (and model) with the ensemble mean, the
#' ensemble variance, the central interval limits, the CRPS and whether the
#' observation fell inside the interval.
#' @param x an `aeme_ensemble_summary` with members and observations.
#' @param conf_int numeric; central interval.
#' @noRd
ens_cases <- function(x, conf_int) {

  probs <- c((1 - conf_int) / 2, 1 - (1 - conf_int) / 2)

  members <- x$members[!is.na(x$members$value), , drop = FALSE]
  members <- dplyr::summarise(
    dplyr::group_by(members, .data$Model, .data$var_sim, .data$depth,
                    .data$Date),
    vals = list(.data$value), .groups = "drop")

  obs <- x$obs[, c("Date", "depth", "var_sim", "value"), drop = FALSE]
  names(obs)[names(obs) == "value"] <- "obs"
  obs <- obs[!is.na(obs$obs), , drop = FALSE]

  cases <- dplyr::inner_join(members, obs,
                             by = c("var_sim", "depth", "Date"),
                             relationship = "many-to-many")
  cases <- cases[lengths(cases$vals) >= 2, , drop = FALSE]
  if (nrow(cases) == 0) return(cases)

  q <- t(vapply(cases$vals, stats::quantile, numeric(2), probs = probs,
                names = FALSE))
  cases$lower <- q[, 1]
  cases$upper <- q[, 2]
  cases$ens_mean <- vapply(cases$vals, mean, numeric(1))
  cases$ens_var <- vapply(cases$vals, stats::var, numeric(1))
  cases$crps <- mapply(crps_ensemble, cases$vals, cases$obs)
  cases$covered <- cases$obs >= cases$lower & cases$obs <= cases$upper
  cases$err <- cases$ens_mean - cases$obs
  cases$vals <- NULL
  cases
}

#' Continuous ranked probability score of one ensemble forecast
#'
#' `CRPS = E|X - y| - 0.5 E|X - X'|` with the expectations taken over the
#' empirical distribution of the members. The pairwise term uses the sorted
#' members, so it is O(n log n) rather than O(n^2).
#' @param x numeric; ensemble members.
#' @param y numeric; the observation.
#' @noRd
crps_ensemble <- function(x, y) {
  n <- length(x)
  x <- sort(x)
  pairs <- 2 * sum((2 * seq_len(n) - n - 1) * x)  # sum_ij |x_i - x_j|
  mean(abs(x - y)) - 0.5 * pairs / n^2
}

#' Aggregate case-level scores
#' @param cases output of `ens_cases()`.
#' @param by character; grouping columns.
#' @param scope character; label for the `scope` column.
#' @noRd
score_cases <- function(cases, by, scope) {
  out <- dplyr::summarise(
    dplyr::group_by(cases, dplyr::across(dplyr::all_of(by))),
    n_obs = dplyr::n(),
    coverage = mean(.data$covered),
    bias = mean(.data$err),
    rmse = sqrt(mean(.data$err^2)),
    spread = sqrt(mean(.data$ens_var)),
    crps = mean(.data$crps),
    .groups = "drop")
  out$spread_skill <- out$spread / out$rmse
  if (!"depth" %in% names(out)) out$depth <- NA_real_
  out$scope <- scope
  out <- as.data.frame(out[, c("Model", "var_sim", "depth", "scope", "n_obs",
                               "coverage", "bias", "rmse", "spread",
                               "spread_skill", "crps")])
  out
}
