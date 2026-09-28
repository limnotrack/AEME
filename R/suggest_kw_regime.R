#' Suggest a light extinction (Kw) regime for GLM
#'
#' Diagnoses a light extinction coefficient (Kw, i.e. Kd, m^-1) or Secchi
#' depth time series and recommends how to parameterise light extinction for
#' a GLM(-AED) run: a single static Kw, a monthly Kw climatology, or a fully
#' dated time-varying Kw forcing.
#'
#' @section Checks performed:
#' \itemize{
#'   \item \strong{sufficient_n}: are there enough observations
#'     (\code{>= min_n}) to resolve any seasonal or interannual pattern.
#'   \item \strong{sufficient_years}: does the record span enough distinct
#'     years (\code{>= min_years}) to separate a recurring seasonal cycle
#'     from interannual variability.
#'   \item \strong{sufficient_monthly_resolution}: is the median sampling
#'     gap (days) small enough (\code{<= max_median_gap_days}) to support a
#'     monthly climatology.
#'   \item \strong{interannual_dominant}: does year explain more of the
#'     total variance in Kw (\code{>= interannual_r2_threshold}) than a
#'     recurring seasonal cycle would.
#'   \item \strong{seasonal_signal_present}: is there a real recurring
#'     seasonal cycle -- the detrended (year-mean-removed) ratio of max:min
#'     monthly mean is \code{>= seasonal_ratio_threshold}.
#' }
#' Based on these, the function recommends one of \code{"static"},
#' \code{"static_or_single_year_monthly"}, \code{"dated_timeseries"} or
#' \code{"monthly_climatology"}.
#'
#' @param date Date vector (or coercible via \code{as.Date}), one per
#'   observation.
#' @param kw Numeric vector of light extinction coefficients (Kd, m^-1).
#'   Supply either \code{kw} or \code{secchi}, not both.
#' @param secchi Numeric vector of Secchi depths (m). Converted to Kd via
#'   \code{Kd = secchi_coef / secchi}.
#' @param secchi_coef Coefficient used to convert Secchi depth to Kd
#'   (default 1.7, the commonly used Poole & Atkins mid-range value).
#'   Override if a site-specific Secchi:Kd relationship is known.
#' @param min_n Minimum number of observations required before any seasonal
#'   or interannual pattern is considered resolvable (default \code{12}).
#' @param min_years Minimum span of distinct years required to assess
#'   interannual variability (default \code{3}).
#' @param max_median_gap_days Maximum acceptable median sampling gap (days)
#'   for a monthly climatology to be considered resolvable (default
#'   \code{60}).
#' @param interannual_r2_threshold Fraction of total variance explained by
#'   year, above which interannual variability is judged to dominate over
#'   any recurring seasonal cycle (default \code{0.30}).
#' @param seasonal_ratio_threshold Ratio of max:min monthly mean (after
#'   removing each year's own mean) above which a recurring seasonal cycle
#'   is judged strong enough to justify a monthly climatology (default
#'   \code{1.3}, i.e. a >30% swing).
#'
#' @return An object of class \code{kw_regime} (a list) with the
#'   recommendation, supporting diagnostics, and (where relevant) a monthly
#'   Kw climatology data frame ready to use as GLM forcing.
#'
#' @seealso [plot.kw_regime()], [set_glm_param()]
#' @export
#'
#' @examples
#' set.seed(1)
#' dates <- seq.Date(as.Date("2018-01-01"), as.Date("2021-12-01"), by = "month")
#' kw <- 1 + 0.4 * sin(2 * pi * (as.integer(format(dates, "%m")) - 3) / 12) +
#'   rnorm(length(dates), sd = 0.05)
#' rec <- suggest_kw_regime(dates, kw = kw)
#' rec
suggest_kw_regime <- function(date,
                               kw = NULL,
                               secchi = NULL,
                               secchi_coef = 1.7,
                               min_n = 12,
                               min_years = 3,
                               max_median_gap_days = 60,
                               interannual_r2_threshold = 0.30,
                               seasonal_ratio_threshold = 1.3) {

  if (is.null(kw) == is.null(secchi)) {
    cli::cli_abort("Supply exactly one of {.arg kw} or {.arg secchi}.")
  }

  date <- as.Date(date)

  if (!is.null(secchi)) {
    if (length(secchi) != length(date)) {
      cli::cli_abort("{.arg date} and {.arg secchi} must be the same length.")
    }
    if (any(secchi <= 0, na.rm = TRUE)) {
      cli::cli_abort("{.arg secchi} must be strictly positive.")
    }
    kw <- secchi_coef / secchi
    input_type <- "secchi"
  } else {
    if (length(kw) != length(date)) {
      cli::cli_abort("{.arg date} and {.arg kw} must be the same length.")
    }
    if (any(kw <= 0, na.rm = TRUE)) {
      cli::cli_abort("{.arg kw} must be strictly positive.")
    }
    input_type <- "kw"
  }

  df <- data.frame(date = date, kw = kw) |>
    dplyr::filter(stats::complete.cases(date, kw)) |>
    dplyr::arrange(date)
  n_dropped <- length(date) - nrow(df)
  date <- df$date
  kw <- df$kw

  n_obs <- length(kw)
  if (n_obs < 3) {
    cli::cli_abort("Need at least 3 valid observations to give a recommendation.")
  }

  years <- as.integer(format(date, "%Y"))
  months <- as.integer(format(date, "%m"))
  n_years <- length(unique(years))
  gaps <- diff(as.numeric(date))
  median_gap_days <- if (length(gaps) > 0) stats::median(gaps) else NA_real_
  max_gap_days <- if (length(gaps) > 0) max(gaps) else NA_real_

  mean_kw <- mean(kw)
  median_kw <- stats::median(kw)
  sd_kw <- if (n_obs > 1) stats::sd(kw) else NA_real_
  cv_kw <- if (!is.na(sd_kw)) sd_kw / mean_kw else NA_real_

  # Fraction of total variance explained by year (interannual regime shifts
  # vs. a recurring seasonal cycle).
  interannual_r2 <- NA_real_
  if (n_years >= 2) {
    grand_mean <- mean(kw)
    year_means <- tapply(kw, years, mean)
    ss_between <- sum((year_means[as.character(years)] - grand_mean)^2)
    ss_total <- sum((kw - grand_mean)^2)
    interannual_r2 <- if (ss_total > 0) ss_between / ss_total else NA_real_
  }

  # Pooled monthly climatology (raw).
  monthly <- data.frame(date = date, kw = kw, month = months) |>
    dplyr::group_by(month = factor(months, levels = 1:12)) |>
    dplyr::summarise(
      n = dplyr::n(),
      mean = if (dplyr::n() > 0) mean(kw) else NA_real_,
      sd = if (dplyr::n() > 1) stats::sd(kw) else NA_real_,
      .groups = "drop"
    ) |>
    dplyr::mutate(month = as.integer(as.character(month)),
                  n = ifelse(is.na(n), 0L, n)) |>
    dplyr::arrange(month)

  present_means <- monthly$mean[!is.na(monthly$mean)]
  seasonal_ratio_raw <- if (length(present_means) > 0) {
    max(present_means) / min(present_means)
  } else {
    NA_real_
  }

  # Detrended seasonal signal: remove each year's own mean before pooling by
  # month, so a real recurring seasonal cycle can be told apart from
  # interannual regime shifts / one-off bloom or turbidity events.
  seasonal_ratio_detrended <- NA_real_
  if (n_years >= 2) {
    year_means <- tapply(kw, years, mean)
    kw_detrended <- kw / year_means[as.character(years)]
    month_ratio <- tapply(kw_detrended, factor(months, levels = 1:12), mean)
    month_ratio <- month_ratio[!is.na(month_ratio)]
    if (length(month_ratio) > 0) {
      seasonal_ratio_detrended <- max(month_ratio) / min(month_ratio)
    }
  }

  flags <- list(
    sufficient_n = n_obs >= min_n,
    sufficient_years = n_years >= min_years,
    sufficient_monthly_resolution = !is.na(median_gap_days) &&
      median_gap_days <= max_median_gap_days,
    interannual_dominant = !is.na(interannual_r2) &&
      interannual_r2 >= interannual_r2_threshold,
    seasonal_signal_present = !is.na(seasonal_ratio_detrended) &&
      seasonal_ratio_detrended >= seasonal_ratio_threshold
  )

  # --- Decision logic ---------------------------------------------------
  if (!flags$sufficient_n) {
    recommendation <- "static"
    reason <- sprintf(
      paste0("Only %d valid observation(s) supplied (need >= %d). Too little ",
             "data to resolve any seasonal or interannual pattern."),
      n_obs, min_n
    )
  } else if (!flags$sufficient_years) {
    recommendation <- "static_or_single_year_monthly"
    reason <- sprintf(
      paste0("Data span only %d year(s) (need >= %d to separate a recurring ",
             "seasonal cycle from interannual variability). If the model run ",
             "covers exactly the sampled period, a monthly pattern from this ",
             "single year is usable; otherwise use the static mean/median Kw ",
             "(%.3f / %.3f m^-1)."),
      n_years, min_years, mean_kw, median_kw
    )
  } else if (!flags$sufficient_monthly_resolution) {
    recommendation <- "static"
    reason <- sprintf(
      paste0("Median sampling gap is %.0f days (> %.0f day threshold), too ",
             "sparse to build a reliable monthly climatology. Recommend a ",
             "static Kw = %.3f m^-1 (mean) or %.3f m^-1 (median, more robust ",
             "to bloom/turbidity outliers)."),
      median_gap_days, max_median_gap_days, mean_kw, median_kw
    )
  } else if (flags$interannual_dominant) {
    recommendation <- "dated_timeseries"
    reason <- sprintf(
      paste0("Year explains %.0f%% of total variance in Kw, more than the ",
             "seasonal cycle does (interannual regime shifts / trends ",
             "dominate over any recurring monthly pattern, detrended ",
             "seasonal ratio = %.2fx). A generic monthly climatology would ",
             "blend these regimes together and misrepresent most individual ",
             "years. Recommend driving GLM with the actual dated ",
             "observations (interpolated/held constant between sampling ",
             "dates) for the specific years being simulated, rather than a ",
             "synthetic 'typical year' climatology. If a single ",
             "representative value is still needed (e.g. for scenario runs ",
             "outside the observed period), use the static mean/median ",
             "(%.3f / %.3f m^-1) with the caveat that it will still miss ",
             "whichever regime the simulated period actually falls into."),
      100 * interannual_r2, seasonal_ratio_detrended, mean_kw, median_kw
    )
  } else if (!flags$seasonal_signal_present) {
    recommendation <- "static"
    reason <- sprintf(
      paste0("Interannual variability is limited (year R^2 = %.0f%%) and the ",
             "detrended seasonal swing is modest (%.2fx across months). A ",
             "monthly time-varying Kw is unlikely to add real skill over a ",
             "static value. Recommend static Kw = %.3f m^-1 (mean) or ",
             "%.3f m^-1 (median)."),
      100 * ifelse(is.na(interannual_r2), 0, interannual_r2),
      ifelse(is.na(seasonal_ratio_detrended), 1, seasonal_ratio_detrended),
      mean_kw, median_kw
    )
  } else {
    recommendation <- "monthly_climatology"
    reason <- sprintf(
      paste0("Sampling is frequent enough (median gap %.0f days) and spans ",
             "%d years with a real recurring seasonal cycle (detrended ",
             "seasonal ratio = %.2fx) that is not swamped by interannual ",
             "variability (year R^2 = %.0f%%). Recommend a monthly Kw ",
             "climatology (see `$monthly_climatology`) as a time-varying GLM ",
             "forcing. If the model run covers the actual sampled period, ",
             "the dated observed time series is still preferable to the ",
             "climatology."),
      median_gap_days, n_years, seasonal_ratio_detrended, 100 * interannual_r2
    )
  }

  out <- list(
    recommendation = recommendation,
    reason = reason,
    input_type = input_type,
    secchi_coef = if (input_type == "secchi") secchi_coef else NA_real_,
    n_obs = n_obs,
    n_dropped = n_dropped,
    date_range = range(date),
    n_years = n_years,
    median_gap_days = median_gap_days,
    max_gap_days = max_gap_days,
    mean_kw = mean_kw,
    median_kw = median_kw,
    sd_kw = sd_kw,
    cv_kw = cv_kw,
    interannual_r2 = interannual_r2,
    seasonal_ratio_raw = seasonal_ratio_raw,
    seasonal_ratio_detrended = seasonal_ratio_detrended,
    flags = flags,
    monthly_climatology = as.data.frame(monthly),
    data = data.frame(date = date, kw = kw)
  )
  class(out) <- "kw_regime"
  out
}

#' Print a Kw regime suggestion
#'
#' @param x an object of class `kw_regime` (see
#'   [suggest_kw_regime()]).
#' @param ... unused.
#'
#' @return `x`, invisibly.
#' @export
print.kw_regime <- function(x, ...) {
  cli::cli_h1("Kw regime suggestion")
  cli::cli_bullets(c(
    "*" = "Input: {x$input_type} (n = {x$n_obs}, {x$n_dropped} dropped for NA/ordering)",
    "*" = "Date range: {format(x$date_range[1])} to {format(x$date_range[2])} ({x$n_years} distinct year{?s})",
    "*" = "Median sample gap: {round(x$median_gap_days)} days (max {round(x$max_gap_days)} days)",
    "*" = sprintf("Kw: mean %.3f, median %.3f, sd %.3f, CV %.2f (m^-1)",
                   x$mean_kw, x$median_kw, x$sd_kw, x$cv_kw)
  ))
  if (!is.na(x$interannual_r2)) {
    cli::cli_bullets(c(
      "*" = sprintf("Interannual R^2: %.0f%% of variance explained by year",
                     100 * x$interannual_r2)
    ))
  }
  if (!is.na(x$seasonal_ratio_detrended)) {
    cli::cli_bullets(c(
      "*" = sprintf("Seasonal ratio: %.2fx (max:min monthly mean, detrended by year)",
                     x$seasonal_ratio_detrended)
    ))
  }

  label <- switch(x$recommendation,
    static = "STATIC Kw",
    static_or_single_year_monthly = "STATIC Kw (or single-year monthly pattern)",
    dated_timeseries = "DATED TIME-VARYING Kw (use observed time series directly)",
    monthly_climatology = "MONTHLY Kw CLIMATOLOGY (time-varying)"
  )
  cli::cli_h2("Recommendation: {label}")
  cli::cli_text(x$reason)

  if (x$recommendation == "monthly_climatology") {
    cli::cli_h2("Monthly climatology")
    print(x$monthly_climatology, row.names = FALSE)
  }
  invisible(x)
}

#' Plot a Kw regime suggestion
#'
#' @param x an object of class `kw_regime` (see
#'   [suggest_kw_regime()]).
#' @param ... unused.
#'
#' @return A `patchwork` object combining the observed Kw time series and
#'   the monthly climatology.
#'
#' @method plot kw_regime
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_hline geom_ribbon labs theme_bw
#' @importFrom patchwork wrap_plots
#' @export
plot.kw_regime <- function(x, ...) {
  p1 <- ggplot2::ggplot(x$data, ggplot2::aes(x = date, y = kw)) +
    ggplot2::geom_point(colour = "steelblue") +
    ggplot2::geom_hline(yintercept = x$mean_kw, linetype = "dashed",
                         colour = "grey40") +
    ggplot2::labs(x = "Date", y = expression(K[d]~(m^-1)), title = "Observed Kw") +
    ggplot2::theme_bw()

  mc <- x$monthly_climatology
  p2 <- ggplot2::ggplot(mc, ggplot2::aes(x = month, y = mean)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = mean - sd, ymax = mean + sd),
                          fill = "darkorange", alpha = 0.25) +
    ggplot2::geom_line(colour = "darkorange") +
    ggplot2::geom_point(colour = "darkorange") +
    ggplot2::labs(x = "Month", y = expression(K[d]~(m^-1)),
                  title = "Monthly climatology") +
    ggplot2::theme_bw()

  patchwork::wrap_plots(p1, p2)
}
