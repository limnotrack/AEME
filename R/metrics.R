#' Prepare and Validate Metric Inputs
#' 
#' Private helper to check types, lengths, and handle NA removal.
#' @keywords internal
#' @noRd
prep_metric_data <- function(obs, sim, na.rm) {
  if (!is.numeric(obs) || !is.numeric(sim)) {
    cli::cli_abort("`obs` and `sim` must be numeric vectors.")
  }
  if (length(obs) != length(sim)) {
    cli::cli_abort("`obs` and `sim` must have the same length.")
  }
  
  if (na.rm) {
    ok <- stats::complete.cases(obs, sim)
    obs <- obs[ok]
    sim <- sim[ok]
  }
  
  if (length(obs) == 0) {
    cli::cli_abort("No valid numeric pairs remaining after NA removal.")
  }
  
  list(obs = obs, sim = sim, n = length(obs))
}

#' Nash-Sutcliffe Efficiency (NSE) fit function
#'
#' Calculates the Nash-Sutcliffe Efficiency between observed and modelled
#' values, in its conventional orientation where **higher is better**
#' (`1` = perfect fit).
#'
#' NSE is calculated as:
#' \deqn{\text{NSE} = 1 - \frac{\sum_{i=1}^n (\text{obs}_i - \text{sim}_i)^2}{\sum_{i=1}^n (\text{obs}_i - \bar{\text{obs}})^2}}
#' 
#' It ranges from `-Inf` to `1` (`1` = perfect fit, `0` = no better than the mean of
#' the observations). It is dimensionless, which makes it a reasonable
#' default when combining fit values across variables with different units
#' or magnitudes - but it is squared-error based, so it over-weights peaks
#' and under-weights errors in the low/baseline range, and it conflates
#' bias, variability and timing error into one number. See
#' \code{\link{kge}}/\code{\link{kge_prime}} for a fit function that keeps
#' those separate.
#'
#' `calib_aeme()` and `run_and_fit()` **minimise** the values returned by
#' `FUN_list` entries, so `nse()` is not suitable as a calibration
#' objective directly - use \code{\link{nse_loss}} (which returns
#' `-1 * nse(obs, sim)`) for that.
#'
#' @param obs Numeric vector of observed values.
#' @param sim Numeric vector of simulated (modelled) values.
#' @param na.rm Logical; should missing values (NA) in either vector be removed 
#' pairwise before computation? Default is \code{TRUE}.
#'
#' @return numeric; NSE, ranging `-Inf` to `1` (`1` = perfect fit). Returns `NA` if 
#' undefined (e.g., zero observed variance).
#'
#' @seealso \code{\link{nse_loss}} for the minimise-oriented variant used in
#' `FUN_list`.
#'
#' @examples
#' obs <- c(1, 2, 3, 4)
#' sim <- c(1.1, 2.1, 2.9, 4.2)
#' nse(obs, sim)
#'
#' @export
nse <- function(obs, sim, na.rm = TRUE) {
  dat <- prep_metric_data(obs, sim, na.rm)
  if (dat$n == 0) return(NA_real_)
  
  obs_var <- sum((dat$obs - mean(dat$obs))^2)
  
  if (obs_var == 0) {
    cli::cli_alert_warning("Observed variance is zero; NSE denominator is undefined.")
    return(NA_real_)
  }
  
  1 - (sum((dat$obs - dat$sim)^2) / obs_var)
}

#' Kling-Gupta Efficiency (KGE) fit function
#'
#' Calculates the Kling-Gupta Efficiency (Gupta et al. 2009) between
#' observed and modelled values, in its conventional orientation where
#' **higher is better** (`1` = perfect fit).
#'
#' KGE decomposes fit into three components instead of conflating them the
#' way \code{\link{nse}}'s single squared-error term does:
#'
#' \deqn{\text{KGE} = 1 - \sqrt{(r - 1)^2 + (\alpha - 1)^2 + (\beta - 1)^2}}
#'
#' where the variability ratio (\eqn{\alpha}) and bias ratio (\eqn{\beta}) are calculated as:
#'
#' \deqn{\alpha = \frac{\sigma_{\text{sim}}}{\sigma_{\text{obs}}}, \quad \beta = \frac{\mu_{\text{sim}}}{\mu_{\text{obs}}}}
#' 
#' with \eqn{r} being the Pearson correlation, \eqn{\sigma} the standard deviation, 
#' and \eqn{\mu} the mean. Maximum is `1` (perfect fit). Being dimensionless like NSE, 
#' it is also a reasonable choice when combining fit values across variables with
#' different units or magnitudes, and is generally preferred over NSE in
#' current hydrological/environmental modelling practice.
#'
#' `calib_aeme()` and `run_and_fit()` **minimise** the values returned by
#' `FUN_list` entries - use \code{\link{kge_loss}} (which returns
#' `-1 * kge(obs, sim)`) as a calibration objective.
#'
#' @inheritParams nse
#'
#' @return numeric; KGE, with a maximum of `1` (perfect fit). Returns `NA` if 
#' undefined (e.g., zero observed variance or mean).
#'
#' @seealso \code{\link{kge_loss}} for the minimise-oriented variant used in
#' `FUN_list`.
#'
#' @examples
#' obs <- c(1, 2, 3, 4)
#' sim <- c(1.1, 2.1, 2.9, 4.2)
#' kge(obs, sim)
#'
#' @export
kge <- function(obs, sim, na.rm = TRUE) {
  dat <- prep_metric_data(obs, sim, na.rm)
  if (dat$n < 2) return(NA_real_) # Needs at least 2 points for sd() and cor()
  
  sd_obs <- stats::sd(dat$obs)
  mean_obs <- mean(dat$obs)
  
  if (sd_obs == 0 || mean_obs == 0) {
    cli::cli_alert_warning("Observed standard deviation or mean is zero; KGE components undefined.")
    return(NA_real_)
  }
  
  r <- stats::cor(dat$obs, dat$sim)
  alpha <- stats::sd(dat$sim) / sd_obs
  beta <- mean(dat$sim) / mean_obs
  
  1 - sqrt((r - 1)^2 + (alpha - 1)^2 + (beta - 1)^2)
}

#' Modified Kling-Gupta Efficiency (KGE') fit function
#'
#' As \code{\link{kge}}, but replaces the raw variability ratio with a
#' coefficient-of-variation ratio (Kling et al. 2012):
#'
#' \deqn{\text{KGE}' = 1 - \sqrt{(r - 1)^2 + (\gamma - 1)^2 + (\beta - 1)^2}}
#'
#' where the coefficient-of-variation ratio (\eqn{\gamma}) decouples the variability 
#' term from the bias term more cleanly:
#' 
#' \deqn{\gamma = \frac{\sigma_{\text{sim}} / \mu_{\text{sim}}}{\sigma_{\text{obs}} / \mu_{\text{obs}}}}
#' 
#' This is generally the recommended default over the original KGE. Returned in its
#' conventional orientation where **higher is better** (`1` = perfect fit).
#'
#' `calib_aeme()` and `run_and_fit()` **minimise** the values returned by
#' `FUN_list` entries - use \code{\link{kge_prime_loss}} (which returns
#' `-1 * kge_prime(obs, sim)`) as a calibration objective.
#'
#' @inheritParams nse
#'
#' @return numeric; KGE', with a maximum of `1` (perfect fit). Returns `NA` if 
#' undefined.
#'
#' @seealso \code{\link{kge_prime_loss}} for the minimise-oriented variant
#' used in `FUN_list`.
#'
#' @examples
#' obs <- c(1, 2, 3, 4)
#' sim <- c(1.1, 2.1, 2.9, 4.2)
#' kge_prime(obs, sim)
#'
#' @export
kge_prime <- function(obs, sim, na.rm = TRUE) {
  dat <- prep_metric_data(obs, sim, na.rm)
  if (dat$n < 2) return(NA_real_)
  
  sd_obs <- stats::sd(dat$obs)
  mean_obs <- mean(dat$obs)
  mean_sim <- mean(dat$sim)
  
  if (sd_obs == 0 || mean_obs == 0 || mean_sim == 0) {
    cli::cli_alert_warning("Observed SD, mean(obs), or mean(sim) is zero; KGE' components undefined.")
    return(NA_real_)
  }
  
  r <- stats::cor(dat$obs, dat$sim)
  beta <- mean_sim / mean_obs
  gamma <- (stats::sd(dat$sim) / mean_sim) / (sd_obs / mean_obs)
  
  1 - sqrt((r - 1)^2 + (gamma - 1)^2 + (beta - 1)^2)
}

#' Log-transformed Kling-Gupta Efficiency fit function
#'
#' \code{\link{kge}} calculated on `log1p()`-transformed observed and
#' modelled values, for skewed/concentration-type variables (e.g. oxygen,
#' chlorophyll, nutrients) where a few peak events would otherwise dominate
#' the fit. 
#' 
#' \deqn{\text{KGE}_{\text{log}} = \text{KGE}(\log_{1p}(\text{obs}), \log_{1p}(\text{sim}))}
#' 
#' `log1p()` tolerates zeros but not negative values - not
#' suitable for variables that can be negative. Returned in its conventional
#' orientation where **higher is better** (`1` = perfect fit).
#'
#' `calib_aeme()` and `run_and_fit()` **minimise** the values returned by
#' `FUN_list` entries - use \code{\link{log_kge_loss}} (which returns
#' `-1 * log_kge(obs, sim)`) as a calibration objective.
#'
#' @inheritParams nse
#'
#' @return numeric; KGE calculated on `log1p(obs)`/`log1p(sim)`, with a
#' maximum of `1` (perfect fit).
#'
#' @seealso \code{\link{log_kge_loss}} for the minimise-oriented variant
#' used in `FUN_list`.
#'
#' @examples
#' obs <- c(1, 2, 3, 4)
#' sim <- c(1.1, 2.1, 2.9, 4.2)
#' log_kge(obs, sim)
#'
#' @export
log_kge <- function(obs, sim, na.rm = TRUE) {
  if (any(obs < 0, na.rm = TRUE) || any(sim < 0, na.rm = TRUE)) {
    cli::cli_alert_warning("Negative values found; log1p transformation will yield NaNs.")
  }
  
  obs <- log1p(obs)
  sim <- log1p(sim)
  kge(obs, sim, na.rm = na.rm)
}

#' Minimise-oriented (loss) variants of NSE / KGE fit functions
#'
#' \code{\link{nse}}, \code{\link{kge}}, \code{\link{kge_prime}} and
#' \code{\link{log_kge}} return their conventional statistic, where `1` is a
#' perfect fit and higher is better. `calib_aeme()` and `run_and_fit()`
#' instead **minimise** the values returned by `FUN_list` entries, so these
#' `_loss` companions return `-1 *` the corresponding statistic (lower is
#' better, `-1` = perfect fit) and are what you pass in `FUN_list` for
#' calibration:
#'
#' \preformatted{FUN_list <- list(HYD_temp = kge_loss, LKE_lvlwtr = rmse)}
#'
#' \code{\link{mae}}, \code{\link{rmse}} and \code{\link{pbias}} are already
#' `0`-is-best, minimise-oriented, so they have no `_loss` companion - use
#' them directly.
#'
#' @inheritParams nse
#'
#' @return numeric; `-1 *` the corresponding statistic (`-1` = perfect fit,
#' higher = worse fit).
#'
#' @seealso \code{\link{nse}}, \code{\link{kge}}, \code{\link{kge_prime}},
#' \code{\link{log_kge}} for the conventional (higher-is-better) statistics.
#'
#' @examples
#' obs <- c(1, 2, 3, 4)
#' sim <- c(1.1, 2.1, 2.9, 4.2)
#' nse_loss(obs, sim)
#' kge_loss(obs, sim)
#' kge_prime_loss(obs, sim)
#' log_kge_loss(obs, sim)
#'
#' @name fit_loss
NULL

#' @rdname fit_loss
#' @export
nse_loss <- function(obs, sim, na.rm = TRUE) -1 * nse(obs, sim, na.rm = na.rm)

#' @rdname fit_loss
#' @export
kge_loss <- function(obs, sim, na.rm = TRUE) -1 * kge(obs, sim, na.rm = na.rm)

#' @rdname fit_loss
#' @export
kge_prime_loss <- function(obs, sim, na.rm = TRUE) -1 * kge_prime(obs, sim, na.rm = na.rm)

#' @rdname fit_loss
#' @export
log_kge_loss <- function(obs, sim, na.rm = TRUE) -1 * log_kge(obs, sim, na.rm = na.rm)

#' Mean Absolute Error (MAE) fit function
#'
#' Calculates the mean absolute error between observed and modelled values:
#' 
#' \deqn{\text{MAE} = \frac{1}{n} \sum_{i=1}^n |\text{sim}_i - \text{obs}_i|}
#'
#' Already `0` (perfect fit) at its best and increasing as fit worsens, so
#' it is minimise-oriented as-is and needs no `_loss` companion - pass it
#' straight into `FUN_list`, which \code{\link{calib_aeme}} and
#' \code{\link{run_and_fit}} minimise.
#'
#' MAE stays in the variable's native units, so it is **not** directly
#' comparable across variables with different units or magnitudes -
#' summing MAE from, say, a temperature fit (degC) and an oxygen fit (mg/L)
#' in a multi-variable `FUN_list` lets whichever variable happens to have
#' the larger natural magnitude dominate the combined fit. Prefer a
#' dimensionless metric such as \code{\link{nse_loss}} or
#' \code{\link{kge_loss}} when combining fits across variables; MAE/RMSE are
#' more suited to single-variable calibration or to reporting fit in
#' interpretable, original units.
#'
#' @inheritParams nse
#'
#' @return numeric; mean absolute error, in the same units as `obs`/`sim`.
#'
#' @examples
#' obs <- c(1, 2, 3, 4)
#' sim <- c(1.1, 2.1, 2.9, 4.2)
#' mae(obs, sim)
#'
#' @export
mae <- function(obs, sim, na.rm = TRUE) {
  dat <- prep_metric_data(obs, sim, na.rm)
  if (dat$n == 0) return(NA_real_)
  
  mean(abs(dat$obs - dat$sim))
}

#' Root Mean Square Error (RMSE) fit function
#'
#' Calculates the root mean square error between observed and modelled values:
#' 
#' \deqn{\text{RMSE} = \sqrt{\frac{1}{n} \sum_{i=1}^n (\text{sim}_i - \text{obs}_i)^2}}
#'
#' Already `0` (perfect fit) at its best and increasing as fit
#' worsens, so it is minimise-oriented as-is and needs no `_loss` companion
#' - pass it straight into `FUN_list`, which \code{\link{calib_aeme}} and
#' \code{\link{run_and_fit}} minimise.
#'
#' Like \code{\link{mae}}, RMSE stays in the variable's native units and so
#' is not directly comparable across variables with different units or
#' magnitudes when combined in a multi-variable `FUN_list` - see
#' \code{\link{mae}} for details. RMSE additionally squares errors before
#' averaging, so - like NSE - it weights large deviations more heavily than
#' MAE does.
#'
#' @inheritParams nse
#'
#' @return numeric; root mean square error, in the same units as `obs`/`sim`.
#'
#' @examples
#' obs <- c(1, 2, 3, 4)
#' sim <- c(1.1, 2.1, 2.9, 4.2)
#' rmse(obs, sim)
#'
#' @export
rmse <- function(obs, sim, na.rm = TRUE) {
  dat <- prep_metric_data(obs, sim, na.rm)
  if (dat$n == 0) return(NA_real_)
  
  sqrt(mean((dat$obs - dat$sim)^2))
}

#' Percent Bias (PBIAS) fit function
#'
#' Calculates the absolute percent bias between observed and modelled
#' values - the average tendency of the modelled values to be larger
#' (positive bias) or smaller (negative bias) than the observed values,
#' expressed as a percentage of the observed total:
#' 
#' \deqn{\text{PBIAS} = 100 \times \left| \frac{\sum_{i=1}^n (\text{sim}_i - \text{obs}_i)}{\sum_{i=1}^n \text{obs}_i} \right|}
#'
#' Already `0` (perfect fit, no systematic bias) at its best and increasing as fit worsens in
#' *either* direction, so it is minimise-oriented as-is and needs no `_loss`
#' companion - pass it straight into `FUN_list`, which
#' \code{\link{calib_aeme}} and \code{\link{run_and_fit}} minimise. The
#' absolute value is used rather than the signed value, since an equally
#' large over- or under-estimate is an equally poor fit - minimising the
#' signed value would instead push the calibration towards the most negative
#' (under-estimating) bias possible.
#'
#' Unlike \code{\link{mae}}/\code{\link{rmse}}, it is expressed as a percentage of
#' the observed total rather than in the variable's native units, which
#' makes it directly comparable across variables with different units or
#' magnitudes - similar to \code{\link{nse}}/\code{\link{kge}} in that
#' respect. On its own it only captures systematic over/under-estimation,
#' not timing, variability or shape - it is typically combined with NSE or
#' KGE (which are largely insensitive to a consistent bias) rather than
#' used alone. See Moriasi et al. (2007) for commonly used PBIAS
#' performance thresholds.
#'
#' @inheritParams nse
#'
#' @return numeric; absolute percent bias. Returns `NA` if the sum of `obs` is zero.
#'
#' @examples
#' obs <- c(1, 2, 3, 4)
#' sim <- c(1.1, 2.1, 2.9, 4.2)
#' pbias(obs, sim)
#'
#' @export
pbias <- function(obs, sim, na.rm = TRUE) {
  dat <- prep_metric_data(obs, sim, na.rm)
  if (dat$n == 0) return(NA_real_)
  
  sum_obs <- sum(dat$obs)
  
  if (sum_obs == 0) {
    cli::cli_alert_warning("Sum of observed values is zero; PBIAS is undefined.")
    return(NA_real_)
  }
  
  abs(100 * sum(dat$sim - dat$obs) / sum_obs)
}

#' Mean bias fit function
#'
#' Calculates the mean difference between modelled and observed values, in
#' the variable's native units:
#' 
#' \deqn{\text{Bias} = \frac{1}{n} \sum_{i=1}^n (\text{sim}_i - \text{obs}_i)}
#' 
#' Positive when the model overestimates on average, negative when it 
#' underestimates. Unlike every other function in this file, the raw (signed) 
#' value is returned rather than a zero-is-best, non-negative one.
#'
#' **This makes `bias()` unsafe to drop directly into `FUN_list` for
#' calibration**: since `calib_aeme()`/`run_and_fit()` minimise the
#' returned value, minimising a signed bias would push the calibration
#' towards the most negative (maximally under-estimating) solution rather
#' than towards zero bias. Use \code{\link{pbias}} (or `abs(bias(obs, sim))`) as
#' a calibration objective; use `bias()` for diagnosing the *direction* of
#' a systematic error when inspecting fit after the fact. Like
#' \code{\link{mae}}/\code{\link{rmse}}, it stays in the variable's native
#' units and so is not directly comparable across variables with different
#' units or magnitudes.
#'
#' @inheritParams nse
#'
#' @return numeric; `mean(sim - obs)`, signed, in the same units as
#' `obs`/`sim`.
#'
#' @examples
#' obs <- c(1, 2, 3, 4)
#' sim <- c(1.1, 2.1, 2.9, 4.2)
#' bias(obs, sim)
#'
#' @export
bias <- function(obs, sim, na.rm = TRUE) {
  dat <- prep_metric_data(obs, sim, na.rm)
  if (dat$n == 0) return(NA_real_)
  
  mean(dat$sim - dat$obs)
}

#' Normalized Mean Absolute Error (NMAE)
#'
#' Calculates the Mean Absolute Error normalized by the mean of the observed values:
#'
#' \deqn{\text{NMAE} = \frac{\frac{1}{n} \sum_{i=1}^n |\text{sim}_i - \text{obs}_i|}{\bar{\text{obs}}}}
#'
#' @inheritParams nse
#'
#' @return numeric; NMAE (dimensionless). Returns `NA` if the mean of observations is zero.
#'
#' @export
nmae <- function(obs, sim, na.rm = TRUE) {
  dat <- prep_metric_data(obs, sim, na.rm)
  if (dat$n == 0) return(NA_real_)
  
  mean_obs <- mean(dat$obs)
  if (mean_obs == 0) {
    cli::cli_alert_warning("Mean of observed values is zero; NMAE is undefined.")
    return(NA_real_)
  }
  
  mean(abs(dat$obs - dat$sim)) / mean_obs
}

#' Willmott Index of Agreement (d)
#'
#' Calculates the Index of Agreement (Willmott, 1981), a standardized measure 
#' of the degree of model prediction error which varies between 0 and 1:
#'
#' \deqn{d = 1 - \frac{\sum_{i=1}^n (\text{sim}_i - \text{obs}_i)^2}{\sum_{i=1}^n (|\text{sim}_i - \bar{\text{obs}}| + |\text{obs}_i - \bar{\text{obs}}|)^2}}
#'
#' A value of 1 indicates a perfect match, and 0 indicates no agreement at all.
#'
#' @inheritParams nse
#'
#' @return numeric; Index of Agreement (d), ranging from 0 to 1.
#'
#' @export
d2 <- function(obs, sim, na.rm = TRUE) {
  dat <- prep_metric_data(obs, sim, na.rm)
  if (dat$n == 0) return(NA_real_)
  
  mean_obs <- mean(dat$obs)
  
  num <- sum((dat$sim - dat$obs)^2)
  den <- sum((abs(dat$sim - mean_obs) + abs(dat$obs - mean_obs))^2)
  
  if (den == 0) {
    cli::cli_alert_warning("Denominator is zero; Index of Agreement is undefined.")
    return(NA_real_)
  }
  
  1 - (num / den)
}

#' Pearson Correlation Coefficient (r)
#'
#' Calculates the linear correlation between simulated and observed values.
#'
#' @inheritParams nse
#'
#' @return numeric; Pearson correlation coefficient ranging from -1 to 1.
#'
#' @export
r_pearson <- function(obs, sim, na.rm = TRUE) {
  dat <- prep_metric_data(obs, sim, na.rm)
  if (dat$n < 2) return(NA_real_)
  
  if (stats::sd(dat$obs) == 0 || stats::sd(dat$sim) == 0) {
    cli::cli_alert_warning("Standard deviation is zero; Pearson correlation is undefined.")
    return(NA_real_)
  }
  
  stats::cor(dat$obs, dat$sim, method = "pearson")
}

#' Spearman Rank Correlation Coefficient (rs)
#'
#' Calculates the monotonic relationship between simulated and observed values
#' using Spearman's rank correlation. It is more robust to outliers than Pearson.
#'
#' @inheritParams nse
#'
#' @return numeric; Spearman correlation coefficient ranging from -1 to 1.
#'
#' @export
r_spearman <- function(obs, sim, na.rm = TRUE) {
  dat <- prep_metric_data(obs, sim, na.rm)
  if (dat$n < 2) return(NA_real_)
  
  # Suppress warnings about ties, which are common and safe in Spearman
  suppressWarnings({
    stats::cor(dat$obs, dat$sim, method = "spearman")
  })
}

#' Bardsley Coefficient (B) fit function
#'
#' Calculates the Bardsley coefficient between observed and modelled
#' values, combining the coefficient of determination (\eqn{R^2}) and 
#' Nash-Sutcliffe Efficiency (\eqn{NSE}).
#'
#' The Bardsley coefficient is calculated as:
#' \deqn{B = \frac{R^2}{2 - \text{NSE}}}
#' 
#' It provides a model performance metric that jointly accounts for 
#' correlation and efficiency characteristics.
#'
#' @param obs Numeric vector of observed values.
#' @param sim Numeric vector of simulated (modelled) values.
#' @param na.rm Logical; should missing values (NA) in either vector be removed 
#' pairwise before computation? Default is \code{TRUE}.
#'
#' @return numeric; Bardsley coefficient. Returns `NA` if undefined 
#' (e.g., zero observed variance or singular model fit).
#'
#' @seealso \code{\link{nse}} for the Nash-Sutcliffe Efficiency component.
#'
#' @examples
#' obs <- c(1, 2, 3, 4)
#' sim <- c(1.1, 2.1, 2.9, 4.2)
#' bardsley(obs, sim)
#'
#' @export
bardsley <- function(obs, sim, na.rm = TRUE) {
  dat <- prep_metric_data(obs, sim, na.rm)
  if (dat$n <= 1) return(NA_real_)
  
  obs_var <- sum((dat$obs - mean(dat$obs))^2)
  
  if (obs_var == 0) {
    cli::cli_alert_warning("Observed variance is zero; Bardsley denominator is undefined.")
    return(NA_real_)
  }
  
  # 1. Calculate R-squared from linear model
  fit_lm <- lm(dat$obs ~ dat$sim)
  r2 <- summary(fit_lm)$r.squared
  
  # 2. Calculate NSE component
  nse_val <- nse(dat$obs, dat$sim, na.rm = FALSE)
  
  # 3. Calculate Bardsley coefficient
  if ((2 - nse_val) == 0) {
    return(NA_real_)
  }
  
  r2 / (2 - nse_val)
}
