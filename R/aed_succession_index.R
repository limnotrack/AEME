#' Compute a system-level phytoplankton succession index
#'
#' A diagnostic score for whether a run shows genuine, recurring, multi-group
#' phytoplankton succession -- computable purely from model output, no
#' observations required, so it can be used as a PEST regularisation target
#' (biasing a calibration away from single-group-monopoly or one-way-drift
#' solutions) alongside whatever data-fit objective exists, or just as a
#' diagnostic on a free-running scenario.
#'
#' Three components are returned separately rather than collapsed into one
#' number, because they answer three different questions and a single
#' scalar can be gamed by satisfying only one of them:
#'
#' - **evenness** - Pielou's J, time-averaged. Is biomass actually shared
#'   across groups, or is one group always ~100%?
#' - **dom_entropy** - Shannon entropy of the "which group is dominant this
#'   month" series. Across the whole run, is *winning* itself spread across
#'   groups, or does one group win almost every month?
#' - **periodicity** - does the same group reliably win the same calendar
#'   month across different years, and is that pattern stable across the
#'   whole record (see Details for why this is not simple autocorrelation)?
#'
#' A high `composite` score needs all three: genuine multi-group coexistence,
#' genuinely shared dominance, and a repeating annual rhythm. Any one alone
#' is not sufficient.
#'
#' @inheritParams build_aeme
#' @param groups character vector of phytoplankton group names, matching the
#' `PHY_<group>` variables simulated in `aeme` (e.g. `c("cyano", "green",
#' "diatom")` matches `PHY_cyano`, `PHY_green`, `PHY_diatom`).
#' @param depth numeric; depth (m, surface-referenced) at which each group's
#' biomass is evaluated. Default `0` (surface).
#' @param ens_n integer; ensemble member to use. Default `1`.
#'
#' @return a list:
#' \itemize{
#' \item{evenness}{ - mean Pielou's J over the simulated (post-spin-up)
#' period, in \[0, 1\]. 0 = permanent single-group monopoly at every
#' timestep. 1 = every group always holds an equal biomass share.}
#' \item{dom_entropy}{ - Shannon entropy of the monthly-dominant-group
#' series, normalised to \[0, 1\] by dividing by ln(S). 0 = one group wins
#' every single month for the whole run. 1 = every group wins an equal share
#' of months.}
#' \item{periodicity}{ - `match_rate * (1 - regime_shift)`, in \[0, 1\].
#' Near 1 = the same group reliably wins the same calendar month every year,
#' consistently across the whole record. Near 0 = no reliable calendar-month
#' pattern, or a pattern that isn't stable across the record.}
#' \item{match_rate}{ - fraction of months where the dominant group matches
#' the modal (most common) dominant group for that calendar month, across
#' all years.}
#' \item{regime_shift}{ - total-variation distance, in \[0, 1\], between the
#' dominant-group frequency distribution in the first vs. second half of the
#' record. Near 0 = consistent throughout. Near 1 = the record is really two
#' different eras (a one-way transient), not one repeating cycle.}
#' \item{composite}{ - geometric mean of `evenness`, `dom_entropy`, and
#' `periodicity` -- 0 if any component is 0. Use the three components
#' individually for diagnosis; use `composite` only as a single PEST
#' regularisation observation.}
#' \item{monthly}{ - the underlying monthly group-fraction/dominant-group
#' table, for inspection/plotting.}
#' }
#'
#' @details
#' A one-way transient (e.g. a spin-up-relaxation artifact where the system
#' slowly drifts from one group's dominance to another's, and never switches
#' back) can score deceptively well on `evenness` and `dom_entropy` alone --
#' both groups get real biomass, both win real months -- while `periodicity`
#' stays near zero, because the pattern never repeats; it just drifts from
#' one regime to another once. That's why periodicity is a separate,
#' required component rather than folded away: it's the only one of the
#' three that distinguishes recurring succession from a slow equilibration.
#'
#' An earlier design for the periodicity component used lag-12
#' autocorrelation of each group's continuous fractional-share series. That
#' failed validation: it scored a confirmed one-way transient almost as
#' "periodic" as a genuinely recurring pattern, including a flat
#' single-group monopoly. The reason is structural, not a detrending bug:
#' light/temperature forcing gives essentially every scenario a real annual
#' bloom-timing signal regardless of which group is blooming, so a single
#' group's continuous share is self-similar at lag 12 whether or not the
#' *identity* of the dominant group is actually what's repeating.
#' Autocorrelation of a continuous series can't distinguish "the same group
#' wins every year" from "a bloom happened on the same calendar schedule
#' while the system was transitioning between two states." The
#' `match_rate`/`regime_shift` construction works directly on which group
#' wins instead, and is what should be used or extended if this index needs
#' further work.
#'
#' This index cannot tell you whether the *timing* is right (diatom
#' blooming in spring vs. diatom blooming in autumn) -- only that a
#' plausible-looking, periodic, multi-group pattern exists at all. Treat a
#' high composite score as necessary, not sufficient; it does not replace
#' calibration against real community-composition data where that is
#' available.
#'
#' @importFrom stats aggregate
#'
#' @export
aed_succession_index <- function(aeme, model, groups = c("cyano", "green", "diatom"),
                                  depth = 0, ens_n = 1) {

  aeme <- check_aeme(aeme)
  if (missing(model)) {
    model <- list_models(aeme)[1]
  } else {
    model <- check_model(model = model)
  }
  if (length(model) > 1) {
    cli::cli_abort("{.arg model} must be a single model; got {.val {model}}.")
  }

  var_sim <- paste0("PHY_", groups)
  df_list <- lapply(seq_along(groups), function(i) {
    d <- get_var(aeme = aeme, model = model, var_sim = var_sim[i], depth = depth,
                depth_ref = "surface", return_df = TRUE, ens_n = ens_n,
                use_obs = FALSE, remove_spin_up = TRUE)
    data.frame(Date = as.Date(d$Date), group = groups[i], value = d$value)
  })
  df <- do.call(rbind, df_list)
  df$month <- format(df$Date, "%Y-%m")

  mon <- stats::aggregate(df["value"], by = list(month = df$month, group = df$group),
                          FUN = mean, na.rm = TRUE)
  mon <- stats::reshape(mon, idvar = "month", timevar = "group", direction = "wide")
  names(mon) <- sub("^value\\.", "", names(mon))
  mon$Date <- as.Date(paste0(mon$month, "-15"))
  mon <- mon[order(mon$Date), ]

  total <- rowSums(mon[groups], na.rm = TRUE)
  frac <- mon[groups] / total
  S <- length(groups)

  # -- evenness (Pielou J), averaged over months --
  H <- apply(frac, 1, function(p) {
    p <- p[p > 0]
    -sum(p * log(p))
  })
  J <- H / log(S)
  evenness <- mean(J, na.rm = TRUE)

  # -- dominance entropy, over the whole run --
  mon$dominant <- groups[apply(frac, 1, which.max)]
  win_freq <- table(factor(mon$dominant, levels = groups)) / nrow(mon)
  win_freq_nz <- win_freq[win_freq > 0]
  dom_entropy <- -sum(win_freq_nz * log(win_freq_nz)) / log(S)

  # -- periodicity: does the SAME group tend to win the SAME calendar month
  # across different years, and is that pattern stable across the whole
  # record (not just true of a majority formed by a one-time regime
  # change)? See Details for why this replaced a lag-12 autocorrelation
  # design that failed validation.
  cal_month <- format(mon$Date, "%m")
  yr <- format(mon$Date, "%Y")
  modal <- tapply(mon$dominant, cal_month,
                  function(x) names(sort(table(x), decreasing = TRUE))[1])
  match_rate <- mean(mon$dominant == modal[cal_month])

  yrs <- sort(unique(yr))
  half <- floor(length(yrs) / 2)
  if (half >= 1 && length(yrs) - half >= 1) {
    first_yrs <- yrs[seq_len(half)]; second_yrs <- yrs[(half + 1):length(yrs)]
    f1 <- prop.table(table(factor(mon$dominant[yr %in% first_yrs], levels = groups)))
    f2 <- prop.table(table(factor(mon$dominant[yr %in% second_yrs], levels = groups)))
    regime_shift <- 0.5 * sum(abs(f1 - f2))
  } else {
    regime_shift <- NA_real_
  }
  periodicity <- match_rate * (1 - ifelse(is.na(regime_shift), 0, regime_shift))

  composite <- (evenness * dom_entropy * max(periodicity, 0)) ^ (1 / 3)

  list(evenness = evenness, dom_entropy = dom_entropy,
       periodicity = periodicity, match_rate = match_rate,
       regime_shift = regime_shift, composite = composite, monthly = mon)
}
