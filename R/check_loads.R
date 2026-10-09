#' Sanity-check lake loading against simple hydrological benchmarks
#'
#' Flags discharge and load values that look implausible relative to the
#' lake's own volume and surface area, or relative to broad plausible
#' concentration ranges (the same ranges applied at ingestion by
#' \code{standardise_inflow()}). This is a lightweight order-of-magnitude
#' sanity check -- not a substitute for expert review -- intended to catch
#' things like unit errors (e.g. m3/s mistaken for m3/day), a lake volume/area
#' that doesn't match the inflow data, or forcing data that has been
#' mis-scaled.
#'
#' @section Checks performed:
#' \itemize{
#'   \item \strong{residence_time_days}: lake volume divided by annual
#'     discharge, in days. Flagged if outside \code{[1, 36525]} (1 day to 100
#'     years) -- a lake flushing in under a day, or essentially never,
#'     usually indicates a units or magnitude problem rather than a real lake.
#'   \item \strong{hydraulic_loading_m_yr}: annual discharge divided by lake
#'     surface area, in metres of water per year. Flagged if outside
#'     \code{[0.01, 1e5]} m/yr.
#'   \item \strong{implied_conc_*}: the flow-weighted mean concentration
#'     implied by \code{load / discharge} for each of \code{NIT_amm},
#'     \code{NIT_nit}, \code{PHS_frp}, \code{CAR_doc} and \code{CHM_oxy} (when
#'     present), checked against the same plausible ranges used by
#'     \code{standardise_inflow()}.
#' }
#' Thresholds are deliberately loose (order-of-magnitude) so that only clearly
#' implausible values are flagged.
#'
#' @inheritParams build_aeme
#' @param loads data.frame; output of \code{summarise_loads(aeme, by_inflow = FALSE)}.
#'   If \code{NULL} (default), it is calculated internally.
#' @param verbose logical; if \code{TRUE} (default), emit a \code{cli_warn}
#'   for every flagged check.
#'
#' @return A data frame with columns \code{check}, \code{value}, \code{unit},
#' \code{lower}, \code{upper}, \code{flag} (\code{TRUE} if \code{value} falls
#' outside \code{[lower, upper]}) and \code{message} (\code{NA} when not
#' flagged).
#'
#' @importFrom dplyr bind_rows
#' @export
#'
#' @examples
#' aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
#' aeme <- readRDS(aeme_file)
#' check_loads(aeme)
check_loads <- function(aeme, loads = NULL, verbose = TRUE) {

  aeme <- check_aeme(aeme)
  if (is.null(loads)) {
    loads <- summarise_loads(aeme, by_inflow = FALSE)
  }

  lke <- lake(aeme)
  lake_area <- lke[["area"]]
  lake_vol  <- tryCatch(calc_lake_vol(aeme), error = function(e) NA_real_)

  disch <- loads[loads[["var_aeme"]] == "HYD_flow" &
                  loads[["inflow_id"]] == "all", ]
  annual_discharge <- if (nrow(disch) == 1) disch[["annual_average"]] else NA_real_

  checks <- list()

  # -- Hydraulic residence time ------------------------------------------------
  if (!is.na(lake_vol) && !is.na(annual_discharge) && annual_discharge > 0) {
    residence_days <- lake_vol / annual_discharge * 365.25
    checks[["residence_time_days"]] <- .make_load_check(
      check = "residence_time_days", value = residence_days, unit = "days",
      lower = 1, upper = 365.25 * 100,
      message_low  = "Estimated hydraulic residence time is under 1 day -- lake would flush almost instantly. Check inflow discharge magnitude/units and lake volume.",
      message_high = "Estimated hydraulic residence time exceeds 100 years -- check whether inflow discharge is too low, or lake volume/hypsograph too large."
    )
  }

  # -- Areal hydraulic loading --------------------------------------------------
  if (!is.null(lake_area) && !is.na(annual_discharge) && annual_discharge > 0) {
    hyd_loading <- annual_discharge / lake_area
    checks[["hydraulic_loading_m_yr"]] <- .make_load_check(
      check = "hydraulic_loading_m_yr", value = hyd_loading, unit = "m/yr",
      lower = 0.01, upper = 1e5,
      message_low  = "Areal hydraulic loading (discharge / lake area) is implausibly low -- check discharge units and magnitude.",
      message_high = "Areal hydraulic loading (discharge / lake area) is implausibly high -- check discharge units (e.g. m3/s vs m3/day) or lake area."
    )
  }

  # -- Implied flow-weighted mean concentrations -------------------------------
  conc_ranges <- list(
    NIT_amm = c(0, 10), NIT_nit = c(0, 10), PHS_frp = c(0, 5),
    CAR_doc = c(0, 100), CHM_oxy = c(0, 25)
  )
  if (!is.na(annual_discharge) && annual_discharge > 0) {
    for (v in names(conc_ranges)) {
      load_row <- loads[loads[["var_aeme"]] == v & loads[["inflow_id"]] == "all", ]
      if (nrow(load_row) != 1) next
      implied_conc <- (load_row[["annual_average"]] * 1000) / annual_discharge
      rng <- conc_ranges[[v]]
      checks[[paste0("implied_conc_", v)]] <- .make_load_check(
        check = paste0("implied_conc_", v), value = implied_conc, unit = "g/m3",
        lower = rng[1], upper = rng[2],
        message_low  = paste0("{.code ", v, "}: flow-weighted mean concentration implied by load/discharge is below the plausible range."),
        message_high = paste0("{.code ", v, "}: flow-weighted mean concentration implied by load/discharge is above the plausible range.")
      )
    }
  }

  out <- dplyr::bind_rows(checks)
  rownames(out) <- NULL

  if (verbose && nrow(out) > 0) {
    flagged <- out[out[["flag"]], ]
    for (i in seq_len(nrow(flagged))) {
      cli::cli_warn(c("!" = flagged[["message"]][i]),
                    class = "aeme_warn_check_loads")
    }
  }

  out
}

#' @noRd
.make_load_check <- function(check, value, unit, lower, upper,
                             message_low, message_high) {
  flag <- !is.na(value) && (value < lower || value > upper)
  message <- if (is.na(value) || !flag) {
    NA_character_
  } else if (value < lower) {
    message_low
  } else {
    message_high
  }
  data.frame(
    check   = check,
    value   = value,
    unit    = unit,
    lower   = lower,
    upper   = upper,
    flag    = flag,
    message = message,
    stringsAsFactors = FALSE
  )
}
