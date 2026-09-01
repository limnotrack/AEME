#' Summarise what the observations actually contain
#'
#' @description
#' Reports, for every observed variable: how many observations there are,
#' when they span, how they are distributed across years and seasons, and
#' whether they are depth *profiles* or single-depth samples.
#'
#' The profile question is the one that usually decides whether a
#' calibration is possible. A variable sampled hundreds of times can still be
#' useless for constraining stratification if every one of those samples is a
#' surface grab, and observation counts alone hide that completely.
#'
#' Each variable is classified as one of
#' \describe{
#'   \item{`"profile"`}{at least half its sampling dates carry `min_depths`
#'     or more distinct depths - a depth-resolved record.}
#'   \item{`"discrete"`}{depths are recorded, but most visits carry fewer
#'     than `min_depths` of them - surface grabs, or surface/bottom pairs.}
#'   \item{`"scalar"`}{no depth information at all, e.g. water level.}
#' }
#'
#' The summary also carries the date range of every *forcing* series - met,
#' inflows, outflows - because those, not the observations, are usually what
#' limits how early a simulation can start. See [suggest_sim_period()],
#' which turns this summary into a runnable period.
#'
#' @param aeme Aeme object.
#' @param vars_sim Character. Variables (`var_aeme` values) to summarise.
#'   Default `NULL` summarises every observed variable.
#' @param min_depths Integer. Distinct depths a visit needs before it counts
#'   as a profile. Default `3L`.
#'
#' @return An object of class `aeme_obs_summary`: a list with
#'   \describe{
#'     \item{`variables`}{one row per variable - `n_obs`, `n_dates`,
#'       `first`, `last`, `n_years`, `n_months` (distinct calendar months,
#'       i.e. seasonal coverage), `n_profiles`, `profile_frac`,
#'       `median_depths`, `depth_min`, `depth_max` and `kind`.}
#'     \item{`years`}{one row per variable per calendar year - `n_obs`,
#'       `n_dates`, `n_profiles`.}
#'     \item{`forcing`}{one row per forcing series - `source`, `first`,
#'       `last`, `n`.}
#'     \item{`window`}{the date range over which every forcing series has
#'       data, i.e. the widest period the model can be run over.}
#'   }
#'
#' @seealso [suggest_sim_period()], [set_sim_period()], [get_obs()]
#' @export
#'
#' @examples
#' \dontrun{
#' aeme <- readRDS(system.file("extdata/aeme.rds", package = "AEME"))
#' s <- summarise_obs(aeme)
#' s
#' s$variables
#' s$years[s$years$var_aeme == "HYD_temp", ]
#' }
summarise_obs <- function(aeme, vars_sim = NULL, min_depths = 3L) {

  aeme <- check_aeme(aeme)
  if (!is.numeric(min_depths) || length(min_depths) != 1 || is.na(min_depths) ||
      min_depths < 1) {
    cli::cli_abort("{.arg min_depths} must be a single positive number.")
  }

  obs <- observations(aeme)

  # Lake profiles and level are stored separately and have different columns -
  # level carries no depths at all. Stack them so one code path handles both,
  # with NA depths marking the scalar series.
  lake <- obs$lake
  lvl <- obs$level
  parts <- list()
  if (!is.null(lake) && nrow(lake) > 0) {
    parts$lake <- data.frame(
      Date = as.Date(lake$Date),
      var_aeme = as.character(lake$var_aeme),
      depth = depth_mid_from(lake),
      stringsAsFactors = FALSE
    )
  }
  if (!is.null(lvl) && nrow(lvl) > 0) {
    parts$level <- data.frame(
      Date = as.Date(lvl$Date),
      var_aeme = if ("var_aeme" %in% names(lvl)) as.character(lvl$var_aeme) else "LKE_lvlwtr",
      depth = NA_real_,
      stringsAsFactors = FALSE
    )
  }

  if (length(parts) == 0) {
    cli::cli_abort("This {.cls Aeme} object has no observations to summarise.")
  }
  all_obs <- do.call(rbind, parts)
  rownames(all_obs) <- NULL

  if (!is.null(vars_sim)) {
    missing_vars <- setdiff(vars_sim, unique(all_obs$var_aeme))
    if (length(missing_vars) > 0) {
      cli::cli_warn("No observations for {.val {missing_vars}}.")
    }
    all_obs <- all_obs[all_obs$var_aeme %in% vars_sim, , drop = FALSE]
    if (nrow(all_obs) == 0) {
      cli::cli_abort("No observations for any of {.arg vars_sim}.")
    }
  }

  # Per variable-date: how many distinct depths were sampled on that visit.
  # A visit with no depths at all (level) counts as one, so a scalar series is
  # never mistaken for a failed profile.
  by_visit <- lapply(split(all_obs, list(all_obs$var_aeme, all_obs$Date),
                           drop = TRUE), function(d) {
    dep <- d$depth[!is.na(d$depth)]
    data.frame(var_aeme = d$var_aeme[1], Date = d$Date[1],
               n_obs = nrow(d),
               n_depths = if (length(dep)) length(unique(dep)) else NA_integer_,
               stringsAsFactors = FALSE)
  })
  by_visit <- do.call(rbind, by_visit)
  rownames(by_visit) <- NULL
  by_visit$is_profile <- !is.na(by_visit$n_depths) & by_visit$n_depths >= min_depths

  variables <- lapply(split(by_visit, by_visit$var_aeme), function(v) {
    dep <- all_obs$depth[all_obs$var_aeme == v$var_aeme[1]]
    dep <- dep[!is.na(dep)]
    n_dates <- nrow(v)
    n_prof <- sum(v$is_profile)
    frac <- if (n_dates > 0) n_prof / n_dates else 0
    kind <- if (!length(dep)) "scalar" else if (frac >= 0.5) "profile" else "discrete"
    data.frame(
      var_aeme = v$var_aeme[1],
      n_obs = sum(v$n_obs),
      n_dates = n_dates,
      first = min(v$Date),
      last = max(v$Date),
      n_years = length(unique(format(v$Date, "%Y"))),
      n_months = length(unique(format(v$Date, "%m"))),
      n_profiles = n_prof,
      profile_frac = round(frac, 3),
      median_depths = if (all(is.na(v$n_depths))) NA_real_ else
        stats::median(v$n_depths, na.rm = TRUE),
      depth_min = if (length(dep)) min(dep) else NA_real_,
      depth_max = if (length(dep)) max(dep) else NA_real_,
      kind = kind,
      stringsAsFactors = FALSE
    )
  })
  variables <- do.call(rbind, variables)
  rownames(variables) <- NULL
  variables <- variables[order(-variables$n_obs), , drop = FALSE]

  # Attach the human-readable label where there is one.
  variables$name_text <- var_label(variables$var_aeme)
  variables <- variables[, c("var_aeme", "name_text",
                             setdiff(names(variables),
                                     c("var_aeme", "name_text")))]

  by_visit$year <- as.integer(format(by_visit$Date, "%Y"))
  years <- lapply(split(by_visit, list(by_visit$var_aeme, by_visit$year),
                        drop = TRUE), function(d) {
    data.frame(var_aeme = d$var_aeme[1], year = d$year[1],
               n_obs = sum(d$n_obs), n_dates = nrow(d),
               n_profiles = sum(d$is_profile), stringsAsFactors = FALSE)
  })
  years <- do.call(rbind, years)
  rownames(years) <- NULL
  years <- years[order(years$var_aeme, years$year), , drop = FALSE]

  forcing <- forcing_ranges(aeme)

  # The runnable window is where *every* forcing series has data. Any date
  # outside it cannot be simulated regardless of how good the observations are.
  window <- if (nrow(forcing) > 0) {
    c(start = max(forcing$first, na.rm = TRUE),
      stop = min(forcing$last, na.rm = TRUE))
  } else {
    c(start = as.Date(NA), stop = as.Date(NA))
  }

  structure(list(variables = variables, years = years, forcing = forcing,
                 window = window, min_depths = as.integer(min_depths)),
            class = "aeme_obs_summary")
}


#' Choose a simulation period from what the data can support
#'
#' @description
#' Picks `start` and `stop` dates for a simulation by intersecting three
#' things: where the forcing data exist, where the observations to be fitted
#' exist, and how much spin-up is needed before the first comparison.
#'
#' The forcing constraint is the one most often missed. Observations
#' routinely predate the meteorological record by years, and a period chosen
#' from the observations alone can begin before the model has anything to
#' run on - or leave no room for spin-up, so the first observations are
#' compared against a model still relaxing from its initial condition.
#'
#' Sparse leading and trailing years are trimmed by `min_density`, measured
#' in profiles rather than observations, because a depth-resolved
#' calibration is constrained by casts and not by surface grabs. Only the
#' ends are trimmed, so a lean year inside a dense record is kept and the
#' period stays contiguous.
#'
#' @param aeme Aeme object.
#' @param vars_sim Character. Variables the period must cover. Default
#'   `NULL` uses every observed variable, which is rarely what you want -
#'   name the variables you intend to fit.
#' @param spin_up Numeric. Days of spin-up required before `start`. Default
#'   `NULL` takes the longest spin-up already set on the object.
#' @param min_depths Integer. Distinct depths a visit needs before it counts
#'   as a profile. Default `3L`.
#' @param use_profiles Logical. Count only profiles when judging coverage.
#'   Default `TRUE`; `FALSE` counts every sampling date. Variables of kind
#'   `"scalar"` always count by date.
#' @param min_density Numeric. Drop leading and trailing years holding fewer
#'   than this fraction of the median year's count. Default `0.25`; `0`
#'   keeps the record whole.
#' @param min_years Integer. Fewest years a record must span before
#'   `min_density` is applied. Default `4L`.
#' @param align Character. `"none"` (default) starts and stops on
#'   observation dates; `"year"` snaps outward to whole years beginning
#'   `year_start_month`.
#' @param year_start_month Integer. First month of the year used by
#'   `align = "year"`. Default `7L`, the southern-hemisphere hydrological
#'   year, so a period does not split a stratified season in half.
#'
#' @return An object of class `aeme_sim_period`: a list with `start`,
#'   `stop`, `spin_up`, `spin_up_start`, `limited_by` (what set each end),
#'   `coverage` (per-variable counts inside the chosen period), `dropped`
#'   (years trimmed at each end) and `summary` (the [summarise_obs()]
#'   result).
#'
#' @seealso [summarise_obs()], [set_sim_period()], [set_time()]
#' @export
#'
#' @examples
#' \dontrun{
#' aeme <- readRDS(system.file("extdata/aeme.rds", package = "AEME"))
#' p <- suggest_sim_period(aeme, vars_sim = c("HYD_temp", "CHM_oxy"),
#'                         spin_up = 365)
#' p
#' aeme <- set_sim_period(aeme, p)
#' }
suggest_sim_period <- function(aeme, vars_sim = NULL, spin_up = NULL,
                               min_depths = 3L, use_profiles = TRUE,
                               min_density = 0.25, min_years = 4L,
                               align = c("none", "year"),
                               year_start_month = 7L) {

  aeme <- check_aeme(aeme)
  align <- rlang::arg_match(align)
  if (!is.numeric(min_density) || length(min_density) != 1 ||
      is.na(min_density) || min_density < 0) {
    cli::cli_abort("{.arg min_density} must be a single non-negative number.")
  }

  s <- summarise_obs(aeme, vars_sim = vars_sim, min_depths = min_depths)

  if (is.null(spin_up)) {
    su <- time(aeme)$spin_up
    spin_up <- if (is.list(su)) max(unlist(su), na.rm = TRUE) else su
    if (!length(spin_up) || is.na(spin_up)) spin_up <- 0
  }
  spin_up <- as.numeric(spin_up)

  if (any(is.na(s$window))) {
    cli::cli_abort("No forcing data found - cannot determine a runnable window.")
  }

  # Earliest the simulation proper can start: the forcing has to cover the
  # spin-up as well, so the spin-up eats into the front of the record.
  earliest <- s$window[["start"]] + spin_up
  latest <- s$window[["stop"]]
  if (earliest >= latest) {
    cli::cli_abort(c(
      "Spin-up of {spin_up} day{?s} leaves no runnable period.",
      "i" = "Forcing covers {format(s$window[['start']])} to {format(latest)}."
    ))
  }

  vars <- s$variables$var_aeme
  scalar_vars <- s$variables$var_aeme[s$variables$kind == "scalar"]

  # Count each variable by profiles or by dates, then keep only dates that
  # fall inside the runnable window.
  yr <- s$years[s$years$var_aeme %in% vars, , drop = FALSE]
  yr$count <- if (use_profiles) yr$n_profiles else yr$n_dates
  yr$count[yr$var_aeme %in% scalar_vars] <- yr$n_dates[yr$var_aeme %in% scalar_vars]

  win_years <- as.integer(format(c(earliest, latest), "%Y"))
  yr <- yr[yr$year >= win_years[1] & yr$year <= win_years[2] & yr$count > 0, ,
           drop = FALSE]
  if (nrow(yr) == 0) {
    cli::cli_abort(c(
      "No usable observations inside the runnable window.",
      "i" = "Window is {format(earliest)} to {format(latest)} after spin-up.",
      "i" = "Try {.code use_profiles = FALSE} or a shorter {.arg spin_up}."
    ))
  }

  # A year only counts if every requested variable is represented in it -
  # otherwise the period can start in a year where the variable you care most
  # about has nothing.
  per_year <- stats::aggregate(count ~ year, data = yr, FUN = sum)
  n_vars_year <- stats::aggregate(var_aeme ~ year, data = yr,
                                  FUN = function(v) length(unique(v)))
  per_year$n_vars <- n_vars_year$var_aeme[match(per_year$year, n_vars_year$year)]
  per_year <- per_year[order(per_year$year), , drop = FALSE]

  dropped <- c(head = integer(0), tail = integer(0))
  keep <- per_year
  if (min_density > 0 && nrow(per_year) >= min_years) {
    thresh <- stats::median(per_year$count) * min_density
    ok <- per_year$count >= thresh
    # Trim only from the ends: a lean year mid-record is real data, and
    # removing it would split the period in two.
    first_ok <- which(ok)[1]
    last_ok <- utils::tail(which(ok), 1)
    if (length(first_ok) && !is.na(first_ok)) {
      dropped <- list(head = per_year$year[seq_len(first_ok - 1)],
                      tail = if (last_ok < nrow(per_year))
                        per_year$year[(last_ok + 1):nrow(per_year)] else integer(0))
      keep <- per_year[first_ok:last_ok, , drop = FALSE]
    }
  }
  if (!is.list(dropped)) dropped <- list(head = integer(0), tail = integer(0))

  keep_years <- keep$year
  obs_in <- yr[yr$year %in% keep_years, , drop = FALSE]

  # Start and stop on dates that actually carry data, clamped to the window.
  vis <- visit_dates(aeme, vars = vars, min_depths = min_depths,
                     use_profiles = use_profiles, scalar_vars = scalar_vars)
  vis <- vis[as.integer(format(vis, "%Y")) %in% keep_years]
  vis <- vis[vis >= earliest & vis <= latest]
  if (!length(vis)) {
    cli::cli_abort("No observation dates survive the window and density trim.")
  }
  start <- min(vis)
  stop <- max(vis)

  if (align == "year") {
    start <- year_floor(start, year_start_month)
    stop <- year_ceiling(stop, year_start_month)
    start <- max(start, earliest)
    stop <- min(stop, latest)
  }

  limited_by <- c(
    start = if (start <= earliest + 1) "forcing (spin-up)" else
      if (length(dropped$head)) "sparse early years" else "observations",
    stop = if (stop >= latest - 1) "forcing" else
      if (length(dropped$tail)) "sparse late years" else "observations"
  )

  coverage <- lapply(split(obs_in, obs_in$var_aeme), function(d) {
    data.frame(var_aeme = d$var_aeme[1], n_obs = sum(d$n_obs),
               n_dates = sum(d$n_dates), n_profiles = sum(d$n_profiles),
               first_year = min(d$year), last_year = max(d$year),
               stringsAsFactors = FALSE)
  })
  coverage <- do.call(rbind, coverage)
  rownames(coverage) <- NULL
  coverage$kind <- s$variables$kind[match(coverage$var_aeme, s$variables$var_aeme)]
  coverage <- coverage[order(-coverage$n_obs), , drop = FALSE]

  structure(list(start = start, stop = stop, spin_up = spin_up,
                 spin_up_start = start - spin_up,
                 limited_by = limited_by, coverage = coverage,
                 dropped = dropped, window = s$window, summary = s),
            class = "aeme_sim_period")
}


#' Apply a suggested simulation period to an Aeme object
#'
#' @description
#' Thin wrapper over [set_time()] that takes the object returned by
#' [suggest_sim_period()], so the period that was reported is the period
#' that gets set.
#'
#' @param aeme Aeme object.
#' @param period An `aeme_sim_period` from [suggest_sim_period()].
#' @param spin_up Numeric. Override the period's spin-up. Default `NULL`
#'   keeps it.
#'
#' @return The Aeme object with its time slot set.
#' @seealso [suggest_sim_period()], [set_time()]
#' @export
set_sim_period <- function(aeme, period, spin_up = NULL) {
  if (!inherits(period, "aeme_sim_period")) {
    cli::cli_abort("{.arg period} must come from {.fn suggest_sim_period}.")
  }
  su <- if (is.null(spin_up)) period$spin_up else spin_up
  set_time(aeme, start = format(period$start), stop = format(period$stop),
           spin_up = su)
}


# Helpers ---------------------------------------------------------------------

#' @noRd
depth_mid_from <- function(df) {
  # Current schema: a single `depth` column. Fall back to the legacy
  # depth_from / depth_to pair (or a stray depth_mid) for un-migrated frames.
  if ("depth" %in% names(df)) return(as.numeric(df$depth))
  mid <- if ("depth_mid" %in% names(df)) as.numeric(df$depth_mid) else
    rep(NA_real_, nrow(df))
  have <- all(c("depth_from", "depth_to") %in% names(df))
  if (have) {
    derived <- (as.numeric(df$depth_from) + as.numeric(df$depth_to)) / 2
    mid[is.na(mid)] <- derived[is.na(mid)]
  }
  mid
}

#' @noRd
var_label <- function(vars) {
  kn <- tryCatch({
    e <- new.env()
    utils::data("key_naming", package = "AEME", envir = e)
    get("key_naming", envir = e)
  }, error = function(e) NULL)
  if (is.null(kn)) return(rep(NA_character_, length(vars)))
  as.character(kn$name_text[match(vars, kn$var_aeme)])
}

#' @noRd
forcing_ranges <- function(aeme) {
  rows <- list()
  add <- function(src, d) {
    if (is.null(d) || !nrow(d) || !"Date" %in% names(d)) return(NULL)
    dt <- as.Date(d$Date)
    dt <- dt[!is.na(dt)]
    if (!length(dt)) return(NULL)
    rows[[length(rows) + 1]] <<- data.frame(
      source = src, first = min(dt), last = max(dt), n = length(dt),
      stringsAsFactors = FALSE)
  }

  inp <- tryCatch(input(aeme), error = function(e) NULL)
  add("met", inp$met)

  inf <- tryCatch(inflows(aeme), error = function(e) NULL)
  if (!is.null(inf$data)) {
    for (nm in names(inf$data)) add(paste0("inflow: ", nm), inf$data[[nm]])
  }
  outf <- tryCatch(outflows(aeme), error = function(e) NULL)
  if (!is.null(outf$data)) {
    for (nm in names(outf$data)) add(paste0("outflow: ", nm), outf$data[[nm]])
  }

  if (!length(rows)) {
    return(data.frame(source = character(0), first = as.Date(character(0)),
                      last = as.Date(character(0)), n = integer(0)))
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' @noRd
visit_dates <- function(aeme, vars, min_depths, use_profiles, scalar_vars) {
  obs <- observations(aeme)
  out <- as.Date(character(0))
  lake <- obs$lake
  if (!is.null(lake) && nrow(lake) > 0) {
    d <- data.frame(Date = as.Date(lake$Date),
                    var_aeme = as.character(lake$var_aeme),
                    depth = depth_mid_from(lake), stringsAsFactors = FALSE)
    d <- d[d$var_aeme %in% vars, , drop = FALSE]
    if (nrow(d)) {
      key <- paste(d$var_aeme, d$Date)
      nd <- tapply(d$depth, key, function(x) length(unique(x[!is.na(x)])))
      keep <- if (use_profiles) names(nd)[nd >= min_depths] else names(nd)
      # Scalar variables carry no depths, so a profile filter would drop them
      # entirely - let them through on date alone.
      sc <- names(nd)[sub(" .*$", "", names(nd)) %in% scalar_vars]
      keep <- union(keep, sc)
      out <- c(out, as.Date(sub("^\\S+ ", "", keep)))
    }
  }
  lvl <- obs$level
  if (!is.null(lvl) && nrow(lvl) > 0) {
    lv <- if ("var_aeme" %in% names(lvl)) as.character(lvl$var_aeme) else "LKE_lvlwtr"
    if (any(lv %in% vars)) out <- c(out, as.Date(lvl$Date[lv %in% vars]))
  }
  sort(unique(out[!is.na(out)]))
}

#' @noRd
year_floor <- function(d, m) {
  y <- as.integer(format(d, "%Y"))
  cand <- as.Date(sprintf("%d-%02d-01", y, m))
  ifelse(d >= cand, cand, as.Date(sprintf("%d-%02d-01", y - 1L, m))) |>
    as.Date(origin = "1970-01-01")
}

#' @noRd
year_ceiling <- function(d, m) {
  y <- as.integer(format(d, "%Y"))
  cand <- as.Date(sprintf("%d-%02d-01", y, m)) - 1
  ifelse(d <= cand, cand, as.Date(sprintf("%d-%02d-01", y + 1L, m)) - 1) |>
    as.Date(origin = "1970-01-01")
}


# Print methods ---------------------------------------------------------------

#' @export
print.aeme_obs_summary <- function(x, ...) {
  cli::cli_h1("Observation summary")

  v <- x$variables
  cli::cli_h3("Variables ({nrow(v)})")
  disp <- data.frame(
    variable = v$var_aeme,
    n_obs = v$n_obs,
    dates = v$n_dates,
    profiles = v$n_profiles,
    kind = v$kind,
    depths = ifelse(is.na(v$median_depths), "-",
                    format(v$median_depths, trim = TRUE)),
    from = format(v$first),
    to = format(v$last),
    stringsAsFactors = FALSE
  )
  print(disp, row.names = FALSE)

  cli::cli_text("")
  cli::cli_text("{.emph kind}: profile = {x$min_depths}+ depths on most visits; ",
                "discrete = depths but fewer; scalar = no depths")

  f <- x$forcing
  cli::cli_h3("Forcing")
  if (nrow(f) == 0) {
    cli::cli_alert_warning("No forcing series found.")
  } else {
    print(data.frame(source = f$source, from = format(f$first),
                     to = format(f$last), n = f$n, stringsAsFactors = FALSE),
          row.names = FALSE)
    cli::cli_text("")
    cli::cli_alert_info(
      "Runnable window (all forcing present): {format(x$window[['start']])} to {format(x$window[['stop']])}")
  }
  invisible(x)
}

#' @export
print.aeme_sim_period <- function(x, ...) {
  cli::cli_h1("Suggested simulation period")
  cli::cli_ul(c(
    "Start: {.strong {format(x$start)}}  ({x$limited_by[['start']]})",
    "Stop:  {.strong {format(x$stop)}}  ({x$limited_by[['stop']]})",
    "Spin-up: {x$spin_up} day{?s}, from {format(x$spin_up_start)}"
  ))
  n_days <- as.numeric(x$stop - x$start)
  cli::cli_text("")
  cli::cli_text("Duration: {round(n_days / 365.25, 1)} years ({n_days} days)")

  if (length(x$dropped$head) || length(x$dropped$tail)) {
    cli::cli_text("")
    cli::cli_alert_info("Trimmed sparse years: {.val {c(x$dropped$head, x$dropped$tail)}}")
  }

  cli::cli_h3("Coverage inside the period")
  print(data.frame(variable = x$coverage$var_aeme, n_obs = x$coverage$n_obs,
                   dates = x$coverage$n_dates, profiles = x$coverage$n_profiles,
                   kind = x$coverage$kind, stringsAsFactors = FALSE),
        row.names = FALSE)

  zero <- x$coverage$var_aeme[x$coverage$n_profiles == 0 &
                                x$coverage$kind != "scalar"]
  if (length(zero)) {
    cli::cli_text("")
    cli::cli_alert_warning(
      "No profiles for {.val {zero}} - depth-resolved calibration of {?this/these} variable{?s} is not supported by the data.")
  }

  cli::cli_text("")
  cli::cli_text("Apply with {.code aeme <- set_sim_period(aeme, period)}")
  invisible(x)
}
