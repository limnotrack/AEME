#' Summarise an AEME ensemble across members
#'
#' @description
#' [run_aeme_ensemble()] stores every member's full model output on the
#' `aeme` object. Turning that into a plot or a skill score means, each time,
#' looping over the members and pulling the variable out with
#' [get_var()] - the expensive step. `ensemble_summary()` does that
#' extraction **once** and returns a tidy object holding
#'
#' * `stats` - one row per date (and depth, and model, and variable) with the
#'   ensemble `mean`, `sd`, `n` and the quantiles named in `probs`;
#' * `members` - the extracted long frame the statistics were built from
#'   (`Date`, `depth`, `value`, `Model`, `var_sim`, `ens`), kept unless
#'   `keep_members = FALSE`;
#' * `obs` - the matching lake observations, depth-aligned to `depths`.
#'
#' [plot_ensemble()] accepts the result directly, so a summary built once can
#' be replotted for several variables, depths and intervals for free.
#'
#' @inheritParams plot_ensemble
#' @param vars_sim character; one or more AEME variables to summarise.
#' @param depths numeric; depths (m, referenced to `depth_ref`) at which to
#'   summarise depth-resolved variables. `NULL` (default) pools all layers for
#'   a depth-resolved variable - matching the historical [plot_ensemble()]
#'   behaviour - and is the only sensible value for a 1-D variable such as
#'   `LKE_lvlwtr` or `HYD_thmcln`.
#' @param probs numeric; quantile probabilities for the summary bands. `0.5`
#'   is always added so a median is available.
#' @param depth_ref character; `"surface"` (default) or `"bottom"`, passed to
#'   [get_var()]: whether `depths` are metres below the surface or metres above
#'   the lake bed (see [get_var()] for how the bottom follows the water level).
#'   Observations are placed in the same reference before they are matched to
#'   `depths`. Observations recorded below the surface are the usual case; those
#'   recorded as a height above the bed (see [add_obs()]) are used as they are
#'   for `"bottom"`. An observation in the other reference is converted with the
#'   height of the water surface above the bed on its date: from the lake level
#'   observations, interpolated in time, or where there are none the ensemble
#'   median of the modelled level.
#' @param obs_tol numeric; half-width (m) of the window used to attach
#'   observations to each requested depth. Default 0.5.
#' @param keep_members logical; keep the extracted per-member frame on the
#'   result. Default `TRUE`. `FALSE` drops it - smaller object, but
#'   `plot_ensemble(type = "line")` and future member-level scores then can't
#'   be computed from it.
#'
#' @return An `aeme_ensemble_summary` object: a list of `stats`, `members`
#'   (or `NULL`) and `obs` (or `NULL`), with `probs`, `depths`, `depth_ref`,
#'   `vars_sim`, `vars_2d`, `n_members` and `period` attributes.
#'
#' @seealso [plot_ensemble()], [run_aeme_ensemble()]
#'
#' @export
ensemble_summary <- function(aeme, model, vars_sim = "HYD_temp", depths = NULL,
                             probs = c(0.025, 0.25, 0.5, 0.75, 0.975),
                             remove_spin_up = TRUE, add_obs = TRUE,
                             depth_ref = c("surface", "bottom"), obs_tol = 0.5,
                             keep_members = TRUE) {

  aeme <- check_aeme(aeme)
  model <- check_model(model)
  depth_ref <- rlang::arg_match(depth_ref)

  vars_sim <- vapply(vars_sim, function(v) check_aeme_vars(v),
                     character(1), USE.NAMES = FALSE)
  probs <- sort(unique(c(as.numeric(probs), 0.5)))
  if (any(probs <= 0 | probs >= 1)) {
    cli::cli_abort("{.arg probs} must lie strictly between 0 and 1.")
  }
  if (!is.null(depths)) depths <- sort(unique(as.numeric(depths)))

  outp <- output(aeme)
  n_members <- outp$n_members
  if (is.null(n_members) || n_members < 1L) {
    cli::cli_abort(c("No ensemble output found on {.arg aeme}.",
                     "i" = "Run {.fn run_aeme_ensemble} first."))
  }
  ens1 <- paste0("ens_", sprintf("%03d", 1L))

  q_names <- .ens_q_name(probs)

  # ---- per-variable extraction + summary ---------------------------------
  vars_2d <- character(0)
  per_var <- lapply(vars_sim, function(v) {

    raw <- outp[[ens1]][[model[[1]]]][[v]]
    is_2d <- !is.null(raw) && !is.null(dim(raw)) &&
      !inherits(raw, "aeme_grouped_var")
    if (is_2d) vars_2d[[length(vars_2d) + 1L]] <<- v

    by_depth <- is_2d && !is.null(depths)
    if (is_2d && is.null(depths)) {
      cli::cli_warn(c(
        "!" = "{.val {v}} is depth-resolved but {.arg depths} is {.code NULL};
               pooling all layers.",
        "i" = "Pass {.arg depths} for per-depth bands."))
    }

    members <- dplyr::bind_rows(lapply(seq_len(n_members), function(i) {
      one <- if (by_depth) {
        dplyr::bind_rows(lapply(depths, function(d) {
          get_var(aeme = aeme, model = model, var_sim = v, depth = d,
                        depth_ref = depth_ref, ens_n = i, return_df = TRUE,
                        remove_spin_up = remove_spin_up, cumulative = FALSE)
        }))
      } else {
        get_var(aeme = aeme, model = model, var_sim = v, depth = NULL,
                      depth_ref = depth_ref, ens_n = i, return_df = TRUE,
                      remove_spin_up = remove_spin_up, cumulative = FALSE)
      }
      one$ens <- i
      one
    }))
    members$var_sim <- v

    keys <- c("Date", "Model", "var_sim", if (by_depth) "depth")
    stats <- dplyr::reframe(
      dplyr::group_by(members, dplyr::across(dplyr::all_of(keys))),
      n = sum(!is.na(.data$value)),
      mean = mean(.data$value, na.rm = TRUE),
      sd = stats::sd(.data$value, na.rm = TRUE),
      .stat = q_names,
      .q = stats::quantile(.data$value, probs = probs, na.rm = TRUE,
                           names = FALSE))
    stats <- tidyr::pivot_wider(stats, names_from = ".stat",
                                values_from = ".q")
    if (!by_depth) stats$depth <- NA_real_

    list(members = members, stats = stats)
  })

  members <- dplyr::bind_rows(lapply(per_var, `[[`, "members"))
  stats <- dplyr::bind_rows(lapply(per_var, `[[`, "stats"))
  period <- as.Date(range(members$Date, na.rm = TRUE), tz = "UTC")

  # ---- observations -----------------------------------------------------
  obs_df <- .ens_obs(aeme, vars_sim, vars_2d, depths, obs_tol, period,
                     enabled = add_obs, depth_ref = depth_ref, model = model,
                     remove_spin_up = remove_spin_up)

  structure(
    list(stats = stats,
         members = if (keep_members) members else NULL,
         obs = obs_df),
    class = "aeme_ensemble_summary",
    probs = probs, q_names = q_names, depths = depths, depth_ref = depth_ref,
    vars_sim = vars_sim, vars_2d = vars_2d, n_members = n_members,
    keep_members = keep_members, period = period)
}

#' Quantile-column name for a probability, e.g. 0.025 -> "q2.5".
#' @noRd
.ens_q_name <- function(p) {
  paste0("q", sub("\\.?0+$", "", sprintf("%.4f", p * 100)))
}

#' Depth-aligned lake observations for [ensemble_summary()].
#' @noRd
.ens_obs <- function(aeme, vars_sim, vars_2d, depths, obs_tol, period,
                     enabled = TRUE, depth_ref = "surface", model = NULL,
                     remove_spin_up = TRUE) {
  if (!enabled) return(NULL)
  obs <- observations(aeme)
  if (is.null(obs$lake)) return(NULL)
  # AEME observations may now carry a POSIXct timestamp; `period` is a `Date`
  # range. Reduce the obs timestamp to a UTC calendar `Date` so the window
  # test and the downstream depth alignment stay Date-to-Date.
  obs$lake <- obs_calendar_date(obs$lake)
  lk <- obs$lake[obs$lake$var_aeme %in% vars_sim &
                   obs$lake$Date >= period[1] & obs$lake$Date <= period[2], ,
                 drop = FALSE]
  if (!nrow(lk)) return(NULL)
  lk <- normalise_lake_obs(lk, warn = FALSE)
  lk$depth_mid <- .ens_obs_position(
    aeme = aeme, lk = lk, depth_ref = depth_ref, model = model,
    remove_spin_up = remove_spin_up,
    convert = !is.null(depths) & lk$var_aeme %in% vars_2d)

  out <- lapply(vars_sim, function(v) {
    s <- lk[lk$var_aeme == v, , drop = FALSE]
    if (!nrow(s)) return(NULL)
    if (!is.null(depths) && v %in% vars_2d) {
      dplyr::bind_rows(lapply(depths, function(d) {
        hit <- s[abs(s$depth_mid - d) <= obs_tol, , drop = FALSE]
        if (!nrow(hit)) return(NULL)
        data.frame(Date = hit$Date, depth = d, value = hit$value,
                   var_sim = v, stringsAsFactors = FALSE)
      }))
    } else {
      data.frame(Date = s$Date, depth = NA_real_, value = s$value,
                 var_sim = v, stringsAsFactors = FALSE)
    }
  })
  out <- dplyr::bind_rows(out)
  if (!nrow(out)) NULL else out
}

#' @export
print.aeme_ensemble_summary <- function(x, ...) {
  vs <- attr(x, "vars_sim")
  v2 <- attr(x, "vars_2d")
  dep <- attr(x, "depths")
  per <- attr(x, "period")
  cat("<aeme_ensemble_summary>\n")
  cat(sprintf("  members  : %d %s\n", attr(x, "n_members"),
              if (isTRUE(attr(x, "keep_members"))) "(kept)" else "(dropped)"))
  cat(sprintf("  variables: %s\n", paste(vs, collapse = ", ")))
  if (length(v2)) {
    cat(sprintf("  depths   : %s (%s-ref)\n",
                if (is.null(dep)) "pooled layers" else
                  paste(format(dep), collapse = ", "),
                attr(x, "depth_ref")))
  }
  cat(sprintf("  period   : %s .. %s  (%d rows in $stats)\n",
              per[1], per[2], nrow(x$stats)))
  cat(sprintf("  quantiles: %s %%\n",
              paste(format(attr(x, "probs") * 100), collapse = ", ")))
  cat(sprintf("  obs      : %s\n",
              if (is.null(x$obs)) "none" else paste(nrow(x$obs), "points")))
  invisible(x)
}

#' @description
#' `as.data.frame()` returns the `stats` table - the long per-date summary,
#' ready to write out or join to something else.
#' @rdname ensemble_summary
#' @param row.names,optional passed to the data.frame method; ignored.
#' @param x an `aeme_ensemble_summary`.
#' @param ... unused.
#' @export
as.data.frame.aeme_ensemble_summary <- function(x, row.names = NULL,
                                                optional = FALSE, ...) {
  as.data.frame(x$stats, row.names = row.names, optional = optional)
}

#' Reduce an observation frame's timestamp to a UTC calendar `Date`
#'
#' AEME stores lake and level observation timestamps as UTC `POSIXct`, while
#' daily model output is read back as `Date`. [ensemble_summary()] compares the
#' two on the calendar day, so this guarantees a `Date`-class column named
#' `Date`: a `Date` column is taken as-is (only its class is enforced), and
#' otherwise a `datetime` column is reduced with `as.Date(x, tz = "UTC")`.
#'
#' @param df an observation data frame (`obs$lake` / `obs$level`), or `NULL`.
#' @return `df` with a `Date` column of class `Date`; `df` unchanged when it is
#'   `NULL`, not a data frame, or carries no recognised timestamp column.
#' @noRd
obs_calendar_date <- function(df) {
  if (is.null(df) || !is.data.frame(df)) return(df)
  src <- if ("Date" %in% names(df)) {
    df$Date
  } else if ("datetime" %in% names(df)) {
    df$datetime
  } else {
    return(df)
  }
  df$Date <- if (inherits(src, "Date")) as.Date(src) else as.Date(src, tz = "UTC")
  df
}

#' Position of each observation in the reference an ensemble summary uses
#'
#' Observations are recorded either below the surface (`depth_ref = "surface"`,
#' the default) or as a height above the bed (`"bottom"`, see [add_obs()]). A
#' summary at `depth_ref` needs each observation's position in that reference:
#' the stored `depth` or `height_above_bed` when it already matches, otherwise
#' converted with the height of the water surface above the bed on that date,
#' `level - depth` or `level - height`. `level` is taken from the lake level
#' observations, interpolated in time, and where there are none from the
#' ensemble median of the modelled level.
#'
#' @param aeme `Aeme` object with ensemble output.
#' @param lk lake observations (normalised, with a `Date` column).
#' @param depth_ref `"surface"` or `"bottom"`: the reference of the summary.
#' @param model,remove_spin_up as in [ensemble_summary()], for the modelled level.
#' @param convert logical, one per row of `lk`: whether the position is needed
#'   (depth-resolved variables when `depths` was given). Others are left `NA`.
#' @return Numeric vector, one per row of `lk`.
#' @noRd
.ens_obs_position <- function(aeme, lk, depth_ref, model, remove_spin_up,
                              convert) {
  n <- nrow(lk)
  rec <- if ("depth_ref" %in% names(lk)) lk$depth_ref else rep("surface", n)
  rec[is.na(rec)] <- "surface"
  height <- if ("height_above_bed" %in% names(lk)) lk$height_above_bed else
    rep(NA_real_, n)
  convert <- rep_len(convert, n)

  # already in the reference of the summary
  pos <- if (depth_ref == "surface") lk$depth else height
  pos[rec != depth_ref] <- NA_real_

  need <- which(rec != depth_ref & convert)
  if (length(need)) {
    level <- interp_level_height(obs_level_height(aeme), lk$Date[need])
    if (anyNA(level)) {
      modelled <- .ens_modelled_level(aeme, model, remove_spin_up)
      level[is.na(level)] <- interp_level_height(modelled,
                                                 lk$Date[need][is.na(level)])
    }
    pos[need] <- if (depth_ref == "bottom") {
      level - lk$depth[need]                 # recorded below the surface
    } else {
      level - height[need]                   # recorded above the bed
    }
    if (anyNA(pos[need])) {
      cli::cli_warn(
        c("!" = "{sum(is.na(pos[need]))} observation{?s} could not be placed
                 {if (depth_ref == \"bottom\") \"above the bed\" else
                 \"below the surface\"}: no water level.",
          "i" = "Add lake level observations or keep {.field LKE_lvlwtr} in the
                 model output."),
        class = "aeme_warn_obs_position_no_level")
    }
  }
  pos
}

#' Ensemble-median modelled water level, as height above the bed
#' @return data.frame with `Date` and `height`, or `NULL`.
#' @noRd
.ens_modelled_level <- function(aeme, model, remove_spin_up = TRUE) {
  n <- output(aeme)$n_members
  if (is.null(n) || n < 1L || is.null(model)) return(NULL)
  lv <- dplyr::bind_rows(lapply(seq_len(n), function(i) {
    tryCatch(get_var(aeme = aeme, model = model, var_sim = "LKE_lvlwtr",
                     ens_n = i, return_df = TRUE,
                     remove_spin_up = remove_spin_up),
             error = function(e) NULL)
  }))
  if (is.null(lv) || nrow(lv) == 0) return(NULL)
  lv <- lv[!is.na(lv$value), , drop = FALSE]
  if (!nrow(lv)) return(NULL)
  out <- stats::aggregate(value ~ Date, data = data.frame(Date = as.Date(lv$Date, tz = "UTC"),
                                                          value = lv$value),
                          FUN = stats::median)
  data.frame(Date = out$Date, height = out$value)
}
