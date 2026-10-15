#' S4 class for a lightweight summary of an Aeme object
#'
#' An `AemeSummary` holds what a dashboard or report needs to show the result of
#' an AEME run (surface/bottom time series per model, the observations and
#' forcing that went with them, the hypsograph) without the depth-resolved model
#' output or the model configuration. It is created by [summarise_aeme()]. It is
#' deliberately **not** an [Aeme][Aeme-class]: it cannot be built or run.
#'
#' @name AemeSummary
#' @aliases AemeSummary-class
#' @slot lake list; as the `lake` slot of the source `Aeme`.
#' @slot time list; `start`, `stop`, `tz` and `output_time_step`.
#' @slot models character; the models that were summarised.
#' @slot meta list; `aeme_version` (of the package that made the summary),
#'   `source_version` (of the source object), `created`, `n_members`, `period`,
#'   `layer_rule` and `stats`.
#' @slot output data.frame; long format with columns `Date`, `model`,
#'   `var_aeme`, `layer`, `stat` and `value`. `layer` is `"surface"`, `"bottom"`
#'   or `NA` (variables without a depth dimension). `stat` is `"value"` for a
#'   single run, or the ensemble statistics (e.g. `"mean"`, `"min"`, `"max"`,
#'   `"q05"`, `"q95"`), plus the member labels (`"ens_001"`, ...) when members
#'   are kept.
#' @slot profiles data.frame; seasonal mean profiles (`season`, `model`,
#'   `var_aeme`, `depth`, `mean`, `sd`, `n`). Zero rows unless requested.
#' @slot observations list; `lake` and `level` observation data frames, as in an
#'   `Aeme`, where present.
#' @slot forcing list; daily long tables `meteo` (`Date`, `var_aeme`, `value`),
#'   `inflows` and `outflows` (`Date`, `name`, `var_aeme`, `value`) and
#'   `water_balance` (`Date`, `source`, `var_aeme`, `value`), where present.
#' @slot hypsograph data.frame; the hypsograph, unchanged.
#' @slot parameters data.frame; the model parameters, unchanged.
#' @seealso [summarise_aeme()], [write_aeme_summary()], [read_aeme_summary()]
#' @export
setClass("AemeSummary",
         representation(
           lake = "list",
           time = "list",
           models = "character",
           meta = "list",
           output = "data.frame",
           profiles = "data.frame",
           observations = "list",
           forcing = "list",
           hypsograph = "data.frame",
           parameters = "data.frame"
         )
)

.summary_output_cols <- c("Date", "model", "var_aeme", "layer", "stat", "value")
.summary_profile_cols <- c("season", "model", "var_aeme", "depth", "mean", "sd",
                           "n")

setValidity("AemeSummary", function(object) {
  errors <- character()

  out <- object@output
  if (nrow(out) > 0 || ncol(out) > 0) {
    miss <- setdiff(.summary_output_cols, names(out))
    if (length(miss) > 0) {
      errors <- c(errors, paste0("@output is missing column(s): ",
                                 paste(miss, collapse = ", ")))
    } else {
      if (!inherits(out$Date, c("Date", "POSIXct")))
        errors <- c(errors, "@output$Date must be Date or POSIXct")
      if (!is.numeric(out$value))
        errors <- c(errors, "@output$value must be numeric")
      bad_layer <- setdiff(unique(out$layer), c("surface", "bottom", NA))
      if (length(bad_layer) > 0)
        errors <- c(errors, paste0("@output$layer has unknown value(s): ",
                                   paste(bad_layer, collapse = ", ")))
      bad_model <- setdiff(unique(out$model), .aeme_model_codes)
      if (length(bad_model) > 0)
        errors <- c(errors, paste0("@output$model has unknown model(s): ",
                                   paste(bad_model, collapse = ", ")))
    }
  }

  prof <- object@profiles
  if (nrow(prof) > 0) {
    miss <- setdiff(.summary_profile_cols, names(prof))
    if (length(miss) > 0)
      errors <- c(errors, paste0("@profiles is missing column(s): ",
                                 paste(miss, collapse = ", ")))
  }

  bad_models <- setdiff(object@models, .aeme_model_codes)
  if (length(bad_models) > 0)
    errors <- c(errors, paste0("@models has unknown model(s): ",
                               paste(bad_models, collapse = ", ")))

  nm <- object@meta[["n_members"]]
  if (is.null(nm) || !is.numeric(nm) || length(nm) != 1L || is.na(nm) || nm < 1)
    errors <- c(errors, "@meta$n_members must be a single number >= 1")

  req_cols <- list(meteo = c("Date", "var_aeme", "value"),
                   inflows = c("Date", "name", "var_aeme", "value"),
                   outflows = c("Date", "name", "var_aeme", "value"),
                   water_balance = c("Date", "source", "var_aeme", "value"))
  for (f in intersect(names(req_cols), names(object@forcing))) {
    d <- object@forcing[[f]]
    if (!is.data.frame(d) || !all(req_cols[[f]] %in% names(d)))
      errors <- c(errors, paste0("@forcing$", f, " must be a data.frame with ",
                                 "columns ", paste(req_cols[[f]],
                                                   collapse = ", ")))
  }

  if (length(errors) == 0) TRUE else errors
})

# Accessors ----

#' @name AemeSummary-accessors
#' @rdname AemeSummary-accessors
#' @title Accessors for AemeSummary objects
#' @description `lake()`, `time()`, `observations()`, `output()` and
#'   `parameters()` work on an `AemeSummary` as they do on an `Aeme`, returning
#'   the matching slot. `forcing()`, `profiles()`, `models()` and `hypsograph()`
#'   are specific to it.
#' @param aeme,x An `AemeSummary` object.
#' @return The slot.
#' @export
setMethod("lake", "AemeSummary", function(aeme) aeme@lake)

#' @rdname AemeSummary-accessors
#' @export
setMethod("time", "AemeSummary", function(aeme) aeme@time)

#' @rdname AemeSummary-accessors
#' @export
setMethod("observations", "AemeSummary", function(aeme) aeme@observations)

#' @rdname AemeSummary-accessors
#' @export
setMethod("output", "AemeSummary", function(aeme) aeme@output)

#' @rdname AemeSummary-accessors
#' @export
setMethod("parameters", "AemeSummary", function(aeme) aeme@parameters)

#' @rdname AemeSummary-accessors
#' @export
setGeneric("forcing", function(x) standardGeneric("forcing"))

#' @rdname AemeSummary-accessors
#' @export
setMethod("forcing", "AemeSummary", function(x) x@forcing)

#' @rdname AemeSummary-accessors
#' @export
setGeneric("profiles", function(x) standardGeneric("profiles"))

#' @rdname AemeSummary-accessors
#' @export
setMethod("profiles", "AemeSummary", function(x) x@profiles)

#' @rdname AemeSummary-accessors
#' @export
setGeneric("models", function(x) standardGeneric("models"))

#' @rdname AemeSummary-accessors
#' @export
setMethod("models", "AemeSummary", function(x) x@models)

#' @rdname AemeSummary-accessors
#' @export
setGeneric("hypsograph", function(x) standardGeneric("hypsograph"))

#' @rdname AemeSummary-accessors
#' @export
setMethod("hypsograph", "AemeSummary", function(x) x@hypsograph)

#' Is `x` an `AemeSummary`?
#' @param x object to test.
#' @return logical.
#' @export
is_aeme_summary <- function(x) {
  inherits(x, "AemeSummary")
}

# show / names ----

#' Show an AemeSummary
#' @param object An `AemeSummary` object.
#' @export
setMethod("show", "AemeSummary", function(object) {
  lke <- object@lake
  out <- object@output
  meta <- object@meta
  vars <- if (nrow(out) > 0) sort(unique(out$var_aeme)) else character(0)
  stats <- if (nrow(out) > 0) unique(out$stat) else character(0)
  per <- meta$period
  cli::cli_h1("AemeSummary: {lke$name %||% 'unnamed lake'}")
  cli::cli_bullets(c(
    "*" = "Models: {.val {object@models}}",
    "*" = "Period: {format(per[1])} to {format(per[2])}",
    "*" = "Source members: {meta$n_members}; stats: {.val {stats}}",
    "*" = "Output: {nrow(out)} rows, {length(vars)} variable{?s}",
    "*" = "Observations: {.val {names(object@observations)}}",
    "*" = "Forcing: {.val {names(object@forcing)}}",
    "*" = "Hypsograph: {nrow(object@hypsograph)} row{?s}; profiles: {nrow(object@profiles)} row{?s}"
  ))
  invisible(object)
})

#' @rdname AemeSummary
#' @param x An `AemeSummary` object.
#' @export
setMethod("names", "AemeSummary", function(x) methods::slotNames(x))

# Construction ----

#' Summarise an Aeme object
#'
#' Reduces an [Aeme][Aeme-class] with model output to an [AemeSummary][AemeSummary-class]:
#' daily surface and bottom values for every depth-resolved variable (the split
#' is at the epilimnion depth, `HYD_epidep`, else the thermocline depth,
#' `HYD_thmcln`, else half the water column), daily values for the rest, and, optionally, the
#' observations, daily forcing and the hypsograph. The result is small enough to
#' store and use in a dashboard, and has no model configuration, so it cannot be
#' used to build or run a model.
#'
#' With more than one ensemble member the member values are combined into the
#' statistics named in `stats`.
#'
#' @inheritParams build_aeme
#' @param model character; models to summarise. Default: those with output.
#' @param ens_n integer; ensemble members to use. Default `NULL`: all.
#' @param stats character; statistics across members when there is more than
#'   one: `"mean"`, `"min"`, `"max"`, `"sd"`, or quantiles such as `"q05"` and
#'   `"q95"`. Ignored for a single member (the stat is then `"value"`).
#' @param keep_members logical; also keep each member's series (as stats named
#'   `"ens_001"`, ...). Default `FALSE`.
#' @param vars character; AEME variables to keep. Default `NULL`: every
#'   standard variable in the output.
#' @param observations logical; keep the observations. Default `TRUE`.
#' @param forcing character; which forcing tables to keep, from `"meteo"`,
#'   `"inflows"`, `"outflows"` and `"water_balance"` (all by default). Use
#'   `character(0)` for none. They are daily means.
#' @param profiles logical; also calculate seasonal mean profiles of
#'   temperature and oxygen (see [calc_seasonal_profiles]-style output in the
#'   `profiles` slot). Default `FALSE`.
#' @param digits integer; values are rounded to this many decimal places.
#'
#' @return An [AemeSummary][AemeSummary-class] object.
#' @seealso [write_aeme_summary()], [read_aeme_summary()]
#' @export
summarise_aeme <- function(aeme, model, ens_n = NULL,
                           stats = c("mean", "min", "max", "q05", "q95"),
                           keep_members = FALSE, vars = NULL,
                           observations = TRUE,
                           forcing = c("meteo", "inflows", "outflows",
                                       "water_balance"),
                           profiles = FALSE, digits = 4L) {

  if (is_aeme_summary(aeme)) {
    cli::cli_abort("{.arg aeme} is already an {.cls AemeSummary}.",
                   class = "aeme_error_already_summary")
  }
  aeme <- check_aeme(aeme)
  forcing_opts <- c("meteo", "inflows", "outflows", "water_balance")
  if (length(forcing) > 0) {
    forcing <- match.arg(forcing, forcing_opts, several.ok = TRUE)
  }
  outp <- output(aeme)
  members <- .output_members(outp)
  if (length(members) == 0) {
    cli::cli_abort(c("{.arg aeme} has no model output to summarise.",
                     "i" = "Run a model with {.fn run_aeme} first."),
                   class = "aeme_error_no_output")
  }
  if (!is.null(ens_n)) {
    keep <- format_ens_label(ens_n = ens_n)
    miss <- setdiff(keep, members)
    if (length(miss) > 0) {
      cli::cli_abort("Ensemble member{?s} {.val {miss}} not found.",
                     class = "aeme_error_no_member")
    }
    members <- intersect(members, keep)
  }
  have <- unique(unlist(lapply(members, function(e) {
    names(Filter(Negate(is.null), outp[[e]]))
  })))
  model <- if (missing(model)) {
    intersect(.aeme_model_codes, have)
  } else {
    check_model(model = model)
  }
  model <- intersect(model, have)
  if (length(model) == 0) {
    cli::cli_abort("None of the requested models have output.",
                   class = "aeme_error_no_model_output")
  }

  aeme_time <- time(aeme)

  # Output: per member, per model, daily surface/bottom values ----
  per_member <- lapply(members, function(e) {
    res <- lapply(model, function(m) {
      out <- outp[[e]][[m]]
      if (is.null(out)) return(NULL)
      df <- .summarise_model_output(out, start = aeme_time$start,
                                    stop = aeme_time$stop, vars = vars)
      if (is.null(df) || nrow(df) == 0) return(NULL)
      df$model <- m
      df
    })
    df <- dplyr::bind_rows(res)
    if (nrow(df) > 0) df$member <- e
    df
  })
  all_members <- dplyr::bind_rows(per_member)
  if (nrow(all_members) == 0) {
    cli::cli_abort("No variables found to summarise.",
                   class = "aeme_error_no_vars")
  }
  out_tbl <- .combine_members(all_members, n = length(members), stats = stats,
                              keep_members = keep_members, digits = digits)

  # Other slots ----
  obs <- NULL
  if (isTRUE(observations)) {
    ob <- observations(aeme)
    obs <- Filter(is.data.frame, ob[intersect(c("lake", "level"), names(ob))])
  }
  forc <- .summarise_forcing(aeme, which = forcing)
  hyps <- input(aeme)$hypsograph
  if (!is.data.frame(hyps)) hyps <- data.frame()
  prms <- parameters(aeme)

  prof <- data.frame()
  if (isTRUE(profiles)) {
    prof <- tryCatch(
      .summarise_profiles(aeme, model = model, ens_n = 1),
      error = function(e) {
        cli::cli_warn(c("Could not calculate seasonal profiles.",
                        "x" = conditionMessage(e)))
        data.frame()
      })
  }

  tz <- aeme_time$tz
  new("AemeSummary",
      lake = lake(aeme),
      time = list(start = aeme_time$start, stop = aeme_time$stop,
                  tz = if (is.null(tz)) "UTC" else tz,
                  output_time_step = aeme_time$output_time_step),
      models = model,
      meta = list(
        aeme_version = as.character(utils::packageVersion("AEME")),
        source_version = configuration(aeme)$aeme_version,
        created = Sys.time(),
        n_members = length(members),
        period = as.Date(c(aeme_time$start, aeme_time$stop)),
        layer_rule = paste0("surface: depth <= HYD_epidep, else HYD_thmcln, ",
                            "else half the water column; bottom: deeper."),
        stats = unique(out_tbl$stat)),
      output = out_tbl,
      profiles = prof,
      observations = if (is.null(obs)) list() else obs,
      forcing = forc,
      hypsograph = as.data.frame(hyps),
      parameters = prms)
}

#' Daily surface/bottom values for one model's output
#' @param out one model's `aeme_output` list.
#' @return long data.frame: `Date`, `var_aeme`, `layer`, `value`; or `NULL`.
#' @noRd
.summarise_model_output <- function(out, start, stop, vars = NULL) {

  dates <- out[["Date"]]
  if (length(dates) == 0) return(NULL)
  day <- as.Date(dates, tz = "UTC")
  idx <- which(day >= as.Date(start, tz = "UTC") &
                 day <= as.Date(stop, tz = "UTC"))
  if (length(idx) == 0) return(NULL)
  day <- day[idx]

  skip <- c("Date", "LKE_depths", "ok", "reason", .output_extra_groups)
  nms <- setdiff(names(out), skip)
  if (!is.null(vars)) nms <- intersect(nms, vars)

  depths <- out[["LKE_depths"]]

  # Depth below which a layer counts as "bottom", per time step: the
  # epilimnion depth, else the thermocline depth, else half the water column
  if (is.matrix(depths)) {
    D <- depths[, idx, drop = FALSE]
    split_depth <- apply(D, 2, function(x) {
      if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE) / 2
    })
    for (v in rev(c("HYD_epidep", "HYD_thmcln"))) {
      cand <- out[[v]]
      if (is.numeric(cand) && length(cand) == length(dates)) {
        cand <- cand[idx]
        split_depth <- ifelse(is.na(cand), split_depth, cand)
      }
    }
    is_surf <- sweep(D, 2, split_depth, "<=")
    is_surf[is.na(is_surf)] <- FALSE
    is_btm <- !is_surf & !is.na(D)
  }

  daily <- function(x) {
    # Mean per calendar day (a no-op for daily output)
    if (anyDuplicated(day) == 0) return(x)
    as.numeric(tapply(x, day, mean, na.rm = TRUE))
  }
  days <- sort(unique(day))
  layer_mean <- function(V, mask) {
    n <- colSums(mask & !is.na(V))
    s <- colSums(V * mask, na.rm = TRUE)
    ifelse(n > 0, s / n, NA_real_)
  }

  res <- lapply(nms, function(v) {
    x <- out[[v]]
    if (inherits(x, "aeme_grouped_var") || is.null(x)) return(NULL)
    if (is.matrix(x)) {
      # Skip dimension variables (e.g. `z`) and anything off the depth grid
      if (!is.matrix(depths) || ncol(x) != length(dates) ||
          nrow(x) != nrow(depths)) return(NULL)
      V <- x[, idx, drop = FALSE]
      rbind(
        data.frame(Date = days, var_aeme = v, layer = "surface",
                   value = daily(layer_mean(V, is_surf))),
        data.frame(Date = days, var_aeme = v, layer = "bottom",
                   value = daily(layer_mean(V, is_btm))))
    } else if (is.numeric(x) && length(x) == length(dates)) {
      data.frame(Date = days, var_aeme = v, layer = NA_character_,
                 value = daily(x[idx]))
    } else {
      NULL
    }
  })
  dplyr::bind_rows(res)
}

#' Combine members into statistics (or a single `value`)
#' @noRd
.combine_members <- function(df, n, stats, keep_members, digits) {

  keys <- c("Date", "model", "var_aeme", "layer")
  stat_fun <- function(s) {
    if (s == "mean") return(function(x) mean(x, na.rm = TRUE))
    if (s == "min") return(function(x) suppressWarnings(min(x, na.rm = TRUE)))
    if (s == "max") return(function(x) suppressWarnings(max(x, na.rm = TRUE)))
    if (s == "sd") return(function(x) stats::sd(x, na.rm = TRUE))
    if (grepl("^q[0-9]+$", s)) {
      p <- as.numeric(sub("^q", "", s)) / 100
      return(function(x) stats::quantile(x, p, na.rm = TRUE, names = FALSE))
    }
    cli::cli_abort("Unknown statistic {.val {s}}.",
                   class = "aeme_error_summary_stat")
  }

  if (n == 1) {
    res <- df
    res$stat <- "value"
    res <- res[, c(keys, "stat", "value")]
  } else {
    res <- lapply(stats, function(s) {
      f <- stat_fun(s)
      agg <- dplyr::summarise(
        dplyr::group_by(df, dplyr::across(dplyr::all_of(keys))),
        value = f(.data$value), .groups = "drop")
      agg$stat <- s
      agg
    })
    res <- dplyr::bind_rows(res)
    if (keep_members) {
      mem <- df
      mem$stat <- mem$member
      res <- dplyr::bind_rows(res, mem[, c(keys, "stat", "value")])
    }
  }
  res$value[!is.finite(res$value)] <- NA_real_
  res$value <- round(res$value, digits)
  res <- res[order(res$model, res$var_aeme, res$stat, res$layer, res$Date), ,
             drop = FALSE]
  res <- res[, c("Date", "model", "var_aeme", "layer", "stat", "value")]
  rownames(res) <- NULL
  res
}

#' Daily long tables of the forcing data
#' @noRd
.summarise_forcing <- function(aeme, which) {

  to_day <- function(x) as.Date(x, tz = "UTC")

  long_daily <- function(df, id_cols = character(0)) {
    if (!is.data.frame(df) || !"Date" %in% names(df) || nrow(df) == 0) {
      return(NULL)
    }
    df$Date <- to_day(df$Date)
    df <- df[, setdiff(names(df), "model"), drop = FALSE]
    val_cols <- setdiff(names(df)[vapply(df, is.numeric, logical(1))], id_cols)
    if (length(val_cols) == 0) return(NULL)
    long <- tidyr::pivot_longer(df[, c("Date", id_cols, val_cols)],
                                cols = dplyr::all_of(val_cols),
                                names_to = "var_aeme", values_to = "value")
    long <- dplyr::summarise(
      dplyr::group_by(long, dplyr::across(dplyr::all_of(c("Date", id_cols,
                                                        "var_aeme")))),
      value = mean(.data$value, na.rm = TRUE), .groups = "drop")
    long$value[!is.finite(long$value)] <- NA_real_
    as.data.frame(long)
  }

  flow_tbl <- function(slot_list) {
    dat <- slot_list$data
    if (length(dat) == 0) return(NULL)
    res <- lapply(names(dat), function(nm) {
      d <- long_daily(dat[[nm]])
      if (is.null(d)) return(NULL)
      d$name <- nm
      d[, c("Date", "name", "var_aeme", "value")]
    })
    res <- dplyr::bind_rows(res)
    if (nrow(res) == 0) NULL else res
  }

  forc <- list()
  if ("meteo" %in% which) {
    m <- long_daily(input(aeme)$meteo)
    if (!is.null(m)) forc$meteo <- m
  }
  if ("inflows" %in% which) {
    f <- flow_tbl(inflows(aeme))
    if (!is.null(f)) forc$inflows <- f
  }
  if ("outflows" %in% which) {
    f <- flow_tbl(outflows(aeme))
    if (!is.null(f)) forc$outflows <- f
  }
  if ("water_balance" %in% which) {
    wb <- water_balance(aeme)$data
    res <- lapply(names(wb), function(nm) {
      d <- long_daily(wb[[nm]])
      if (is.null(d)) return(NULL)
      d$source <- nm
      d[, c("Date", "source", "var_aeme", "value")]
    })
    res <- dplyr::bind_rows(res)
    if (nrow(res) > 0) forc$water_balance <- res
  }
  forc
}

#' Seasonal mean profiles in the `profiles` slot's format
#' @noRd
.summarise_profiles <- function(aeme, model, ens_n = 1) {
  sp <- calc_seasonal_profiles(aeme = aeme, model = model, ens_n = ens_n)
  data.frame(season = as.character(sp$season),
             model = toggle_models(sp$Model, to = "code"),
             var_aeme = sp$var_sim,
             depth = sp$depth,
             mean = sp$mean,
             sd = sp$std_dev,
             n = sp$n,
             stringsAsFactors = FALSE)
}

# Using a summary ----

#' `get_var()` for an AemeSummary
#'
#' Returns the summarised series in the same shape as `get_var()` gives for an
#' `Aeme` (`Date`, `depth`, `value`, `Model`, `var_sim`), plus `layer` and
#' `stat`.
#' @noRd
.get_var_summary_obj <- function(aeme, model, var_sim, layer = NULL,
                                 stat = NULL) {
  out <- aeme@output
  if (!missing(model) && !is.null(model)) {
    model <- check_model(model = model)
    out <- out[out$model %in% model, , drop = FALSE]
  }
  out <- out[out$var_aeme %in% var_sim, , drop = FALSE]
  if (nrow(out) == 0) {
    cli::cli_abort(c("{.arg var_sim} {.val {var_sim}} not found in the summary.",
                     "i" = "Available: {.val {sort(unique(aeme@output$var_aeme))}}."),
                   class = "aeme_error_no_var")
  }
  if (is.null(stat)) {
    stat <- if ("value" %in% out$stat) "value" else "mean"
  }
  out <- out[out$stat %in% stat, , drop = FALSE]
  if (!is.null(layer)) out <- out[out$layer %in% layer, , drop = FALSE]
  data.frame(Date = out$Date, depth = NA_real_, value = out$value,
             Model = toggle_models(out$model, to = "display"),
             var_sim = out$var_aeme, layer = out$layer, stat = out$stat,
             stringsAsFactors = FALSE)
}

#' Plot a variable from an AemeSummary
#'
#' @param x An `AemeSummary`.
#' @param y character; the variable (`var_aeme`) to plot. Default: the first.
#' @param ... Unused.
#' @return A ggplot2 object: one panel per model, a line per layer, with a
#'   min-max ribbon when the summary has ensemble statistics.
#' @export
setMethod("plot", "AemeSummary", function(x, y, ...) {
  if (missing(y)) y <- sort(unique(x@output$var_aeme))[1]
  if (length(y) != 1) {
    cli::cli_abort("{.arg y} must be a single variable.")
  }
  df <- .get_var_summary_obj(x, var_sim = y, stat = unique(x@output$stat))
  df$layer <- ifelse(is.na(df$layer), "lake", df$layer)
  central <- if ("value" %in% df$stat) "value" else "mean"
  line <- df[df$stat == central, , drop = FALSE]
  p <- ggplot2::ggplot()
  labs <- ggplot2::labs(x = "Date", y = y, colour = "Layer")
  if (all(c("min", "max") %in% df$stat)) {
    labs <- ggplot2::labs(x = "Date", y = y, colour = "Layer", fill = "Layer")
    rng <- merge(df[df$stat == "min", c("Date", "Model", "layer", "value")],
                 df[df$stat == "max", c("Date", "Model", "layer", "value")],
                 by = c("Date", "Model", "layer"), suffixes = c("_min", "_max"))
    p <- p + ggplot2::geom_ribbon(
      data = rng,
      ggplot2::aes(x = Date, ymin = value_min, ymax = value_max,
                   fill = layer), alpha = 0.2)
  }
  p +
    ggplot2::geom_line(data = line,
                       ggplot2::aes(x = Date, y = value, colour = layer)) +
    ggplot2::facet_wrap(~Model, ncol = 1) +
    labs +
    ggplot2::theme_bw()
})

# Persistence ----

#' Write and read an AemeSummary
#'
#' `write_aeme_summary()` saves a summary for use elsewhere, for example in a
#' dashboard. `read_aeme_summary()` reads one written as `"rds"` or `"parquet"`.
#'
#' * `"rds"`: one `.rds` file holding the whole object (the default).
#' * `"parquet"`: a directory with one parquet file per table (`output`,
#'   `profiles`, `hypsograph`, `parameters`, `observations_*`, `forcing_*`) and
#'   a `meta.rds` with the rest. Needs the `arrow` package.
#' * `"json"`: one `.json` file with every table and the metadata, for
#'   dashboards that are not written in R. It cannot be read back.
#'
#' @param x An [AemeSummary][AemeSummary-class].
#' @param path file path (`"rds"`, `"json"`) or directory (`"parquet"`).
#' @param format `"rds"`, `"parquet"` or `"json"`.
#' @return `write_aeme_summary()` returns `path` invisibly;
#'   `read_aeme_summary()` an `AemeSummary`.
#' @export
write_aeme_summary <- function(x, path, format = c("rds", "parquet", "json")) {

  if (!is_aeme_summary(x)) {
    cli::cli_abort("{.arg x} must be an {.cls AemeSummary}.")
  }
  format <- match.arg(format)
  methods::validObject(x)

  if (format == "rds") {
    dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
    saveRDS(x, path)
  } else if (format == "parquet") {
    rlang::check_installed("arrow", reason = "to write parquet files.")
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
    tbls <- list(output = x@output, profiles = x@profiles,
                 hypsograph = x@hypsograph, parameters = x@parameters)
    for (nm in names(x@observations))
      tbls[[paste0("observations_", nm)]] <- x@observations[[nm]]
    for (nm in names(x@forcing))
      tbls[[paste0("forcing_", nm)]] <- x@forcing[[nm]]
    for (nm in names(tbls)) {
      if (nrow(tbls[[nm]]) == 0) next
      arrow::write_parquet(tbls[[nm]], file.path(path, paste0(nm, ".parquet")))
    }
    saveRDS(list(lake = x@lake, time = x@time, models = x@models,
                 meta = x@meta), file.path(path, "meta.rds"))
  } else {
    dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
    obj <- list(lake = x@lake, time = x@time, models = x@models, meta = x@meta,
                output = x@output, profiles = x@profiles,
                observations = x@observations, forcing = x@forcing,
                hypsograph = x@hypsograph, parameters = x@parameters)
    jsonlite::write_json(obj, path, dataframe = "columns", POSIXt = "ISO8601",
                         Date = "ISO8601", auto_unbox = TRUE, na = "null",
                         digits = NA)
  }
  invisible(path)
}

#' @rdname write_aeme_summary
#' @export
read_aeme_summary <- function(path) {

  if (dir.exists(path)) {
    rlang::check_installed("arrow", reason = "to read parquet files.")
    meta <- readRDS(file.path(path, "meta.rds"))
    rd <- function(nm) {
      f <- file.path(path, paste0(nm, ".parquet"))
      if (file.exists(f)) as.data.frame(arrow::read_parquet(f)) else data.frame()
    }
    pq <- sub("\\.parquet$", "", list.files(path, pattern = "\\.parquet$"))
    pick <- function(prefix) {
      nms <- pq[startsWith(pq, prefix)]
      stats::setNames(lapply(nms, rd), sub(prefix, "", nms, fixed = TRUE))
    }
    x <- new("AemeSummary", lake = meta$lake, time = meta$time,
             models = meta$models, meta = meta$meta, output = rd("output"),
             profiles = rd("profiles"), observations = pick("observations_"),
             forcing = pick("forcing_"), hypsograph = rd("hypsograph"),
             parameters = rd("parameters"))
  } else {
    if (!file.exists(path)) {
      cli::cli_abort("{.path {path}} does not exist.")
    }
    x <- readRDS(path)
    if (!is_aeme_summary(x)) {
      cli::cli_abort(c("{.path {path}} does not hold an {.cls AemeSummary}.",
                       "i" = "Summaries made before AEME 0.4.1 had a different
                              structure; make a new one with {.fn summarise_aeme}."),
                     class = "aeme_error_old_summary")
    }
  }
  methods::validObject(x)
  x
}
