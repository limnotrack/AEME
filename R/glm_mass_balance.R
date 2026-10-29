#' Read a GLM-AED mass balance file
#'
#' Reads the GLMv4 `mass_balance.csv` (the `&mass_balance` block) into a long
#' table. The file has an `Inf`, `Lake` and `Out` column for each variable in
#' `balance_vars`, in AED native units (mmol).
#'
#' @param file character; path to `mass_balance.csv`, or to a GLM-AED model
#'   directory (or `output` directory) containing it. Not needed if `aeme` is
#'   given.
#' @param aeme Aeme object. If given (and `file` is `NULL`), the file is found
#'   from the lake's `glm_aed` output, as in [get_model_outfile()].
#' @param path character; root directory used with `aeme`. Defaults to the
#'   path stored in `aeme`.
#'
#' @return a tibble with columns `datetime` (POSIXct, UTC), `var` (GLM-AED
#'   variable name, e.g. `"NIT_nit"`), `term` (`"Inf"`, `"Lake"` or `"Out"`) and
#'   `value` (mmol).
#' @export
#' @examples
#' \dontrun{
#' mb <- read_glm_mass_balance("path/to/glm_aed/output/mass_balance.csv")
#' }
read_glm_mass_balance <- function(file = NULL, aeme = NULL, path = NULL) {
  if (is.null(file)) {
    if (is.null(aeme)) {
      cli::cli_abort("Either {.arg file} or {.arg aeme} must be provided.")
    }
    files <- get_model_outfile(aeme = aeme, model = "glm_aed", path = path,
                               all = TRUE)[["glm_aed"]]
    file <- files[names(files) == "mass_balance"]
    if (length(file) == 0) {
      cli::cli_abort("No {.file mass_balance.csv} found for this lake. Has
                     {.code glm_aed} been run with a {.code &mass_balance}
                     block (GLMv4)?")
    }
    file <- unname(file[1])
  }
  if (length(file) != 1 || !is.character(file)) {
    cli::cli_abort("{.arg file} must be a single path.")
  }
  if (dir.exists(file)) {
    cand <- c(file.path(file, "mass_balance.csv"),
              file.path(file, "output", "mass_balance.csv"))
    cand <- cand[file.exists(cand)]
    if (length(cand) == 0) {
      cli::cli_abort("No {.file mass_balance.csv} found in {.path {file}}.")
    }
    file <- cand[1]
  }
  if (!file.exists(file)) {
    cli::cli_abort("File {.path {file}} does not exist.")
  }

  df <- utils::read.csv(file, check.names = FALSE, strip.white = TRUE,
                        stringsAsFactors = FALSE)
  if (!"time" %in% names(df)) {
    cli::cli_abort("{.file {basename(file)}} has no {.field time} column.")
  }
  df[["time"]] <- as.POSIXct(df[["time"]], tz = "UTC")

  value_cols <- setdiff(names(df), "time")
  m <- regmatches(value_cols, regexec("^(Inf|Lake|Out)\\s+(.+)$", value_cols))
  ok <- lengths(m) == 3
  if (!any(ok)) {
    cli::cli_abort("No {.val Inf}/{.val Lake}/{.val Out} columns found in
                   {.file {basename(file)}}.")
  }
  value_cols <- value_cols[ok]
  term <- vapply(m[ok], `[`, "", 2)
  var <- vapply(m[ok], `[`, "", 3)

  out <- lapply(seq_along(value_cols), function(i) {
    tibble::tibble(datetime = df[["time"]], var = var[i], term = term[i],
                   value = as.numeric(df[[value_cols[i]]]))
  })
  dplyr::bind_rows(out)
}

#' Sum mass balance variables into AED totals
#'
#' Combines the components of the `aed_totals` block (TN, TP, TOC and, when
#' present, TSS) from a mass balance table, applying the `*_varscale` factors
#' (as in [set_aed_totals()]). Because the scaled components are in mmol, the
#' totals are in mmol of N, P and C respectively.
#'
#' @param mb tibble from [read_glm_mass_balance()].
#' @param aed_totals list; the `aed_totals` block (`TN_vars`, `TN_varscale`,
#'   ...), as built by `derive_aed_totals()`. Ignored if `path_glm` is given.
#' @param path_glm character; GLM-AED model directory (containing `aed/`) to
#'   derive `aed_totals` from. Takes precedence over `aed_totals`.
#' @param units `"mmol"` (default, AED native) or `"kg"`, which converts using
#'   the molar masses of N, P and C. `TSS` is left unchanged when converted.
#'
#' @return a tibble with columns `datetime`, `total` (`"TN"`, `"TP"`, `"TOC"`,
#'   ...), `term` (`"Inf"`, `"Lake"`, `"Out"`), `value`, `units` and
#'   `n_missing`, the number of components absent from `mb` (they are
#'   dropped from the sum). Totals with no components in `mb` are skipped.
#' @export
calc_aed_totals_balance <- function(mb, aed_totals = NULL, path_glm = NULL,
                                    units = c("mmol", "kg")) {
  units <- match.arg(units)
  if (!is.null(path_glm)) {
    cfg <- read_model_config(model = "glm_aed", lake_dir = dirname(path_glm))
    aed_totals <- derive_aed_totals(cfg)[["bgc"]][["aed"]][["aed_totals"]]
  }
  if (is.null(aed_totals)) {
    cli::cli_abort("Provide either {.arg aed_totals} or {.arg path_glm}.")
  }

  total_names <- sub("_vars$", "", grep("_vars$", names(aed_totals),
                                        value = TRUE))
  # molar mass (g/mol) for mmol -> kg
  molar_mass <- c(TN = 14.007, TP = 30.974, TOC = 12.011, TSS = NA_real_)

  res <- lapply(total_names, function(tot) {
    vars <- aed_totals[[paste0(tot, "_vars")]]
    scale <- aed_totals[[paste0(tot, "_varscale")]]
    key <- tibble::tibble(var = vars, scale = scale)
    present <- key$var %in% mb$var
    n_missing <- sum(!present)
    # a total that was not requested in `balance_vars` has no components at all
    if (!any(present)) return(NULL)
    if (n_missing > 0) {
      cli::cli_warn("{tot}: {n_missing} component{?s} not in the mass balance
                    file ({.val {key$var[!present]}}); total is incomplete.")
    }
    d <- dplyr::inner_join(mb, key, by = "var", relationship = "many-to-one")
    if (nrow(d) == 0) return(NULL)
    d |>
      dplyr::mutate(value = value * scale) |>
      dplyr::summarise(value = sum(value), .by = c(datetime, term)) |>
      dplyr::mutate(total = tot, n_missing = n_missing, units = "mmol")
  })
  out <- dplyr::bind_rows(res)

  if (units == "kg" && nrow(out) > 0) {
    mm <- unname(molar_mass[out$total])
    mm[is.na(mm)] <- NA_real_
    out$value <- ifelse(is.na(mm), out$value, out$value * mm / 1e6)
    out$units <- ifelse(is.na(mm), "mmol", "kg")
  }
  out[, c("datetime", "total", "term", "value", "units", "n_missing")]
}

#' Diagnose the GLM-AED mass balance
#'
#' Computes, for each variable (or each AED total), the change in the in-lake
#' inventory and the net external transport, and the remainder between them:
#'
#' `net_internal = d(Lake) - (Inf - Out)`
#'
#' where `d(Lake)` is the change in the `Lake` term since the previous row and
#' `Inf`/`Out` are the inflow and outflow totals over that same interval. GLM
#' writes `Inf` and `Out` as totals over the save interval (`nsave * dt`), so
#' the interval length of each row is taken from the time stamps and per-day
#' rates (`*_rate`, mmol per day) are returned alongside the interval totals;
#' use the rates to compare runs with different `nsave`. If `path_glm` (or
#' `aeme`) is given, the time stamps are checked against `nsave * dt` in the
#' GLM nml. `net_internal` is the
#' amount gained or lost inside the lake (reactions, sediment and atmospheric
#' exchange, repair of negative states). For conservative quantities it should
#' be close to zero. For reactive variables it is expected to be non-zero, but
#' for the totals TN, TP and TOC it is the net sediment/atmosphere exchange
#' and should be small relative to the inventory.
#'
#' @param mb tibble from [read_glm_mass_balance()], or a path to
#'   `mass_balance.csv`. If `NULL`, it is read from `aeme`.
#' @param aeme Aeme object. Used to read `mb` when it is `NULL`, and to
#'   locate the `glm_aed` configuration for the totals when neither
#'   `aed_totals` nor `path_glm` is given.
#' @param path character; root directory used with `aeme`.
#' @param by `"total"` (default) to diagnose the aed totals, or `"var"` to
#'   diagnose each variable in the file.
#' @inheritParams calc_aed_totals_balance
#' @param plot logical; also return a ggplot of cumulative `net_internal`.
#'
#' @return a list with `daily` (tibble: `datetime`, `name`, `interval_days`, `inflow`, `outflow`,
#'   `lake`, `d_lake`, `net_external`, `net_internal`, `cum_net_internal`),
#'   `summary` (tibble: one row per `name`, with the mean inventory,
#'   total external flux, total internal remainder and its percentage of the
#'   mean inventory) and, if `plot = TRUE`, `plot`.
#' @export
diag_glm_mass_balance <- function(mb = NULL, by = c("total", "var"),
                                  aed_totals = NULL, path_glm = NULL,
                                  units = c("mmol", "kg"), plot = FALSE,
                                  aeme = NULL, path = NULL) {
  by <- match.arg(by)
  units <- match.arg(units)
  if (is.null(mb)) {
    mb <- read_glm_mass_balance(aeme = aeme, path = path)
  } else if (is.character(mb)) {
    mb <- read_glm_mass_balance(mb)
  }
  if (is.null(path_glm) && !is.null(aeme) &&
      (by == "var" || is.null(aed_totals))) {
    path_glm <- file.path(get_lake_dir(aeme, path), "glm_aed")
  }

  if (by == "total") {
    long <- calc_aed_totals_balance(mb, aed_totals = aed_totals,
                                    path_glm = path_glm, units = units)
    long <- dplyr::rename(long, name = total)
  } else {
    long <- dplyr::rename(mb, name = var)
    long$units <- "mmol"
  }

  # `Inf` and `Out` are totals over the save interval (nsave * dt), so the
  # interval length scales them. The interval of each row is the time since the
  # previous row; the first row has no previous row, so its interval is
  # nsave * dt from the GLM nml, or else the median interval.
  save_days <- glm_save_interval_days(path_glm)
  step_days <- stats::median(as.numeric(diff(sort(unique(mb$datetime))),
                                        units = "days"))
  if (!is.null(save_days) && is.finite(step_days) &&
      abs(step_days - save_days) > 0.01 * save_days) {
    cli::cli_warn("The mass balance rows are {signif(step_days, 3)} day{?s}
                  apart but the GLM nml saves every {signif(save_days, 3)}
                  day{?s} (nsave * dt). Was the file written by a different
                  run?")
  }
  first_days <- save_days %||% step_days

  daily <- long |>
    dplyr::select(datetime, name, term, value) |>
    tidyr::pivot_wider(names_from = term, values_from = value) |>
    dplyr::rename(inflow = "Inf", lake = "Lake", outflow = "Out") |>
    dplyr::arrange(name, datetime) |>
    dplyr::group_by(name) |>
    dplyr::mutate(
      interval_days = as.numeric(datetime - dplyr::lag(datetime),
                                 units = "days"),
      interval_days = dplyr::coalesce(interval_days, first_days),
      d_lake = lake - dplyr::lag(lake),
      net_external = inflow - outflow,
      net_internal = d_lake - net_external,
      cum_net_internal = cumsum(dplyr::coalesce(net_internal, 0)),
      # per-day rates, comparable between runs with different nsave
      inflow_rate = inflow / interval_days,
      outflow_rate = outflow / interval_days,
      net_internal_rate = net_internal / interval_days) |>
    dplyr::ungroup()

  summ <- daily |>
    dplyr::summarise(
      mean_lake = mean(lake, na.rm = TRUE),
      total_inflow = sum(inflow, na.rm = TRUE),
      total_outflow = sum(outflow, na.rm = TRUE),
      total_net_internal = sum(net_internal, na.rm = TRUE),
      net_internal_pct = 100 * total_net_internal / mean_lake,
      mean_net_internal_rate = sum(net_internal, na.rm = TRUE) /
        sum(interval_days[!is.na(net_internal)]),
      .by = name
    )

  out <- list(daily = daily, summary = summ)
  if (isTRUE(plot)) {
    out$plot <- ggplot2::ggplot(daily, ggplot2::aes(datetime,
                                                    cum_net_internal)) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                          colour = "grey60") +
      ggplot2::geom_line() +
      ggplot2::facet_wrap(~name, scales = "free_y") +
      ggplot2::labs(x = NULL, y = paste0("Cumulative net internal (",
                                         if (by == "total" && units == "kg")
                                           "kg" else "mmol", ")"))
  }
  out
}

#' Length of the GLM save interval in days
#'
#' `nsave * dt` from the GLM nml, i.e. the period covered by each row of the
#' mass balance file.
#'
#' @param path_glm GLM-AED model directory, or `NULL`.
#' @return numeric (days), or `NULL` if it cannot be determined.
#' @noRd
glm_save_interval_days <- function(path_glm) {
  if (is.null(path_glm)) return(NULL)
  glm_file <- find_glm_nml(path_glm, must_exist = FALSE)
  if (is.na(glm_file)) return(NULL)
  nml <- read_nml(glm_file)
  nsave <- suppressWarnings(as.numeric(nml[["output"]][["nsave"]]))
  dt <- suppressWarnings(as.numeric(nml[["time"]][["timestep"]] %||%
                                      nml[["time"]][["dt"]]))
  if (length(nsave) != 1 || length(dt) != 1 || is.na(nsave) || is.na(dt)) {
    return(NULL)
  }
  nsave * dt / 86400
}


#' Whole-lake N and P budget from the GLM-AED mass balance
#'
#' Closes the nitrogen and phosphorus budget of a GLM-AED run from its
#' `mass_balance.csv`, in kg, and summarises it by year, month or the whole run.
#' The components of total N (TN) and total P (TP) are summed as in
#' [calc_aed_totals_balance()] (so they follow the AED configuration, including
#' the phytoplankton internal nutrient quotas) and the budget is closed as in
#' [diag_glm_mass_balance()]:
#'
#' `internal = d(storage) - (inflow - outflow)`
#'
#' Reactions between pools cancel in the totals, so `internal` is the net
#' amount gained (sediment release, atmospheric deposition, N fixation) or lost
#' (burial, denitrification, settling to the sediment, seepage) across the lake
#' boundary that the inflow and outflow terms do not see. A persistently large
#' `internal` points to a missing source or sink. `Inf` and `Out` are totals over
#' the save interval, so the sums are valid whatever `nsave` is.
#'
#' @param aeme Aeme object that has been run with `glm_aed`.
#' @param path character; root directory used with `aeme`.
#' @param element character; `"N"`, `"P"` or both.
#' @param period character; summary resolution, `"year"` (default), `"month"`
#'   or `"all"` (the whole run).
#' @param include_spin_up logical; keep the spin-up period? If `FALSE`, rows
#'   before the start of the simulation are dropped.
#'
#' @return a list with `daily` (the per-row budget from
#'   [diag_glm_mass_balance()] in kg, with an `element` column) and `summary`
#'   (per `element` and `period`: `inflow`, `outflow`, `retained` = inflow -
#'   outflow, `d_storage`, `internal`, all in kg, and `retention_pct` = 100 *
#'   retained / inflow). The first row has no previous storage and is excluded
#'   from the summary.
#' @seealso [set_glm_balance_groups()] to choose which variables GLM writes to
#'   the mass balance.
#' @export
#' @examples
#' \dontrun{
#' nb <- glm_nutrient_balance(aeme, element = "N", period = "year")
#' nb$summary
#' }
glm_nutrient_balance <- function(aeme, path = NULL,
                                 element = c("N", "P"),
                                 period = c("year", "month", "all"),
                                 include_spin_up = TRUE) {
  element <- match.arg(element, several.ok = TRUE)
  period <- match.arg(period)

  diag <- diag_glm_mass_balance(aeme = aeme, path = path, by = "total",
                                units = "kg")
  totals <- c(N = "TN", P = "TP")[element]
  daily <- diag$daily |>
    dplyr::filter(name %in% totals) |>
    dplyr::mutate(element = names(totals)[match(name, totals)])
  if (nrow(daily) == 0) {
    cli::cli_abort("The mass balance holds none of the components of {.val
                   {unname(totals)}}. See {.fn set_glm_balance_groups}.")
  }

  if (!isTRUE(include_spin_up)) {
    start <- as.POSIXct(format(time(aeme)$start), tz = "UTC")
    daily <- daily |>
      dplyr::filter(datetime >= start)
  }

  fmt <- switch(period, year = "%Y", month = "%Y-%m", all = NULL)
  summ <- daily |>
    dplyr::filter(!is.na(d_lake)) |>
    dplyr::mutate(period = if (is.null(fmt)) "all" else
      format(datetime, fmt)) |>
    dplyr::summarise(inflow = sum(inflow), outflow = sum(outflow),
                     d_storage = sum(d_lake), internal = sum(net_internal),
                     .by = c(element, period)) |>
    dplyr::mutate(retained = inflow - outflow,
                  retention_pct = dplyr::if_else(
                    inflow > 0, 100 * retained / inflow, NA_real_)) |>
    dplyr::arrange(element, period) |>
    dplyr::select(element, period, inflow, outflow, retained, d_storage,
                  internal, retention_pct)

  list(daily = dplyr::select(daily, element, dplyr::everything(), -name),
       summary = summ)
}

#' Put the mass balance on the time axis of the GLM netCDF output
#'
#' GLM stamps each row of `mass_balance.csv` with the start of the save
#' interval it covers, while `output.nc` stamps the state at the end of the
#' same interval, so the two files are one `nsave * dt` apart for the same
#' row (the `Lake` inventory of row `i` equals the netCDF state of record
#' `i`). This shifts the mass balance time stamps by the interval between
#' rows so that dates can be joined to anything read from `output.nc`,
#' including [get_var()].
#'
#' @param mb tibble from [read_glm_mass_balance()].
#' @return `mb` with `datetime` moved forward by one save interval.
#' @noRd
glm_mb_to_output_time <- function(mb) {
  step <- stats::median(as.numeric(diff(sort(unique(mb$datetime))),
                                   units = "secs"))
  if (!is.finite(step)) return(mb)
  mb$datetime <- mb$datetime + step
  mb
}
