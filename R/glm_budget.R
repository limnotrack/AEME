#' Carbon, nitrogen and phosphorus budget of a GLM-AED lake
#'
#' Combines the process and interface fluxes from the GLM-AED diagnostic
#' output ([calc_glm_fluxes()]) with the inflow, outflow and in-lake inventory
#' from the mass balance ([diag_glm_mass_balance()]) to give, for each element,
#' where it comes from, where it goes, and how much of the change in the
#' inventory those terms explain.
#'
#' The lake is treated as one pool made of the compartments `inorganic`,
#' `phyto`, `dom` and `pom` (for carbon, `inorganic` is DIC and is outside the
#' pool, as it is not part of TOC). A flux that crosses the boundary of that
#' pool adds to or removes from the inventory. Anything between two
#' compartments inside the pool (uptake, mortality, mineralisation,
#' nitrification, ...) is internal and cancels. The closure is
#'
#' `residual = d(inventory) - (inflow - outflow + sediment + atmosphere)`
#'
#' where `sediment` is the net exchange across the sediment-water interface
#' (dissolved release minus settling) and `atmosphere` is deposition and gas
#' exchange, including N2 fixation and denitrification for nitrogen. A residual
#' near zero means the diagnostics account for the budget. A persistent
#' residual points to a source or sink the diagnostics do not report, for
#' example a module that is not writing its `*_swi` terms, or a mismatch
#' between how a pool is settled and how it is resuspended.
#'
#' The fluxes are the instantaneous rates AED writes at each save time. On the
#' example lake a residual of 3 to 13% of the gross primary production
#' remained with settling, hydrolysis, resuspension and outflow switched off,
#' with hourly instead of daily output, with a ten times shorter time step and
#' with thinner layers, and it disappeared when growth was switched off. It
#' therefore follows the production terms (`PHY_upt_*`) rather than the other
#' pathways, and is not a sampling or resolution effect. Compare the residual
#' with the largest flux, not with the change in storage, which can be
#' close to zero.
#'
#' The inflow, outflow and inventory need the totals in the mass balance:
#' `TN`, `TP` and `TOC` are all in the default `balance_vars`
#' (see [set_glm_balance_groups()] and [add_glm_balance_vars()]). Without them
#' `closure` is `NULL` for that element but the fluxes and pools are still
#' returned.
#'
#' @inheritParams calc_glm_fluxes
#' @param period character; summary resolution, `"all"` (default, the whole
#'   run), `"year"` or `"month"`.
#' @param year_start integer; the month (1 to 12) a year starts in when
#'   `period = "year"`. Default `NULL` takes it from the lake latitude: 1 (the
#'   calendar year, labelled `"2021"`) in the northern hemisphere, and 7 (a
#'   July to June year, labelled by both calendar years, `"2020/21"`) in the
#'   southern hemisphere. The calendar year is used if the latitude is not
#'   set.
#' @param settling character; `"hypsograph"` (default) or `"interface"`, see
#'   [calc_glm_fluxes()].
#' @param include_spin_up logical; keep the spin-up period? If `FALSE`, days
#'   before the simulation start are dropped.
#'
#' @return a list with
#'   * `fluxes`: one row per element, period and process between two
#'     compartments (`from`, `to`), with the mean rate `mmol_d`, the equivalent
#'     `kg_yr` (kg of the element per year), `t_yr` (tonnes per year) and the
#'     areal `mmol_m2_d` (per m2 of mean lake surface).
#'   * `pools`: the mean inventory of each compartment (`mmol`, `kg` and `t`),
#'     from the AED totals of the model configuration.
#'   * `closure`: per element and period, the `term`s `inflow`, `outflow`,
#'     `sediment`, `atmosphere`, `explained`, `d_storage` and `residual`, in
#'     the same units. Positive means a gain for the lake.
#'   * `compartments`: the same closure for each compartment (`inorganic`,
#'     `phyto`, `dom`, `pom`): its `d_storage` and the `inflow`, `outflow`,
#'     `flux_in` and `flux_out` of every diagnostic flux entering or leaving it,
#'     including from other compartments, and the `residual` they do not
#'     explain. A compartment with a large residual has a process or exchange
#'     that is mis-reported or missing: this is where to look when a
#'     concentration drifts.
#'   * `daily`: daily net boundary flux and mass balance terms behind
#'     `closure`, in mmol/d.
#' @seealso [plot_glm_budget()] for a box-and-arrow figure of the result.
#' @export
#' @examples
#' \dontrun{
#' bud <- diag_glm_budget(aeme, element = c("N", "P"))
#' bud$closure
#' plot_glm_budget(bud)
#' }
diag_glm_budget <- function(aeme, element = c("N", "P", "C"),
                            period = c("all", "year", "month"),
                            year_start = NULL, path = NULL,
                            include_spin_up = TRUE,
                            settling = c("hypsograph", "interface")) {
  element <- match.arg(element, several.ok = TRUE)
  period <- match.arg(period)
  if (!is.null(year_start) && (!is.numeric(year_start) ||
                               length(year_start) != 1 ||
                               !year_start %in% 1:12)) {
    cli::cli_abort("{.arg year_start} must be a month number from 1 to 12.")
  }
  if (is.null(year_start)) year_start <- glm_budget_year_start(aeme)
  settling <- match.arg(settling)
  file <- glm_output_nc(aeme = aeme, path = path)
  path_glm <- file.path(get_lake_dir(aeme, path), "glm_aed")
  start <- lubridate::as_date(time(aeme)$start)

  fx <- calc_glm_fluxes(file = file, element = element, settling = settling)
  if (settling == "hypsograph" && !any(grepl("_set(_[cnp])?$", fx$var))) {
    cli::cli_warn(c(
      "No settling rates ({.field *_set}) in the GLM output.",
      i = "Sediment deposition is missing from the budget. Set the AED
           {.code diag_level} to 10 or use {.code settling = \"interface\"}."))
  }
  area <- attr(fx, "surface_area")
  pools <- calc_glm_pools(file, path_glm, element)

  keep <- function(d) {
    if (isTRUE(include_spin_up)) d else d[d$Date >= start, ]
  }
  key <- function(d) {
    d$period <- switch(period, all = "all",
                       year = glm_budget_year(d$Date, year_start),
                       month = format(d$Date, "%Y-%m"))
    d
  }
  mmol_to_kg <- function(x, el) x * glm_element_mass()[el] / 1e6

  # ---- boundary flux of the pool, per day ----
  bound <- fx |>
    dplyr::mutate(
      pool = purrr::map(element, glm_budget_pool),
      into = purrr::map2_lgl(to, pool, `%in%`) &
        !purrr::map2_lgl(from, pool, `%in%`),
      outof = purrr::map2_lgl(from, pool, `%in%`) &
        !purrr::map2_lgl(to, pool, `%in%`),
      partner = dplyr::if_else(into, from, to),
      net = value * (into - outof)) |>
    dplyr::filter(into | outof) |>
    dplyr::summarise(net = sum(net, na.rm = TRUE),
                     .by = c(Date, element, partner))

  # ---- mass balance: inflow, outflow and inventory ----
  mb_daily <- glm_budget_mass_balance(aeme, path, path_glm, element)
  daily <- dplyr::bind_rows(
    dplyr::rename(bound, term = partner, rate = net),
    mb_daily) |>
    keep() |>
    key()

  fluxes <- fx |>
    keep() |>
    key() |>
    dplyr::summarise(mmol_d = sum(value, na.rm = TRUE) /
                       dplyr::n_distinct(Date),
                     .by = c(element, period, process, from, to, var)) |>
    dplyr::summarise(mmol_d = sum(mmol_d),
                     .by = c(element, period, process, from, to)) |>
    glm_budget_units(area)

  closure <- NULL
  if (nrow(mb_daily) > 0) {
    # only elements whose totals are in the mass balance can be closed
    closure <- daily |>
      dplyr::filter(element %in% unique(mb_daily$element)) |>
      dplyr::summarise(mmol_d = mean(rate, na.rm = TRUE),
                       .by = c(element, period, term)) |>
      tidyr::pivot_wider(names_from = term, values_from = mmol_d,
                         values_fill = 0)
    for (nm in c("sediment", "atmosphere")) {
      if (!nm %in% names(closure)) closure[[nm]] <- 0
    }
    # every partner across the pool boundary counts: for carbon that includes
    # the exchange with DIC (photosynthesis, respiration, mineralisation)
    ext <- setdiff(names(closure), c("element", "period", "inflow", "outflow",
                                     "d_storage"))
    closure$explained <- closure$inflow - closure$outflow +
      rowSums(closure[ext])
    closure$residual <- closure$d_storage - closure$explained
    closure <- closure |>
      tidyr::pivot_longer(c(inflow, outflow, dplyr::all_of(ext), explained,
                            d_storage, residual),
                          names_to = "term", values_to = "mmol_d") |>
      glm_budget_units(area)
  }

  # ---- closure of each compartment ----
  compartments <- NULL
  aed_totals <- glm_aed_totals(path_glm)
  mb <- tryCatch(
    glm_mb_to_output_time(read_glm_mass_balance(aeme = aeme, path = path)),
    error = function(e) NULL)
  if (!is.null(mb) && !is.null(aed_totals)) {
    compartments <- glm_budget_compartments(mb, aed_totals, fx, element)
    if (nrow(compartments) > 0) {
      compartments <- compartments |>
        keep() |>
        key() |>
        dplyr::summarise(dplyr::across(c(inflow, outflow, d_storage, flux_in,
                                         flux_out, residual),
                                       \(x) mean(x, na.rm = TRUE)),
                         .by = c(element, period, compartment)) |>
        tidyr::pivot_longer(c(inflow, outflow, d_storage, flux_in, flux_out,
                              residual),
                            names_to = "term", values_to = "mmol_d") |>
        glm_budget_units(area)
    } else {
      compartments <- NULL
    }
  }

  pools <- pools |>
    keep() |>
    key() |>
    dplyr::summarise(mmol = mean(mmol, na.rm = TRUE),
                     .by = c(element, period, compartment)) |>
    dplyr::mutate(kg = mmol_to_kg(mmol, element), t = kg / 1e3)

  structure(list(fluxes = fluxes, pools = pools, closure = closure,
                 compartments = compartments, daily = daily),
            class = c("glm_budget", "list"), surface_area = area)
}

#' AED totals (component variables and scales) of a GLM-AED model
#'
#' @param path_glm GLM-AED model directory.
#' @return the `aed_totals` list, or `NULL` if it cannot be derived.
#' @noRd
glm_aed_totals <- function(path_glm) {
  cfg <- tryCatch(
    read_model_config(model = "glm_aed", lake_dir = dirname(path_glm)),
    error = function(e) NULL)
  if (is.null(cfg)) return(NULL)
  tryCatch(derive_aed_totals(cfg)[["bgc"]][["aed"]][["aed_totals"]],
           error = function(e) NULL)
}

#' Closure of the budget of each compartment
#'
#' For each compartment of each element: the change in its inventory from the
#' mass balance (the `Lake` term of its components), the external inflow and
#' outflow, and every diagnostic flux entering or leaving it (including the
#' ones from other compartments). `residual` is what the fluxes do not
#' explain, so a compartment with a large residual has a process or exchange
#' that is mis-reported or missing.
#'
#' @param mb mass balance on the netCDF time axis, see
#'   `glm_mb_to_output_time()`.
#' @param aed_totals list from `glm_aed_totals()`.
#' @param fx tibble from [calc_glm_fluxes()].
#' @param element elements wanted.
#' @return tibble with `Date`, `element`, `compartment`, and daily rates in
#'   mmol/d: `inflow`, `outflow`, `d_storage`, `flux_in`, `flux_out`,
#'   `residual`.
#' @noRd
glm_budget_compartments <- function(mb, aed_totals, fx, element) {
  totals <- c(N = "TN", P = "TP", C = "TOC")
  res <- lapply(element, function(el) {
    tot <- totals[[el]]
    comp <- tibble::tibble(var = aed_totals[[paste0(tot, "_vars")]],
                           scale = aed_totals[[paste0(tot, "_varscale")]])
    comp$compartment <- glm_var_compartment(comp$var)
    comp <- comp[!is.na(comp$compartment), ]
    d <- dplyr::inner_join(mb, comp, by = "var",
                           relationship = "many-to-one") |>
      dplyr::mutate(value = value * scale) |>
      dplyr::summarise(value = sum(value),
                       .by = c(datetime, compartment, term))
    if (nrow(d) == 0) return(NULL)
    step <- stats::median(as.numeric(diff(sort(unique(d$datetime))),
                                     units = "days"))
    mbw <- d |>
      tidyr::pivot_wider(names_from = term, values_from = value) |>
      dplyr::arrange(compartment, datetime) |>
      dplyr::mutate(
        days = dplyr::coalesce(as.numeric(datetime - dplyr::lag(datetime),
                                          units = "days"), step),
        d_storage = (Lake - dplyr::lag(Lake)) / days,
        inflow = `Inf` / days, outflow = Out / days,
        .by = compartment) |>
      dplyr::transmute(Date = lubridate::as_date(datetime), compartment,
                       inflow, outflow, d_storage)

    f <- fx[fx$element == el & fx$from != fx$to, ]
    f_in <- f |>
      dplyr::rename(compartment = to) |>
      dplyr::summarise(flux_in = sum(value, na.rm = TRUE),
                       .by = c(Date, compartment))
    f_out <- f |>
      dplyr::rename(compartment = from) |>
      dplyr::summarise(flux_out = sum(value, na.rm = TRUE),
                       .by = c(Date, compartment))
    mbw |>
      dplyr::left_join(f_in, by = c("Date", "compartment")) |>
      dplyr::left_join(f_out, by = c("Date", "compartment")) |>
      dplyr::mutate(
        flux_in = dplyr::coalesce(flux_in, 0),
        flux_out = dplyr::coalesce(flux_out, 0),
        residual = d_storage - (inflow - outflow + flux_in - flux_out),
        element = el) |>
      dplyr::filter(!is.na(d_storage))
  })
  dplyr::bind_rows(res)
}

#' Month a budget year starts in, from the lake latitude
#'
#' @param aeme Aeme object.
#' @return 7 (July) for a lake in the southern hemisphere, otherwise 1.
#' @noRd
glm_budget_year_start <- function(aeme) {
  lat <- tryCatch(lake(aeme)$latitude, error = function(e) NULL)
  if (is.numeric(lat) && length(lat) == 1 && !is.na(lat) && lat < 0) 7L else 1L
}

#' Label dates by year, starting the year in a given month
#'
#' @param date vector of dates.
#' @param year_start month (1 to 12) the year starts in.
#' @return character; `"2021"` for the calendar year, `"2020/21"` for a year
#'   that starts in July 2020.
#' @noRd
glm_budget_year <- function(date, year_start = 1L) {
  yr <- as.integer(format(date, "%Y"))
  if (year_start == 1L) return(as.character(yr))
  first <- yr - (as.integer(format(date, "%m")) < year_start)
  sprintf("%d/%02d", first, (first + 1L) %% 100L)
}

#' Add kg per year, tonnes per year and areal units to a table of mean rates in
#' mmol/d
#'
#' @param d data frame with `element` and `mmol_d`.
#' @param area mean lake surface area (m2).
#' @noRd
glm_budget_units <- function(d, area) {
  d |>
    dplyr::mutate(kg_yr = mmol_d * glm_element_mass()[element] / 1e6 * 365.25,
                  t_yr = kg_yr / 1e3,
                  mmol_m2_d = mmol_d / area)
}

#' Inflow, outflow and storage change of the AED totals, in mmol/d
#'
#' @param element elements wanted.
#' @return tibble with `Date`, `element`, `term` (`inflow`, `outflow`,
#'   `d_storage`) and `rate` (mmol/d); empty if the totals are not in the mass
#'   balance.
#' @noRd
glm_budget_mass_balance <- function(aeme, path, path_glm, element) {
  empty <- tibble::tibble(Date = as.Date(character()), element = character(),
                          term = character(), rate = numeric())
  totals <- c(N = "TN", P = "TP", C = "TOC")[element]
  mb <- tryCatch(read_glm_mass_balance(aeme = aeme, path = path),
                 error = function(e) NULL)
  if (is.null(mb)) return(empty)
  # dates joined to the netCDF fluxes, which are stamped at the end of the step
  d <- tryCatch(
    suppressWarnings(diag_glm_mass_balance(glm_mb_to_output_time(mb),
                                           by = "total",
                                           path_glm = path_glm)$daily),
    error = function(e) NULL)
  if (is.null(d)) return(empty)
  d <- d |>
    dplyr::filter(name %in% totals) |>
    dplyr::mutate(element = names(totals)[match(name, totals)],
                  Date = lubridate::as_date(datetime))
  if (nrow(d) == 0) return(empty)
  d |>
    dplyr::transmute(Date, element,
                     inflow = inflow / interval_days,
                     outflow = outflow / interval_days,
                     d_storage = d_lake / interval_days) |>
    tidyr::pivot_longer(c(inflow, outflow, d_storage), names_to = "term",
                        values_to = "rate") |>
    dplyr::filter(!is.na(rate)) |>
    dplyr::summarise(rate = mean(rate), .by = c(Date, element, term))
}

#' Inventory of each budget compartment from the GLM state variables
#'
#' The state variables of each element are taken from the `aed_totals` of the
#' model configuration (so the phytoplankton quota or biomass choice and the
#' `*_varscale` factors follow the model), assigned to a compartment and
#' integrated over the layer volumes. For carbon the DIC pool is added.
#'
#' @param file path to `output.nc`.
#' @param path_glm GLM-AED model directory.
#' @param element elements wanted.
#' @return tibble with `Date`, `element`, `compartment` and `mmol`.
#' @noRd
calc_glm_pools <- function(file, path_glm, element) {
  empty <- tibble::tibble(Date = as.Date(character()), element = character(),
                          compartment = character(), mmol = numeric())
  aed_totals <- glm_aed_totals(path_glm)
  if (is.null(aed_totals)) return(empty)

  nc <- ncdf4::nc_open(file)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  vars <- names(nc$var)
  time <- lubridate::as_date(glm_nc_time(nc))
  nt <- length(time)
  get_mat <- function(v) {
    x <- ncdf4::ncvar_get(nc, v)
    x[!is.finite(x) | abs(x) > 1e30] <- NA_real_
    if (length(dim(x)) != 2) x <- matrix(x, ncol = nt)
    x
  }
  volume <- get_mat("V")

  totals <- c(N = "TN", P = "TP", C = "TOC")
  res <- lapply(element, function(el) {
    tot <- totals[[el]]
    comp <- tibble::tibble(var = aed_totals[[paste0(tot, "_vars")]],
                           scale = aed_totals[[paste0(tot, "_varscale")]])
    if (el == "C") {
      comp <- dplyr::bind_rows(comp, tibble::tibble(var = "CAR_dic",
                                                    scale = 1))
    }
    comp <- comp[comp$var %in% vars, ]
    comp$compartment <- glm_var_compartment(comp$var)
    comp <- comp[!is.na(comp$compartment), ]
    if (nrow(comp) == 0) return(NULL)
    inv <- lapply(seq_len(nrow(comp)), function(i) {
      x <- get_mat(comp$var[i]) * volume
      tot_i <- colSums(x, na.rm = TRUE) * comp$scale[i]
      tibble::tibble(Date = time, element = el,
                     compartment = comp$compartment[i], mmol = tot_i)
    })
    dplyr::bind_rows(inv)
  })
  out <- dplyr::bind_rows(res)
  if (nrow(out) == 0) return(empty)
  out |>
    dplyr::summarise(mmol = sum(mmol), .by = c(Date, element, compartment))
}

#' @export
print.glm_budget <- function(x, ...) {
  cli::cli_h1("GLM-AED budget")
  if (!is.null(x$closure)) {
    cat("Closure (kg per year; positive is a gain for the lake):\n")
    cl <- x$closure[x$closure$period == x$closure$period[1], ]
    print(tidyr::pivot_wider(cl[, c("element", "period", "term", "kg_yr")],
                             names_from = term, values_from = kg_yr)[
      , c("element", "period", "inflow", "outflow", "sediment", "atmosphere",
          "explained", "d_storage", "residual")], digits = 3)
  } else {
    cat("No closure: the mass balance does not hold the AED totals.\n")
  }
  invisible(x)
}
