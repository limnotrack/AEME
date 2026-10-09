#' Summarise lake loading from inflows
#'
#' Calculates the discharge and constituent loads (e.g. nitrogen, phosphorus,
#' carbon) entering the lake via its inflows, expressed as both totals over
#' the simulation period and annual averages. Useful for getting a quick
#' overview of how much water and nutrient mass is being delivered to the
#' lake, and by which inflow.
#'
#' Discharge is summed directly from the \code{HYD_flow} column (m3/day).
#' Constituent loads are calculated as \code{HYD_flow * concentration} for
#' each recognised mass-concentration variable (units \code{g/m^3} in
#' \code{key_naming}, e.g. \code{NIT_tn}, \code{NIT_amm}, \code{PHS_tp},
#' \code{PHS_frp}, \code{CAR_doc}, \code{CAR_poc}) present in the inflow data,
#' summed over time and converted from g to kg.
#'
#' @inheritParams build_aeme
#' @param inflow_vars character; vector of AEME inflow concentration variable
#'   names to calculate loads for (e.g. \code{c("NIT_tn", "PHS_tp")}). If
#'   \code{NULL} (default), all recognised mass-concentration variables
#'   present in the inflow data are used.
#' @param by_inflow logical; if \code{TRUE} (default), totals are broken down
#'   by \code{inflow_id} as well as summed across all inflows (labelled
#'   \code{"all"}). If \code{FALSE}, only the combined total across all
#'   inflows is returned.
#'
#' @return A data frame with columns:
#' \itemize{
#'   \item \code{inflow_id}: the inflow identifier, or \code{"all"} for the
#'     sum across all inflows.
#'   \item \code{var_aeme}: variable name (\code{"HYD_flow"} for discharge, or
#'     the AEME concentration variable name for a load).
#'   \item \code{name_text}: human-readable variable name.
#'   \item \code{metric}: \code{"discharge"} or \code{"load"}.
#'   \item \code{unit}: unit of the \code{total} and \code{annual_average}
#'     columns (\code{"m3"} for discharge, \code{"kg"} for loads).
#'   \item \code{total}: total discharge/load summed over the full inflow
#'     record.
#'   \item \code{annual_average}: \code{total} divided by the number of years
#'     spanned by the inflow record.
#'   \item \code{n_years}: number of years spanned by the inflow record, used
#'     to calculate \code{annual_average}.
#' }
#'
#' @importFrom dplyr bind_rows left_join
#' @export
#'
#' @examples
#' \dontrun{
#' summarise_loads(aeme)
#' summarise_loads(aeme, inflow_vars = c("NIT_tn", "PHS_tp"))
#' }

summarise_loads <- function(aeme, inflow_vars = NULL, by_inflow = TRUE) {

  aeme <- check_aeme(aeme)
  inf_df <- get_inflows(aeme, return_df = TRUE)

  if (is.null(inf_df) || nrow(inf_df) == 0) {
    cli::cli_abort(
      c("!" = "No inflow data found in {.arg aeme}."),
      class = "aeme_error_loads_no_inflows"
    )
  }
  if (!"HYD_flow" %in% names(inf_df)) {
    cli::cli_abort(
      c("!" = "Inflow data does not contain a {.code HYD_flow} column.",
        "i" = "Discharge and loads cannot be calculated without flow."),
      class = "aeme_error_loads_no_flow"
    )
  }

  env <- new.env(parent = emptyenv())
  data("key_naming", package = "AEME", envir = env)
  key_naming <- env$key_naming

  # Concentration variables eligible for load calculation: any column present
  # in the inflow data that key_naming records as a mass concentration
  # (g/m^3), excluding flow/temperature/salinity/bookkeeping columns.
  non_conc <- c("Date", "time", "date", "inflow_id", "model", "HYD_flow",
                "HYD_temp", "CHM_salt")
  mass_conc_vars <- key_naming$var_aeme[key_naming$units %in% "g/m^3"]
  conc_vars <- intersect(setdiff(names(inf_df), non_conc), mass_conc_vars)
  if (!is.null(inflow_vars)) {
    conc_vars <- intersect(conc_vars, inflow_vars)
  }

  n_days  <- as.numeric(difftime(max(inf_df$Date), min(inf_df$Date),
                                 units = "days")) + 1
  n_years <- n_days / 365.25

  calc_group <- function(df, grp_label) {
    discharge_total <- sum(df[["HYD_flow"]], na.rm = TRUE)
    rows <- list(
      data.frame(
        inflow_id      = grp_label,
        var_aeme       = "HYD_flow",
        metric         = "discharge",
        unit           = "m3",
        total          = discharge_total,
        annual_average = discharge_total / n_years,
        stringsAsFactors = FALSE
      )
    )
    for (v in conc_vars) {
      if (!v %in% names(df) || all(is.na(df[[v]]))) next
      load_g_d  <- df[["HYD_flow"]] * df[[v]]
      total_kg  <- sum(load_g_d, na.rm = TRUE) / 1000
      rows[[length(rows) + 1]] <- data.frame(
        inflow_id      = grp_label,
        var_aeme       = v,
        metric         = "load",
        unit           = "kg",
        total          = total_kg,
        annual_average = total_kg / n_years,
        stringsAsFactors = FALSE
      )
    }
    dplyr::bind_rows(rows)
  }

  out_list <- list()
  if (by_inflow && "inflow_id" %in% names(inf_df)) {
    for (id in unique(inf_df[["inflow_id"]])) {
      out_list[[id]] <- calc_group(inf_df[inf_df[["inflow_id"]] == id, ], id)
    }
  }
  out_list[["all"]] <- calc_group(inf_df, "all")

  out <- dplyr::bind_rows(out_list)
  out[["n_years"]] <- n_years

  out <- out |>
    dplyr::left_join(key_naming[, c("var_aeme", "name_text")],
                     by = "var_aeme")
  out <- out[, c("inflow_id", "var_aeme", "name_text", "metric", "unit",
                 "total", "annual_average", "n_years")]
  rownames(out) <- NULL
  out
}
