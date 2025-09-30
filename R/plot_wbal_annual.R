#' Plot annual water balance components
#' 
#' Calculates and plots annual water balance components including evaporation,
#' precipitation, inflow, outflow, and net balance for each model in the Aeme
#'  object.
#'
#' @inheritParams plot_var
#'
#' @return ggplot2 object
#'
#' @export
#'
#' @importFrom dplyr bind_rows case_when  group_by left_join mutate 
#'  summarise
#' @importFrom tidyr pivot_wider
#' @importFrom lubridate year
#' @importFrom ggplot2 ggplot geom_bar geom_point position_stack geom_hline
#'  facet_wrap labs aes

plot_wbal_annual <- function(aeme, remove_spin_up = FALSE) {
  
  utils::data("key_naming", package = "AEME", envir = environment())
  
  lake_vol <- calc_lake_vol(aeme)
  
  vars <- c("LKE_evpvol", "LKE_pcpvol", "LKE_inflow", "LKE_outflow")
  df <- lapply(vars, \(v) {
    get_var(aeme, model = model, var_sim = v, return_df = TRUE,
            cumulative = FALSE, remove_spin_up = )
  }) |>
    dplyr::bind_rows() |> 
    dplyr::left_join(key_naming[, c("name", "name_parse", "name_text")],
                     by = c("var_sim" = "name")) |>
    dplyr::mutate(
      label = factor(name_text, levels = c("Evaporation",
                                               "Precipitation" ,
                                               "Inflow",
                                               "Outflow"))
    )
  
  n_years <- ceiling(nrow(df) / 365)
  
  df <- df |>
    dplyr::mutate(year_count = 1 + as.integer(difftime(Date, min(Date),
                                                       units = "days")) %/% 365,
                  year_class = lubridate::year(min(Date)) + year_count - 1
                  ) |> 
    dplyr::group_by(Model, label, year_class) |>
    dplyr::summarise(value = sum(value, na.rm = TRUE),
                     # Date = mean(Date),
                     .groups = "drop") |>
    dplyr::mutate(
      value = dplyr::case_when(
        label %in% c("Evaporation", "Outflow") ~ -value,
        TRUE ~ value
      ),
      lake_frac = abs(value) / lake_vol
      ) # Convert to fraction of lake volume
  
  df_wid <- df |> 
    dplyr::select(-lake_frac) |> 
    tidyr::pivot_wider(names_from = label, values_from = value) |> 
    dplyr::mutate(
      net = (Inflow + Precipitation) + (Outflow + Evaporation),
      net_frac = net / lake_vol,
      type = "Net Balance"
    ) 
  

  ggplot2::ggplot() +
    ggplot2::geom_bar(data = df, ggplot2::aes(x = year_class, y = value, 
                                              fill = label), 
             stat = "identity", position = "stack") +
    ggplot2::geom_segment(data = df_wid, 
                     ggplot2::aes(x = year_class, xend = year_class,
                                  y = 0, yend = net), 
                     color = "black") +
    ggplot2::geom_point(data = df_wid, ggplot2::aes(x = year_class, y = net,
                                                    shape = type), 
               position = position_stack(vjust = 0.5), color = "black",
               size = 3) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::facet_wrap(~ Model, ncol = 1) +
    ggplot2::labs(
      x = "Year",
      y = "Volume (m3)",
      fill = "Variable",
      shape = ""
    )
}
