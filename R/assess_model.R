#' Assess model performance
#'
#' @inheritParams build_aeme
#' @inheritParams plot_output
#'
#' @return Data frame with model performance statistics for each model and
#' variable. These include:
#' \itemize{
#' \item{bias}{ - Bias}
#' \item{mae}{ - Mean absolute error}
#' \item{rmse}{ - Root mean square error}
#' \item{nmae}{ - Normalised mean absolute error}
#' \item{nse}{ - Nash-Sutcliffe efficiency}
#' \item{d2}{ - Index of agreement model skill score Willmott index}
#' \item{r}{ - Pearson correlation coefficient}
#' \item{rs}{ - Spearman correlation coefficient}
#' \item{r2}{ - R-squared value from linear model}
#' \item{B}{ - Bardsley coefficient}
#' \item{n}{ - number of observations}
#' }
#'
#' @importFrom dplyr group_by summarise mutate n case_when where across
#' relocate filter left_join select last_col
#' @importFrom stats cor cor.test lm
#'
#' @export
#'

assess_model <- function(aeme, model, var_sim = "HYD_temp") {

  # Extract observations
  obs <- observations(aeme)

  # Loop through variables, extract model statistics, bind to dataframe and
  # return
  lst <- lapply(var_sim, \(v) {

    # Extract variable from aeme
    df <- get_var(aeme = aeme, model = model, var_sim = v,
                  use_obs = TRUE)

    model_names <- data.frame(model = c("dy_cd", "glm_aed", "gotm_wet"),
                              Model = c("DYRESM-CAEDYM", "GLM-AED", "GOTM-WET"))

    # Fit linear model for each model
    fit <- lapply(model, \(m) {
      r2 <- NA
      sub_df <- df |>
        dplyr::filter(Model == model_names$Model[model_names$model == m])
      if (!all(is.na(sub_df$sim))) {
        fit <- stats::lm(obs ~ sim,
                         data = sub_df)
        r2 <- summary(fit)$r.squared
      }
      data.frame(model = m, r2 = r2)
    }) |>
      do.call(rbind, args = _)
    fit$Model <- model_names$Model[match(fit$model, model_names$model)]
    fit <- fit |>
      dplyr::select(Model, r2)

    # Calculate statistics for each model
    res <- df |>
      dplyr::mutate(Residual = sim - obs) |>
      dplyr::group_by(Model, var_sim) |>
      dplyr::summarise(bias = mean(Residual, na.rm = TRUE),
                       mae = mean(abs(Residual), na.rm = TRUE),
                       rmse = sqrt(mean(Residual^2, na.rm = TRUE)),
                       nmae = mae/mean(obs, na.rm = TRUE),
                       nse = 1 - sum(Residual^2) /
                         sum((obs - mean(obs, na.rm = TRUE))^2),
                       # Index of Agreement Model Skill Score Willmott index
                       d2 = mae^2 / mean((abs(mean(sim -
                                                     mean(obs, na.rm = TRUE))) +
                                            abs(obs - mean(obs, na.rm = TRUE)))^2),
                       r = ifelse(all(is.na(sim)), NA, tryCatch(stats::cor(sim, obs, use = "complete.obs"), error = function(e) NA)),
                       rs = ifelse(all(is.na(sim)), NA, suppressWarnings({
                         tryCatch(stats::cor.test(sim, obs, method = "spearman")$estimate, error = function(e) NA)
                       })),
                       n = dplyr::n(),
                       obs_na = sum(is.na(obs)),
                       sim_na = sum(is.na(sim)),
                       .groups = "drop") |>
      as.data.frame()

    # Add linear model statistics to results and calculate Bardsley coefficient
    res |>
      dplyr::left_join(fit, by = "Model") |>
      dplyr::mutate(
        # r2 = dplyr::case_when(
        #   Model == "DYRESM-CAEDYM" ~ fit$r2[fit$model == "dy_cd"],
        #   Model == "GLM-AED" ~ fit$r2[fit$model == "glm_aed"],
        #   Model == "GOTM-WET" ~ fit$r2[fit$model == "gotm_wet"],
        #   .default = NA
        # ),
        B = r2 / (2 - nse), # Bardsley coefficient
      ) |>
      # Round all columns with with numeric values to 3 decimal places
      dplyr::mutate(
        # dplyr::across(dplyr::where(is.numeric), \(x) round(x, 3)),
        dplyr::across(bias:B, \(x) ifelse(is.na(x), NA, round(x, 3))),
      ) |>
      dplyr::relocate(c(n, obs_na, sim_na), .after = dplyr::last_col())

  }) |>
    do.call(rbind, args = _) # Bind list of data frames into one data frame and return

  lst
}
