glm_cfg_dir <- function() test_path("LID45819_wainamu", "glm_aed")

mb_out_fixture <- function() {
  d <- as.POSIXct("2020-01-01", tz = "UTC") + 86400 * 0:2
  tibble::tibble(
    datetime = rep(d, 3),
    var = rep(c("NIT_nit", "OGM_don", "NIT_amm"), each = 3),
    term = "Out",
    value = c(100, 200, 300, 50, 50, 50, 10, 10, 10)
  )
}

test_that("outflow concentration is mass per day over outflow volume", {
  flow <- data.frame(Date = as.Date("2020-01-01") + 0:2, flow = c(10, 20, 0))
  conc <- glm_mass_balance_outflow_conc(
    mb_out_fixture(), flow, var_sim = c("NIT_nit", "NIT_don", "HYD_temp"))
  # NIT_don is OGM_don in GLM; HYD_temp is not in the mass balance
  expect_setequal(unique(conc$var_sim), c("NIT_nit", "NIT_don"))
  nit <- conc[conc$var_sim == "NIT_nit", ]
  # day 3 has no outflow so no concentration; days 1-2 are mass / flow
  expect_equal(nit$value, c(100 / 10, 200 / 20))
  expect_equal(conc$value[conc$var_sim == "NIT_don"], c(50 / 10, 50 / 20))
})

test_that("concentrations are converted to AEME units", {
  flow <- data.frame(Date = as.Date("2020-01-01") + 0:2, flow = 10)
  conc <- glm_mass_balance_outflow_conc(
    mb_out_fixture(), flow, c("NIT_nit", "NIT_don"),
    conversion = c(NIT_nit = 0.014, NIT_don = 0.5))
  expect_equal(conc$value[conc$var_sim == "NIT_nit"], c(100, 200, 300) / 10 * 0.014)
  expect_equal(conc$value[conc$var_sim == "NIT_don"], c(50, 50, 50) / 10 * 0.5)
})

test_that("mass per day accounts for the save interval", {
  d <- as.POSIXct("2020-01-01", tz = "UTC") + 43200 * 0:3  # 12-hourly rows
  mb <- tibble::tibble(datetime = d, var = "NIT_nit", term = "Out",
                       value = 50)
  flow <- data.frame(Date = as.Date("2020-01-01") + 0:1, flow = 10)
  conc <- glm_mass_balance_outflow_conc(mb, flow, "NIT_nit")
  # 50 mmol per half day = 100 mmol/d over 10 m3/d
  expect_equal(unique(conc$value), 10)
})

test_that("TN and TP come from the summed totals", {
  skip_if_not(dir.exists(glm_cfg_dir()))
  comps <- glm_totals_component_vars(glm_cfg_dir())
  d <- as.POSIXct("2020-01-01", tz = "UTC") + 86400 * 0:1
  vars <- unique(c(comps$TN, comps$TP))
  mb <- tidyr::expand_grid(datetime = d, var = vars) |>
    dplyr::mutate(term = "Out", value = 1)
  flow <- data.frame(Date = as.Date("2020-01-01") + 0:1, flow = 1)
  conc <- suppressWarnings(glm_mass_balance_outflow_conc(
    mb, flow, c("NIT_tn", "PHS_tp"), path_glm = glm_cfg_dir()))
  expect_setequal(unique(conc$var_sim), c("NIT_tn", "PHS_tp"))
  # every component is 1 mmol, so a total is the sum of its scale factors
  expect_true(all(conc$value > 0))
})

test_that("replace_inflow_values() replaces matches and adds new rows", {
  df <- data.frame(Date = as.Date("2020-01-01") + 0:1, var_sim = "NIT_nit",
                   value = c(1, 2))
  new <- data.frame(Date = as.Date("2020-01-01") + 1:2,
                    var_sim = c("NIT_nit", "NIT_tn"), value = c(9, 8))
  out <- replace_inflow_values(df, new)
  expect_equal(out$value[out$var_sim == "NIT_nit"], c(1, 9))
  expect_equal(out$value[out$var_sim == "NIT_tn"], 8)
  expect_identical(replace_inflow_values(df, NULL), df)
})
