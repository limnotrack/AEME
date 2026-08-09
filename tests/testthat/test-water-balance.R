# test-water-balance.R
#
# Tests for all water balance functions
# Run with: testthat::test_file("tests/testthat/test-water-balance.R")
#
# Organised by function. Each test_that() label follows the convention:
#   "<function>: <what it does>"
# so that failures are immediately identifiable in the reporter output.

# ══════════════════════════════════════════════════════════════════════════════
# sat_vapour_pressure() — saturation vapour pressure calculation
# ══════════════════════════════════════════════════════════════════════════════

test_that("sat_vapour_pressure: returns numeric value for single temperature", {
  result <- sat_vapour_pressure(20)
  expect_type(result, "double")
  expect_length(result, 1L)
})

test_that("sat_vapour_pressure: returns correct value for 20°C", {
  # Expected value based on Magnus formula at 20°C
  result <- sat_vapour_pressure(20)
  # exp(2.3026 * ((7.5 * 20) / (20 + 237.3) + 0.7858))
  expected <- exp(2.3026 * ((7.5 * 20) / (20 + 237.3) + 0.7858))
  expect_equal(result, expected)
  # Approximately 23.37 hPa
  expect_true(result > 20 && result < 25)
})

test_that("sat_vapour_pressure: works with vector input", {
  temps <- c(15, 20, 25)
  result <- sat_vapour_pressure(temps)
  expect_type(result, "double")
  expect_length(result, 3L)
})

test_that("sat_vapour_pressure: increases with temperature", {
  result <- sat_vapour_pressure(c(10, 20, 30))
  expect_true(result[2] > result[1])
  expect_true(result[3] > result[2])
})

test_that("sat_vapour_pressure: handles zero temperature", {
  result <- sat_vapour_pressure(0)
  expect_type(result, "double")
  expect_true(result > 0)
})

test_that("sat_vapour_pressure: handles negative temperatures", {
  result <- sat_vapour_pressure(-10)
  expect_type(result, "double")
  expect_true(result > 0)
})


# ══════════════════════════════════════════════════════════════════════════════
# latent_heat_flux() — latent heat flux from lake surface
# ══════════════════════════════════════════════════════════════════════════════

test_that("latent_heat_flux: returns numeric value", {
  result <- latent_heat_flux(Ts = 20, wndspd = 3, prvapr = 10)
  expect_type(result, "double")
  expect_length(result, 1L)
})

test_that("latent_heat_flux: returns non-positive values (evaporative loss only)", {
  result <- latent_heat_flux(Ts = 20, wndspd = 3, prvapr = 10)
  expect_true(result <= 0)
})

test_that("latent_heat_flux: returns zero when prvapr equals es", {
  # When air vapour pressure equals saturation, no flux
  Ts <- 20
  es <- sat_vapour_pressure(Ts)
  result <- latent_heat_flux(Ts = Ts, wndspd = 3, prvapr = es)
  expect_equal(result, 0)
})

test_that("latent_heat_flux: flux increases with wind speed", {
  # Higher wind should increase evaporative flux (more negative)
  flux_low_wind <- latent_heat_flux(Ts = 20, wndspd = 1, prvapr = 10)
  flux_high_wind <- latent_heat_flux(Ts = 20, wndspd = 5, prvapr = 10)
  expect_true(flux_high_wind < flux_low_wind)  # More negative = greater evaporation
})

test_that("latent_heat_flux: accepts custom parameters", {
  result <- latent_heat_flux(
    Ts = 25, wndspd = 4, prvapr = 15,
    prsttn = 1000, Ce = 0.0015, rho_air = 1.2, Lv = 2500000
  )
  expect_type(result, "double")
  expect_true(result <= 0)
})

test_that("latent_heat_flux: works with vector inputs", {
  Ts <- c(15, 20, 25)
  result <- latent_heat_flux(Ts = Ts, wndspd = 3, prvapr = 10)
  expect_length(result, 3L)
  expect_true(all(result <= 0))
})

test_that("latent_heat_flux: handles high vapour pressure (condensation scenario)", {
  # High prvapr should lead to zero flux (capped at 0)
  result <- latent_heat_flux(Ts = 10, wndspd = 3, prvapr = 50)
  expect_equal(result, 0)
})


# ══════════════════════════════════════════════════════════════════════════════
# flux_to_evap() — convert flux to evaporation depth
# ══════════════════════════════════════════════════════════════════════════════

test_that("flux_to_evap: returns numeric value", {
  result <- flux_to_evap(-50)
  expect_type(result, "double")
  expect_length(result, 1L)
})

test_that("flux_to_evap: converts negative flux to negative evaporation", {
  result <- flux_to_evap(-100)
  expect_true(result < 0)
})

test_that("flux_to_evap: returns correct conversion for known flux", {
  # Qlh = -100 W/m², Lv = 2453000 J/kg, rho = 1000 kg/m³
  # E = (-100 / 2453000) * (86400 / 1000)
  Qlh <- -100
  expected <- (Qlh / 2453000) * (86400 / 1000)
  result <- flux_to_evap(Qlh)
  expect_equal(result, expected)
})

test_that("flux_to_evap: works with vector input", {
  fluxes <- c(-50, -100, -150)
  result <- flux_to_evap(fluxes)
  expect_length(result, 3L)
  expect_true(all(result < 0))
})

test_that("flux_to_evap: accepts custom Lv and rho_water", {
  result <- flux_to_evap(-100, Lv = 2500000, rho_water = 998)
  expect_type(result, "double")
})

test_that("flux_to_evap: handles zero flux", {
  result <- flux_to_evap(0)
  expect_equal(result, 0)
})

test_that("flux_to_evap: magnitude increases with flux magnitude", {
  result <- flux_to_evap(c(-50, -100, -150))
  expect_true(abs(result[2]) > abs(result[1]))
  expect_true(abs(result[3]) > abs(result[2]))
})


# ══════════════════════════════════════════════════════════════════════════════
# get_wbal_param() — retrieve water balance parameters
# ══════════════════════════════════════════════════════════════════════════════

test_that("get_wbal_param: returns NULL when no parameters set", {
  # Create a fresh AEME object without water balance params
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  aeme <- yaml_to_aeme(path = aeme_dir, file = "aeme.yaml")

  # Suppress the informational message about no parameters
  options(AEME.inform = TRUE)
  expect_message(
    result <- get_wbal_param(aeme),
    "No water balance parameters"
  )
  expect_null(result)
  options(AEME.inform = FALSE)
})

test_that("get_wbal_param: with model resolves a single family's flat vector", {
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  aeme <- yaml_to_aeme(path = aeme_dir, file = "aeme.yaml")

  # Set parameters (no model -> applied to every family)
  aeme <- set_wbal_param(aeme, C = 0.5, h_inv = 1.0)

  # Get parameters for one model
  result <- get_wbal_param(aeme, model = "glm_aed")
  expect_type(result, "double")
  expect_named(result, c("C", "h_inv"))
  expect_equal(result[["C"]], 0.5)
  expect_equal(result[["h_inv"]], 1.0)
})

test_that("get_wbal_param: without model returns the full family-keyed list", {
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  aeme <- yaml_to_aeme(path = aeme_dir, file = "aeme.yaml")

  aeme <- set_wbal_param(aeme, C = 0.5, h_inv = 1.0)

  result <- get_wbal_param(aeme)
  expect_type(result, "list")
  expect_true(all(c("glm_aed", "gotm_wet", "simstrat_aed2") %in% names(result)))
  expect_equal(result$glm_aed[["C"]], 0.5)
})

test_that("get_wbal_param: dy_cd resolves to the same family as glm_aed", {
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  aeme <- yaml_to_aeme(path = aeme_dir, file = "aeme.yaml")

  aeme <- set_wbal_param(aeme, C = 0.5, h_inv = 1.0, model = "glm_aed")

  expect_equal(get_wbal_param(aeme, model = "dy_cd"),
              get_wbal_param(aeme, model = "glm_aed"))
})

test_that("get_wbal_param: returns Aeme object check error for invalid input", {
  expect_error(
    get_wbal_param("not_an_aeme"),
    class = "aeme_error_aeme_type"
  )
})


# ══════════════════════════════════════════════════════════════════════════════
# set_wbal_param() — set water balance parameters
# ══════════════════════════════════════════════════════════════════════════════

test_that("set_wbal_param: with no model sets every evaporation family", {
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  aeme <- yaml_to_aeme(path = aeme_dir, file = "aeme.yaml")

  aeme <- set_wbal_param(aeme, C = 0.5, h_inv = 1.0)
  wb <- water_balance(aeme)

  expect_equal(wb$params$glm_aed[["C"]], 0.5)
  expect_equal(wb$params$gotm_wet[["h_inv"]], 1.0)
  expect_equal(wb$params$simstrat_aed2[["C"]], 0.5)
})

test_that("set_wbal_param: with model only sets that model's family", {
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  aeme <- yaml_to_aeme(path = aeme_dir, file = "aeme.yaml")

  aeme <- set_wbal_param(aeme, C = 0.5, h_inv = 1.0, model = "gotm_wet")
  wb <- water_balance(aeme)

  expect_equal(wb$params$gotm_wet[["C"]], 0.5)
  expect_null(wb$params$glm_aed)
  expect_null(wb$params$simstrat_aed2)
})

test_that("set_wbal_param: different families can hold different values", {
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  aeme <- yaml_to_aeme(path = aeme_dir, file = "aeme.yaml")

  aeme <- set_wbal_param(aeme, C = 0.5, h_inv = 1.0, model = "glm_aed")
  aeme <- set_wbal_param(aeme, C = 0.9, h_inv = 2.0, model = "gotm_wet")
  wb <- water_balance(aeme)

  expect_equal(wb$params$glm_aed[["C"]], 0.5)
  expect_equal(wb$params$gotm_wet[["C"]], 0.9)
})

test_that("set_wbal_param: accepts a family-keyed list from get_wbal_param()", {
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  aeme <- yaml_to_aeme(path = aeme_dir, file = "aeme.yaml")

  aeme <- set_wbal_param(aeme, C = 0.5, h_inv = 1.0, model = "glm_aed")
  aeme <- set_wbal_param(aeme, C = 0.9, h_inv = 2.0, model = "gotm_wet")
  saved <- get_wbal_param(aeme)

  aeme2 <- yaml_to_aeme(path = aeme_dir, file = "aeme.yaml")
  aeme2 <- set_wbal_param(aeme2, params = saved)

  expect_equal(get_wbal_param(aeme2), saved)
})

test_that("set_wbal_param: accepts params vector instead of individual args", {
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  aeme <- yaml_to_aeme(path = aeme_dir, file = "aeme.yaml")

  params <- c(C = 0.75, h_inv = 1.5)
  aeme <- set_wbal_param(aeme, params = params, model = "glm_aed")
  wb <- water_balance(aeme)

  expect_equal(wb$params$glm_aed[["C"]], 0.75)
  expect_equal(wb$params$glm_aed[["h_inv"]], 1.5)
})

test_that("set_wbal_param: params vector overrides individual arguments", {
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  aeme <- yaml_to_aeme(path = aeme_dir, file = "aeme.yaml")

  params <- c(C = 0.75, h_inv = 1.5)
  aeme <- set_wbal_param(aeme, C = 0.1, h_inv = 0.1, params = params,
                         model = "glm_aed")
  wb <- water_balance(aeme)

  # params should override individual args
  expect_equal(wb$params$glm_aed[["C"]], 0.75)
  expect_equal(wb$params$glm_aed[["h_inv"]], 1.5)
})

test_that("set_wbal_param: errors when params missing required names", {
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  aeme <- yaml_to_aeme(path = aeme_dir, file = "aeme.yaml")

  # Missing h_inv
  params <- c(C = 0.5)
  expect_error(
    set_wbal_param(aeme, params = params),
    "must contain.*C.*h_inv"
  )

  # Missing C
  params <- c(h_inv = 1.0)
  expect_error(
    set_wbal_param(aeme, params = params),
    "must contain.*C.*h_inv"
  )
})

test_that("set_wbal_param: returns an Aeme object", {
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  aeme <- yaml_to_aeme(path = aeme_dir, file = "aeme.yaml")

  result <- set_wbal_param(aeme, C = 0.5, h_inv = 1.0)
  expect_s4_class(result, "Aeme")
})


# ══════════════════════════════════════════════════════════════════════════════
# reset_wbal_param() — reset water balance parameters
# ══════════════════════════════════════════════════════════════════════════════

test_that("reset_wbal_param: sets params to NULL", {
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  aeme <- yaml_to_aeme(path = aeme_dir, file = "aeme.yaml")

  # Set parameters
  aeme <- set_wbal_param(aeme, C = 0.5, h_inv = 1.0)
  expect_false(is.null(water_balance(aeme)$params))

  # Reset parameters
  aeme <- reset_wbal_param(aeme)
  expect_null(water_balance(aeme)$params)
})

test_that("reset_wbal_param: with model only clears that model's family", {
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  aeme <- yaml_to_aeme(path = aeme_dir, file = "aeme.yaml")

  aeme <- set_wbal_param(aeme, C = 0.5, h_inv = 1.0)
  aeme <- reset_wbal_param(aeme, model = "glm_aed")
  wb <- water_balance(aeme)

  expect_null(wb$params$glm_aed)
  expect_equal(wb$params$gotm_wet[["C"]], 0.5)
})

test_that("reset_wbal_param: returns an Aeme object", {
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  aeme <- yaml_to_aeme(path = aeme_dir, file = "aeme.yaml")

  aeme <- set_wbal_param(aeme, C = 0.5, h_inv = 1.0)
  result <- reset_wbal_param(aeme)
  expect_s4_class(result, "Aeme")
})

test_that("reset_wbal_param: can be called on object without params", {
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  aeme <- yaml_to_aeme(path = aeme_dir, file = "aeme.yaml")

  # Reset on fresh object should not error
  expect_no_error(reset_wbal_param(aeme))
})


# ══════════════════════════════════════════════════════════════════════════════
# get_wbal_components() — retrieve water balance components
# ══════════════════════════════════════════════════════════════════════════════

test_that("get_wbal_components: returns a list with expected structure", {
  # Load a pre-built AEME object with output
  aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
  aeme <- readRDS(aeme_file) |> 
    build_aeme(ext_elev = 3, model = "dy_cd")
  
  result <- get_wbal_components(aeme, model = "dy_cd")
  
  expect_type(result, "list")
  expect_named(result, c("meta", "obs", "wb", "wb_sum", "mod", "mod_sum"))
})

test_that("get_wbal_components: meta contains expected fields", {
  aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
  aeme <- readRDS(aeme_file) |> 
    build_aeme(ext_elev = 3, model = "dy_cd")
  
  result <- get_wbal_components(aeme, model = "dy_cd")
  
  expect_named(result$meta, c("elev_offset", "cumulative"))
  expect_type(result$meta$elev_offset, "double")
  expect_type(result$meta$cumulative, "logical")
})

test_that("get_wbal_components: wb is a data frame", {
  aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
  aeme <- readRDS(aeme_file) |> 
    build_aeme(ext_elev = 3, model = "dy_cd")
  
  result <- get_wbal_components(aeme, model = "dy_cd")
  
  expect_s3_class(result$wb, "data.frame")
  expect_true("Date" %in% names(result$wb))
  expect_true("level" %in% names(result$wb))
})

test_that("get_wbal_components: mod is a list of data frames", {
  aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
  aeme <- readRDS(aeme_file) |> 
    build_aeme(ext_elev = 3, model = "dy_cd")
  
  result <- get_wbal_components(aeme, model = "dy_cd")
  
  expect_type(result$mod, "list")
  expect_named(result$mod, c("level", "inflow", "outflow", "rain", "evap", "ts"))
  expect_s3_class(result$mod$level, "data.frame")
  expect_s3_class(result$mod$inflow, "data.frame")
})

test_that("get_wbal_components: cumulative option works", {
  aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
  aeme <- readRDS(aeme_file) |> 
    build_aeme(ext_elev = 3, model = "glm_aed") |> 
    run_aeme()
  
  result <- get_wbal_components(aeme, model = "glm_aed", cumulative = TRUE)
  
  expect_true(result$meta$cumulative)
})

test_that("get_wbal_components: remove_spin_up option works", {
  aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
  aeme <- readRDS(aeme_file) |> 
    build_aeme(ext_elev = 3, model = "dy_cd")
  
  result_with_spinup <- get_wbal_components(aeme, model = "dy_cd", 
                                             remove_spin_up = FALSE)
  result_without_spinup <- get_wbal_components(aeme, model = "dy_cd", 
                                                remove_spin_up = TRUE)
  
  # Result without spinup should have fewer or equal rows
  expect_true(nrow(result_without_spinup$wb) <= nrow(result_with_spinup$wb))
})


# ══════════════════════════════════════════════════════════════════════════════
# water_balance() — accessor and setter
# ══════════════════════════════════════════════════════════════════════════════

test_that("water_balance accessor retrieves water balance slot", {
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  aeme <- yaml_to_aeme(path = aeme_dir, file = "aeme.yaml")
  
  wb <- water_balance(aeme)
  expect_type(wb, "list")
})

test_that("water_balance setter updates water balance slot", {
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  aeme <- yaml_to_aeme(path = aeme_dir, file = "aeme.yaml")
  
  # Get current water balance
  wb <- water_balance(aeme)
  
  # Modify it
  wb$params <- c(C = 0.8, h_inv = 1.2)
  
  # Set it back
  water_balance(aeme) <- wb
  
  # Verify it was set
  wb_new <- water_balance(aeme)
  expect_equal(wb_new$params[["C"]], 0.8)
  expect_equal(wb_new$params[["h_inv"]], 1.2)
})

test_that("water_balance accessor works after set_wbal_param", {
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  aeme <- yaml_to_aeme(path = aeme_dir, file = "aeme.yaml")

  aeme <- set_wbal_param(aeme, C = 0.5, h_inv = 1.0, model = "glm_aed")
  wb <- water_balance(aeme)

  expect_equal(wb$params$glm_aed[["C"]], 0.5)
  expect_equal(wb$params$glm_aed[["h_inv"]], 1.0)
})
