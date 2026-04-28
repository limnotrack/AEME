test_that("rename_modelvars basic name -> name_parse mapping works", {
  result <- rename_modelvars("HYD_temp", type_input = "name", type_output = "name_parse")
  expect_equal(result, "Temperature~(degree~C)")
})

test_that("rename_modelvars returns correct length output", {
  input <- c("HYD_temp", "CHM_oxy", "PHS_frp")
  result <- rename_modelvars(input, type_input = "name", type_output = "name_parse")
  expect_length(result, 3)
})

test_that("rename_modelvars preserves input order", {
  input <- c("PHS_frp", "HYD_temp", "CHM_oxy")
  result <- rename_modelvars(input, type_input = "name", type_output = "name_parse")
  expect_equal(result[[1]], "Phosphate-P~(g~m^-3)")
  expect_equal(result[[2]], "Temperature~(degree~C)")
  expect_equal(result[[3]], "Dissolved~oxygen~(mg~L^-1)")
})

test_that("rename_modelvars works across different column pairs", {
  result <- rename_modelvars("HYD_temp", type_input = "name", type_output = "name_text")
  expect_equal(result, "Water temperature")
  
  result <- rename_modelvars("HYD_temp", type_input = "name", type_output = "name_full")
  expect_equal(result, "Water_temperature_degC")
  
  result <- rename_modelvars("HYD_temp", type_input = "name", type_output = "units")
  expect_equal(result, "degC")
})

test_that("rename_modelvars can map from non-name input columns", {
  result <- rename_modelvars(
    "Temperature~(degree~C)",
    type_input  = "name_parse",
    type_output = "name"
  )
  expect_equal(result, "HYD_temp")
})

test_that("rename_modelvars errors on unmatched name by default", {
  expect_error(
    rename_modelvars("NOT_A_VAR"),
    class = "rlang_error"
  )
})

test_that("rename_modelvars warns (not errors) on unmatched with warn_unmatched = TRUE", {
  expect_warning(
    result <- rename_modelvars("NOT_A_VAR", warn_unmatched = TRUE),
    class = "rlang_warning"
  )
  expect_true(is.na(result))
})

test_that("rename_modelvars returns NA only for unmatched, not whole vector", {
  input <- c("HYD_temp", "NOT_A_VAR", "CHM_oxy")
  expect_warning(
    result <- rename_modelvars(input, warn_unmatched = TRUE),
    class = "rlang_warning"
  )
  expect_equal(result[[1]], "Temperature~(degree~C)")
  expect_true(is.na(result[[2]]))
  expect_equal(result[[3]], "Dissolved~oxygen~(mg~L^-1)")
})

test_that("rename_modelvars errors on invalid type_input column", {
  expect_error(
    rename_modelvars("HYD_temp", type_input = "not_a_column"),
    class = "rlang_error"
  )
})

test_that("rename_modelvars errors on invalid type_output column", {
  expect_error(
    rename_modelvars("HYD_temp", type_output = "not_a_column"),
    class = "rlang_error"
  )
})

test_that("rename_modelvars errors on empty input", {
  expect_error(
    rename_modelvars(character(0)),
    class = "rlang_error"
  )
})

test_that("rename_modelvars errors on non-character input", {
  expect_error(
    rename_modelvars(123),
    class = "rlang_error"
  )
})

test_that("rename_modelvars errors on non-scalar type_input", {
  expect_error(
    rename_modelvars("HYD_temp", type_input = c("name", "name_parse")),
    class = "rlang_error"
  )
})

test_that("rename_modelvars verbose runs without error", {
  expect_no_error(
    rename_modelvars(input = c("HYD_temp", "CHM_oxy"), verbose = TRUE)
  )
})

test_that("rename_modelvars handles duplicate inputs correctly", {
  input  <- c("HYD_temp", "HYD_temp")
  result <- rename_modelvars(input)
  expect_length(result, 2)
  expect_equal(result[[1]], result[[2]])
})

test_that("rename_modelvars handles all MET variables", {
  met_vars <- c("MET_radswd", "MET_radlwd", "MET_cldcvr", "MET_tmpair",
                "MET_wndspd", "MET_wnddir", "MET_pprain", "MET_ppsnow")
  result <- rename_modelvars(met_vars, type_output = "name_text")
  expect_length(result, length(met_vars))
  expect_false(any(is.na(result)))
})
