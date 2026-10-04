test_that("module registry derives the maps consistently", {
  expect_identical(unname(.aed_module_map[["ALU"]]), "aed_alum")
  expect_identical(aed_prefixes_to_modules(c("ALU", "PHS")),
                   c("aed_alum", "aed_phosphorus"))
  expect_identical(unname(.glm_aed_prefix_map[["aed_alum"]]), "ALU")
  expect_identical(unname(.glm_aed_prefix_map[["aed_carbon"]]), "CAR")
  # carbon is not ordered/activated; totals is ordered but not activated
  expect_false("aed_carbon" %in% .aed_module_order)
  expect_true("aed_totals" %in% .aed_module_order)
  expect_false("TOT" %in% names(.aed_module_map))
  expect_lt(which(.aed_module_order == "aed_phosphorus"),
            which(.aed_module_order == "aed_alum"))
})

test_that("aed_alum pulls in its dependencies", {
  expect_setequal(resolve_aed_active_modules("aed_alum"),
                  c("aed_sedflux", "aed_oxygen", "aed_phosphorus", "aed_alum"))
})

test_that("ALU variables are in key_naming", {
  expect_identical(rename_modelvars(c("ALU_ala", "ALU_alp"),
                                    type_output = "glm_aed"),
                   c("ALU_ala", "ALU_alp"))
  expect_true(glm_aed_var_active("ALU_ala", c("aed_alum", "aed_phosphorus")))
  expect_false(glm_aed_var_active("ALU_ala", "aed_phosphorus"))
})

test_that("rename_modelvars passthrough handles unregistered AED variables", {
  expect_error(rename_modelvars("ZZZ_new", type_output = "glm_aed"),
               "unmatched")
  expect_warning(
    out <- rename_modelvars(c("ALU_ala", "ZZZ_new"), type_output = "glm_aed",
                            passthrough = TRUE),
    "passed through")
  expect_identical(out, c("ALU_ala", "ZZZ_new"))
  # non-AED-looking names still error
  expect_error(rename_modelvars("not a var", type_output = "glm_aed",
                                passthrough = TRUE), "unmatched")
})
