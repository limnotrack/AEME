test_that("glm_po4_adsorption_on() reads simPO4Adsorption and the module list", {
  nml <- list(aed_models = list(models = c("aed_oxygen", "aed_phosphorus")),
              aed_phosphorus = list(simpo4adsorption = TRUE))
  expect_true(AEME:::glm_po4_adsorption_on(nml))
  nml$aed_phosphorus$simpo4adsorption <- FALSE
  expect_false(AEME:::glm_po4_adsorption_on(nml))
  nml$aed_phosphorus$simpo4adsorption <- TRUE
  nml$aed_models$models <- "aed_oxygen"
  expect_false(AEME:::glm_po4_adsorption_on(nml))
  expect_false(AEME:::glm_po4_adsorption_on(tempfile()))
})

test_that("PHS_pip is initialised via wq_init_vals only when adsorption is on", {
  mc <- get_model_controls()
  mc <- mc[mc$var_aeme %in% c("PHS_frp", "PHS_pip", "CHM_oxy"), ]
  mc$simulate <- TRUE
  mc$initial_wc[mc$var_aeme == "PHS_pip"] <- 0.31
  aed_models <- c("aed_oxygen", "aed_phosphorus")

  off <- AEME:::glm_wq_init_args(mc, n_depths = 2, aed_models = aed_models)
  expect_false("PHS_frp_ads" %in% off$wq_names)

  on <- AEME:::glm_wq_init_args(mc, n_depths = 2, aed_models = aed_models,
                                po4_ads = TRUE)
  expect_true("PHS_frp_ads" %in% on$wq_names)
  expect_equal(on$num_wq_vars, off$num_wq_vars + 1)
  idx <- which(on$wq_names == "PHS_frp_ads")
  conv <- mc$conversion_aed[mc$var_aeme == "PHS_pip"]
  expect_equal(on$wq_init_vals[(idx - 1) * 2 + 1:2], rep(round(0.31 / conv, 4), 2))
})
