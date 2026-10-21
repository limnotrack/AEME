glm_cfg_dir <- function() {
  test_path("LID45819_wainamu", "glm_aed")
}

mb_nml <- function() {
  list(
    init_profiles = list(wq_names = c("OXY_oxy", "NIT_nit", "SIL_rsi")),
    mass_balance = list(balance_file = "mass_balance",
                        balance_varnum = 0, timezone = 0)
  )
}

test_that("glm_totals_component_vars() lists the components by total", {
  skip_if_not(dir.exists(glm_cfg_dir()))
  comps <- glm_totals_component_vars(glm_cfg_dir())
  expect_type(comps, "list")
  expect_true(all(c("TN", "TP", "TOC") %in% names(comps)))
  expect_true(all(c("NIT_nit", "NIT_amm", "OGM_don", "OGM_pon") %in% comps$TN))
  expect_true(all(c("PHS_frp", "OGM_dop", "OGM_pop") %in% comps$TP))
  expect_true(all(c("OGM_doc", "OGM_poc") %in% comps$TOC))
  expect_true(any(grepl("^PHY_", comps$TN)))
  expect_null(glm_totals_component_vars(NULL))
})

test_that("set_glm_mass_balance() defaults to the N, P and TSS groups", {
  skip_if_not(dir.exists(glm_cfg_dir()))
  comps <- glm_totals_component_vars(glm_cfg_dir())
  out <- set_glm_mass_balance(mb_nml(), use_bgc = TRUE,
                              path_glm = glm_cfg_dir())
  mb <- out$mass_balance

  expect_true(all(c(comps$TN, comps$TP, comps$TSS) %in% mb$balance_vars))
  # carbon, oxygen and silica are not in the default groups
  expect_false(any(c("OGM_doc", "OXY_oxy", "SIL_rsi") %in% mb$balance_vars))
  expect_false(anyDuplicated(mb$balance_vars) > 0)
  expect_lte(length(mb$balance_vars), 15)
  expect_equal(mb$balance_varnum, length(mb$balance_vars))
})

test_that("set_glm_mass_balance() groups can be toggled", {
  skip_if_not(dir.exists(glm_cfg_dir()))
  comps <- glm_totals_component_vars(glm_cfg_dir())

  toc <- set_glm_mass_balance(mb_nml(), path_glm = glm_cfg_dir(),
                              groups = "TOC")$mass_balance$balance_vars
  expect_setequal(toc, comps$TOC)

  oth <- set_glm_mass_balance(mb_nml(), path_glm = glm_cfg_dir(),
                              groups = "other")$mass_balance$balance_vars
  expect_setequal(oth, c("OXY_oxy", "SIL_rsi"))

  # order follows `groups`
  tp_first <- set_glm_mass_balance(mb_nml(), path_glm = glm_cfg_dir(),
                                   groups = c("TP", "TOC"))$mass_balance
  expect_equal(tp_first$balance_vars[seq_along(comps$TP)], comps$TP)

  # the option sets the default
  withr::with_options(list(AEME.glm_balance_groups = "TOC"), {
    opt <- set_glm_mass_balance(mb_nml(),
                                path_glm = glm_cfg_dir())$mass_balance
  })
  expect_setequal(opt$balance_vars, comps$TOC)

  expect_error(set_glm_mass_balance(mb_nml(), path_glm = glm_cfg_dir(),
                                    groups = "XX"), "Unknown")
})

test_that("set_glm_mass_balance() never exceeds GLM's 15 variable limit", {
  skip_if_not(dir.exists(glm_cfg_dir()))
  expect_warning(
    out <- set_glm_mass_balance(mb_nml(), path_glm = glm_cfg_dir(),
                                groups = c("TN", "TP", "TOC", "TSS", "other")),
    "at most 15")
  expect_length(out$mass_balance$balance_vars, 15)
  expect_equal(out$mass_balance$balance_varnum, 15)
})

test_that("set_glm_mass_balance() falls back without path_glm or bgc", {
  # no path_glm: totals cannot be derived, so the active states are reported
  out0 <- set_glm_mass_balance(mb_nml(), use_bgc = TRUE)
  expect_equal(out0$mass_balance$balance_vars,
               c("OXY_oxy", "NIT_nit", "SIL_rsi"))

  # no bgc: nothing reported
  out1 <- set_glm_mass_balance(mb_nml(), use_bgc = FALSE,
                               path_glm = glm_cfg_dir())
  expect_equal(out1$mass_balance$balance_varnum, 0)
  expect_null(out1$mass_balance$balance_vars)
})

test_that("set_glm_balance_groups() updates an Aeme configuration", {
  skip_if_not(dir.exists(glm_cfg_dir()))
  aeme <- readRDS(system.file("extdata/aeme.rds", package = "AEME"))
  lke <- lake(aeme)
  root <- file.path(tempdir(), "sgbg_root")
  lake_dir <- file.path(root, paste0(lke$id, "_", tolower(lke$name)))
  dir.create(lake_dir, recursive = TRUE)
  file.copy(glm_cfg_dir(), lake_dir, recursive = TRUE)

  hd <- read_nml(file.path(glm_cfg_dir(), "glm4.nml"))
  hd$init_profiles$wq_names <- c("OXY_oxy", "NIT_nit", "SIL_rsi")
  hd$wq_setup <- list(wq_lib = "api")
  cfg <- configuration(aeme)
  cfg$glm_aed$hydrodynamic <- hd
  configuration(aeme) <- cfg

  comps <- glm_totals_component_vars(file.path(lake_dir, "glm_aed"))
  aeme <- set_glm_balance_groups(aeme, groups = "TOC", path = root)
  mb <- configuration(aeme)$glm_aed$hydrodynamic$mass_balance
  expect_setequal(mb$balance_vars, comps$TOC)
  expect_equal(mb$balance_varnum, length(comps$TOC))
  expect_error(set_glm_balance_groups(aeme, groups = "nope", path = root),
               "Unknown")
})
