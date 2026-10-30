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

test_that("set_glm_mass_balance() defaults to every aed_totals component", {
  skip_if_not(dir.exists(glm_cfg_dir()))
  comps <- glm_totals_component_vars(glm_cfg_dir())
  out <- set_glm_mass_balance(mb_nml(), use_bgc = TRUE,
                              path_glm = glm_cfg_dir())
  mb <- out$mass_balance

  expect_true(all(c(comps$TN, comps$TP, comps$TOC, comps$TSS) %in%
                    mb$balance_vars))
  # oxygen and silica are not part of a total
  expect_false(any(c("OXY_oxy", "SIL_rsi") %in% mb$balance_vars))
  expect_false(anyDuplicated(mb$balance_vars) > 0)
  # more than the 15 variables older GLM builds accepted
  expect_gt(length(mb$balance_vars), 15)
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

test_that("set_glm_mass_balance() does not truncate the variable list", {
  skip_if_not(dir.exists(glm_cfg_dir()))
  expect_no_warning(
    out <- set_glm_mass_balance(mb_nml(), path_glm = glm_cfg_dir(),
                                groups = c("TN", "TP", "TOC", "TSS", "other")))
  mb <- out$mass_balance
  expect_gt(length(mb$balance_vars), 15)
  expect_false(anyDuplicated(mb$balance_vars) > 0)
  expect_equal(mb$balance_varnum, length(mb$balance_vars))
  expect_true(all(c("OXY_oxy", "SIL_rsi") %in% mb$balance_vars))
})

test_that("add_glm_balance_vars() adds groups and variables", {
  skip_if_not(dir.exists(glm_cfg_dir()))
  aeme <- readRDS(system.file("extdata/aeme.rds", package = "AEME"))
  lke <- lake(aeme)
  root <- file.path(tempdir(), "agbv_root")
  lake_dir <- file.path(root, paste0(lke$id, "_", tolower(lke$name)))
  dir.create(lake_dir, recursive = TRUE)
  file.copy(glm_cfg_dir(), lake_dir, recursive = TRUE)

  hd <- read_nml(file.path(glm_cfg_dir(), "glm4.nml"))
  hd$init_profiles$wq_names <- c("OXY_oxy", "NIT_nit", "SIL_rsi")
  hd$wq_setup <- list(wq_lib = "api")
  cfg <- configuration(aeme)
  cfg$glm_aed$hydrodynamic <- hd
  configuration(aeme) <- cfg
  mb_vars <- function(a) {
    configuration(a)$glm_aed$hydrodynamic$mass_balance$balance_vars
  }
  comps <- glm_totals_component_vars(file.path(lake_dir, "glm_aed"))

  aeme <- suppressWarnings(
    set_glm_balance_groups(aeme, groups = "TP", path = root))
  expect_setequal(mb_vars(aeme), comps$TP)

  # a group is added to what is there
  aeme <- add_glm_balance_vars(aeme, groups = "TOC", path = root)
  expect_equal(mb_vars(aeme), c(comps$TP, comps$TOC))

  # individual variables, and adding twice does not repeat them
  aeme <- add_glm_balance_vars(aeme, vars = "OXY_oxy", path = root)
  aeme <- add_glm_balance_vars(aeme, groups = "TP", vars = c("OXY_oxy", "SIL_rsi"),
                               path = root)
  expect_equal(mb_vars(aeme), c(comps$TP, comps$TOC, "OXY_oxy", "SIL_rsi"))
  mb <- configuration(aeme)$glm_aed$hydrodynamic$mass_balance
  expect_equal(mb$balance_varnum, length(mb$balance_vars))

  expect_error(add_glm_balance_vars(aeme, vars = "XXX_nope", path = root),
               "not an active variable")
  expect_error(add_glm_balance_vars(aeme, groups = "nope", path = root),
               "Unknown")
  expect_error(add_glm_balance_vars(aeme, path = root), "Provide")
})

test_that("GLM writes more than 15 mass balance variables", {
  skip_on_cran()
  skip_if_models_unavailable("glm_aed")
  aeme <- readRDS(system.file("extdata/aeme.rds", package = "AEME"))
  tmp <- withr::local_tempdir()
  vars_sim <- c("HYD_temp", "NIT_tn", "PHS_tp", "PHY_tchla")
  mc <- get_model_controls(use_bgc = TRUE)
  mc <- set_vars_sim(model_controls = mc, vars_sim = vars_sim)
  aeme <- suppressWarnings(set_sim_period(
    aeme, period = suggest_sim_period(aeme, vars_sim = vars_sim)))

  # default groups: every aed_totals component
  aeme <- suppressWarnings(
    build_aeme(path = tmp, aeme = aeme, model = "glm_aed",
               model_controls = mc, ext_elev = 5, use_bgc = TRUE))
  aeme <- add_glm_balance_vars(aeme, groups = "other", path = tmp)
  vars_req <- configuration(aeme)$glm_aed$hydrodynamic$mass_balance$balance_vars
  skip_if(length(vars_req) <= 15, "Fewer than 16 balance variables available")
  write_configuration(aeme, model = "glm_aed", path = tmp)

  # an older executable stops with an error rather than writing the file
  aeme <- suppressWarnings(run_aeme(aeme, verbose = FALSE, path = tmp))
  mb <- read_glm_mass_balance(aeme = aeme, path = tmp)
  expect_gt(length(unique(mb$var)), 15)
  expect_setequal(unique(mb$var), vars_req)
  expect_true(all(c("Lake", "Inf", "Out") %in% mb$term))
  expect_false(anyNA(mb$value[mb$term == "Lake"]))

  # the totals can all be reported together
  bal <- diag_glm_mass_balance(aeme = aeme, path = tmp, by = "total",
                               units = "kg")
  expect_true(all(c("TN", "TP", "TOC") %in% bal$daily$name))
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
