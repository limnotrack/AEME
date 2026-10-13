# ---- registry and name resolution (no model needed) -------------------------

test_that("glm_extra_vars registry is internally consistent", {
  data("glm_extra_vars", package = "AEME", envir = environment())
  data("key_naming", package = "AEME", envir = environment())
  expect_false(anyDuplicated(glm_extra_vars$var_aeme) > 0)
  expect_false(anyDuplicated(glm_extra_vars$glm_aed) > 0)
  expect_true(all(glm_extra_vars$group %in% c("core", "sediment", "diag")))
  # never shadows a regular AEME variable
  expect_false(any(glm_extra_vars$var_aeme %in% key_naming$var_aeme))
  expect_true(all(c("SED_temp", "SED_hflux", "SED_Fsed_oxy", "SEDL_Fsed_oxy") %in%
                    glm_extra_vars$var_aeme))
})

test_that(".resolve_glm_extra_vars() maps AEME names, raw names and keywords", {
  r <- AEME:::.resolve_glm_extra_vars(c("SED_temp", "sed_heatflux",
                                        "SED_Fsed_oxy_lake", "sediment"))
  expect_equal(r$raw, c("sed_temp", "sed_heatflux", "SDF_Fsed_oxy_Z_lake"))
  expect_equal(r$groups, "sediment")
  expect_false(r$none)
  expect_true(AEME:::.resolve_glm_extra_vars("none")$none)
  # unknown names pass through unchanged
  expect_equal(AEME:::.resolve_glm_extra_vars("not_a_var")$raw, "not_a_var")
  expect_equal(AEME:::.glm_extra_aeme_name(c("sed_temp", "SDF_Fsed_oxy_Z_lake",
                                             "nope")),
               c("SED_temp", "SED_Fsed_oxy_lake", NA))
})

test_that(".get_output_var() finds extras by AEME or raw name, in any group", {
  gv <- new_grouped_var(matrix(1:6, 2), c("nzones", "time"),
                        list(nzones = 1:2, time = 1:3))
  out <- list(HYD_temp = matrix(1:4, 2), SDF_Fsed_oxy = 1:3,
              sediment = list(sed_temp = gv), diag = list(x = 1))
  expect_identical(AEME:::.get_output_var(out, "sed_temp"), gv)
  expect_identical(AEME:::.get_output_var(out, "SED_temp"), gv)
  expect_identical(AEME:::.get_output_var(out, "SEDL_Fsed_oxy"), 1:3)
  expect_identical(AEME:::.get_output_var(out, "HYD_temp"), out$HYD_temp)
  expect_null(AEME:::.get_output_var(out, "SED_hflux"))
  expect_true(AEME:::is_var_present(out, "SED_temp"))
  expect_false(AEME:::is_var_present(out, "SED_hflux"))
})

test_that("check_aeme_vars() accepts registry names", {
  expect_equal(check_aeme_vars("SED_temp"), "SED_temp")
  expect_equal(check_aeme_vars("SED_Fsed_oxy_lake"), "SED_Fsed_oxy_lake")
})

# ---- real GLM-AED run -------------------------------------------------------

.glm_extra_fixture <- local({
  fx <- NULL
  function() {
    if (is.null(fx)) {
      skip_if_no_glm()
      aeme <- readRDS(system.file("extdata/aeme.rds", package = "AEME"))
      path <- tempfile("glm_extra_")
      dir.create(path)
      mc <- get_model_controls(use_bgc = TRUE)
      aeme <- build_aeme(path = path, aeme = aeme, model = "glm_aed",
                         model_controls = mc, ext_elev = 5, use_bgc = TRUE)
      fx <<- list(aeme = aeme, path = path, mc = mc)
    }
    fx
  }
})

.run_extra <- function(fx, ...) {
  run_aeme(fx$aeme, model = "glm_aed", path = fx$path,
           model_controls = fx$mc, ...)
}

test_that("extra_vars selects what is read; core output is untouched", {
  fx <- .glm_extra_fixture()
  a_all <- .run_extra(fx)
  a_sel <- .run_extra(fx, extra_vars = c("SED_Fsed_oxy", "zarea"))
  a_non <- .run_extra(fx, extra_vars = "none")

  g_all <- output(a_all)$ens_001$glm_aed
  g_sel <- output(a_sel)$ens_001$glm_aed
  g_non <- output(a_non)$ens_001$glm_aed

  expect_setequal(names(g_sel$sediment), c("SDF_Fsed_oxy_Z", "zarea"))
  expect_null(g_non$sediment)
  expect_gt(length(g_all$sediment), length(g_sel$sediment))
  expect_lt(as.numeric(object.size(g_sel)), as.numeric(object.size(g_all)))
  expect_identical(g_sel$HYD_temp, g_all$HYD_temp)
  expect_identical(g_non$HYD_temp, g_all$HYD_temp)

  # keep_diag = FALSE still drops both sub-lists
  a_nd <- .run_extra(fx, keep_diag = FALSE)
  expect_null(output(a_nd)$ens_001$glm_aed$sediment)
})

test_that("sediment group keyword, sed_layers and unknown names behave", {
  fx <- .glm_extra_fixture()
  a <- .run_extra(fx, extra_vars = "sediment")
  g <- output(a)$ens_001$glm_aed
  expect_gt(length(g$sediment), 5)
  expect_false("PHY_cyano_fT" %in% names(g))   # core extras not requested

  skip_if_not("sed_temp" %in% names(g$sediment),
              "this GLM configuration does not write sed_temp")
  a2 <- .run_extra(fx, extra_vars = "SED_temp", sed_layers = 1:2)
  st <- output(a2)$ens_001$glm_aed$sediment$sed_temp
  expect_equal(dim(st$value)[1], 2L)
  expect_equal(st$dim_values$sed_layers, 1:2)

  expect_warning(.run_extra(fx, extra_vars = "no_such_var"),
                 "no_such_var")
})

test_that("zone_summary gives the zone-area weighted lake-wide mean", {
  fx <- .glm_extra_fixture()
  a <- .run_extra(fx, extra_vars = c("SED_Fsed_oxy", "zarea"),
                  zone_summary = TRUE)
  s <- output(a)$ens_001$glm_aed$sediment
  expect_true("SDF_Fsed_oxy_Z_lake" %in% names(s))

  v <- s$SDF_Fsed_oxy_Z$value
  w <- s$zarea$value
  ok <- !is.na(w) & !is.na(v)
  expected <- colSums(ifelse(ok, v * w, 0)) / colSums(ifelse(ok, w, 0))
  expect_equal(as.vector(s$SDF_Fsed_oxy_Z_lake), expected)

  # asking for just the `_lake` series drops the zone array behind it
  a2 <- .run_extra(fx, extra_vars = "SED_Fsed_oxy_lake")
  s2 <- output(a2)$ens_001$glm_aed$sediment
  expect_equal(names(s2), "SDF_Fsed_oxy_Z_lake")
  expect_equal(as.vector(s2$SDF_Fsed_oxy_Z_lake), expected)
})

test_that("get_var() / check_var_in_output() work with AEME names and filters", {
  fx <- .glm_extra_fixture()
  a <- .run_extra(fx, extra_vars = c("SED_Fsed_oxy", "zarea"),
                  zone_summary = TRUE)

  by_aeme <- get_var(a, "glm_aed", "SED_Fsed_oxy")
  by_raw  <- get_var(a, "glm_aed", "SDF_Fsed_oxy_Z")
  expect_equal(by_aeme$value, by_raw$value)
  expect_true(all(c("nzones", "Date", "value", "var_sim") %in% names(by_aeme)))

  z1 <- get_var(a, "glm_aed", "SED_Fsed_oxy", zone = 1)
  expect_true(all(z1$nzones == 1))
  expect_lt(nrow(z1), nrow(by_aeme))

  lake_df <- get_var(a, "glm_aed", "SED_Fsed_oxy_lake")
  expect_gt(nrow(lake_df), 0)

  expect_true(check_var_in_output(a, "glm_aed", "SED_Fsed_oxy")$all_present)
  labs <- get_output_vars(a, "glm_aed", group = "sediment")
  expect_true(any(grepl("by zone", names(labs))))
  expect_true(any(grepl("lake-wide mean", names(labs))))
})

test_that("run_aeme_ensemble() forwards extra_vars to every member", {
  fx <- .glm_extra_fixture()
  a <- .run_extra(fx, extra_vars = "none")
  data("aeme_parameters", package = "AEME", envir = environment())
  p <- aeme_parameters[aeme_parameters$model == "glm_aed", ]
  sets <- lapply(c(1, 1.02), function(f) { q <- p; q$value <- q$value * f; q })
  ens <- run_aeme_ensemble(a, model = "glm_aed", path = fx$path,
                           param_sets = sets, extra_vars = "zarea")
  expect_equal(output(ens)$n_members, 2)
  for (m in c("ens_001", "ens_002")) {
    expect_equal(names(output(ens)[[m]]$glm_aed$sediment), "zarea")
  }
})
