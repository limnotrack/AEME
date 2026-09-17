test_that(".check_perturb validates names and values", {
  mc <- get_model_controls()
  expect_error(AEME:::.check_perturb(list(1, 2), mc),
               class = "aeme_error_spin_up")
  expect_error(AEME:::.check_perturb(list(foo = 1), mc),
               class = "aeme_error_spin_up")
  expect_error(AEME:::.check_perturb(list(temperature = "x"), mc),
               class = "aeme_error_spin_up")
  ok <- AEME:::.check_perturb(list(temperature = c(-2, 0, 2), CHM_oxy = c(0.5, 2)),
                              mc)
  expect_named(ok, c("temperature", "CHM_oxy"))
})

test_that(".member_ic applies additive temp offsets and multiplicative wq factors", {
  mc <- get_model_controls()
  base <- list(
    profile = data.frame(depth = c(0, 5, 10), temperature = c(18, 14, 11),
                         salt = c(0, 0, 0)),
    wc = stats::setNames(mc$initial_wc, mc$var_aeme)
  )
  perturb <- list(temperature = c(-3, 0, 3), HYD_temp = c(1, 1, 2))
  # replace HYD_temp with a genuine wq var present in mc
  perturb <- list(temperature = c(-3, 0, 3))

  m1 <- AEME:::.member_ic(base, lapply(perturb, rep_len, 3), 1)
  expect_equal(m1$profile$temperature, c(15, 11, 8))
  m3 <- AEME:::.member_ic(base, lapply(perturb, rep_len, 3), 3)
  expect_equal(m3$profile$temperature, c(21, 17, 14))
})

test_that(".summarise_spin_up and .recommend_spin_up behave", {
  d <- expand.grid(spin_up = c(0, 30, 90), member = 1:3, depth = c(0, 5),
                   var = "HYD_temp", KEEP.OUT.ATTRS = FALSE,
                   stringsAsFactors = FALSE)
  set.seed(42)
  d$value <- 15 + rnorm(nrow(d), 0, pmax(1e-3, 1 - d$spin_up / 60))

  s <- AEME:::.summarise_spin_up(d)
  expect_true(all(c("spin_up", "var", "spread", "drift",
                    "spread_cv", "drift_cv") %in% names(s)))
  # spread should decrease as spin-up increases
  expect_true(s$spread[s$spin_up == 0] > s$spread[s$spin_up == 90])
  # CV is spread / mean (mean ~ 15 here)
  expect_equal(s$spread_cv, s$spread / 15, tolerance = 0.05)

  rec <- AEME:::.recommend_spin_up(s, 0.5, "HYD_temp")
  expect_named(rec, c("HYD_temp", "overall"))
  expect_true(rec[["overall"]] %in% c(30, 90))

  # metric switch targets a different column
  rec_cv <- AEME:::.recommend_spin_up(s, 0.001, "HYD_temp", metric = "spread_cv")
  expect_named(rec_cv, c("HYD_temp", "overall"))
})

test_that(".summarise_spin_up sets CV to NA where the ensemble mean crosses zero", {
  d <- expand.grid(spin_up = c(0, 30), member = 1:3, depth = 0,
                   var = "X", KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  d$value <- c(-1, 0, 1, -1, 0, 1)          # mean 0 at every (spin_up, depth)
  s <- AEME:::.summarise_spin_up(d)
  expect_true(all(is.na(s$spread_cv)))
  expect_false(anyNA(s$spread))             # raw spread still fine
})

test_that("assess_spin_up runs an ensemble and recommends a spin-up (GLM)", {
  skip_if_models_unavailable(c("glm_aed"))
  td <- withr::local_tempdir()
  file.copy(system.file("extdata/lake/", package = "AEME"), td, recursive = TRUE)
  path <- file.path(td, "lake")
  aeme <- suppressWarnings(yaml_to_aeme(path = path, "aeme.yaml"))
  aeme <- set_initial_conditions(
    aeme, profile = data.frame(depth = c(0, 13), temperature = c(16, 12))
  )

  res <- assess_spin_up(
    aeme, model = "glm_aed", spin_up = c(0, 60),
    perturb = list(temperature = c(-3, 3)), vars = "HYD_temp",
    path = td, tolerance = 1, build_args = list(ext_elev = 5)
  )

  skip_if(length(res$failures) > 0, "model runs failed in this environment")
  expect_s3_class(res, "aeme_spin_up")
  expect_true(all(c("spread", "spread_cv", "drift", "drift_cv") %in%
                    names(res$summary)))
  expect_setequal(unique(res$data$spin_up), c(0, 60))
  # the perturbation memory should fade with spin-up
  sp0 <- res$summary$spread[res$summary$spin_up == 0]
  sp60 <- res$summary$spread[res$summary$spin_up == 60]
  expect_true(sp60 < sp0)
  expect_equal(unname(res$recommended[["overall"]]), 60)
  expect_s3_class(plot_spin_up(res), "ggplot")
})
