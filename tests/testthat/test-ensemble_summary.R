# Tests for ensemble_summary() and plot_ensemble(), on a small GLM-AED
# ensemble. Parameters are scattered within 10 % of their defaults so every
# member survives and the member count is predictable.

es_cache <- new.env()

es_ensemble <- function(n = 6) {
  if (!is.null(es_cache$res)) return(es_cache$res)

  tmpdir <- tempfile("ens_summary_")
  dir.create(tmpdir)
  file.copy(system.file("extdata/lake/", package = "AEME"), tmpdir,
            recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- yaml_to_aeme(path = path, "aeme.yaml")
  aeme <- build_aeme(aeme, model = "glm_aed", path = path, ext_elev = 5)

  param <- aeme_parameters[aeme_parameters$model == "glm_aed", ]
  set.seed(1)
  sets <- lapply(seq_len(n), function(i) {
    p <- param
    p$value <- pmin(pmax(p$value * runif(nrow(p), 0.9, 1.1), p$min), p$max)
    p
  })
  aeme <- run_aeme_ensemble(aeme, model = "glm_aed", path = path,
                            param_sets = sets)
  es_cache$res <- list(aeme = aeme, n = output(aeme)$n_members)
  es_cache$res
}

test_that("ensemble_summary summarises a depth-resolved variable", {
  skip_if_models_unavailable("glm_aed")
  e <- es_ensemble()

  s <- ensemble_summary(e$aeme, "glm_aed", vars_sim = "HYD_temp",
                        depths = c(1, 5))

  expect_s3_class(s, "aeme_ensemble_summary")
  expect_setequal(unique(s$stats$depth), c(1, 5))
  expect_true(all(c("mean", "sd", "n", "q2.5", "q50", "q97.5") %in%
                    names(s$stats)))
  expect_true(all(s$stats$n <= e$n) && max(s$stats$n) == e$n)
  # bands are ordered
  expect_true(all(s$stats$q2.5 <= s$stats$q50 + 1e-8, na.rm = TRUE))
  expect_true(all(s$stats$q50 <= s$stats$q97.5 + 1e-8, na.rm = TRUE))
  # members retained, one block per realisation
  expect_setequal(unique(s$members$ens), seq_len(e$n))
  expect_setequal(unique(s$members$depth), c(1, 5))

  expect_output(print(s), "aeme_ensemble_summary")
  expect_identical(as.data.frame(s), as.data.frame(s$stats))
})

test_that("ensemble_summary attaches observations as calendar dates", {
  skip_if_models_unavailable("glm_aed")
  e <- es_ensemble()
  s <- ensemble_summary(e$aeme, "glm_aed", vars_sim = "HYD_temp",
                        depths = c(1, 5))
  skip_if(is.null(s$obs), "example lake has no observations in this window")
  expect_s3_class(s$obs$Date, "Date")
  expect_true(all(s$obs$depth %in% c(1, 5)))
})

test_that("ensemble_summary handles a 1-D variable", {
  skip_if_models_unavailable("glm_aed")
  e <- es_ensemble()

  s <- ensemble_summary(e$aeme, "glm_aed", vars_sim = "LKE_lvlwtr")
  expect_true(all(is.na(s$stats$depth)))
  expect_true(all(c("q2.5", "q50", "q97.5") %in% names(s$stats)))

  p <- plot_ensemble(s, var_sim = "LKE_lvlwtr")
  expect_true(ggplot2::is_ggplot(p))
})

test_that("ensemble_summary errors without ensemble output", {
  aeme <- readRDS(system.file("extdata/aeme.rds", package = "AEME"))
  expect_error(ensemble_summary(aeme, "glm_aed"), "run_aeme_ensemble")
})

test_that("plot_ensemble renders from a summary without re-extracting", {
  skip_if_models_unavailable("glm_aed")
  e <- es_ensemble()
  s <- ensemble_summary(e$aeme, "glm_aed", vars_sim = "HYD_temp",
                        depths = c(1, 5))

  expect_true(ggplot2::is_ggplot(
    plot_ensemble(s, model = "glm_aed", var_sim = "HYD_temp", depth = 5)))
  expect_true(ggplot2::is_ggplot(
    plot_ensemble(s, var_sim = "HYD_temp", depth = 1, type = "line")))
})

test_that("plot_ensemble(aeme) and plot_ensemble(summary) agree", {
  skip_if_models_unavailable("glm_aed")
  e <- es_ensemble()

  from_aeme <- plot_ensemble(e$aeme, model = "glm_aed", var_sim = "HYD_temp",
                             depth = 2)
  s <- ensemble_summary(e$aeme, "glm_aed", vars_sim = "HYD_temp", depths = 2,
                        probs = c(0.025, 0.5, 0.975))
  from_summary <- plot_ensemble(s, model = "glm_aed", var_sim = "HYD_temp",
                                depth = 2)

  expect_equal(from_aeme$data$.mid, from_summary$data$.mid)
  expect_equal(from_aeme$data$.lower, from_summary$data$.lower)
})

test_that("a summary without the requested interval errors informatively", {
  skip_if_models_unavailable("glm_aed")
  e <- es_ensemble()
  s <- ensemble_summary(e$aeme, "glm_aed", vars_sim = "HYD_temp",
                        depths = 3, probs = c(0.25, 0.75))

  expect_error(plot_ensemble(s, var_sim = "HYD_temp", depth = 3),
               "does not carry|matching")
})

test_that("keep_members = FALSE drops the frame and blocks type = line", {
  skip_if_models_unavailable("glm_aed")
  e <- es_ensemble()
  s <- ensemble_summary(e$aeme, "glm_aed", vars_sim = "HYD_temp", depths = 4,
                        keep_members = FALSE)
  expect_null(s$members)
  expect_error(plot_ensemble(s, var_sim = "HYD_temp", depth = 4,
                             type = "line"), "per-member")
})

test_that("obs_calendar_date reduces timestamps to UTC dates", {
  df <- data.frame(datetime = as.POSIXct("2021-01-02 23:30:00", tz = "UTC"))
  out <- obs_calendar_date(df)
  expect_s3_class(out$Date, "Date")
  expect_equal(out$Date, as.Date("2021-01-02"))

  # A Date column is authoritative and taken as-is
  df2 <- data.frame(Date = as.POSIXct("2021-01-02 23:30:00", tz = "UTC"),
                    datetime = as.POSIXct("2021-01-05", tz = "UTC"))
  expect_equal(obs_calendar_date(df2)$Date, as.Date("2021-01-02"))

  expect_null(obs_calendar_date(NULL))
  expect_identical(obs_calendar_date(data.frame(x = 1)), data.frame(x = 1))
})

test_that("score_ensemble scores a real ensemble from a summary or an Aeme", {
  skip_if_models_unavailable("glm_aed")
  e <- es_ensemble()
  depths <- c(1, 5)

  s <- ensemble_summary(e$aeme, "glm_aed", vars_sim = "HYD_temp",
                        depths = depths)
  skip_if(is.null(s$obs), "example lake has no observations in this window")
  sc <- score_ensemble(s)

  expect_setequal(sc$scope, c("depth", "overall"))
  expect_true(all(sc$depth[sc$scope == "depth"] %in% depths))
  expect_true(all(sc$coverage >= 0 & sc$coverage <= 1))
  expect_true(all(sc$rmse >= 0 & sc$crps >= 0 & sc$spread >= 0))
  # the pooled row counts every depth's observations
  expect_equal(sc$n_obs[sc$scope == "overall"],
               sum(sc$n_obs[sc$scope == "depth"]))

  # straight from the Aeme object, arguments passed to ensemble_summary()
  sc2 <- score_ensemble(e$aeme, model = "glm_aed", vars_sim = "HYD_temp",
                        depths = depths)
  expect_equal(sc2, sc)
})
