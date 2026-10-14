# Synthetic summaries with hand-computed scores, so no model run is needed.

fake_summary <- function(members, obs, vars_2d = "HYD_temp") {
  if (is.data.frame(members)) members$Model <- "GLM-AED"
  structure(list(stats = NULL, members = members, obs = obs),
            class = "aeme_ensemble_summary", vars_2d = vars_2d,
            n_members = 3L)
}

d1 <- as.Date("2021-01-01")
d2 <- as.Date("2021-01-02")

mem <- function(date, depth, vals, var = "HYD_temp") {
  data.frame(Date = date, depth = depth, value = vals, var_sim = var,
             ens = seq_along(vals))
}
ob <- function(date, depth, value, var = "HYD_temp") {
  data.frame(Date = date, depth = depth, value = value, var_sim = var)
}

test_that("crps_ensemble matches hand calculation", {
  # E|X-y| = 2/3, 0.5 * E|X-X'| = 0.5 * 8/9
  expect_equal(crps_ensemble(c(1, 2, 3), 2), 2 / 9)
  expect_equal(crps_ensemble(c(1, 2, 3), 10), 8 - 4 / 9)
  # a single member is the absolute error
  expect_equal(crps_ensemble(3, 5), 2)
  # order does not matter
  expect_equal(crps_ensemble(c(3, 1, 2), 2), crps_ensemble(c(1, 2, 3), 2))
})

test_that("score_ensemble gives the expected scores per depth and overall", {
  s <- fake_summary(
    members = rbind(mem(d1, 1, c(1, 2, 3)), mem(d2, 1, c(1, 2, 3)),
                    mem(d1, 5, c(10, 12))),
    obs = rbind(ob(d1, 1, 2), ob(d2, 1, 10), ob(d1, 5, 10),
                ob(as.Date("2021-06-01"), 1, 99))   # no members: ignored
  )
  sc <- score_ensemble(s)

  expect_named(sc, c("Model", "var_sim", "depth", "scope", "n_obs", "coverage",
                     "bias", "rmse", "spread", "spread_skill", "crps"))
  expect_equal(attr(sc, "conf_int"), 0.95)
  expect_equal(sc$scope, c("depth", "depth", "overall"))
  expect_equal(sc$depth, c(1, 5, NA))

  d_1 <- sc[sc$depth %in% 1, ]
  expect_equal(d_1$n_obs, 2)
  expect_equal(d_1$coverage, 0.5)
  expect_equal(d_1$bias, -4)
  expect_equal(d_1$rmse, sqrt(32))
  expect_equal(d_1$spread, 1)
  expect_equal(d_1$spread_skill, 1 / sqrt(32))
  expect_equal(d_1$crps, mean(c(2 / 9, 68 / 9)))

  d_5 <- sc[sc$depth %in% 5, ]
  expect_equal(c(d_5$n_obs, d_5$coverage, d_5$bias, d_5$rmse), c(1, 0, 1, 1))
  expect_equal(d_5$spread, sqrt(2))
  expect_equal(d_5$crps, 0.5)

  all <- sc[sc$scope == "overall", ]
  expect_equal(all$n_obs, 3)
  expect_equal(all$coverage, 1 / 3)
  expect_equal(all$bias, mean(c(0, -8, 1)))
  expect_equal(all$rmse, sqrt(mean(c(0, 64, 1))))
  expect_equal(all$spread, sqrt(mean(c(1, 1, 2))))
  expect_equal(all$crps, mean(c(2 / 9, 68 / 9, 0.5)))
})

test_that("a perfect, well-spread ensemble covers its observations", {
  set.seed(1)
  dates <- d1 + 0:999
  members <- do.call(rbind, lapply(dates, function(d) {
    mem(d, 1, rnorm(30, 10, 1))
  }))
  truth <- rnorm(1000, 10, 1)           # drawn from the same distribution
  sc <- score_ensemble(fake_summary(members, ob(dates, 1, truth)),
                       conf_int = 0.9)
  overall <- sc[sc$scope == "overall", ]
  expect_equal(attr(sc, "conf_int"), 0.9)
  # an n-member ensemble covers about (n - 1) * conf / (n + 1) even when it
  # is perfectly reliable, because the interval is from empirical quantiles
  expect_true(abs(overall$coverage - 29 * 0.9 / 31) < 0.03)
  expect_true(abs(overall$spread_skill - 1) < 0.08)
})

test_that("a variable without a depth dimension has only the pooled row", {
  s <- fake_summary(
    members = rbind(mem(d1, NA_real_, c(1, 2, 3), "LKE_lvlwtr"),
                    mem(d2, NA_real_, c(1, 2, 3), "LKE_lvlwtr")),
    obs = rbind(ob(d1, NA_real_, 2, "LKE_lvlwtr"),
                ob(d2, NA_real_, 3, "LKE_lvlwtr")),
    vars_2d = character(0))
  sc <- score_ensemble(s)
  expect_equal(nrow(sc), 1)
  expect_equal(sc$scope, "overall")
  expect_true(is.na(sc$depth))
  expect_equal(sc$n_obs, 2)
})

test_that("missing member values and single-member dates are ignored", {
  members <- rbind(mem(d1, 1, c(1, 2, 3)),
                   mem(d2, 1, c(1, NA, NA)))       # only one usable member
  sc <- score_ensemble(fake_summary(members, rbind(ob(d1, 1, 2),
                                                   ob(d2, 1, 2))))
  expect_equal(sc$n_obs[sc$scope == "overall"], 1)
})

test_that("score_ensemble explains what is missing", {
  members <- mem(d1, 1, c(1, 2, 3))
  no_obs <- fake_summary(members, NULL)
  expect_error(score_ensemble(no_obs), "observations")

  no_members <- fake_summary(NULL, ob(d1, 1, 2))
  expect_error(score_ensemble(no_members), "keep_members")

  pooled <- fake_summary(mem(d1, NA_real_, c(1, 2, 3)), ob(d1, NA_real_, 2))
  expect_error(score_ensemble(pooled), "depths")

  disjoint <- fake_summary(members, ob(d2, 1, 2))
  expect_error(score_ensemble(disjoint), "coincides")

  ok <- fake_summary(members, ob(d1, 1, 2))
  expect_error(score_ensemble(ok, conf_int = 1), "conf_int")
  expect_error(score_ensemble(ok, conf_int = c(0.5, 0.9)), "conf_int")
})
