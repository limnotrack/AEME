# The summary tier used to be built with dplyr::summarise() and a list-column per
# group, which took over a minute on a realistic ensemble. These tests hold the
# vectorised summarise_ensemble() to that definition.

reference_summary <- function(df, quantiles) {
  q_names <- paste0("q", formatC(round(quantiles * 100), width = 2, flag = "0"))
  df |>
    dplyr::group_by(date, var_sim, depth) |>
    dplyr::summarise(
      mean  = mean(value, na.rm = TRUE),
      sd    = stats::sd(value, na.rm = TRUE),
      n_ens = dplyr::n(),
      q     = list(stats::setNames(
        as.numeric(stats::quantile(value, probs = quantiles, na.rm = TRUE)),
        q_names)),
      .groups = "drop") |>
    tidyr::unnest_wider(q)
}

edge_case_ensemble <- function() {
  set.seed(3)
  d <- data.frame(
    date = rep(as.Date("2021-01-01") + 0:5, each = 40),
    var_sim = rep(c("a", "b"), 120),
    depth = rep(c(1, 2, NA, 5), 60),
    value = round(rnorm(240), 2))
  d$value[sample(240, 40)] <- NA                       # scattered NAs
  rbind(
    d,
    data.frame(date = as.Date("2021-02-01"), var_sim = "z", depth = 1,
               value = c(NA, NA)),                      # all-missing group
    data.frame(date = as.Date("2021-02-02"), var_sim = "z", depth = 1,
               value = 7),                              # a single member
    data.frame(date = as.Date("2021-02-03"), var_sim = "z", depth = 1,
               value = c(5, 5, 5)),                     # ties
    data.frame(date = as.Date("2021-02-04"), var_sim = "z", depth = 1,
               value = c(1, 2)))                        # two members
}

test_that("summarise_ensemble matches the dplyr definition on edge cases", {
  d <- edge_case_ensemble()
  for (qs in list(c(0.05, 0.25, 0.5, 0.75, 0.95), c(0.5), c(0.1, 0.9))) {
    expect_equal(as.data.frame(summarise_ensemble(d, qs)),
                 as.data.frame(reference_summary(d, qs)))
  }
})

test_that("summarise_ensemble handles empty, singleton and all-missing groups", {
  s <- summarise_ensemble(edge_case_ensemble(), c(0.05, 0.5, 0.95))
  s <- as.data.frame(s)

  empty <- s[s$var_sim == "z" & s$date == as.Date("2021-02-01"), ]
  expect_equal(empty$n_ens, 2)
  expect_true(is.nan(empty$mean))
  expect_true(all(is.na(c(empty$sd, empty$q05, empty$q50, empty$q95))))

  one <- s[s$var_sim == "z" & s$date == as.Date("2021-02-02"), ]
  expect_equal(c(one$mean, one$q05, one$q50, one$q95), rep(7, 4))
  expect_true(is.na(one$sd))

  ties <- s[s$var_sim == "z" & s$date == as.Date("2021-02-03"), ]
  expect_equal(c(ties$sd, ties$q05, ties$q95), c(0, 5, 5))
})

test_that("summarise_ensemble keeps the column layout the store relies on", {
  s <- summarise_ensemble(edge_case_ensemble(), c(0.05, 0.5, 0.95))
  expect_named(s, c("date", "var_sim", "depth", "mean", "sd", "n_ens",
                    "q05", "q50", "q95"))
  expect_s3_class(s$date, "Date")
  expect_type(s$n_ens, "integer")
  # sorted as group_by() sorts, NA depth last
  expect_false(is.unsorted(as.numeric(s$date[s$var_sim == "a"])))
})

test_that("summarise_ensemble copes with a logical value column", {
  d <- data.frame(date = as.Date("2021-01-01"), var_sim = "ok", depth = NA_real_,
                  value = c(TRUE, TRUE, FALSE))
  s <- summarise_ensemble(d, 0.5)
  expect_equal(s$mean, 2 / 3)
  expect_equal(s$q50, 1)
})

test_that("summarise_ensemble is fast on a realistic number of groups", {
  skip_on_cran()
  set.seed(5)
  n_groups <- 3e5
  d <- data.frame(date = as.Date("2021-01-01") + (seq_len(n_groups) %% 300),
                  var_sim = "HYD_temp",
                  depth = seq_len(n_groups) / 100,
                  value = rnorm(n_groups))
  d <- d[rep(seq_len(n_groups), each = 2), ]            # two members per group
  d$value <- rnorm(nrow(d))
  elapsed <- system.time(s <- summarise_ensemble(d, c(0.05, 0.5, 0.95)))[["elapsed"]]
  expect_equal(nrow(s), n_groups)
  expect_lt(elapsed, 20)   # the dplyr list-column version needs well over a minute
})

test_that("write_aeme_parquet stores an ensemble that read_aeme_parquet returns", {
  skip_if_not_installed("arrow")
  model <- "glm_aed"
  skip_if_models_unavailable(model)

  tmpdir <- tempfile("parquet_ens_")
  dir.create(tmpdir)
  file.copy(system.file("extdata/lake/", package = "AEME"), tmpdir,
            recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- yaml_to_aeme(path = path, "aeme.yaml")
  aeme <- build_aeme(aeme, model = model, path = path, ext_elev = 5)

  param <- aeme_parameters[aeme_parameters$model == model, ]
  set.seed(1)
  sets <- lapply(1:4, function(i) {
    p <- param
    p$value <- pmin(pmax(p$value * runif(nrow(p), 0.9, 1.1), p$min), p$max)
    p
  })
  aeme <- run_aeme_ensemble(aeme, model = model, path = path, param_sets = sets)
  n_members <- output(aeme)$n_members

  out_root <- file.path(tmpdir, "store")
  res <- write_aeme_parquet(aeme, out_root = out_root,
                            quantiles = c(0.05, 0.5, 0.95))
  expect_length(res$summary_files, 1)
  expect_equal(read_aeme_parquet(out_root, what = "lakes")$n_ens, n_members)

  raw <- read_aeme_parquet(out_root, what = "output_raw", var_sim = "LKE_lvlwtr")
  expect_setequal(unique(raw$ens), seq_len(n_members))

  # The summary tier for a variable with no depth dimension agrees with the
  # same quantities computed by ensemble_summary()
  summ <- read_aeme_parquet(out_root, what = "output_summary",
                            var_sim = "LKE_lvlwtr")
  s <- ensemble_summary(aeme, model, vars_sim = "LKE_lvlwtr",
                        probs = c(0.05, 0.5, 0.95), add_obs = FALSE)$stats

  from_store <- as.data.frame(summ)[, c("date", "mean", "q05", "q50", "q95",
                                        "n_ens")]
  from_summary <- as.data.frame(s)[, c("Date", "mean", "q5", "q50", "q95", "n")]
  names(from_summary) <- c("date", "mean_es", "q05_es", "q50_es", "q95_es",
                           "n_es")
  m <- merge(from_store, from_summary, by = "date")

  expect_gt(nrow(m), 100)
  # the store rounds values to 4 d.p.
  expect_equal(m$mean, m$mean_es, tolerance = 1e-3)
  expect_equal(m$q05, m$q05_es, tolerance = 1e-3)
  expect_equal(m$q50, m$q50_es, tolerance = 1e-3)
  expect_equal(m$q95, m$q95_es, tolerance = 1e-3)
  expect_equal(m$n_ens, m$n_es)
})
