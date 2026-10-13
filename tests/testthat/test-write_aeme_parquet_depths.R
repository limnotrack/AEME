# Fixed-depth summaries in write_aeme_parquet(): the stored depth grid is scaled
# to each member's water level, so members only line up at fixed depths.

# Three members, two dates. Every profile is linear in depth, value = 2 * depth +
# member, so the right answer at any depth is known. Member levels differ, as
# they do in an ensemble with different inflow/outflow parameters.
linear_ensemble <- function() {
  dates <- as.Date("2021-01-01") + 0:1
  levels <- c(10.5, 11, 11.5)
  grid <- seq(0.1, 9.9, length.out = 50)
  prof <- do.call(rbind, lapply(1:3, function(m) {
    do.call(rbind, lapply(dates, function(d) {
      data.frame(date = d, var_sim = "HYD_temp", depth = grid * levels[m] / 10,
                 value = 2 * grid * levels[m] / 10 + m, ens = m)
    }))
  }))
  scal <- do.call(rbind, lapply(1:3, function(m) {
    rbind(
      data.frame(date = dates, var_sim = "LKE_lvlwtr", depth = NA_real_,
                 value = levels[m], ens = m),
      data.frame(date = dates, var_sim = "HYD_thmcln", depth = NA_real_,
                 value = 5, ens = m))
  }))
  list(df = rbind(prof, scal), levels = levels, dates = dates)
}

test_that("interpolate_to_depths gives the linear profile at surface depths", {
  e <- linear_ensemble()
  out <- interpolate_to_depths(e$df, depths = c(1.5, 4), depth_ref = "surface")
  hit <- out[out$var_sim == "HYD_temp", ]
  expect_setequal(unique(hit$depth), c(1.5, 4))
  expect_equal(nrow(hit), 3 * 2 * 2)                 # members x dates x depths
  expect_equal(hit$value, 2 * hit$depth + hit$ens, tolerance = 1e-4)
})

test_that("bottom-referenced depths are measured up from the lake bed", {
  e <- linear_ensemble()
  out <- interpolate_to_depths(e$df, depths = 1, depth_ref = "bottom")
  hit <- out[out$var_sim == "HYD_temp", ]
  expect_equal(nrow(hit), 6)
  # 1 m above the bed is (level - 1) m below the surface, and the level differs
  # between members
  expect_equal(hit$value, 2 * (e$levels[hit$ens] - 1) + hit$ens, tolerance = 1e-4)
  surf <- interpolate_to_depths(e$df, depths = 1, depth_ref = "surface")
  expect_false(isTRUE(all.equal(sort(surf$value[surf$var_sim == "HYD_temp"]),
                                sort(hit$value))))
})

test_that("depths outside the water column give no value for that member", {
  e <- linear_ensemble()
  # 11.2 m below the surface is below the bed of members 1 (10.5 m) and
  # 2 (11 m) but inside member 3's water column (11.5 m) and its grid, which
  # ends at 9.9 * 11.5 / 10 = 11.385 m, so only member 3 has a value
  out <- interpolate_to_depths(e$df, depths = 11.2, depth_ref = "surface")
  hit <- out[out$var_sim == "HYD_temp", ]
  expect_equal(unique(hit$ens), 3)
  expect_equal(hit$value, rep(2 * 11.2 + 3, 2), tolerance = 1e-4)

  # between the end of member 3's grid (11.385 m) and its bed (11.5 m) the
  # deepest grid value is used, as interp_static_grid() does
  out <- interpolate_to_depths(e$df, depths = 11.45, depth_ref = "surface")
  hit <- out[out$var_sim == "HYD_temp", ]
  expect_equal(unique(hit$ens), 3)
  expect_equal(hit$value, rep(2 * 11.385 + 3, 2), tolerance = 1e-4)

  # 12 m above the bed is above the surface of every member
  none <- interpolate_to_depths(e$df, depths = 12, depth_ref = "bottom")
  expect_equal(nrow(none[none$var_sim == "HYD_temp", ]), 0)
})

test_that("interpolate_to_depths keeps scalar variables and drops empty rows", {
  e <- linear_ensemble()
  out <- interpolate_to_depths(e$df, depths = 2)
  expect_equal(sum(out$var_sim == "HYD_thmcln"), 6)
  expect_true(all(is.na(out$depth[out$var_sim == "HYD_thmcln"])))
  expect_false(anyNA(out$value))

  # a profile with a single value cannot be interpolated
  one <- rbind(e$df, data.frame(date = e$dates[1], var_sim = "CHM_oxy",
                                depth = 3, value = 9, ens = 1))
  res <- interpolate_to_depths(one, 2)
  expect_equal(nrow(res[res$var_sim == "CHM_oxy", ]), 0)
})

test_that("interpolate_to_depths does not depend on row order", {
  e <- linear_ensemble()
  set.seed(1)
  shuffled <- e$df[sample(nrow(e$df)), ]
  a <- interpolate_to_depths(e$df, depths = c(1, 3))
  b <- interpolate_to_depths(shuffled, depths = c(1, 3))
  key <- function(x) x[order(x$ens, x$date, x$var_sim, x$depth), ]
  expect_equal(key(a), key(b), ignore_attr = TRUE)
})

test_that("interpolate_to_depths agrees with approx() on random profiles", {
  set.seed(9)
  n_prof <- 200
  df <- do.call(rbind, lapply(seq_len(n_prof), function(p) {
    k <- sample(3:30, 1)
    d <- sort(runif(k, 0.05, 12))
    data.frame(date = as.Date("2021-01-01") + (p %% 20), var_sim = "HYD_temp",
               depth = round(d, 2), value = round(rnorm(k, 15, 3), 4),
               ens = p %/% 20 + 1)
  }))
  df <- df[!duplicated(df[, c("date", "ens", "depth")]), ]
  # one level per member-date, deeper than every grid depth
  lv <- unique(df[, c("date", "ens")])
  df <- rbind(df, data.frame(date = lv$date, var_sim = "LKE_lvlwtr",
                             depth = NA_real_, value = 13, ens = lv$ens))
  targets <- c(0.5, 2, 6.25, 9)
  out <- interpolate_to_depths(df, depths = targets)
  out <- out[out$var_sim == "HYD_temp", ]

  prof <- df[df$var_sim == "HYD_temp", ]
  expected <- do.call(rbind, lapply(
    split(prof, list(prof$ens, prof$date), drop = TRUE),
    function(p) {
      if (nrow(p) < 2) return(NULL)
      data.frame(ens = p$ens[1], date = p$date[1], depth = targets,
                 value = round(stats::approx(p$depth, p$value, xout = targets,
                                             rule = 2)$y, 4))
    }))
  key <- function(x) x[order(x$ens, x$date, x$depth),
                       c("ens", "date", "depth", "value")]
  expect_equal(key(out), key(expected), ignore_attr = TRUE, tolerance = 1e-6)
})

test_that("bottom-referenced interpolation needs the water level", {
  e <- linear_ensemble()
  no_level <- e$df[e$df$var_sim != "LKE_lvlwtr", ]
  expect_error(interpolate_to_depths(no_level, 1, "bottom"), "water level")
  expect_no_error(interpolate_to_depths(no_level, 1, "surface"))
})

test_that("write_aeme_parquet checks its depth arguments up front", {
  skip_if_not_installed("arrow")
  aeme <- readRDS(system.file("extdata/aeme.rds", package = "AEME"))
  out_root <- tempfile("pq_args_")
  expect_error(write_aeme_parquet(aeme, out_root, depths = -1), "non-negative")
  expect_error(write_aeme_parquet(aeme, out_root, depths = NA_real_),
               "non-negative")
  expect_error(write_aeme_parquet(aeme, out_root, depth_ref = "bottom"),
               "needs.*depths")
  expect_error(write_aeme_parquet(aeme, out_root, depths = 1,
                                  depth_ref = "middle"))
})

test_that("write_aeme_parquet summarises depth-resolved variables at fixed depths", {
  skip_if_not_installed("arrow")
  model <- "glm_aed"
  skip_if_models_unavailable(model)

  tmpdir <- tempfile("parquet_depths_")
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

  # default: stored depths, labelled as below the surface
  root0 <- file.path(tmpdir, "store_default")
  write_aeme_parquet(aeme, root0)
  s0 <- as.data.frame(read_aeme_parquet(
    root0, var_sim = c("HYD_temp", "LKE_lvlwtr")))
  expect_true("depth_ref" %in% names(s0))
  expect_true(all(s0$depth_ref[!is.na(s0$depth)] == "surface"))
  expect_true(all(is.na(s0$depth_ref[is.na(s0$depth)])))

  # fixed depths below the surface: members line up
  root1 <- file.path(tmpdir, "store_surface")
  write_aeme_parquet(aeme, root1, depths = c(1, 5))
  s1 <- as.data.frame(read_aeme_parquet(root1, var_sim = "HYD_temp"))
  expect_setequal(unique(s1$depth), c(1, 5))
  expect_true(all(s1$depth_ref == "surface"))
  expect_equal(max(s1$n_ens), n_members)
  expect_gt(mean(s1$n_ens == n_members), 0.95)

  # ... and it agrees with ensemble_summary(), which interpolates via get_var()
  es <- as.data.frame(ensemble_summary(aeme, model, vars_sim = "HYD_temp",
                                       depths = c(1, 5), add_obs = FALSE)$stats)
  m <- merge(s1[, c("date", "depth", "mean", "n_ens")],
             es[, c("Date", "depth", "mean", "n")],
             by.x = c("date", "depth"), by.y = c("Date", "depth"),
             suffixes = c("", "_es"))
  expect_gt(nrow(m), 500)
  expect_equal(m$mean, m$mean_es, tolerance = 1e-3)
  expect_equal(m$n_ens, m$n)

  # fixed height above the lake bed
  root2 <- file.path(tmpdir, "store_bottom")
  write_aeme_parquet(aeme, root2, depths = 1, depth_ref = "bottom")
  s2 <- as.data.frame(read_aeme_parquet(root2, var_sim = "HYD_temp"))
  expect_true(all(s2$depth == 1))
  expect_true(all(s2$depth_ref == "bottom"))
  expect_equal(max(s2$n_ens), n_members)

  # check one date by hand: each member's profile at (its level - 1) m
  d0 <- s2$date[round(nrow(s2) / 2)]
  by_hand <- vapply(seq_len(n_members), function(i) {
    prof <- get_var(aeme, model, "HYD_temp", ens_n = i)
    prof <- prof[prof$Date == d0 & !is.na(prof$value), ]
    lvl <- get_var(aeme, model, "LKE_lvlwtr", ens_n = i)
    lvl <- lvl$value[lvl$Date == d0]
    stats::approx(prof$depth, prof$value, xout = lvl - 1, rule = 2)$y
  }, numeric(1))
  expect_equal(s2$mean[s2$date == d0], mean(by_hand), tolerance = 1e-3)
})
