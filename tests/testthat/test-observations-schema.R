test_that("get_obs_column_names() returns the current schema", {
  expect_identical(get_obs_column_names(),
                   c("Date", "var_aeme", "depth", "value"))
  expect_identical(get_obs_column_names(include_optional = TRUE),
                   c("Date", "var_aeme", "depth", "value", "depth_to", "sd"))
})

test_that("add_obs() accepts the legacy depth_from / depth_to layout", {
  # The deprecation warning is emitted once per session; reset so it fires here
  # regardless of what other tests have run first.
  try(rlang::reset_warning_verbosity("aeme_warn_obs_legacy_depth"), silent = TRUE)

  legacy <- data.frame(
    Date = as.Date(c("2020-01-01", "2020-01-01", "2020-02-01")),
    var_aeme = "HYD_temp",
    depth_from = c(0, 4, 0),
    depth_to = c(0, 4, 0),
    value = c(15, 10, 14)
  )

  aeme <- new_aeme()
  expect_warning(
    aeme <- add_obs(aeme, lake = legacy),
    class = "aeme_warn_obs_legacy_depth"
  )

  lk <- observations(aeme)$lake
  expect_true("depth" %in% names(lk))
  expect_false(any(c("depth_from", "depth_to") %in% names(lk)))
  expect_setequal(lk$depth, c(0, 4, 0))
})

test_that("legacy interval samples keep depth_to as the interval bottom", {
  legacy <- data.frame(
    Date = as.Date("2020-01-01"),
    var_aeme = "PHY_tchla",
    depth_from = c(0, 0),
    depth_to = c(0, 5),
    value = c(2, 3)
  )

  out <- suppressWarnings(normalise_lake_obs(legacy))
  expect_equal(out$depth, c(0, 2.5))
  expect_true("depth_to" %in% names(out))
  expect_equal(out$depth_to, c(NA_real_, 5))
})

test_that("normalise_lake_obs() is idempotent on the current schema", {
  df <- data.frame(
    Date = as.Date("2020-01-01"), var_aeme = "HYD_temp",
    depth = 1.5, value = 12, sd = 0.3
  )
  expect_identical(normalise_lake_obs(df), df)
  expect_null(normalise_lake_obs(NULL))
})

test_that("optional depth_to and sd survive an add_obs() / get_obs() round trip", {
  lake <- data.frame(
    Date = as.Date(c("2020-01-01", "2020-01-01")),
    var_aeme = "HYD_temp",
    depth = c(0, 5),
    value = c(18, 11),
    depth_to = c(NA_real_, 8),
    sd = c(0.5, 0.5)
  )

  aeme <- add_obs(new_aeme(), lake = lake)
  got <- get_obs(aeme, var_sim = "HYD_temp")

  expect_true(all(c("depth", "depth_to", "sd") %in% names(got)))
  expect_equal(sort(got$depth), c(0, 5))
  expect_equal(got$sd, c(0.5, 0.5))
})

test_that("migrate_aeme() collapses legacy lake observations on load", {
  aeme <- new_aeme()
  obs <- observations(aeme)
  obs$lake <- data.frame(
    Date = as.Date("2020-01-01"), var_aeme = "HYD_temp",
    depth_from = 2, depth_to = 2, value = 13
  )
  # Assign without going through add_obs()' normalisation
  aeme@observations <- obs

  migrated <- suppressWarnings(migrate_aeme(aeme))
  lk <- observations(migrated)$lake
  expect_true("depth" %in% names(lk))
  expect_false(any(c("depth_from", "depth_to") %in% names(lk)))
  expect_equal(lk$depth, 2)
})
