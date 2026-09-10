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


# -- observation Date column is noon-anchored UTC POSIXct ----------------------

test_that(".as_obs_datetime() anchors daily input at 12:00:00 UTC", {
  noon <- function(x) format(x, "%H:%M:%S", tz = "UTC")

  d_date <- .as_obs_datetime(as.Date(c("2020-01-01", "2020-02-15")))
  expect_s3_class(d_date, "POSIXct")
  expect_identical(attr(d_date, "tzone"), "UTC")
  expect_true(all(noon(d_date) == "12:00:00"))
  expect_equal(as.Date(d_date, tz = "UTC"),
               as.Date(c("2020-01-01", "2020-02-15")))

  # date-only strings, with repeats (a depth profile on one day)
  d_chr <- .as_obs_datetime(c("2020-01-01", "2020-01-01", "2020-02-15"))
  expect_true(all(noon(d_chr) == "12:00:00"))

  # a midnight POSIXct column is daily; a real time-of-day is kept
  expect_true(all(noon(.as_obs_datetime(
    as.POSIXct(c("2020-01-01", "2020-02-15"), tz = "UTC"))) == "12:00:00"))
  sub <- .as_obs_datetime(c("2020-01-01 06:00:00", "2020-01-01 18:00:00"))
  expect_equal(noon(sub), c("06:00:00", "18:00:00"))

  # NA rows propagate
  expect_true(is.na(.as_obs_datetime(as.Date(c(NA, "2020-01-01")))[1]))
})

test_that(".as_obs_datetime() is idempotent and never shifts a calendar date", {
  x1 <- .as_obs_datetime(as.Date("2020-06-15"))
  expect_identical(.as_obs_datetime(x1), x1)
  x2 <- .as_obs_datetime("2020-06-15 06:30:00")
  expect_identical(.as_obs_datetime(x2), x2)

  # a non-UTC declared timezone must not move a daily calendar date
  d <- .as_obs_datetime("2020-01-01", tz = "Pacific/Auckland",
                        reinterpret_utc_tag = TRUE)
  expect_equal(as.Date(d, tz = "UTC"), as.Date("2020-01-01"))
})

test_that("migrate_aeme() converts a Date observation column to noon POSIXct", {
  aeme <- new_aeme()
  obs <- observations(aeme)
  obs$lake <- data.frame(
    Date = as.Date(c("2020-01-01", "2020-01-05")), var_aeme = "HYD_temp",
    depth = c(0, 2), value = c(15, 11)
  )
  obs$level <- data.frame(
    Date = as.Date("2020-01-03"), var_aeme = "LKE_lvlwtr", value = 42
  )
  aeme@observations <- obs

  m1 <- suppressWarnings(migrate_aeme(aeme))
  lk <- observations(m1)$lake
  lv <- observations(m1)$level
  expect_s3_class(lk$Date, "POSIXct")
  expect_s3_class(lv$Date, "POSIXct")
  expect_true(all(format(lk$Date, "%H:%M:%S", tz = "UTC") == "12:00:00"))

  # idempotent
  m2 <- suppressWarnings(migrate_aeme(m1))
  expect_identical(observations(m2)$lake$Date, lk$Date)
})

test_that("add_obs() stores Date as noon-anchored POSIXct", {
  lake <- data.frame(
    Date = as.Date(c("2020-01-01", "2020-01-05")), var_aeme = "HYD_temp",
    depth = c(0, 2), value = c(18, 11)
  )
  level <- data.frame(
    Date = as.Date("2020-01-03"), var_aeme = "LKE_lvlwtr", value = 42
  )
  aeme <- add_obs(new_aeme(), lake = lake, level = level)
  o <- observations(aeme)
  expect_s3_class(o$lake$Date, "POSIXct")
  expect_s3_class(o$level$Date, "POSIXct")
  expect_true(all(format(o$lake$Date, "%H:%M:%S", tz = "UTC") == "12:00:00"))

  # a character Date column is coerced (with a warning) rather than rejected
  chr <- data.frame(Date = c("2020-01-01", "2020-01-05"), var_aeme = "HYD_temp",
                    depth = c(0, 2), value = c(18, 11))
  expect_warning(a2 <- add_obs(new_aeme(), lake = chr),
                 class = "aeme_warn_obs_date_coerced")
  expect_s3_class(observations(a2)$lake$Date, "POSIXct")
})

test_that("get_obs() returns a POSIXct Date column", {
  aeme <- readRDS(system.file("extdata/aeme.rds", package = "AEME"))
  got <- get_obs(check_aeme(aeme), var_sim = "HYD_temp")
  expect_s3_class(got$Date, "POSIXct")
})
