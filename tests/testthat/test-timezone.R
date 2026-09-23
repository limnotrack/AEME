# Declared input timezone: everything the user types is interpreted in
# time$tz and stored as UTC; daily data is never shifted; internal datetimes
# are always UTC and independent of the session timezone.

test_that(".to_utc() localises naive input and re-expresses tz-aware input", {
  # naive character, sub-daily, NZ standard time (UTC+12 in June)
  x <- AEME:::.to_utc("2020-06-01 12:00:00", tz = "Pacific/Auckland")
  expect_s3_class(x, "POSIXct")
  expect_identical(attr(x, "tzone"), "UTC")
  expect_equal(x, as.POSIXct("2020-06-01 00:00:00", tz = "UTC"))

  # a Date carries no timezone and must never be shifted
  d <- as.Date("2020-06-01")
  expect_identical(AEME:::.to_utc(d, tz = "Pacific/Auckland"), d)

  # a POSIXct that already carries a real tzone is an absolute instant
  # (2020-06-01 is EDT, UTC-4)
  y <- as.POSIXct("2020-06-01 00:00:00", tz = "America/New_York")
  expect_equal(AEME:::.to_utc(y, tz = "Pacific/Auckland"),
               as.POSIXct("2020-06-01 04:00:00", tz = "UTC"))

  # idempotent
  expect_identical(AEME:::.to_utc(x, tz = "Pacific/Auckland"), x)
})

test_that(".as_forcing_datetime() keeps daily calendar data unshifted", {
  daily <- c("2020-06-01", "2020-06-02", "2020-06-03")
  out <- AEME:::.as_forcing_datetime(daily, tz = "Pacific/Auckland")
  expect_s3_class(out, "Date")
  expect_equal(out, as.Date(daily))

  sub <- format(seq(as.POSIXct("2020-06-01 00:00", tz = "UTC"),
                    by = "6 hours", length.out = 5))
  out2 <- AEME:::.as_forcing_datetime(sub, tz = "Pacific/Auckland")
  expect_s3_class(out2, "POSIXct")
  expect_identical(attr(out2, "tzone"), "UTC")
  # 12 h earlier than the wall-clock reading
  expect_equal(out2[3], as.POSIXct("2020-06-01 00:00", tz = "UTC"))
})

test_that("session timezone does not change the ingested values", {
  run <- function() {
    s <- AEME:::.as_forcing_datetime(
      format(seq(as.POSIXct("2020-06-01 00:00", tz = "UTC"),
                 by = "3 hours", length.out = 9)),
      tz = "Pacific/Auckland")
    d <- AEME:::.as_forcing_datetime(c("2020-06-01", "2020-06-02"),
                                     tz = "Pacific/Auckland")
    list(s = s, d = d)
  }
  a <- withr::with_timezone("UTC", run())
  b <- withr::with_timezone("Pacific/Auckland", run())
  c <- withr::with_timezone("America/New_York", run())
  expect_identical(a, b)
  expect_identical(a, c)
})

test_that("new_aeme(tz=) interprets start/stop in that zone and stores UTC", {
  a <- suppressMessages(
    new_aeme(tz = "Pacific/Auckland", start = "2020-06-01", stop = "2020-12-31")
  )
  tt <- time(a)
  expect_identical(tt$tz, "Pacific/Auckland")
  expect_identical(attr(tt$start, "tzone"), "UTC")
  # NZ midnight 2020-06-01 is 2020-05-31 12:00 UTC
  expect_equal(tt$start, as.POSIXct("2020-05-31 12:00:00", tz = "UTC"))
})

test_that("set_time(tz=) re-interprets supplied start/stop and validates", {
  a <- suppressMessages(new_aeme(start = "2020-01-01", stop = "2020-12-31"))
  a <- suppressMessages(
    set_time(a, start = "2020-06-01 09:00:00", tz = "Pacific/Auckland")
  )
  expect_identical(time(a)$tz, "Pacific/Auckland")
  expect_equal(time(a)$start, as.POSIXct("2020-05-31 21:00:00", tz = "UTC"))

  expect_error(
    suppressMessages(set_time(a, tz = "Not/AZone")),
    class = "aeme_error_time_tz"
  )
})

test_that("time$tz defaults to UTC when not declared", {
  a <- suppressMessages(new_aeme())
  expect_identical(time(a)$tz, "UTC")
  b <- suppressMessages(aeme_constructor(
    lake = lake(a), time = list(start = "2020-01-01", stop = "2020-12-31"),
    input = input(a)))
  expect_identical(time(b)$tz, "UTC")
})

test_that("a declared non-UTC tz reinterprets a UTC-tagged column at ingest", {
  # UTC is R's default fallback label; when the user declares a data zone,
  # add_met() treats a "UTC"-tagged sub-daily column as local wall-clock time.
  hrs <- seq(as.POSIXct("2020-06-01 00:00", tz = "UTC"), by = "hour",
             length.out = 72)
  met <- data.frame(Date = hrs, MET_radswd = 100, MET_tmpair = 10,
                    MET_wndspd = 2, MET_pprain = 0)

  a <- suppressMessages(new_aeme(tz = "Pacific/Auckland",
                                 start = "2020-06-01", stop = "2020-06-02"))
  a <- suppressMessages(add_met(a, met))
  d <- input(a)$meteo$Date
  expect_identical(attr(d, "tzone"), "UTC")
  # 00:00 "UTC" in the file was really NZ midnight -> 12:00 UTC the day before
  expect_equal(d[1], as.POSIXct("2020-05-31 12:00", tz = "UTC"))

  # ...but with tz = "UTC" (the default) the same column is left alone
  a2 <- suppressMessages(new_aeme(start = "2020-06-01", stop = "2020-06-02"))
  a2 <- suppressMessages(add_met(a2, met))
  expect_equal(input(a2)$meteo$Date[1], as.POSIXct("2020-06-01 00:00", tz = "UTC"))
})

test_that(".to_utc() reinterpret_utc_tag only fires for a non-UTC tz", {
  x <- as.POSIXct("2020-06-01 09:00", tz = "UTC")
  # default: UTC tag respected as absolute
  expect_equal(AEME:::.to_utc(x, tz = "Pacific/Auckland"), x)
  # opt-in + non-UTC tz: treated as naive local
  expect_equal(
    AEME:::.to_utc(x, tz = "Pacific/Auckland", reinterpret_utc_tag = TRUE),
    as.POSIXct("2020-05-31 21:00", tz = "UTC"))
  # opt-in but tz is UTC: no-op
  expect_equal(AEME:::.to_utc(x, tz = "UTC", reinterpret_utc_tag = TRUE), x)
  # a real non-UTC tzone is always absolute, never reinterpreted
  y <- as.POSIXct("2020-06-01 00:00", tz = "America/New_York")
  expect_equal(
    AEME:::.to_utc(y, tz = "Pacific/Auckland", reinterpret_utc_tag = TRUE),
    as.POSIXct("2020-06-01 04:00", tz = "UTC"))
})

test_that(".to_utc() does not drop the DST spring-forward hour", {
  # 02:00-02:59 on 2020-09-27 does not exist in Pacific/Auckland
  gap <- seq(as.POSIXct("2020-09-27 00:00", tz = "UTC"), by = "hour",
             length.out = 6)
  out <- AEME:::.to_utc(gap, tz = "Pacific/Auckland",
                        reinterpret_utc_tag = TRUE)
  expect_false(anyNA(out))
  expect_identical(attr(out, "tzone"), "UTC")
})

test_that("migrate_aeme() backfills time$tz on legacy objects", {
  a <- suppressMessages(new_aeme())
  a@time$tz <- NULL
  a <- AEME:::migrate_aeme(a)
  expect_identical(a@time$tz, "UTC")
})

test_that("write_aeme_to_files() serialises time$tz and start/stop as UTC", {
  skip_if_not(file.exists(system.file("extdata/aeme.rds", package = "AEME")))
  a <- readRDS(system.file("extdata/aeme.rds", package = "AEME"))
  a <- suppressMessages(set_time(a, tz = "Pacific/Auckland"))

  td <- withr::local_tempdir()
  suppressMessages(write_aeme_to_files(a, path = td))
  tf <- list.files(td, pattern = "time.csv", recursive = TRUE, full.names = TRUE)
  time_csv <- read.csv(tf)
  expect_true("tz" %in% names(time_csv))
  expect_identical(time_csv$tz[1], "Pacific/Auckland")
  # start is written as a UTC wall-clock string and reads straight back
  expect_equal(as.POSIXct(time_csv$start[1], tz = "UTC"), time(a)$start)
})

test_that("standardise_met() flags sub-daily shortwave offset from solar noon", {
  # solar noon for lon 174.5 is ~00:22 UTC; build a series that (correctly)
  # peaks there, and a local-time copy that peaks ~13:00.
  days <- 6
  hrs <- seq(as.POSIXct("2020-06-01 00:00", tz = "UTC"), by = "hour",
             length.out = days * 24)
  # shortwave ~ cos around solar noon 00:22 UTC (0.37 h)
  hod <- as.numeric(difftime(hrs, as.POSIXct(as.Date(hrs), tz = "UTC"),
                             units = "hours"))
  sw_utc <- pmax(0, cos((hod - 0.37) / 24 * 2 * pi)) * 800
  base <- data.frame(MET_tmpair = 10, MET_wndspd = 2, MET_pprain = 0)

  utc_met <- cbind(data.frame(Date = hrs, MET_radswd = sw_utc), base)
  expect_no_warning(
    suppressMessages(standardise_met(utc_met, verbose = FALSE,
                                     longitude = 174.5)),
    class = "aeme_warn_met_solar_offset"
  )

  local_met <- utc_met
  local_met$Date <- local_met$Date + 13 * 3600   # label UTC but really +13
  expect_warning(
    suppressMessages(standardise_met(local_met, verbose = FALSE,
                                     longitude = 174.5)),
    class = "aeme_warn_met_solar_offset"
  )

  # no longitude -> no check
  expect_no_warning(
    suppressMessages(standardise_met(local_met, verbose = FALSE)),
    class = "aeme_warn_met_solar_offset"
  )
})

test_that("standardise_met() localises a sub-daily Date column to UTC", {
  met <- data.frame(
    Date = format(seq(as.POSIXct("2020-06-01 00:00", tz = "UTC"),
                      by = "hour", length.out = 48)),
    MET_radswd = 100, MET_tmpair = 10, MET_wndspd = 2, MET_pprain = 0
  )
  out <- suppressMessages(
    standardise_met(met, verbose = FALSE, tz = "Pacific/Auckland")
  )
  expect_s3_class(out$Date, "POSIXct")
  expect_identical(attr(out$Date, "tzone"), "UTC")
  expect_equal(out$Date[1], as.POSIXct("2020-05-31 12:00", tz = "UTC"))

  # a daily series is not shifted
  metd <- data.frame(Date = c("2020-06-01", "2020-06-02", "2020-06-03"),
                     MET_radswd = 100, MET_tmpair = 10, MET_wndspd = 2,
                     MET_pprain = 0)
  outd <- suppressMessages(
    standardise_met(metd, verbose = FALSE, tz = "Pacific/Auckland")
  )
  expect_s3_class(outd$Date, "Date")
  expect_equal(outd$Date, as.Date(metd$Date))
})
