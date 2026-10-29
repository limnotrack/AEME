mb_file <- function() {
  test_path("LID45819_wainamu", "glm_aed", "output", "mass_balance.csv")
}
mb_fixture <- function() {
  d <- seq(as.POSIXct("2020-01-01", tz = "UTC"), by = "day", length.out = 4)
  tibble::tibble(
    datetime = rep(d, 3 * 2),
    var = rep(c("NIT_nit", "NIT_amm"), each = 12),
    term = rep(rep(c("Inf", "Lake", "Out"), each = 4), 2),
    value = c(1, 1, 1, 1, 10, 11, 12, 13, 0, 0, 0, 0,
              2, 2, 2, 2, 20, 22, 24, 26, 1, 1, 1, 1)
  )
}
totals_fixture <- list(TN_vars = c("NIT_nit", "NIT_amm"),
                       TN_varscale = c(1, 2))

test_that("calc_aed_totals_balance() applies scales and converts units", {
  tot <- calc_aed_totals_balance(mb_fixture(), aed_totals = totals_fixture)
  lake <- tot[tot$term == "Lake", ]
  expect_equal(lake$value, c(50, 55, 60, 65))
  kg <- calc_aed_totals_balance(mb_fixture(), aed_totals = totals_fixture,
                                units = "kg")
  expect_equal(kg$value[kg$term == "Lake"][1], 50 * 14.007 / 1e6)
  expect_warning(
    calc_aed_totals_balance(mb_fixture(),
                            aed_totals = list(TN_vars = c("NIT_nit", "X"),
                                              TN_varscale = c(1, 1))),
    "incomplete")
})

test_that("diag_glm_mass_balance() computes the internal remainder", {
  out <- diag_glm_mass_balance(mb_fixture(), aed_totals = totals_fixture,
                               plot = TRUE)
  d <- out$daily
  # d_lake = 5, net external = (1 + 4) - (0 + 2) = 3 -> internal 2
  expect_equal(d$d_lake[-1], rep(5, 3))
  expect_equal(d$net_internal[-1], rep(2, 3))
  expect_equal(d$cum_net_internal, c(0, 2, 4, 6))
  expect_s3_class(out$plot, "ggplot")
  expect_equal(out$summary$name, "TN")
  by_var <- diag_glm_mass_balance(mb_fixture(), by = "var")
  expect_setequal(by_var$summary$name, c("NIT_nit", "NIT_amm"))
})

test_that("read_glm_mass_balance() reads the model output", {
  skip_if_not(file.exists(mb_file()))
  mb <- read_glm_mass_balance(mb_file())
  expect_named(mb, c("datetime", "var", "term", "value"))
  expect_s3_class(mb$datetime, "POSIXct")
  expect_setequal(unique(mb$term), c("Inf", "Lake", "Out"))
  expect_true("NIT_nit" %in% mb$var)
  expect_identical(read_glm_mass_balance(dirname(mb_file())), mb)

  out <- suppressWarnings(diag_glm_mass_balance(mb, path_glm = test_path("LID45819_wainamu",
                                                        "glm_aed")))
  expect_true(all(c("TN", "TP", "TOC") %in% out$summary$name))
})

test_that("read_glm_mass_balance() locates the file from an Aeme object", {
  skip_if_not(file.exists(mb_file()))
  aeme <- readRDS(system.file("extdata/aeme.rds", package = "AEME"))
  lke <- lake(aeme)
  root <- file.path(tempdir(), "mb_root")
  lake_dir <- file.path(root, paste0(lke$id, "_", tolower(lke$name)))
  dir.create(lake_dir, recursive = TRUE)
  file.copy(test_path("LID45819_wainamu", "glm_aed"), lake_dir,
            recursive = TRUE)

  mb <- read_glm_mass_balance(aeme = aeme, path = root)
  expect_identical(mb, read_glm_mass_balance(mb_file()))
  expect_error(read_glm_mass_balance(), "file")
})

test_that("diag_glm_mass_balance() accounts for the save interval", {
  mb <- mb_fixture()
  # daily rows: interval 1 day
  d1 <- diag_glm_mass_balance(mb, aed_totals = totals_fixture)$daily
  expect_equal(d1$interval_days, rep(1, 4))
  # same totals saved every 6 hours: rates scale by 4, totals do not change
  mb6 <- mb
  mb6$datetime <- as.POSIXct("2020-01-01", tz = "UTC") +
    as.numeric(mb$datetime - min(mb$datetime), units = "secs") / 4
  d6 <- diag_glm_mass_balance(mb6, aed_totals = totals_fixture)$daily
  expect_equal(d6$interval_days, rep(0.25, 4))
  expect_equal(d6$net_internal, d1$net_internal)
  expect_equal(d6$net_internal_rate[-1], d1$net_internal_rate[-1] * 4)
  expect_equal(d6$inflow_rate, d1$inflow_rate * 4)

  # nsave * dt from the nml: daily rows agree with the Wainamu config
  # (nsave = 24, dt = 3600), hourly-spaced rows would warn
  glm_dir <- test_path("LID45819_wainamu", "glm_aed")
  skip_if_not(dir.exists(glm_dir))
  expect_equal(glm_save_interval_days(glm_dir), 1)
  expect_null(glm_save_interval_days(NULL))
  expect_warning(
    diag_glm_mass_balance(mb6, aed_totals = totals_fixture, path_glm = glm_dir),
    "nsave")
})

test_that("glm_nutrient_balance() summarises the N and P budgets in kg", {
  glm_dir <- test_path("LID45819_wainamu", "glm_aed")
  skip_if_not(dir.exists(glm_dir))
  comps <- glm_totals_component_vars(glm_dir)
  skip_if(is.null(comps))

  aeme <- readRDS(system.file("extdata/aeme.rds", package = "AEME"))
  lke <- lake(aeme)
  root <- file.path(tempdir(), "nb_root")
  lake_dir <- file.path(root, paste0(lke$id, "_", tolower(lke$name)))
  dir.create(lake_dir, recursive = TRUE)
  file.copy(glm_dir, lake_dir, recursive = TRUE)

  # synthetic mass balance: 2 years, daily, constant inflow/outflow, with the
  # lake storage rising by 2 mmol per day in each component
  vars <- unique(unlist(comps[c("TN", "TP")]))
  tm <- seq(as.POSIXct("2020-01-01", tz = "UTC"), by = "day",
            length.out = 731)
  mb <- data.frame(time = format(tm, "%Y-%m-%d %H:%M:%S"))
  for (v in vars) {
    mb[[paste("Inf", v)]] <- 10
    mb[[paste("Out", v)]] <- 4
    mb[[paste("Lake", v)]] <- 100 + 2 * seq_along(tm)
  }
  out_dir <- file.path(lake_dir, "glm_aed", "output")
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  utils::write.csv(mb, file.path(out_dir, "mass_balance.csv"),
                   row.names = FALSE, quote = FALSE)

  nb <- glm_nutrient_balance(aeme, path = root, period = "all")
  expect_named(nb, c("daily", "summary"))
  expect_equal(nb$summary$element, c("N", "P"))
  n <- nb$summary[nb$summary$element == "N", ]
  expect_equal(n$retained, n$inflow - n$outflow)
  expect_equal(n$retention_pct, 100 * 6 / 10)
  # storage change of 2 per day, net external +6: internal is -4 per day
  expect_equal(n$internal / n$d_storage, (2 - 6) / 2)

  yr <- glm_nutrient_balance(aeme, path = root, element = "N",
                             period = "year")
  expect_equal(yr$summary$period, c("2020", "2021"))
  expect_equal(sum(yr$summary$internal), n$internal)

  # spin-up is dropped before the simulation start
  start <- as.Date(time(aeme)$start)
  ns <- glm_nutrient_balance(aeme, path = root, element = "P",
                             include_spin_up = FALSE)
  expect_true(all(as.Date(ns$daily$datetime) >= start))
  expect_error(
    glm_nutrient_balance(aeme, path = root, element = "N")$summary |>
      nrow() |> expect_gt(0), NA)
})
