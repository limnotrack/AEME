base_aeme <- function() {
  aeme <- readRDS(system.file("extdata/aeme.rds", package = "AEME"))
  suppressWarnings(check_aeme(aeme)) # absorbs the once-per-session version note
  remove_output(aeme)
}

# A small synthetic model output on the example lake's simulation window:
# 4 layers at 0.5, 1.5, 2.5, 3.5 m below the surface; temperature falls with
# depth by `offset` degrees per metre; epilimnion depth of 2 m.
fake_model_output <- function(aeme, model = "glm_aed", base = 20, offset = 1,
                              epidep = 2) {
  tm <- time(aeme)
  dates <- seq(as.Date(tm$start), by = "day", length.out = 10)
  n <- length(dates)
  depths <- matrix(c(0.5, 1.5, 2.5, 3.5), nrow = 4, ncol = n)
  temp <- base - offset * depths
  out <- list(Date = dates,
              LKE_lvlwtr = rep(4, n),
              LKE_depths = depths,
              HYD_temp = temp,
              HYD_epidep = rep(epidep, n),
              z = matrix(1, nrow = 7, ncol = n))
  .new_aeme_output(out, model = model)
}

with_fake_output <- function(n = 1) {
  aeme <- base_aeme()
  for (i in seq_len(n)) {
    aeme <- add_output(aeme, list(glm_aed = fake_model_output(
      aeme, base = 20 + (i - 1))))
  }
  aeme
}

test_that("summarise_aeme builds a valid AemeSummary", {
  s <- summarise_aeme(with_fake_output())
  expect_s4_class(s, "AemeSummary")
  expect_true(is_aeme_summary(s))
  expect_true(validObject(s))
  expect_equal(models(s), "glm_aed")
  expect_equal(s@meta$n_members, 1)

  out <- output(s)
  expect_named(out, c("Date", "model", "var_aeme", "layer", "stat", "value"))
  expect_equal(unique(out$stat), "value")
  # the dimension variable `z` is not summarised
  expect_setequal(unique(out$var_aeme), c("LKE_lvlwtr", "HYD_temp",
                                          "HYD_epidep"))
})

test_that("surface and bottom layers are split at the epilimnion depth", {
  s <- summarise_aeme(with_fake_output())
  out <- output(s)
  d1 <- out[out$var_aeme == "HYD_temp" & out$Date == min(out$Date), ]
  # surface: layers at 0.5 and 1.5 m (19.5, 18.5); bottom: 2.5 and 3.5 (17.5, 16.5)
  expect_equal(d1$value[d1$layer == "surface"], 19)
  expect_equal(d1$value[d1$layer == "bottom"], 17)
  # variables without depth have no layer
  lvl <- out[out$var_aeme == "LKE_lvlwtr", ]
  expect_true(all(is.na(lvl$layer)))
  expect_true(all(lvl$value == 4))
})

test_that("without an epilimnion depth the water column is split in half", {
  aeme <- base_aeme()
  fo <- fake_model_output(aeme)
  fo$HYD_epidep <- NULL
  aeme <- add_output(aeme, list(glm_aed = fo))
  out <- output(summarise_aeme(aeme))
  d1 <- out[out$var_aeme == "HYD_temp" & out$Date == min(out$Date), ]
  # max depth 3.5 -> split at 1.75 m
  expect_equal(d1$value[d1$layer == "surface"], 19)
  expect_equal(d1$value[d1$layer == "bottom"], 17)
})

test_that("members are combined into statistics", {
  s <- summarise_aeme(with_fake_output(3), stats = c("mean", "min", "max"))
  out <- output(s)
  expect_equal(s@meta$n_members, 3)
  expect_setequal(unique(out$stat), c("mean", "min", "max"))
  d1 <- out[out$var_aeme == "HYD_temp" & out$layer == "surface" &
              out$Date == min(out$Date), ]
  # members have base 20, 21, 22 -> surface 19, 20, 21
  expect_equal(d1$value[d1$stat == "min"], 19)
  expect_equal(d1$value[d1$stat == "mean"], 20)
  expect_equal(d1$value[d1$stat == "max"], 21)

  s2 <- summarise_aeme(with_fake_output(3), stats = "mean", keep_members = TRUE)
  expect_setequal(unique(output(s2)$stat),
                  c("mean", "ens_001", "ens_002", "ens_003"))

  s3 <- summarise_aeme(with_fake_output(3), ens_n = 2)
  expect_equal(unique(output(s3)$stat), "value")
  expect_error(summarise_aeme(with_fake_output(1), ens_n = 2),
               class = "aeme_error_no_member")
  expect_error(summarise_aeme(with_fake_output(3), stats = "bogus"),
               class = "aeme_error_summary_stat")
})

test_that("summarise_aeme needs output and rejects a summary", {
  expect_error(summarise_aeme(base_aeme()), class = "aeme_error_no_output")
  s <- summarise_aeme(with_fake_output())
  expect_error(summarise_aeme(s), class = "aeme_error_already_summary")
})

test_that("observations, forcing and hypsograph are carried over", {
  aeme <- with_fake_output()
  s <- summarise_aeme(aeme)
  expect_equal(hypsograph(s), input(aeme)$hypsograph)
  expect_setequal(names(observations(s)), intersect(c("lake", "level"),
                                                    names(observations(aeme))))
  f <- forcing(s)
  expect_true("meteo" %in% names(f))
  expect_named(f$meteo, c("Date", "var_aeme", "value"))
  expect_s3_class(f$meteo$Date, "Date")
  # one value per day and variable
  expect_false(anyDuplicated(f$meteo[, c("Date", "var_aeme")]) > 0)
  expect_true(all(c("Date", "name", "var_aeme", "value") %in% names(f$inflows)))

  s0 <- summarise_aeme(aeme, observations = FALSE, forcing = character(0))
  expect_length(observations(s0), 0)
  expect_length(forcing(s0), 0)
  s1 <- summarise_aeme(aeme, forcing = "meteo", vars = "HYD_temp")
  expect_named(forcing(s1), "meteo")
  expect_equal(unique(output(s1)$var_aeme), "HYD_temp")
})

test_that("the summary is much smaller than the source object", {
  aeme <- with_fake_output()
  # make the source output realistically large: 100 layers over 400 days
  tm <- time(aeme)
  n <- 400
  dates <- seq(as.Date(tm$start), by = "day", length.out = n)
  depths <- matrix(seq(0.1, 10, length.out = 100), nrow = 100, ncol = n)
  big <- .new_aeme_output(list(Date = dates, LKE_lvlwtr = rep(10, n),
                               LKE_depths = depths,
                               HYD_temp = 20 - depths,
                               CHM_oxy = 9 - depths / 2,
                               HYD_epidep = rep(3, n)), model = "glm_aed")
  aeme <- remove_output(aeme) |> add_output(list(glm_aed = big))
  s <- summarise_aeme(aeme, forcing = character(0))
  expect_lt(as.numeric(object.size(output(s))),
            0.2 * as.numeric(object.size(output(aeme))))
})

test_that("an AemeSummary works with the shared accessors and get_var()", {
  aeme <- with_fake_output()
  s <- summarise_aeme(aeme)
  expect_equal(lake(s), lake(aeme))
  expect_equal(parameters(s), parameters(aeme))
  expect_true(all(c("start", "stop", "tz") %in% names(time(s))))
  expect_null(time(s)$spin_up)

  df <- get_var(s, var_sim = "HYD_temp")
  expect_true(all(c("Date", "depth", "value", "Model", "var_sim", "layer",
                    "stat") %in% names(df)))
  expect_equal(unique(df$Model), "GLM-AED")
  expect_error(get_var(s, var_sim = "CHM_oxy"), class = "aeme_error_no_var")
})

test_that("an AemeSummary cannot be used as an Aeme", {
  s <- summarise_aeme(with_fake_output())
  expect_error(check_aeme(s), class = "aeme_error_aeme_summary")
  expect_error(run_aeme(s, model = "glm_aed"), class = "aeme_error_aeme_summary")
})

test_that("an AemeSummary plots", {
  s <- summarise_aeme(with_fake_output(2))
  p <- plot(s, "HYD_temp")
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("validity rejects a malformed summary", {
  s <- summarise_aeme(with_fake_output())
  bad <- s
  bad@output$layer[1] <- "middle"
  expect_error(validObject(bad), "layer")
  bad <- s
  bad@output$value <- as.character(bad@output$value)
  expect_error(validObject(bad), "numeric")
  bad <- s
  bad@output <- bad@output[, setdiff(names(bad@output), "stat")]
  expect_error(validObject(bad), "stat")
  bad <- s
  bad@meta$n_members <- 0
  expect_error(validObject(bad), "n_members")
})

test_that("write/read round-trips through rds and exports json", {
  s <- summarise_aeme(with_fake_output(2))
  f <- withr::local_tempfile(fileext = ".rds")
  write_aeme_summary(s, f)
  s2 <- read_aeme_summary(f)
  expect_identical(output(s2), output(s))
  expect_identical(hypsograph(s2), hypsograph(s))

  j <- withr::local_tempfile(fileext = ".json")
  write_aeme_summary(s, j, format = "json")
  expect_true(file.exists(j))
  js <- jsonlite::read_json(j)
  expect_true(all(c("output", "forcing", "meta") %in% names(js)))

  # something that is not a summary is reported as such
  g <- withr::local_tempfile(fileext = ".rds")
  saveRDS(list(a = 1), g)
  expect_error(read_aeme_summary(g), class = "aeme_error_old_summary")
})

test_that("write/read round-trips through parquet", {
  skip_if_not_installed("arrow")
  s <- summarise_aeme(with_fake_output(2))
  d <- withr::local_tempdir()
  write_aeme_summary(s, file.path(d, "pq"), format = "parquet")
  s2 <- read_aeme_summary(file.path(d, "pq"))
  expect_equal(nrow(output(s2)), nrow(output(s)))
  expect_equal(sort(output(s2)$value), sort(output(s)$value))
  expect_equal(hypsograph(s2)$area, hypsograph(s)$area)
  expect_setequal(names(forcing(s2)), names(forcing(s)))
  expect_equal(models(s2), models(s))
})

test_that("summarise_aeme works on real GLM output", {
  skip_if_models_unavailable("glm_aed")
  aeme <- readRDS(system.file("extdata/aeme.rds", package = "AEME"))
  path <- withr::local_tempdir()
  suppressWarnings({
    aeme <- build_aeme(path = path, aeme = aeme, model = "glm_aed", ext_elev = 5)
    aeme <- run_aeme(aeme, model = "glm_aed", path = path)
  })
  s <- summary(aeme)
  expect_s4_class(s, "AemeSummary")
  expect_true("HYD_temp" %in% output(s)$var_aeme)
  expect_false("z" %in% output(s)$var_aeme)
  expect_lt(as.numeric(object.size(output(s))),
            as.numeric(object.size(output(aeme))))
})
