test_that("plot_model_output() works directly on an Aeme object", {
  tmpdir <- tempfile("plot_model_output_")
  dir.create(tmpdir)
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")

  aeme <- yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- get_model_controls(use_bgc = TRUE)
  aeme <- build_aeme(path = path, aeme = aeme, model = "glm_aed",
                     model_controls = model_controls, ext_elev = 5,
                     use_bgc = TRUE)
  aeme <- run_aeme(aeme = aeme, model = "glm_aed", verbose = FALSE,
                   model_controls = model_controls, path = path)
  aeme_time <- time(aeme)

  # Model auto-selected (only one present) -- (z, time) tile plot
  r1 <- plot_model_output(aeme, "HYD_temp")
  testthat::expect_true(ggplot2::is_ggplot(r1))
  # plot_model_output() always deals with a single model/variable, so the
  # Model x name_text facet strip plot_var()/plot_output() uses for
  # multi-panel comparisons should be dropped, not shown as a redundant
  # 1x1 facet
  testthat::expect_true(inherits(r1$facet, "FacetNull"))

  # Explicit model works the same as the auto-selected default
  r2 <- plot_model_output(aeme, "HYD_temp", model = "glm_aed")
  testthat::expect_true(ggplot2::is_ggplot(r2))
  # Spin-up removed by default -- plotted dates should be within the
  # configured simulation window, not the (earlier) spin-up window.
  # plot_var_depth() builds its ggplot with layer-level (not plot-level)
  # data, so pull the actual plotted data from the layer.
  testthat::expect_true(all(r2$layers[[1]]$data$Date >= as.Date(aeme_time$start)))

  # (time)-only vector variable
  r3 <- plot_model_output(aeme, "LKE_lvlwtr", model = "glm_aed")
  testthat::expect_true(ggplot2::is_ggplot(r3))

  # Grouped (nzones, time) variable -- one line per zone, trimmed to the
  # simulation window (not the spin-up window) by default
  lake_dir <- get_lake_dir(aeme, path)
  outfile <- file.path(lake_dir, "glm_aed", "output", "output.nc")
  out_full <- read_glm_output(file = outfile)
  grouped_name <- names(out_full)[vapply(out_full, inherits, logical(1),
                                         "aeme_grouped_var")][1]
  testthat::expect_false(is.na(grouped_name))

  r4 <- plot_model_output(aeme, grouped_name, model = "glm_aed")
  testthat::expect_true(ggplot2::is_ggplot(r4))
  if (nrow(r4$data) > 0) {
    testthat::expect_true(all(r4$data$Date >= as.Date(aeme_time$start)))
  }

  # Unknown variable -- clear error
  testthat::expect_error(plot_model_output(aeme, "not_a_real_variable",
                                           model = "glm_aed"))

  # Model with no stored output -- clear error
  testthat::expect_error(plot_model_output(aeme, "HYD_temp", model = "gotm_wet"))
})

test_that("plot_model_output() still works on a raw output list (via plot_glm_output alias)", {
  tmpdir <- tempfile("plot_model_output_raw_")
  dir.create(tmpdir)
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")

  aeme <- yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- get_model_controls()
  aeme <- build_aeme(path = path, aeme = aeme, model = "glm_aed",
                     model_controls = model_controls, ext_elev = 5,
                     use_bgc = FALSE)
  aeme <- run_aeme(aeme = aeme, model = "glm_aed", verbose = FALSE,
                   model_controls = model_controls, path = path)

  lake_dir <- get_lake_dir(aeme, path)
  outfile <- file.path(lake_dir, "glm_aed", "output", "output.nc")
  out <- read_glm_output(file = outfile)

  r1 <- plot_model_output(out, "HYD_temp")
  testthat::expect_true(ggplot2::is_ggplot(r1))
  testthat::expect_null(r1$labels$subtitle)

  # raw_output = TRUE -- plotted output is visibly marked as raw
  out_raw <- read_glm_output(file = outfile, vars_sim = "HYD_temp",
                             raw_output = TRUE)
  r2 <- plot_model_output(out_raw, "temp")
  testthat::expect_true(ggplot2::is_ggplot(r2))
  testthat::expect_match(r2$labels$subtitle, "raw output")

  # Unclassed list -- clear error, not a cryptic one
  testthat::expect_error(plot_model_output(list(a = 1), "a"))
})

test_that("plot_model_output() masks sentinel-filled/mismatched depth-value pairs", {
  # GLM pads an inactive layer with either NA or the raw netCDF fill value
  # (9.96921e+36) depending on which of "z"/the variable itself ncdf4
  # happened to auto-convert -- synthesise a minimal raw output list with
  # both cases and check neither leaks into the plotted data.
  dates <- as.Date("2020-01-01") + 0:1
  variable  <- matrix(c(10, 9.96921e+36, 11, 12), nrow = 2, ncol = 2)
  depth_mat <- matrix(c(-1, -2, -1, NA), nrow = 2, ncol = 2)

  out <- list(Date = dates, LKE_lvlwtr = c(5, 5), LKE_depths = depth_mat,
             HYD_temp = variable, ok = TRUE, reason = NULL)
  out <- AEME:::.new_aeme_output(out, model = "glm_aed", raw = TRUE)

  p <- plot_model_output(out, "HYD_temp")
  testthat::expect_true(ggplot2::is_ggplot(p))
  plotted <- p$layers[[1]]$data
  testthat::expect_true(all(is.na(plotted$value) | abs(plotted$value) < 1e6))
  testthat::expect_true(all(is.na(plotted$depth) | abs(plotted$depth) < 1e6))
  # the two affected cells (sentinel value, NA depth) must both be masked
  testthat::expect_equal(sum(is.na(plotted$value)), 2)
})
