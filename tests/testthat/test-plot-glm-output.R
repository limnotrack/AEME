test_that("plot_glm_output() plots vector, matrix, and grouped variables from a raw read_glm_output() list", {
  tmpdir <- tempfile("plot_glm_output_")
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

  lake_dir <- get_lake_dir(aeme, path)
  outfile <- file.path(lake_dir, "glm_aed", "output", "output.nc")
  out <- read_glm_output(file = outfile)

  plot_file <- file.path(tmpdir, "p.png")

  # (z, time) matrix variable -- depth x time contour
  grDevices::png(plot_file)
  r1 <- plot_glm_output(out, "HYD_temp")
  grDevices::dev.off()
  testthat::expect_true(is.matrix(r1))

  # (time)-only vector variable -- simple line plot
  grDevices::png(plot_file)
  r2 <- plot_glm_output(out, "LKE_lvlwtr")
  grDevices::dev.off()
  testthat::expect_true(is.numeric(r2))

  # Grouped (nzones, time) variable -- one line per zone
  grouped_name <- names(out)[vapply(out, inherits, logical(1), "aeme_grouped_var")][1]
  testthat::expect_false(is.na(grouped_name))
  grDevices::png(plot_file)
  r3 <- plot_glm_output(out, grouped_name)
  grDevices::dev.off()
  testthat::expect_s3_class(r3, "data.frame")
  testthat::expect_true(all(c("Date", "value") %in% names(r3)))

  # Unknown variable -- clear error, not a cryptic one
  testthat::expect_error(plot_glm_output(out, "not_a_real_variable"))

  # Wrong input type -- clear error
  testthat::expect_error(plot_glm_output(list(a = 1), "a"))
})
