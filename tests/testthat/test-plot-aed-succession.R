test_that("plot_aed_succession() returns a two-panel plot", {
  model <- "glm_aed"
  skip_if_models_unavailable(model)

  tmpdir <- tempfile("succession")
  dir.create(tmpdir)
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- readRDS(system.file("extdata/aeme.rds", package = "AEME"))

  groups <- c("cyano", "green", "diatom")
  vars_sim <- c("HYD_temp", "PHY_tchla", paste0("PHY_", groups))
  model_controls <- get_model_controls(use_bgc = TRUE)
  model_controls <- set_vars_sim(model_controls = model_controls,
                                 vars_sim = vars_sim)
  sim_period <- suggest_sim_period(aeme, vars_sim = vars_sim)
  aeme <- set_sim_period(aeme, period = sim_period)
  aeme <- build_aeme(path = path, aeme = aeme, model = model,
                     model_controls = model_controls,
                     ext_elev = 5, use_bgc = TRUE) |>
    run_aeme()

  p <- plot_aed_succession(aeme, model = model, groups = groups)
  expect_s3_class(plot_phytos(aeme), "ggplot")
  expect_s3_class(p, "patchwork")
  expect_length(p$patches$plots, 1)

  # Bottom panel: monthly shares sum to 100 % in every month
  mon <- p[[2]]$data
  expect_setequal(as.character(mon$group), groups)
  tot <- tapply(mon$pct, mon$month, sum, na.rm = TRUE)
  expect_equal(as.numeric(tot), rep(100, length(tot)))

  # Date axes are shared and the succession panel gets two thirds of the height
  expect_s3_class(mon$month, "Date")
  expect_equal(p[[1]]$scales$get_scales("x")$limits,
               p[[2]]$scales$get_scales("x")$limits)
  expect_equal(p$patches$layout$heights, c(1, 2))

  expect_error(plot_aed_succession(aeme, model = c("glm_aed", "dy_cd")),
               "single model")
})
