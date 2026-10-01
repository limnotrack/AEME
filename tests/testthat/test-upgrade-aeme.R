test_that("cli_inform_safe / cli_safe interpolate in the caller environment", {
  withr::local_options(AEME.inform = TRUE)

  f_inform <- function() {
    matched <- c(a = 1, b = 2, c = 3)
    cli_inform_safe(c("i" = "Renaming {length(matched)} column{?s}"))
  }
  expect_no_error(f_inform())

  f_safe <- function() {
    dropped <- c("x", "y")
    cli_safe(c("!" = "{length(dropped)} column{?s}: {.val {dropped}}"),
             FUN = cli::cli_bullets)
  }
  expect_no_error(f_safe())
})

test_that("upgrade_aeme() migrates a pre-0.4.0 Aeme object", {
  old_file <- system.file("extdata/aeme_old.rds", package = "AEME")
  skip_if(old_file == "", "extdata/aeme_old.rds fixture not installed")
  old <- readRDS(old_file)

  up <- upgrade_aeme(old, quiet = TRUE)
  expect_s4_class(up, "Aeme")

  models <- unname(list_models())

  # per-model backfills
  expect_true(all(models %in% names(time(up)$spin_up)))
  expect_true(all(models %in% names(inflows(up)$factor)))
  expect_true(all(models %in% names(outflows(up)$factor)))
  expect_true(all(models %in% names(configuration(up))))

  # legacy outflow-level element renamed
  expect_false(any(c("lvl", "outflow_lvl") %in% names(outflows(up))))
  expect_false(is.null(outflows(up)$elevation))

  # output placeholders + integer n_members
  expect_true(all(models %in% names(output(up))))
  expect_type(output(up)$n_members, "integer")

  # level coerced to a plain data.frame
  if (!is.null(observations(up)$level)) {
    expect_false(inherits(observations(up)$level, "tbl_df"))
    expect_true("var_aeme" %in% names(observations(up)$level))
  }

  # scalar configuration defaults filled + parameters column order
  for (k in c("ext_elev", "calc_wbal", "wb_method", "calc_wlev",
              "hum_type", "est_swr_hr")) {
    expect_false(is.null(configuration(up)[[k]]), info = k)
  }
  want <- param_colnames(incl_opt = FALSE)
  expect_identical(names(parameters(up))[seq_along(want)], want)

  # version stamp
  expect_identical(configuration(up)$aeme_upgraded,
                   as.character(utils::packageVersion("AEME")))
})

test_that("upgrade_aeme() is idempotent", {
  old_file <- system.file("extdata/aeme_old.rds", package = "AEME")
  skip_if(old_file == "", "extdata/aeme_old.rds fixture not installed")
  old <- readRDS(old_file)

  up1 <- upgrade_aeme(old, quiet = TRUE)
  up2 <- upgrade_aeme(up1, quiet = TRUE)
  expect_identical(up1, up2)
})

test_that("upgrade_aeme() rejects non-Aeme input", {
  expect_error(upgrade_aeme(list(1)), class = "aeme_error_aeme_type")
})

test_that("build_aeme() works on a pre-0.4.0 object with messages on", {
  skip_if_no_glm()
  withr::local_options(AEME.inform = TRUE)

  old_file <- system.file("extdata/aeme_old.rds", package = "AEME")
  skip_if(old_file == "", "extdata/aeme_old.rds fixture not installed")
  old <- readRDS(old_file)

  path <- withr::local_tempdir()
  # domain warnings (unversioned object, legacy inflow column names) are
  # expected here; this test only guards against the cli-interpolation abort.
  expect_no_error(
    suppressWarnings(
      build_aeme(old, model = "glm_aed", path = path,
                 model_controls = get_model_controls(),
                 calc_wbal = FALSE, calc_wlev = FALSE, wb_method = 1)
    )
  )
})
