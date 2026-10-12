make_sets <- function(n, ids = seq_len(n)) {
  param <- aeme_parameters[aeme_parameters$model == "glm_aed", ]
  set.seed(1)
  sets <- lapply(seq_len(n), function(i) {
    p <- param
    p$value <- pmin(pmax(p$value * runif(nrow(p), 0.95, 1.05), p$min), p$max)
    p
  })
  names(sets) <- ids
  sets
}

test_that("normalise_param_sets accepts a list or a long data.frame", {
  sets <- make_sets(3)
  expect_length(normalise_param_sets(sets), 3)

  long <- dplyr::bind_rows(sets, .id = "ensemble")
  from_long <- normalise_param_sets(long)
  expect_length(from_long, 3)
  expect_false("ensemble" %in% names(from_long[[1]]))
})

test_that("normalise_param_sets keeps member order for numeric-like ids", {
  sets <- make_sets(11)
  long <- dplyr::bind_rows(sets, .id = "ensemble")
  expect_identical(names(normalise_param_sets(long)), as.character(1:11))
})

test_that("normalise_param_sets rejects malformed input", {
  sets <- make_sets(2)
  expect_error(normalise_param_sets(list()), "non-empty")
  expect_error(normalise_param_sets(dplyr::bind_rows(sets)), "ensemble")
  sets[[2]]$value <- NULL
  expect_error(normalise_param_sets(sets), "malformed")
})

test_that("run_aeme_ensemble requires param_sets", {
  aeme <- readRDS(system.file("extdata/aeme.rds", package = "AEME"))
  expect_error(run_aeme_ensemble(aeme, model = "glm_aed", path = tempdir()),
               "param_sets")
})

test_that("assemble_ens_output drops failed members and renumbers", {
  aeme <- readRDS(system.file("extdata/aeme.rds", package = "AEME"))
  mod_list <- list(glm_aed = list(a = list(x = 1), b = NULL, c = list(x = 3)))
  expect_warning(res <- assemble_ens_output(aeme, mod_list, "glm_aed"),
                 "1 of 3")
  outp <- output(res)
  expect_equal(outp$n_members, 2)
  expect_equal(outp$ens_002$glm_aed$x, 3)
  expect_equal(attr(outp, "realisation"), c("a", "c"))

  # An errored member is dropped and its message reported
  err <- structure(list(message = "boom"), class = "aeme_member_error")
  mod_list <- list(glm_aed = list(a = list(x = 1), b = err))
  expect_warning(res <- assemble_ens_output(aeme, mod_list, "glm_aed"), "boom")
  expect_equal(output(res)$n_members, 1)

  mod_list <- list(glm_aed = list(a = NULL))
  expect_error(assemble_ens_output(aeme, mod_list, "glm_aed"), "No ensemble")
})

test_that("an ensemble of GLM-AED parameter sets runs in series", {
  model <- "glm_aed"
  skip_if_models_unavailable(model)

  tmpdir <- tempdir()
  file.copy(system.file("extdata/lake/", package = "AEME"), tmpdir,
            recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- yaml_to_aeme(path = path, "aeme.yaml")
  aeme <- build_aeme(aeme, model = model, path = path, ext_elev = 5)

  sets <- make_sets(3, ids = c("a", "b", "c"))
  aeme <- run_aeme_ensemble(aeme, model = model, path = path,
                            param_sets = sets)

  outp <- output(aeme)
  expect_equal(outp$n_members, 3)
  expect_equal(attr(outp, "realisation"), c("a", "b", "c"))

  # The members differ, since the parameters do
  t1 <- get_var(aeme, model = model, var_sim = "LKE_lvlwtr", ens_n = 1,
                return_df = TRUE)
  t3 <- get_var(aeme, model = model, var_sim = "LKE_lvlwtr", ens_n = 3,
                return_df = TRUE)
  expect_false(isTRUE(all.equal(t1$value, t3$value)))
})

test_that("a member that drains the lake is dropped unless told otherwise", {
  model <- "glm_aed"
  skip_if_models_unavailable(model)

  tmpdir <- tempdir()
  file.copy(system.file("extdata/lake/", package = "AEME"), tmpdir,
            recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- yaml_to_aeme(path = path, "aeme.yaml")
  aeme <- build_aeme(aeme, model = model, path = path, ext_elev = 5)

  # Outflow at its maximum with little inflow empties the lake. GLM still
  # exits normally: the level goes below zero and then NA.
  sets <- make_sets(3, ids = c("a", "drains", "c"))
  drain <- sets$drains$name %in% c("outflow", "inflow")
  sets$drains$value[drain] <- ifelse(sets$drains$name[drain] == "outflow",
                                     2.5, 0.5)

  expect_warning(
    res <- run_aeme_ensemble(aeme, model = model, path = path,
                             param_sets = sets),
    "drains")
  expect_equal(output(res)$n_members, 2)
  expect_equal(attr(output(res), "realisation"), c("a", "c"))

  res_all <- run_aeme_ensemble(aeme, model = model, path = path,
                               param_sets = sets, drop_incomplete = FALSE)
  expect_equal(output(res_all)$n_members, 3)
  lvl <- get_var(res_all, model = model, var_sim = "LKE_lvlwtr", ens_n = 2,
                 return_df = TRUE, remove_spin_up = FALSE)
  expect_true(anyNA(lvl$value) || any(lvl$value <= 0))
})

test_that("an ensemble of GLM-AED parameter sets runs in parallel", {
  model <- "glm_aed"
  skip_if_models_unavailable(model)
  skip_on_cran()
  # Workers load the *installed* AEME, so from a load_all() checkout they would
  # run older code than the one under test.
  skip_if(requireNamespace("pkgload", quietly = TRUE) &&
            pkgload::is_dev_package("AEME"),
          "parallel workers load the installed AEME, not the dev checkout")

  tmpdir <- tempdir()
  file.copy(system.file("extdata/lake/", package = "AEME"), tmpdir,
            recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- yaml_to_aeme(path = path, "aeme.yaml")
  aeme <- build_aeme(aeme, model = model, path = path, ext_elev = 5)

  long <- dplyr::bind_rows(make_sets(4), .id = "ensemble")
  aeme <- run_aeme_ensemble(aeme, model = model, path = path,
                            param_sets = long, parallel = TRUE, ncore = 2)
  expect_equal(output(aeme)$n_members, 4)
})
