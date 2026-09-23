test_that("set_initial_conditions stores a generic spec and updates input", {
  aeme <- readRDS(system.file("extdata/aeme.rds", package = "AEME"))
  prof <- data.frame(depth = c(0, 5, 10), temperature = c(18, 14, 11))

  aeme <- set_initial_conditions(aeme, depth = 12, profile = prof,
                                 wq = list(CHM_oxy = 300))

  spec <- get_initial_conditions(aeme)
  expect_equal(spec$default$depth, 12)
  expect_equal(spec$default$profile$salt, c(0, 0, 0))
  expect_equal(spec$default$wq$CHM_oxy, 300)

  # Back-compatible writes take effect immediately
  expect_equal(input(aeme)$init_depth, 12)
  expect_equal(input(aeme)$init_profile$temperature, c(18, 14, 11))
  mc <- get_model_controls(aeme)
  expect_equal(mc$initial_wc[mc$var_aeme == "CHM_oxy"], 300)
})

test_that("model_init overrides are merged over the defaults", {
  aeme <- readRDS(system.file("extdata/aeme.rds", package = "AEME"))
  aeme <- set_initial_conditions(
    aeme,
    profile = data.frame(depth = c(0, 10), temperature = c(15, 10)),
    wq = list(CHM_oxy = 300),
    model_init = list(
      glm_aed = list(
        profile = data.frame(depth = c(0, 10), temperature = c(20, 12)),
        wq = list(NIT_amm = 0.5)
      )
    )
  )

  glm_ic <- get_initial_conditions(aeme, model = "glm_aed")
  expect_equal(glm_ic$profile$temperature, c(20, 12))
  expect_equal(glm_ic$wq$CHM_oxy, 300)   # inherited from default
  expect_equal(glm_ic$wq$NIT_amm, 0.5)   # from override

  gotm_ic <- get_initial_conditions(aeme, model = "gotm_wet")
  expect_equal(gotm_ic$profile$temperature, c(15, 10))
  expect_null(gotm_ic$wq$NIT_amm)
})

test_that("repeated calls merge, reset clears", {
  aeme <- readRDS(system.file("extdata/aeme.rds", package = "AEME"))
  aeme <- set_initial_conditions(aeme, depth = 8, wq = list(CHM_oxy = 250))
  aeme <- set_initial_conditions(aeme, wq = list(NIT_nit = 0.2))
  spec <- get_initial_conditions(aeme)
  expect_equal(spec$default$depth, 8)
  expect_equal(names(spec$default$wq), c("CHM_oxy", "NIT_nit"))

  aeme <- set_initial_conditions(aeme, depth = 3, reset = TRUE)
  spec <- get_initial_conditions(aeme)
  expect_equal(spec$default$depth, 3)
  expect_null(spec$default$wq)
})

test_that("depth-resolved wq values are kept as data.frames", {
  aeme <- readRDS(system.file("extdata/aeme.rds", package = "AEME"))
  aeme <- set_initial_conditions(
    aeme,
    wq = list(NIT_amm = data.frame(depth = c(0, 10), value = c(0.1, 0.4)))
  )
  ic <- get_initial_conditions(aeme, model = "glm_aed")
  expect_s3_class(ic$wq$NIT_amm, "data.frame")
  expect_equal(ic$wq$NIT_amm$value, c(0.1, 0.4))
})

test_that("input validation rejects bad arguments", {
  aeme <- readRDS(system.file("extdata/aeme.rds", package = "AEME"))

  expect_error(set_initial_conditions(aeme, depth = -1),
               class = "aeme_error_init_depth")
  expect_error(set_initial_conditions(aeme, depth = c(1, 2)),
               class = "aeme_error_init_depth")
  expect_error(
    set_initial_conditions(aeme, profile = data.frame(depth = c(0, 1))),
    class = "aeme_error_init_profile"
  )
  expect_error(
    set_initial_conditions(aeme, profile = data.frame(temperature = 10)),
    class = "aeme_error_init_profile"
  )
  expect_error(set_initial_conditions(aeme, wq = list(NOT_A_VAR = 1)),
               class = "aeme_error_init_wq")
  expect_error(set_initial_conditions(aeme, wq = list(CHM_oxy = c(1, 2))),
               class = "aeme_error_init_wq")
  expect_error(
    set_initial_conditions(aeme, model_init = list(nope = list(depth = 1))),
    class = "aeme_error_model_init"
  )
  expect_error(
    set_initial_conditions(aeme,
                           model_init = list(glm_aed = list(foo = 1))),
    class = "aeme_error_model_init"
  )
})

test_that("get_initial_conditions returns NULL before anything is set", {
  aeme <- readRDS(system.file("extdata/aeme.rds", package = "AEME"))
  expect_null(get_initial_conditions(aeme))
})

test_that("build_aeme honours a GLM-AED-specific profile and scalar wq", {
  skip_if_models_unavailable(c("glm_aed"))
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- yaml_to_aeme(path = path, "aeme.yaml")

  glm_prof <- data.frame(depth = c(0, 4, 8, 12), temperature = c(21, 17, 13, 11))
  aeme <- set_initial_conditions(
    aeme,
    wq = list(CHM_oxy = 275),
    model_init = list(glm_aed = list(profile = glm_prof))
  )

  aeme <- build_aeme(path = path, aeme = aeme, model = "glm_aed",
                     model_controls = get_model_controls(use_bgc = TRUE),
                     ext_elev = 5, use_bgc = TRUE)

  # Spec survives the build (load_configuration() round-trip)
  expect_equal(get_initial_conditions(aeme, model = "glm_aed")$profile$temperature,
               glm_prof$temperature)

  lake_dir <- get_lake_dir(aeme = aeme, path = path)
  glm_nml <- read_nml(find_glm_nml(file.path(lake_dir, "glm_aed")))
  expect_equal(glm_nml$init_profiles$the_temps, glm_prof$temperature)
  expect_equal(glm_nml$init_profiles$the_depths, glm_prof$depth)

  # Scalar wq folded into model_controls -> init_profiles$wq_init_vals
  oxy_idx <- match("OXY_oxy", glm_nml$init_profiles$wq_names)
  skip_if(is.na(oxy_idx))
  n_dep <- glm_nml$init_profiles$num_depths
  oxy_vals <- glm_nml$init_profiles$wq_init_vals[
    ((oxy_idx - 1) * n_dep + 1):(oxy_idx * n_dep)
  ]
  mc <- get_model_controls(use_bgc = TRUE)
  conv <- mc$conversion_aed[mc$var_aeme == "CHM_oxy"]
  expect_true(all(abs(oxy_vals - 275 * conv) < 1e-4))
})
