make_glm_cfg <- function() {
  nml_file <- system.file("extdata/glm_aed/glm4.nml", package = "AEME")
  aed <- list(aed_sed_const2d = list(n_zones = 3L,
                                     fsed_oxy = c(-25, -25, -25),
                                     fsed_amm = c(2, 2, 2)))
  class(aed) <- "nml"
  list(
    hydrodynamic = read_nml(nml_file),
    hydrodynamic_file = "glm4.nml",
    bgc = list(
      aed = aed,
      aed_phyto_pars = tibble::tibble(p_name = c("p_initial", "R_growth"),
                                      cyano = c(1, 2), diatom = c(3, 4))
    )
  )
}

make_param <- function(file, name, value, group = NA_character_, index = NA) {
  data.frame(model = "glm_aed", file = file, name = name, value = value,
             min = value / 2, max = value * 2, group = group, index = index)
}

test_that("apply_parameters() sets nml values and reports the touched file", {
  cfg <- make_glm_cfg()
  res <- apply_parameters(cfg, make_param("glm4.nml", "light/Kw", 1.5),
                          model = "glm_aed")
  testthat::expect_equal(res$config$hydrodynamic$light$Kw, 1.5)
  testthat::expect_equal(res$touched, "glm4.nml")
})

test_that("apply_parameters() is pure and idempotent", {
  cfg <- make_glm_cfg()
  orig <- cfg
  p <- make_param("glm4.nml", "light/Kw", 1.5)
  res1 <- apply_parameters(cfg, p, model = "glm_aed")
  testthat::expect_identical(cfg, orig)
  res2 <- apply_parameters(res1$config, p, model = "glm_aed")
  testthat::expect_identical(res1$config, res2$config)
})

test_that("apply_parameters() routes any glm<version>.nml label to the config's nml", {
  cfg <- make_glm_cfg()
  p <- dplyr::bind_rows(make_param("glm3.nml", "light/Kw", 1.5),
                        make_param("glm4.nml", "light/Kw", 1.5))
  res <- apply_parameters(cfg, p, model = "glm_aed")
  testthat::expect_equal(res$config$hydrodynamic$light$Kw, 1.5)
  testthat::expect_equal(res$touched, "glm4.nml")
})

test_that("apply_parameters() updates a single group in an AED csv table", {
  cfg <- make_glm_cfg()
  res <- apply_parameters(
    cfg, make_param("aed_phyto_pars.csv", "p_initial", 25, group = "cyano"),
    model = "glm_aed")
  df <- res$config$bgc$aed_phyto_pars
  testthat::expect_equal(df$cyano[df$p_name == "p_initial"], 25)
  testthat::expect_equal(df$diatom[df$p_name == "p_initial"], 3)
  testthat::expect_equal(res$touched, "aed_phyto_pars.csv")
})

test_that("apply_parameters() recycles scalar sediment fluxes to n_zones", {
  cfg <- make_glm_cfg()
  res <- apply_parameters(
    cfg, make_param("aed.nml", "aed_sed_const2d/fsed_oxy", -10),
    model = "glm_aed")
  testthat::expect_equal(res$config$bgc$aed$aed_sed_const2d$fsed_oxy,
                         rep(-10, 3))
})

test_that("apply_parameters() warns on keys that are not in the file", {
  cfg <- make_glm_cfg()
  testthat::expect_warning(
    apply_parameters(cfg, make_param("glm4.nml", "light/not_a_key", 1),
                     model = "glm_aed"),
    "not found")
})

test_that("apply_parameters() ignores boundary scaling rows and other models", {
  cfg <- make_glm_cfg()
  p <- dplyr::bind_rows(
    make_param("met", "MET_wndspd", 1.1),
    transform(make_param("gotm.yaml", "light/Kw", 1), model = "gotm_wet")
  )
  res <- apply_parameters(cfg, p, model = "glm_aed")
  testthat::expect_identical(res$config, cfg)
  testthat::expect_length(res$touched, 0)
})

test_that("apply_parameters() errors when the target file is not in the config", {
  cfg <- make_glm_cfg()
  cfg$bgc$aed_phyto_pars <- NULL
  testthat::expect_error(
    apply_parameters(
      cfg, make_param("aed_phyto_pars.csv", "p_initial", 25, group = "cyano"),
      model = "glm_aed"),
    "not in the model configuration")
})

test_that("apply_parameters() rejects an invalid or multiple models", {
  p <- make_param("glm4.nml", "a/b", 1)
  testthat::expect_error(apply_parameters(list(), p, model = "not_a_model"))
  testthat::expect_error(
    apply_parameters(list(), p, model = c("glm_aed", "gotm_wet")),
    "one model")
})

# Boundary-condition scaling ----
make_bnd_param <- function(file, name, value, model = "glm_aed") {
  data.frame(model = model, file = file, name = name, value = value,
             min = value / 2, max = value * 2, group = NA_character_,
             index = NA)
}

test_that("apply_boundary_params() scales met, with wind and cloud rules", {
  met <- data.frame(MET_wndspd = c(2, 4), MET_wnduvu = c(1, 2),
                    MET_wnduvv = c(1, 2), MET_cldcvr = c(0.5, 0.9),
                    MET_radswd = c(100, 200))
  p <- dplyr::bind_rows(make_bnd_param("met", "MET_wndspd", 1.5),
                        make_bnd_param("met", "MET_cldcvr", 1.5),
                        make_bnd_param("met", "MET_radswd", 0.5))
  res <- apply_boundary_params(met, outflows = NULL, param = p)
  testthat::expect_equal(res$meteo$MET_wndspd, c(3, 6))
  testthat::expect_equal(res$meteo$MET_wnduvu, c(1.5, 3))
  testthat::expect_equal(res$meteo$MET_wnduvv, c(1.5, 3))
  testthat::expect_equal(res$meteo$MET_cldcvr, c(0.75, 1))
  testthat::expect_equal(res$meteo$MET_radswd, c(50, 100))
  testthat::expect_null(res$outflows)
  testthat::expect_null(res$inf_factor)
  # input untouched
  testthat::expect_equal(met$MET_wndspd, c(2, 4))
})

test_that("apply_boundary_params() scales outflows and returns the inflow factor", {
  outf <- list(wbal = data.frame(outflow = c(10, 20)),
               outlet1 = data.frame(HYD_flow = c(1, 2)))
  p <- dplyr::bind_rows(make_bnd_param("wdr", "outflow", 0.5),
                        make_bnd_param("inf", "inflow", 1.3))
  res <- apply_boundary_params(meteo = NULL, outflows = outf, param = p)
  testthat::expect_equal(res$outflows$wbal$outflow, c(5, 10))
  testthat::expect_equal(res$outflows$outlet1$HYD_flow, c(0.5, 1))
  testthat::expect_equal(res$inf_factor, 1.3)
  testthat::expect_null(res$meteo)
  testthat::expect_equal(outf$wbal$outflow, c(10, 20))
})

test_that("apply_boundary_params() returns NULLs when there are no boundary rows", {
  p <- make_param("glm4.nml", "light/Kw", 1.5)
  res <- apply_boundary_params(meteo = NULL, outflows = NULL, param = p)
  testthat::expect_null(res$meteo)
  testthat::expect_null(res$outflows)
  testthat::expect_null(res$inf_factor)
})

test_that("write_aed_param_csv() keeps small values and strips float noise", {
  df <- data.frame(p_name = "k", cyano = 1.2345678e-5, diatom = 0.1 + 0.2)
  f <- tempfile(fileext = ".csv")
  write_aed_param_csv(df, f)
  out <- read_aed_param_csv(f)
  testthat::expect_equal(out$cyano, 1.23457e-5)
  testthat::expect_identical(out$diatom, 0.3)
})

# write_configuration() ----
test_that("write_configuration() applies parameters at write time only", {
  path <- file.path(tempdir(), "wc_params")
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  aeme <- suppressWarnings(suppressMessages(
    yaml_to_aeme(path = aeme_dir, "aeme.yaml")))
  aeme <- suppressWarnings(suppressMessages(
    build_aeme(path = path, aeme = aeme, model = "glm_aed", ext_elev = 3,
               model_controls = get_model_controls(), use_bgc = FALSE)))

  p <- dplyr::bind_rows(
    make_param("glm4.nml", "light/Kw", 0.77),
    make_bnd_param("met", "MET_wndspd", 1.2),
    make_bnd_param("inf", "inflow", 1.3)
  )
  parameters(aeme) <- p
  before <- list(input = input(aeme), config = configuration(aeme),
                 inflows = inflows(aeme), outflows = outflows(aeme))

  out_on <- file.path(tempdir(), "wc_on")
  out_off <- file.path(tempdir(), "wc_off")
  write_configuration(aeme, model = "glm_aed", path = out_on)
  write_configuration(aeme, model = "glm_aed", path = out_off,
                      apply_params = FALSE)

  # The aeme object is untouched
  testthat::expect_identical(input(aeme), before$input)
  testthat::expect_identical(configuration(aeme), before$config)
  testthat::expect_equal(inflows(aeme), before$inflows)
  testthat::expect_equal(outflows(aeme), before$outflows)

  # Config parameter reaches disk only when applied
  cfg_on <- read_model_config("glm_aed", get_lake_dir(aeme, out_on))
  cfg_off <- read_model_config("glm_aed", get_lake_dir(aeme, out_off))
  testthat::expect_equal(cfg_on$hydrodynamic$light$Kw, 0.77)
  testthat::expect_false(isTRUE(cfg_off$hydrodynamic$light$Kw == 0.77))

  # So does the boundary scaling
  met_on <- read.csv(file.path(get_lake_dir(aeme, out_on), "glm_aed", "bcs",
                               "meteo_glm.csv"))
  met_off <- read.csv(file.path(get_lake_dir(aeme, out_off), "glm_aed", "bcs",
                                "meteo_glm.csv"))
  testthat::expect_equal(mean(met_on$WindSpeed) / mean(met_off$WindSpeed), 1.2,
                         tolerance = 1e-3)
})

test_that("write_configuration() skips parameters for files it doesn't have", {
  path <- file.path(tempdir(), "wc_skip")
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  aeme <- suppressWarnings(suppressMessages(
    yaml_to_aeme(path = aeme_dir, "aeme.yaml")))
  aeme <- suppressWarnings(suppressMessages(
    build_aeme(path = path, aeme = aeme, model = "glm_aed", ext_elev = 3,
               model_controls = get_model_controls(), use_bgc = FALSE)))
  parameters(aeme) <- dplyr::bind_rows(
    make_param("glm4.nml", "light/Kw", 0.77),
    make_param("aed2.nml", "aed2_x/k", 1))
  out <- file.path(tempdir(), "wc_skip_out")
  testthat::expect_warning(
    write_configuration(aeme, model = "glm_aed", path = out), "not in the model")
  cfg <- read_model_config("glm_aed", get_lake_dir(aeme, out))
  testthat::expect_equal(cfg$hydrodynamic$light$Kw, 0.77)
})

# aed_totals and effective_configuration() ----
make_totals_cfg <- function(with_totals = TRUE) {
  aed <- list(aed_phytoplankton = list(the_phytos = c(1, 2)),
              aed_models = list(models = "aed_phytoplankton"))
  if (with_totals) aed$aed_totals <- list(TN_vars = "placeholder")
  class(aed) <- "nml"
  list(
    hydrodynamic = list(light = list(Kw = 1)),
    hydrodynamic_file = "glm4.nml",
    bgc = list(
      aed = aed,
      aed_phyto_pars = tibble::tibble(
        p_name = c("X_ncon", "X_pcon", "simINDynamics", "simIPDynamics"),
        cyano = c(0.1, 0.01, 1, 0), diatom = c(0.2, 0.02, 0, 0))
    )
  )
}

test_that("derive_aed_totals() uses internal pools or biomass x quota per group", {
  res <- derive_aed_totals(make_totals_cfg(with_totals = FALSE))
  tot <- res$bgc$aed$aed_totals
  # cyano has dynamic internal N (simINDynamics = 1), diatom uses fixed N:C
  testthat::expect_true("PHY_cyano_IN" %in% tot$TN_vars)
  testthat::expect_true("PHY_diatom" %in% tot$TN_vars)
  testthat::expect_equal(tot$TN_varscale[tot$TN_vars == "PHY_diatom"], 0.2)
  # neither group has dynamic P: biomass x fixed P:C
  testthat::expect_equal(tot$TP_varscale[tot$TP_vars == "PHY_cyano"], 0.01)
})

test_that("apply_parameters() re-derives aed_totals when phyto parameters change", {
  cfg <- make_totals_cfg()
  p <- dplyr::bind_rows(
    make_param("aed_phyto_pars.csv", "simINDynamics", 0, group = "cyano"),
    make_param("aed_phyto_pars.csv", "X_ncon", 0.5, group = "cyano"))
  res <- apply_parameters(cfg, p, model = "glm_aed")
  tot <- res$config$bgc$aed$aed_totals
  testthat::expect_true("PHY_cyano" %in% tot$TN_vars)
  testthat::expect_false("PHY_cyano_IN" %in% tot$TN_vars)
  testthat::expect_equal(tot$TN_varscale[tot$TN_vars == "PHY_cyano"], 0.5)
  testthat::expect_true("aed.nml" %in% res$touched)
})

test_that("apply_parameters() leaves aed_totals alone when there are none", {
  cfg <- make_totals_cfg(with_totals = FALSE)
  res <- apply_parameters(
    cfg, make_param("aed_phyto_pars.csv", "X_ncon", 0.5, group = "cyano"),
    model = "glm_aed")
  testthat::expect_null(res$config$bgc$aed$aed_totals)
  testthat::expect_false("aed.nml" %in% res$touched)
})

test_that("effective_configuration() applies parameters without changing aeme", {
  aeme <- new_aeme()
  cfg <- list(glm_aed = make_glm_cfg())
  configuration(aeme) <- cfg
  parameters(aeme) <- dplyr::bind_rows(
    make_param("glm4.nml", "light/Kw", 0.77),
    make_bnd_param("met", "MET_wndspd", 1.2))
  eff <- effective_configuration(aeme, model = "glm_aed")
  testthat::expect_equal(eff$glm_aed$hydrodynamic$light$Kw, 0.77)
  testthat::expect_identical(configuration(aeme)$glm_aed, cfg$glm_aed)
  testthat::expect_false(isTRUE(
    configuration(aeme)$glm_aed$hydrodynamic$light$Kw == 0.77))
})

test_that("effective_configuration() returns the configuration when there are no parameters", {
  aeme <- new_aeme()
  configuration(aeme) <- list(glm_aed = make_glm_cfg())
  testthat::expect_identical(
    effective_configuration(aeme, model = "glm_aed")$glm_aed,
    configuration(aeme)$glm_aed)
})

# Boundary files for every model ----
test_that("write_configuration() writes scaled boundary files for every model", {
  mods <- c("glm_aed", "gotm_wet", "dy_cd", "simstrat_aed2", "simstrat_aed")
  path <- file.path(tempdir(), "wc_all_models")
  aeme <- suppressWarnings(suppressMessages(
    yaml_to_aeme(path = system.file("extdata/lake/", package = "AEME"),
                 "aeme.yaml")))
  aeme <- suppressWarnings(suppressMessages(
    build_aeme(path = path, aeme = aeme, model = mods, ext_elev = 3,
               model_controls = get_model_controls(), use_bgc = FALSE)))

  p <- aeme_parameters[aeme_parameters$model %in% mods, ]
  p$value[p$name == "MET_wndspd"] <- 1.2
  p$value[p$name == "inflow"] <- 1.3
  p$value[p$name == "outflow"] <- 0.8
  parameters(aeme) <- p
  before <- list(input = input(aeme), inflows = inflows(aeme),
                 outflows = outflows(aeme))

  out_on <- file.path(tempdir(), "wc_all_on")
  out_off <- file.path(tempdir(), "wc_all_off")
  suppressWarnings(suppressMessages(
    write_configuration(aeme, model = mods, path = out_on)))
  suppressWarnings(suppressMessages(
    write_configuration(aeme, model = mods, path = out_off,
                        apply_params = FALSE)))

  # Nothing held in the aeme object changed
  testthat::expect_identical(input(aeme), before$input)
  testthat::expect_equal(inflows(aeme), before$inflows)
  testthat::expect_equal(outflows(aeme), before$outflows)

  for (m in mods) {
    d_on <- file.path(get_lake_dir(aeme, out_on), m)
    d_off <- file.path(get_lake_dir(aeme, out_off), m)
    files <- list.files(d_off, recursive = TRUE)
    testthat::expect_setequal(files, list.files(d_on, recursive = TRUE))
    # The meteorology file is written for the model...
    met_files <- files[grepl("meteo|Meteo|[.]met$", files)]
    testthat::expect_gt(length(met_files), 0)
    # ... and scaled when the parameters are applied (the shipped table has
    # no simstrat_aed boundary rows, so nothing to scale for that model)
    if (!any(p$model == m & p$file == "met")) next
    changed <- vapply(met_files, function(f) {
      !identical(unname(tools::md5sum(file.path(d_on, f))),
                 unname(tools::md5sum(file.path(d_off, f))))
    }, logical(1))
    testthat::expect_true(any(changed), info = m)
  }
})

test_that("write_configuration() reproduces a build's boundary files without parameters", {
  mods <- c("gotm_wet", "simstrat_aed2")
  path <- file.path(tempdir(), "wc_repro")
  aeme <- suppressWarnings(suppressMessages(
    yaml_to_aeme(path = system.file("extdata/lake/", package = "AEME"),
                 "aeme.yaml")))
  aeme <- suppressWarnings(suppressMessages(
    build_aeme(path = path, aeme = aeme, model = mods, ext_elev = 3,
               model_controls = get_model_controls(), use_bgc = FALSE)))
  out <- file.path(tempdir(), "wc_repro_out")
  suppressWarnings(suppressMessages(
    write_configuration(aeme, model = mods, path = out)))
  for (m in mods) {
    d_a <- file.path(get_lake_dir(aeme, path), m)
    d_b <- file.path(get_lake_dir(aeme, out), m)
    written <- list.files(d_b, recursive = TRUE)
    written <- written[grepl("meteo|Meteo|Qinp|Tinp|Sinp|Qout|inflow|outflow|inputs/",
                             written)]
    testthat::expect_gt(length(written), 0)
    same <- vapply(written, function(f) {
      identical(unname(tools::md5sum(file.path(d_a, f))),
                unname(tools::md5sum(file.path(d_b, f))))
    }, logical(1))
    testthat::expect_true(all(same), info = paste(m, names(same)[!same],
                                                  collapse = ", "))
  }
})

test_that("apply_parameters() sets Simstrat-AED phyto csv parameters", {
  cfg <- list(
    hydrodynamic = list(),
    bgc = list(aed_phyto_pars = tibble::tibble(
      p_name = c("p_initial", "R_growth"), cyano = c(1, 2), diatom = c(3, 4)))
  )
  p <- transform(make_param("aed_phyto_pars.csv", "p_initial", 25,
                            group = "cyano"), model = "simstrat_aed")
  res <- apply_parameters(cfg, p, model = "simstrat_aed")
  df <- res$config$bgc$aed_phyto_pars
  testthat::expect_equal(df$cyano[df$p_name == "p_initial"], 25)
  testthat::expect_equal(df$diatom[df$p_name == "p_initial"], 3)
  testthat::expect_equal(res$touched, "aed_phyto_pars.csv")
  # Simstrat-AED2's phyto parameters are nml-based and still unsupported
  p2 <- transform(make_param("aed2_phyto_pars.nml", "x/y", 1),
                  model = "simstrat_aed2")
  testthat::expect_warning(
    apply_parameters(list(hydrodynamic = list()), p2, model = "simstrat_aed2"),
    "not supported")
})

# Other models ----
mk_param <- function(model, file, name, value) {
  data.frame(model = model, file = file, name = name, value = value,
             min = value / 2, max = value * 2, group = NA_character_,
             index = NA)
}

test_that("apply_parameters() sets nested gotm_wet yaml values at any depth", {
  cfg <- list(
    hydrodynamic = list(gotm = list(light = list(extinct = list(g2 = list(
      constant_value = 1)))), output = list()),
    bgc = list(fabm = list(instances = list(a = list(parameters = list(k = 1)))))
  )
  p <- dplyr::bind_rows(
    mk_param("gotm_wet", "gotm.yaml", "light/extinct/g2/constant_value", 2),
    mk_param("gotm_wet", "fabm.yaml", "instances/a/parameters/k", 3)
  )
  res <- apply_parameters(cfg, p, model = "gotm_wet")
  testthat::expect_equal(
    res$config$hydrodynamic$gotm$light$extinct$g2$constant_value, 2)
  testthat::expect_equal(res$config$bgc$fabm$instances$a$parameters$k, 3)
  testthat::expect_setequal(res$touched, c("gotm.yaml", "fabm.yaml"))
  testthat::expect_equal(
    cfg$hydrodynamic$gotm$light$extinct$g2$constant_value, 1)
})

test_that("apply_parameters() warns on gotm_wet keys that don't exist", {
  cfg <- list(hydrodynamic = list(gotm = list(light = list(a = 1))))
  testthat::expect_warning(
    apply_parameters(cfg, mk_param("gotm_wet", "gotm.yaml", "light/b", 2),
                     model = "gotm_wet"),
    "not found")
})

test_that("apply_parameters() handles simstrat par and nml parameters", {
  aed <- list(aed2_x = list(k = 1)); class(aed) <- "nml"
  cfg <- list(hydrodynamic = list(ModelParameters = list(f_wind = 1)),
              bgc = list(aed2 = aed))
  p <- dplyr::bind_rows(
    mk_param("simstrat_aed2", "simstrat.par", "ModelParameters/f_wind", 0.5),
    mk_param("simstrat_aed2", "aed2.nml", "aed2_x/k", 7)
  )
  res <- apply_parameters(cfg, p, model = "simstrat_aed2")
  testthat::expect_equal(res$config$hydrodynamic$ModelParameters$f_wind, 0.5)
  testthat::expect_equal(res$config$bgc$aed2$aed2_x$k, 7)
  testthat::expect_setequal(res$touched, c("simstrat.par", "aed2.nml"))
})

test_that("apply_parameters() uses aed.nml for simstrat_aed", {
  aed <- list(aed_x = list(k = 1)); class(aed) <- "nml"
  cfg <- list(hydrodynamic = list(), bgc = list(aed = aed))
  res <- apply_parameters(cfg, mk_param("simstrat_aed", "aed.nml", "aed_x/k", 4),
                          model = "simstrat_aed")
  testthat::expect_equal(res$config$bgc$aed$aed_x$k, 4)
  testthat::expect_error(
    apply_parameters(list(hydrodynamic = list()),
                     mk_param("simstrat_aed", "aed.nml", "aed_x/k", 4),
                     model = "simstrat_aed"),
    "not in|No ")
})

test_that("apply_parameters() edits DYRESM par and cfg lines by line number", {
  cfg <- list(hydrodynamic = list(par = c("1.0 # a", "2.0 # b"),
                                  cfg = c("5 # light", "6 # layer")))
  p <- dplyr::bind_rows(mk_param("dy_cd", "dyresm3p1.par", "thing/2", 9),
                        mk_param("dy_cd", "cfg", "light/1", 8))
  res <- apply_parameters(cfg, p, model = "dy_cd")
  testthat::expect_match(res$config$hydrodynamic$par[2], "^9 #")
  testthat::expect_equal(res$config$hydrodynamic$par[1], "1.0 # a")
  testthat::expect_match(res$config$hydrodynamic$cfg[1], "^8 #")
  testthat::expect_equal(res$config$hydrodynamic$cfg[2], "6 # layer")
  testthat::expect_setequal(res$touched, c("par", "cfg"))
})

test_that("apply_parameters() warns about unsupported files", {
  cfg <- list(hydrodynamic = list(par = "1 # a", cfg = "1 # b"))
  testthat::expect_warning(
    apply_parameters(cfg, mk_param("dy_cd", "caedym3p1.bio", "x/1", 1),
                     model = "dy_cd"),
    "not supported")
})
