# A small GLM-like netCDF: 3 layers (bottom first), 2 zones, 2 days.
# Layer tops H = 1, 2, 3 m (1 m thick) and areas A = 100, 300, 600 m2, so the
# exposed sediment per layer is 100, 200 and 300 m2 and V = 100, 200, 300 m3.
make_glm_nc <- function(path) {
  d_z <- ncdf4::ncdim_def("z", "", 1:3)
  d_zone <- ncdf4::ncdim_def("nzones", "", 1:2)
  d_t <- ncdf4::ncdim_def("time", "hours since 2020-01-01 00:00:00", c(24, 48))
  v <- function(nm, dims) ncdf4::ncvar_def(nm, "", dims, missval = -999)
  vars <- list(
    v("H", list(d_z, d_t)), v("A", list(d_z, d_t)), v("V", list(d_z, d_t)),
    v("zarea", list(d_zone, d_t)), v("surface_area", list(d_t)),
    v("NIT_denit", list(d_z, d_t)),        # mmol N/m3/d
    v("OGM_pon_set", list(d_z, d_t)),      # settling, negative
    v("NIT_amm_dsf_Z", list(d_zone, d_t)), # mmol N/m2/d
    v("NIT_din_atm", list(d_t)),           # mmol N/m2/d
    v("PHY_upt_no3", list(d_z, d_t)),
    v("PHY_cyano_rsp_n", list(d_z, d_t)))  # AED stores respiration as negative
  nc <- ncdf4::nc_create(path, vars)
  on.exit(ncdf4::nc_close(nc))
  both <- function(x) matrix(rep(x, 2), ncol = 2)
  ncdf4::ncvar_put(nc, "H", both(1:3))
  ncdf4::ncvar_put(nc, "A", both(c(100, 300, 600)))
  ncdf4::ncvar_put(nc, "V", both(c(100, 200, 300)))
  ncdf4::ncvar_put(nc, "zarea", both(c(400, 200)))
  ncdf4::ncvar_put(nc, "surface_area", c(600, 600))
  ncdf4::ncvar_put(nc, "NIT_denit", both(c(1, 1, 1)))
  ncdf4::ncvar_put(nc, "OGM_pon_set", both(c(-0.5, -0.5, -0.5)))
  ncdf4::ncvar_put(nc, "NIT_amm_dsf_Z", both(c(2, 4)))
  ncdf4::ncvar_put(nc, "NIT_din_atm", c(0.1, 0.1))
  ncdf4::ncvar_put(nc, "PHY_upt_no3", both(c(0, 0, 2)))
  ncdf4::ncvar_put(nc, "PHY_cyano_rsp_n", both(c(0, 0, -1)))
  path
}

test_that("calc_glm_fluxes() integrates each kind of variable", {
  f <- make_glm_nc(withr::local_tempfile(fileext = ".nc"))
  fx <- calc_glm_fluxes(file = f, element = "N")
  val <- function(var) unique(fx$value[fx$var == var])

  expect_equal(val("NIT_denit"), 600)                  # sum(V * 1)
  expect_equal(val("OGM_pon_set"), -0.5 * (100 + 200 + 300))  # dz * dA
  expect_equal(val("NIT_amm_dsf_Z"), 2 * 400 + 4 * 200)       # zone area
  expect_equal(val("NIT_din_atm"), 0.1 * 600, tolerance = 1e-6)  # surface
  expect_equal(val("PHY_upt_no3"), 2 * 300)
  # respiration is stored negative by AED; the flux is phyto -> inorganic
  expect_equal(val("PHY_cyano_rsp_n"), 300)

  expect_equal(unique(fx$Date), as.Date(c("2020-01-02", "2020-01-03")))
  expect_equal(unique(fx$from[fx$var == "OGM_pon_set"]), "sediment")
  expect_equal(unique(fx$to[fx$var == "OGM_pon_set"]), "pom")
  expect_equal(attr(fx, "surface_area"), 600)
  # variables GLM did not write are listed, and with none at all it errors
  expect_true("^NIT_nitrif$" %in% attr(fx, "missing"))
  expect_error(calc_glm_fluxes(file = f, element = "P"),
               "No flux diagnostics")
})

test_that("the interface settling diagnostics are only used on request", {
  map <- glm_flux_map()
  expect_true(all(map$method %in% c("all", "hypsograph", "interface")))
  expect_true(all(map$kind %in% c("vol", "zone", "surface", "settle")))
  expect_true(all(map$element %in% c("C", "N", "P")))
  expect_true(all(map$from %in% c("inorganic", "phyto", "dom", "pom",
                                  "sediment", "atmosphere")))
  # no pattern is both a hypsograph and an interface source
  expect_false(any(grepl("_swi_Z", map$pattern[map$method == "hypsograph"])))
  expect_true(all(map$kind[map$method == "hypsograph"] == "settle"))
})

test_that("compartments are assigned from the variable name", {
  expect_equal(
    glm_var_compartment(c("NIT_amm", "PHS_frp", "OGM_don", "OGM_donr",
                          "OGM_pon", "OGM_cpom", "PHY_cyano_IN", "XXX_a")),
    c("inorganic", "inorganic", "dom", "dom", "pom", "pom", "phyto", NA))
  expect_false("inorganic" %in% glm_budget_pool("C"))
  expect_true("inorganic" %in% glm_budget_pool("N"))
})

test_that("the mass balance is moved onto the netCDF time axis", {
  tm <- seq(as.POSIXct("2020-01-01", tz = "UTC"), by = "day", length.out = 3)
  mb <- tibble::tibble(datetime = tm, var = "NIT_nit", term = "Lake",
                       value = 1:3)
  out <- glm_mb_to_output_time(mb)
  expect_equal(out$datetime, tm + 86400)
  expect_equal(out$value, mb$value)
})

test_that("glm_budget_compartments() closes a compartment exactly", {
  tm <- seq(as.POSIXct("2020-01-01", tz = "UTC"), by = "day", length.out = 4)
  # one N component in the inorganic box: stored +10 per day, with an inflow
  # of 4, an outflow of 1 and a net flux in from the other boxes of 5 + 3 - 2
  mb <- tibble::tibble(
    datetime = rep(tm, 3), var = "NIT_nit",
    term = rep(c("Inf", "Lake", "Out"), each = 4),
    value = c(rep(4, 4), 100 + 10 * 0:3, rep(1, 4)))
  mb <- glm_mb_to_output_time(mb)
  fx <- tibble::tibble(
    Date = rep(as.Date("2020-01-02") + 0:3, 3),
    element = "N", process = rep(c("a", "b", "c"), each = 4),
    from = rep(c("dom", "sediment", "inorganic"), each = 4),
    to = rep(c("inorganic", "inorganic", "phyto"), each = 4),
    var = "x", value = rep(c(5, 3, 2), each = 4))
  res <- glm_budget_compartments(
    mb, list(TN_vars = "NIT_nit", TN_varscale = 1), fx, "N")
  ino <- res[res$compartment == "inorganic", ]
  expect_equal(unique(ino$d_storage), 10)
  expect_equal(unique(ino$flux_in), 8)
  expect_equal(unique(ino$flux_out), 2)
  # 10 - (4 - 1 + 8 - 2) = 1 unexplained
  expect_equal(unique(ino$residual), 1)
})

test_that("diag_glm_budget() and plot_glm_budget() work on a GLM run", {
  skip_on_cran()
  skip_if_models_unavailable("glm_aed")
  aeme <- readRDS(system.file("extdata/aeme.rds", package = "AEME"))
  tmp <- withr::local_tempdir()
  vars_sim <- c("HYD_temp", "NIT_tn", "PHS_tp", "PHY_tchla")
  mc <- get_model_controls(use_bgc = TRUE)
  mc <- set_vars_sim(model_controls = mc, vars_sim = vars_sim)
  aeme <- suppressWarnings(set_sim_period(aeme, period = suggest_sim_period(aeme,
                                                           vars_sim = vars_sim)))
  aeme <- suppressWarnings(
    build_aeme(path = tmp, aeme = aeme, model = "glm_aed",
               model_controls = mc, ext_elev = 5, use_bgc = TRUE) |>
      run_aeme(verbose = FALSE))

  bud <- suppressWarnings(diag_glm_budget(aeme, path = tmp, element = c("N", "P", "C")))
  expect_s3_class(bud, "glm_budget")
  expect_named(bud, c("fluxes", "pools", "closure", "compartments", "daily"))
  # the default mass balance holds the TN, TP and TOC totals
  expect_setequal(unique(bud$closure$element), c("N", "P", "C"))
  expect_true("C" %in% bud$fluxes$element)
  expect_true(all(c("inflow", "outflow", "sediment", "atmosphere", "explained",
                    "d_storage", "residual") %in% bud$closure$term))
  # the residual is what the other terms do not explain
  n <- bud$closure[bud$closure$element == "N", ]
  g <- function(t) n$mmol_d[n$term == t]
  expect_equal(g("residual"), g("d_storage") - g("explained"))
  expect_equal(g("explained"),
               g("inflow") - g("outflow") + g("sediment") + g("atmosphere"))
  # every partner across the pool boundary is counted: for carbon the
  # exchange with DIC (photosynthesis, respiration) is by far the largest
  for (el in unique(bud$closure$element)) {
    cl <- bud$closure[bud$closure$element == el, ]
    ext <- setdiff(cl$term, c("inflow", "outflow", "explained", "d_storage",
                              "residual"))
    expect_equal(cl$mmol_d[cl$term == "explained"],
                 cl$mmol_d[cl$term == "inflow"] -
                   cl$mmol_d[cl$term == "outflow"] +
                   sum(cl$mmol_d[cl$term %in% ext]))
  }
  expect_true("inorganic" %in% bud$closure$term[bud$closure$element == "C"])
  cc <- bud$closure[bud$closure$element == "C", ]
  expect_lt(abs(cc$mmol_d[cc$term == "residual"]),
            max(abs(cc$mmol_d[cc$term == "inorganic"])))
  # the mass balance and netCDF inventories are the same state
  expect_true(all(bud$pools$kg >= 0))

  # settling from the hypsograph is far smaller than the interface terms
  nb_if <- suppressWarnings(diag_glm_budget(aeme, path = tmp, element = "N",
                                            settling = "interface"))
  settle <- function(b) sum(b$fluxes$mmol_d[b$fluxes$element == "N" &
                                              b$fluxes$from == "sediment" &
                                              b$fluxes$to == "pom"])
  expect_lt(abs(settle(bud)), abs(settle(nb_if)))

  p <- plot_glm_budget(bud)
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
  expect_s3_class(plot_glm_budget(bud, element = "N", units = "mmol_m2_d",
                                  show_residual = TRUE), "ggplot")
  expect_error(plot_glm_budget(bud, element = "Z"))
})
