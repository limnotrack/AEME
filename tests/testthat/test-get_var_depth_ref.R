# get_var(depth_ref = "bottom") measures up from the lake bed using the water
# level on each date. A synthetic Aeme with a known linear profile and a level
# that moves makes the right answers exact.

depth_ref_aeme <- function() {
  aeme <- readRDS(system.file("extdata/aeme.rds", package = "AEME"))
  dates <- as.Date("2021-01-01") + 0:19
  level <- seq(12, 14, length.out = length(dates))       # rises through the run
  frac <- seq(0.02, 0.98, length.out = 40)
  depths <- outer(frac, level)                           # below the surface
  temp <- 20 - 0.5 * depths                              # linear in depth
  output(aeme) <- list(
    n_members = 1L,
    ens_001 = list(glm_aed = list(Date = dates, LKE_lvlwtr = level,
                                  LKE_depths = depths, HYD_temp = temp)))
  list(aeme = aeme, dates = dates, level = level)
}

temp_at <- function(depth_below_surface) 20 - 0.5 * depth_below_surface

test_that("a surface depth is metres below the surface", {
  e <- depth_ref_aeme()
  v <- get_var(e$aeme, "glm_aed", "HYD_temp", depth = 3, remove_spin_up = FALSE)
  expect_equal(nrow(v), length(e$dates))
  expect_equal(v$value, rep(temp_at(3), length(e$dates)), tolerance = 1e-3)
  expect_true(all(v$depth == 3))
})

test_that("a bottom depth is metres above the bed, following the water level", {
  e <- depth_ref_aeme()
  v <- get_var(e$aeme, "glm_aed", "HYD_temp", depth = 1, depth_ref = "bottom",
               remove_spin_up = FALSE)
  # 1 m above the bed is (level - 1) m below the surface on every date
  expect_equal(v$value, temp_at(e$level - 1), tolerance = 1e-3)
  # ... which moves as the level rises, unlike a fixed datum
  expect_gt(diff(range(v$value)), 0.5)
  expect_true(all(v$depth == 1))
})

test_that("the bottom of the lake is read at the water level", {
  e <- depth_ref_aeme()
  v <- get_var(e$aeme, "glm_aed", "HYD_temp", depth = 0, depth_ref = "bottom",
               remove_spin_up = FALSE)
  # the deepest grid value, as the profile is held constant below the last layer
  deepest <- temp_at(0.98 * e$level)
  expect_equal(v$value, deepest, tolerance = 1e-3)
})

test_that("a height above the bed that is above the surface gives NA", {
  e <- depth_ref_aeme()
  # level runs 12 -> 14 m, so 13 m above the bed is out of the water until the
  # level reaches 13 m
  v <- get_var(e$aeme, "glm_aed", "HYD_temp", depth = 13, depth_ref = "bottom",
               remove_spin_up = FALSE)
  expect_true(all(is.na(v$value[e$level < 13])))
  expect_false(anyNA(v$value[e$level >= 13]))
  expect_gt(sum(is.na(v$value)), 0)
})

test_that("depth_ref is ignored when no depth is given", {
  e <- depth_ref_aeme()
  a <- get_var(e$aeme, "glm_aed", "HYD_temp", remove_spin_up = FALSE)
  b <- get_var(e$aeme, "glm_aed", "HYD_temp", depth_ref = "bottom",
               remove_spin_up = FALSE)
  expect_equal(a, b)
})

test_that("surface and bottom agree when the level is the full depth", {
  e <- depth_ref_aeme()
  full <- 13
  # with the level held at 13 m, 1 m above the bed is 12 m below the surface
  o <- output(e$aeme)
  o$ens_001$glm_aed$LKE_lvlwtr <- rep(full, length(e$dates))
  o$ens_001$glm_aed$LKE_depths <- outer(seq(0.02, 0.98, length.out = 40),
                                        rep(full, length(e$dates)))
  o$ens_001$glm_aed$HYD_temp <- temp_at(o$ens_001$glm_aed$LKE_depths)
  output(e$aeme) <- o
  bottom <- get_var(e$aeme, "glm_aed", "HYD_temp", depth = 1,
                    depth_ref = "bottom", remove_spin_up = FALSE)
  surface <- get_var(e$aeme, "glm_aed", "HYD_temp", depth = full - 1,
                     remove_spin_up = FALSE)
  expect_equal(bottom$value, surface$value, tolerance = 1e-6)
})
