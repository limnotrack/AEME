# Observations recorded relative to the lake bed. A synthetic Aeme with a known
# linear profile (temperature = 20 - 0.5 * depth) and chosen water levels makes
# every expected value exact.

dates <- as.Date("2021-01-01") + 0:19

# `levels`: modelled water level (height above the bed) of each ensemble member,
# held constant through the run
synthetic_aeme <- function(levels = 14) {
  aeme <- readRDS(system.file("extdata/aeme.rds", package = "AEME"))
  observations(aeme) <- list(lake = NULL, level = NULL)
  frac <- seq(0.02, 0.98, length.out = 40)
  members <- lapply(levels, function(L) {
    depths <- outer(frac, rep(L, length(dates)))
    list(glm_aed = list(Date = dates, LKE_lvlwtr = rep(L, length(dates)),
                        LKE_depths = depths, HYD_temp = 20 - 0.5 * depths))
  })
  names(members) <- sprintf("ens_%03d", seq_along(levels))
  output(aeme) <- c(list(n_members = length(levels)), members)
  aeme
}

temp_at <- function(depth) 20 - 0.5 * depth

# the lowest point of the hypsograph, which turns a level observation (an
# elevation) into a height above the bed
bed_elev <- function(aeme) min(input(aeme)$hypsograph$elev)

lake_df <- function(date, depth, value = 15, ...) {
  data.frame(Date = date, var_aeme = "HYD_temp", depth = depth, value = value, ...)
}

level_df <- function(aeme, date, height) {
  data.frame(Date = date, var_aeme = "LKE_lvlwtr", value = bed_elev(aeme) + height)
}

# ---- schema ------------------------------------------------------------------

test_that("depth_ref and height_above_bed are optional observation columns", {
  expect_false(any(c("depth_ref", "height_above_bed") %in% get_obs_column_names()))
  expect_true(all(c("depth_ref", "height_above_bed") %in%
                    get_obs_column_names(include_optional = TRUE)))
})

test_that("normalise_obs_depth_ref leaves observations without the column alone", {
  lk <- lake_df(dates[1:2], c(1, 2))
  expect_identical(normalise_obs_depth_ref(lk), lk)
  expect_null(normalise_obs_depth_ref(NULL))
})

test_that("normalise_obs_depth_ref fills, checks and requires heights", {
  lk <- lake_df(dates[1:2], c(1, 2), depth_ref = c(NA, "bottom"),
                height_above_bed = c(NA, 1))
  out <- normalise_obs_depth_ref(lk)
  expect_equal(out$depth_ref, c("surface", "bottom"))

  expect_error(normalise_obs_depth_ref(transform(lk, depth_ref = "middle")),
               class = "aeme_error_obs_depth_ref")

  # input to add_obs() has the height in `depth`, so a missing height_above_bed
  # is only an error for stored observations
  no_height <- lk
  no_height$height_above_bed[2] <- NA
  expect_no_error(normalise_obs_depth_ref(no_height))
  expect_error(normalise_obs_depth_ref(no_height, require_height = TRUE),
               class = "aeme_error_obs_depth_ref")
  negative <- lk
  negative$height_above_bed[2] <- -1
  expect_error(normalise_obs_depth_ref(negative, require_height = TRUE),
               class = "aeme_error_obs_depth_ref")
})

# ---- add_obs -----------------------------------------------------------------

test_that("add_obs adds no new columns for ordinary surface observations", {
  aeme <- synthetic_aeme()
  aeme <- add_obs(aeme, lake = lake_df(dates[1:2], c(1, 2)))
  expect_false(any(c("depth_ref", "height_above_bed") %in%
                     names(observations(aeme)$lake)))
  expect_equal(observations(aeme)$lake$depth, c(1, 2))
})

test_that("add_obs places a bottom observation below the observed level", {
  aeme <- synthetic_aeme()
  aeme <- add_obs(aeme, level = level_df(aeme, dates[5], 12))
  aeme <- add_obs(aeme, lake = lake_df(dates[5], 1), depth_ref = "bottom")

  lk <- observations(aeme)$lake
  expect_equal(lk$depth, 11)                       # 12 m of water - 1 m
  expect_equal(lk$height_above_bed, 1)
  expect_equal(lk$depth_ref, "bottom")
})

test_that("add_obs interpolates the observed level between level observations", {
  aeme <- synthetic_aeme()
  lvl <- rbind(level_df(aeme, dates[1], 12), level_df(aeme, dates[11], 14))
  aeme <- add_obs(aeme, level = lvl)
  aeme <- add_obs(aeme, lake = lake_df(dates[6], 1), depth_ref = "bottom")
  expect_equal(observations(aeme)$lake$depth, 13 - 1)      # level 13 on day 6
})

test_that("add_obs uses level observations passed in the same call", {
  aeme <- synthetic_aeme()
  aeme <- add_obs(aeme, lake = lake_df(dates[3], 2), level = level_df(aeme, dates[3], 10),
                  depth_ref = "bottom")
  expect_equal(observations(aeme)$lake$depth, 8)
})

test_that("add_obs without level observations falls back to the full basin, with a warning", {
  aeme <- synthetic_aeme()
  full <- basin_full_depth(aeme)
  expect_warning(
    aeme <- add_obs(aeme, lake = lake_df(dates[3], 1), depth_ref = "bottom"),
    class = "aeme_warn_obs_bottom_no_level")
  expect_equal(observations(aeme)$lake$depth, full - 1)
  expect_equal(observations(aeme)$lake$height_above_bed, 1)
})

test_that("a height above the bed greater than the water level is placed at the surface", {
  aeme <- synthetic_aeme()
  aeme <- add_obs(aeme, level = level_df(aeme, dates[5], 5))
  expect_warning(
    aeme <- add_obs(aeme, lake = lake_df(dates[5], 8), depth_ref = "bottom"),
    class = "aeme_warn_obs_bottom_above_surface")
  expect_equal(observations(aeme)$lake$depth, 0)
  expect_equal(observations(aeme)$lake$height_above_bed, 8)
})

test_that("depth_ref can be set per row, and earlier rows become surface", {
  aeme <- synthetic_aeme()
  aeme <- add_obs(aeme, lake = lake_df(dates[2], 3))                  # existing
  aeme <- add_obs(aeme, level = level_df(aeme, dates[5], 12))
  new <- lake_df(c(dates[5], dates[5]), c(2, 1),
                 depth_ref = c("surface", "bottom"))
  aeme <- add_obs(aeme, lake = new)

  lk <- observations(aeme)$lake
  lk <- lk[order(lk$Date, lk$depth), ]
  expect_equal(lk$depth_ref, c("surface", "surface", "bottom"))
  expect_equal(lk$depth, c(3, 2, 11))
  expect_equal(lk$height_above_bed, c(NA, NA, 1))
})

test_that("add_obs rejects an unknown depth_ref", {
  aeme <- synthetic_aeme()
  expect_error(add_obs(aeme, lake = lake_df(dates[1], 1), depth_ref = "middle"))
  expect_error(add_obs(aeme, lake = lake_df(dates[1], 1, depth_ref = "middle")),
               class = "aeme_error_obs_depth_ref")
})

# ---- get_var(use_obs = TRUE) ---------------------------------------------------

test_that("get_var compares a bottom observation with the model at its own level", {
  # the model's level is 14 m but the logger was placed using an observed level
  # of 12 m, so its stored depth (11 m) is not where the model should be read
  aeme <- synthetic_aeme(levels = 14)
  aeme <- add_obs(aeme, level = level_df(aeme, dates[5], 12))
  aeme <- add_obs(aeme, lake = lake_df(dates[5], 1, value = 14), depth_ref = "bottom")
  aeme <- add_obs(aeme, lake = lake_df(dates[5], 2, value = 19))        # surface
  expect_equal(observations(aeme)$lake$depth[observations(aeme)$lake$depth_ref == "bottom"], 11)

  v <- get_var(aeme, "glm_aed", "HYD_temp", use_obs = TRUE, remove_spin_up = FALSE)
  v <- v[order(v$depth), ]
  expect_equal(nrow(v), 2)

  surf <- v[v$depth_ref == "surface", ]
  bot <- v[v$depth_ref == "bottom", ]
  expect_equal(surf$value, temp_at(2), tolerance = 1e-3)
  # 1 m above the bed in a 14 m column is 13 m down, not the stored 11 m
  expect_equal(bot$value, temp_at(14 - 1), tolerance = 1e-3)
  expect_false(isTRUE(all.equal(bot$value, temp_at(11), tolerance = 1e-3)))
  expect_equal(bot$obs, 14)
})

test_that("get_var is unchanged for surface-only observations", {
  aeme <- synthetic_aeme(levels = 14)
  aeme <- add_obs(aeme, lake = lake_df(dates[c(3, 8)], c(1, 4), value = c(15, 16)))
  v <- get_var(aeme, "glm_aed", "HYD_temp", use_obs = TRUE, remove_spin_up = FALSE)
  expect_equal(nrow(v), 2)
  expect_equal(v$value, temp_at(c(1, 4)), tolerance = 1e-3)
  expect_false("depth_ref" %in% names(v))
})

test_that("get_var handles observations that are all bottom-referenced", {
  aeme <- synthetic_aeme(levels = 14)
  aeme <- add_obs(aeme, level = level_df(aeme, dates[5], 14))
  aeme <- add_obs(aeme, lake = lake_df(dates[5], c(1, 3), value = 13),
                  depth_ref = "bottom")
  v <- get_var(aeme, "glm_aed", "HYD_temp", use_obs = TRUE, remove_spin_up = FALSE)
  v <- v[order(v$height_above_bed), ]
  expect_equal(v$value, temp_at(14 - c(1, 3)), tolerance = 1e-3)
})

# ---- ensemble_summary() observation alignment ----------------------------------

test_that("ensemble_summary matches observations to depths below the surface", {
  aeme <- synthetic_aeme(levels = c(13, 14, 15))
  aeme <- add_obs(aeme, level = level_df(aeme, dates[5], 12))
  aeme <- add_obs(aeme, lake = lake_df(dates[5], 2, value = 18))             # surface
  aeme <- add_obs(aeme, lake = lake_df(dates[5], 1, value = 19), depth_ref = "bottom")

  s <- ensemble_summary(aeme, "glm_aed", vars_sim = "HYD_temp", depths = c(2, 11),
                        remove_spin_up = FALSE, depth_ref = "surface")
  ob <- s$obs[order(s$obs$depth), ]
  # the surface observation sits at 2 m; 1 m above the bed in a 12 m water
  # column (the observed level that day) is 11 m down
  expect_equal(ob$depth, c(2, 11))
  expect_equal(ob$value, c(18, 19))
})

test_that("ensemble_summary matches observations to heights above the bed", {
  aeme <- synthetic_aeme(levels = c(13, 14, 15))
  aeme <- add_obs(aeme, level = level_df(aeme, dates[5], 12))
  aeme <- add_obs(aeme, lake = lake_df(dates[5], 2, value = 18))             # surface
  aeme <- add_obs(aeme, lake = lake_df(dates[5], 1, value = 19), depth_ref = "bottom")

  s <- ensemble_summary(aeme, "glm_aed", vars_sim = "HYD_temp", depths = c(1, 10),
                        remove_spin_up = FALSE, depth_ref = "bottom")
  ob <- s$obs[order(s$obs$depth), ]
  # the bottom observation is at 1 m above the bed as recorded; the surface
  # observation 2 m down in 12 m of water is 10 m above the bed
  expect_equal(ob$depth, c(1, 10))
  expect_equal(ob$value, c(19, 18))
})

test_that("without level observations the modelled median level is used to convert", {
  aeme <- synthetic_aeme(levels = c(12, 13, 14))          # median level 13 m
  aeme <- add_obs(aeme, lake = lake_df(dates[5], 2, value = 18))
  s <- ensemble_summary(aeme, "glm_aed", vars_sim = "HYD_temp", depths = 11,
                        remove_spin_up = FALSE, depth_ref = "bottom")
  # 2 m below the surface of a 13 m column is 11 m above the bed
  expect_equal(s$obs$depth, 11)
  expect_equal(s$obs$value, 18)
})

test_that("surface summaries of surface observations are unchanged", {
  aeme <- synthetic_aeme(levels = c(13, 14))
  aeme <- add_obs(aeme, lake = lake_df(dates[c(4, 9)], c(2, 6), value = c(18, 16)))
  s <- ensemble_summary(aeme, "glm_aed", vars_sim = "HYD_temp", depths = c(2, 6),
                        remove_spin_up = FALSE)
  ob <- s$obs[order(s$obs$depth), ]
  expect_equal(ob$depth, c(2, 6))
  expect_equal(ob$value, c(18, 16))
})

# ---- get_var() returns the same columns with and without observations ----------

test_that("get_var returns consistent leading columns for use_obs = TRUE/FALSE", {
  aeme <- synthetic_aeme(levels = 14)
  aeme <- add_obs(aeme, level = level_df(aeme, dates[c(3, 8)], 14))
  aeme <- add_obs(aeme, lake = lake_df(dates[c(3, 8)], c(1, 4), value = c(15, 16)))
  core <- c("Date", "depth", "value", "var_sim", "Model")

  no_obs <- get_var(aeme, "glm_aed", "HYD_temp", remove_spin_up = FALSE)
  with_obs <- get_var(aeme, "glm_aed", "HYD_temp", use_obs = TRUE,
                      remove_spin_up = FALSE)
  expect_identical(names(no_obs), core)
  expect_identical(names(with_obs), c("Date", "depth", "value", "obs",
                                      "var_sim", "Model"))
  expect_false(anyNA(with_obs$Model))

  # level has no depth dimension but still gets the column
  lvl <- get_var(aeme, "glm_aed", "LKE_lvlwtr", use_obs = TRUE,
                 remove_spin_up = FALSE)
  expect_true(all(c(core, "obs") %in% names(lvl)))
  expect_true(all(is.na(lvl$depth)))
  expect_identical(names(get_var(aeme, "glm_aed", "LKE_lvlwtr",
                                 remove_spin_up = FALSE)), core)
})
