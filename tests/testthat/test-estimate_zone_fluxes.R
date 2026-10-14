test_that("normalise_zone_flux matches the baseline area-weighted total", {
  raw <- c(-40, -25, -10)
  area_frac <- c(0.2, 0.3, 0.5)
  out <- normalise_zone_flux(raw, base_val = -25, area_frac = area_frac)
  expect_equal(sum(out * area_frac), -25 * sum(area_frac))
  # inter-zone ratios are preserved
  expect_equal(out / out[1], raw / raw[1])
})

test_that("normalise_zone_flux keeps the total after a Tier 2 style adjustment", {
  area_frac <- c(0.25, 0.25, 0.5)
  amm <- normalise_zone_flux(c(3, 2, 1), 2, area_frac)
  adjusted <- amm * c(1.5, 1, 0.5)
  expect_false(isTRUE(all.equal(sum(adjusted * area_frac), 2)))
  renorm <- normalise_zone_flux(adjusted, 2, area_frac)
  expect_equal(sum(renorm * area_frac), 2)
  # the adjustment still changes the shares between zones
  expect_false(isTRUE(all.equal(renorm / renorm[1], amm / amm[1])))
})

test_that("normalise_zone_flux leaves an all-zero flux alone", {
  expect_equal(normalise_zone_flux(c(0, 0), 2, c(0.5, 0.5)), c(0, 0))
})

test_that("normalise_zone_flux scales a positive baseline and area subset", {
  out <- normalise_zone_flux(c(1, 2), base_val = 4, area_frac = c(0.25, 0.25))
  expect_equal(sum(out * c(0.25, 0.25)), 4 * 0.5)
})
