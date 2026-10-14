test_that("encode_param keeps the group/name[index] format", {
  expect_equal(encode_param("light", "Kw", 1L), "light/Kw[1]")
  expect_equal(encode_param(NA, "inflow", 1L), "NA/inflow[1]")
  expect_equal(encode_param(NA_character_, "ce", NA_integer_), "NA/ce[NA]")
  expect_equal(encode_param(c("light", NA), c("Kw", "inflow"), c(1L, NA)),
               c("light/Kw[1]", "NA/inflow[NA]"))
})

test_that("encode_param on empty input is empty, not a stray '/[]'", {
  expect_identical(encode_param(character(0), character(0), integer(0)),
                   character(0))
  p <- aeme_parameters[0, ]
  expect_identical(encode_param(p$group, p$name, p$index), character(0))
})

test_that("decode_param_full recovers group, name and index", {
  x <- encode_param(c("light", NA, "aed_phyto"),
                    c("Kw", "mixing/coef_mix_conv", "pd%R_growth[1]x"),
                    c(1L, NA, 3L))
  d <- decode_param_full(x)
  expect_equal(d$group, c("light", NA, "aed_phyto"))
  expect_equal(d$name, c("Kw", "mixing/coef_mix_conv", "pd%R_growth[1]x"))
  expect_equal(d$index, c(1L, NA, 3L))
  expect_equal(decode_param(x), d$name)
})

test_that("decoding round-trips the shipped parameter table", {
  p <- aeme_parameters
  d <- decode_param_full(encode_param(p$group, p$name, p$index))
  expect_equal(d$group, as.character(p$group))
  expect_equal(d$name, p$name)
  expect_equal(d$index, as.integer(p$index))
})

test_that("malformed or missing names decode to NA instead of garbage", {
  d <- decode_param_full(c("noindex", "grp/name", NA))
  expect_true(all(is.na(d$group)))
  expect_true(all(is.na(d$name)))
  expect_true(all(is.na(d$index)))
  expect_equal(nrow(d), 3)
  expect_true(all(is.na(decode_param(c("noindex", NA)))))
  expect_equal(nrow(decode_param_full(character(0))), 0)
})
