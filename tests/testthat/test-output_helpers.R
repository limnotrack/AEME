base_aeme <- function() {
  aeme <- readRDS(system.file("extdata/aeme.rds", package = "AEME"))
  remove_output(aeme)
}

fake_out <- function(model) {
  .new_aeme_output(list(Date = as.Date("2020-01-01")), model = model)
}

test_that("an empty Aeme has no members", {
  aeme <- base_aeme()
  expect_equal(.count_members(output(aeme)), 0L)
  expect_equal(output(remove_output(aeme)), .empty_output())
})

test_that("add_output appends members and keeps n_members in step", {
  aeme <- base_aeme()
  aeme <- add_output(aeme, list(glm_aed = fake_out("glm_aed")))
  aeme <- add_output(aeme, list(glm_aed = fake_out("glm_aed"),
                                simstrat_aed2 = fake_out("simstrat_aed2")))
  outp <- output(aeme)
  expect_equal(outp$n_members, 2L)
  expect_equal(.output_members(outp), c("ens_001", "ens_002"))
  expect_null(outp$ens_001$simstrat_aed2)
  expect_true(is_aeme_output(outp$ens_002$simstrat_aed2))
  expect_equal(output(remove_output(aeme))$n_members, 0L)
})

test_that("members are found by exact name, not by substring", {
  outp <- list(n_members = 1L, ens_001 = list(), other_ens = 1, tens = 2)
  expect_equal(.output_members(outp), "ens_001")
})

test_that(".get_member_output gives clear errors", {
  aeme <- base_aeme()
  expect_error(.get_member_output(aeme, "glm_aed"),
               class = "aeme_error_no_output")
  aeme <- add_output(aeme, list(glm_aed = fake_out("glm_aed")))
  expect_error(.get_member_output(aeme, "glm_aed", ens_n = 2),
               class = "aeme_error_no_member")
  expect_error(.get_member_output(aeme, "dy_cd"),
               class = "aeme_error_no_model_output")
  expect_null(.get_member_output(aeme, "dy_cd", null_ok = TRUE))
  expect_true(is_aeme_output(.get_member_output(aeme, "glm_aed")))
})

test_that("validity rejects malformed output", {
  aeme <- base_aeme()
  expect_error(output(aeme) <- list(n_members = "a"),
               class = "aeme_error_output_set")
  expect_error(output(aeme) <- list(n_members = 1L, ens_001 = list(foo = list())),
               class = "aeme_error_output_set")
  expect_error(output(aeme) <- list(n_members = 1L, ens_001 = list(glm_aed = 1)),
               class = "aeme_error_output_set")
})
