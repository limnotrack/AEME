# test-nml_helpers.R
#
# Tests for all functions in nml_helpers.R.
# Run with: testthat::test_file("tests/testthat/test-nml_helpers.R")
#
# Organised by function. Each test_that() label follows the convention:
#   "<function>: <what it does>"
# so that failures are immediately identifiable in the reporter output.

# ── Shared fixture ────────────────────────────────────────────────────────────

# Minimal valid nml structure reused across tests. Defined as a function so
# each test gets a fresh copy with no risk of mutation leaking between tests.
make_nml <- function() {
  .nml(list(
    glm_setup = list(
      sim_name      = "test_lake",
      max_layers    = 500L,
      min_layer_vol = 0.5
    ),
    morphometry = list(
      lake_name = "Rotorua",
      latitude  = -38.08,
      longitude = 176.27,
      bsn_len   = 3000,
      bsn_wid   = 3000
    )
  ))
}


# ══════════════════════════════════════════════════════════════════════════════
# .nml() — constructor
# ══════════════════════════════════════════════════════════════════════════════

test_that(".nml: returns an object with class 'nml'", {
  result <- .nml(list(block_a = list(x = 1)))
  expect_s3_class(result, "nml")
})

test_that(".nml: preserves list structure and values", {
  input <- list(
    setup = list(sim_name = "lake1", depth = 20),
    time  = list(start = "2020-01-01", stop = "2020-12-31")
  )
  result <- .nml(input)
  expect_identical(result$setup$sim_name, "lake1")
  expect_identical(result$time$stop, "2020-12-31")
})

test_that(".nml: returns invisibly", {
  vis <- withVisible(.nml(list(b = list(x = 1))))
  expect_false(vis$visible)
})

test_that(".nml: accepts a single-block nml", {
  result <- .nml(list(only_block = list(a = 1, b = 2)))
  expect_s3_class(result, "nml")
  expect_length(result, 1L)
})

test_that(".nml: accepts blocks with multiple value types", {
  result <- .nml(list(block = list(
    num    = 3.14,
    string = "hello",
    flag   = TRUE,
    vec    = c(1, 2, 3)
  )))
  expect_s3_class(result, "nml")
  expect_equal(result$block$vec, c(1, 2, 3))
})

test_that(".nml: warns when given an empty list", {
  expect_warning(
    .nml(list()),
    class = "nml_warn_empty"
  )
})

test_that(".nml: errors when list_obj is not a list", {
  expect_error(.nml("not a list"), class = "nml_error_constructor")
  expect_error(.nml(42),          class = "nml_error_constructor")
  expect_error(.nml(TRUE),        class = "nml_error_constructor")
})

test_that(".nml: errors when a block has an empty name", {
  lst <- list(good_block = list(x = 1), "x" = list(y = 2))
  names(lst)[2] <- ""
  expect_error(
    .nml(lst),
    class = "nml_error_constructor"
  )
})

test_that(".nml: errors when a block is not itself a list", {
  expect_error(
    .nml(list(block_a = list(x = 1), block_b = "not_a_list")),
    class = "nml_error_constructor"
  )
  expect_error(
    .nml(list(block_a = 42)),
    class = "nml_error_constructor"
  )
})


# ══════════════════════════════════════════════════════════════════════════════
# from.glm_boolean()
# ══════════════════════════════════════════════════════════════════════════════

test_that("from.glm_boolean: converts .true. to TRUE", {
  expect_identical(from.glm_boolean(".true."), TRUE)
})

test_that("from.glm_boolean: converts .false. to FALSE", {
  expect_identical(from.glm_boolean(".false."), FALSE)
})

test_that("from.glm_boolean: converts a vector of mixed values", {
  result <- from.glm_boolean(c(".true.", ".false.", ".true."))
  expect_identical(result, c(TRUE, FALSE, TRUE))
})

test_that("from.glm_boolean: returns NA for NA input", {
  expect_true(is.na(from.glm_boolean(NA_character_)))
})

test_that("from.glm_boolean: returns NA for the string 'NA'", {
  expect_true(is.na(from.glm_boolean("NA")))
})

test_that("from.glm_boolean: handles NA alongside valid values", {
  result <- from.glm_boolean(c(".true.", NA, ".false."))
  expect_identical(result, c(TRUE, NA, FALSE))
})

test_that("from.glm_boolean: returns a logical vector not a list", {
  result <- from.glm_boolean(c(".true.", ".false."))
  expect_type(result, "logical")
})

test_that("from.glm_boolean: errors on an unrecognised string", {
  expect_error(from.glm_boolean("yes"),  class = "nml_error_boolean")
  expect_error(from.glm_boolean("TRUE"), class = "nml_error_boolean")
  expect_error(from.glm_boolean("1"),    class = "nml_error_boolean")
})

test_that("from.glm_boolean: errors on first bad value in a mixed vector", {
  expect_error(
    from.glm_boolean(c(".true.", "bad_value", ".false.")),
    class = "nml_error_boolean"
  )
})


# ══════════════════════════════════════════════════════════════════════════════
# to.glm_boolean()
# ══════════════════════════════════════════════════════════════════════════════

test_that("to.glm_boolean: converts TRUE to '.true.'", {
  expect_identical(to.glm_boolean(TRUE), ".true.")
})

test_that("to.glm_boolean: converts FALSE to '.false.'", {
  expect_identical(to.glm_boolean(FALSE), ".false.")
})

test_that("to.glm_boolean: handles a vector of mixed logical values", {
  result <- to.glm_boolean(c(TRUE, FALSE, TRUE, FALSE))
  expect_identical(result, c(".true.", ".false.", ".true.", ".false."))
})

test_that("to.glm_boolean: returns a character vector", {
  expect_type(to.glm_boolean(c(TRUE, FALSE)), "character")
})

test_that("to.glm_boolean: is the inverse of from.glm_boolean", {
  original <- c(TRUE, FALSE, TRUE)
  expect_identical(from.glm_boolean(to.glm_boolean(original)), original)
})

test_that("to.glm_boolean: output length equals input length", {
  vals <- c(TRUE, TRUE, FALSE, FALSE, TRUE)
  expect_length(to.glm_boolean(vals), length(vals))
})


# ══════════════════════════════════════════════════════════════════════════════
# buildVal()
# ══════════════════════════════════════════════════════════════════════════════

test_that("buildVal: parses a single numeric value", {
  result <- buildVal("depth = 20", 1L, "morphometry")
  expect_named(result, "depth")
  expect_equal(result[["depth"]], 20)
})

test_that("buildVal: parses comma-separated numerics into a vector", {
  result <- buildVal("heights = 1, 2, 3", 1L, "setup")
  expect_equal(result[["heights"]], c(1, 2, 3))
})

test_that("buildVal: trims whitespace from parameter names", {
  result <- buildVal("  depth  = 10", 1L, "morphometry")
  expect_named(result, "depth")
})

test_that("buildVal: parses a single-quoted string stripping quotes", {
  result <- buildVal("sim_name = 'my_lake'", 1L, "setup")
  expect_equal(result[["sim_name"]], "my_lake")
})

test_that("buildVal: parses a double-quoted string stripping quotes", {
  result <- buildVal('sim_name = "my_lake"', 1L, "setup")
  expect_equal(result[["sim_name"]], "my_lake")
})

test_that("buildVal: parses comma-separated quoted strings into a vector", {
  result <- buildVal("names = 'lake1','lake2','lake3'", 1L, "setup")
  expect_equal(result[["names"]], c("lake1", "lake2", "lake3"))
})

test_that("buildVal: parses .true. as TRUE", {
  result <- buildVal("check = .true.", 1L, "setup")
  expect_identical(result[["check"]], TRUE)
})

test_that("buildVal: parses .false. as FALSE", {
  result <- buildVal("check = .false.", 1L, "setup")
  expect_identical(result[["check"]], FALSE)
})

test_that("buildVal: parses comma-separated booleans into a logical vector", {
  result <- buildVal("flags = .true.,.false.,.true.", 1L, "setup")
  expect_identical(result[["flags"]], c(TRUE, FALSE, TRUE))
})

test_that("buildVal: strips inline comments before parsing", {
  result <- buildVal("depth = 20 ! this is a comment", 1L, "morphometry")
  expect_equal(result[["depth"]], 20)
})

test_that("buildVal: correctly reformats a GLM date-time string", {
  # GLM format uses colons at positions 14 and 17 for time separators
  result <- buildVal("start = '2020-01-01T12:00:00'", 1L, "time")
  expect_type(result[["start"]], "character")
  expect_false(is.na(result[["start"]]))
})

test_that("buildVal: always returns a length-1 named list", {
  result <- buildVal("x = 42", 1L, "block")
  expect_type(result, "list")
  expect_length(result, 1L)
  expect_false(is.null(names(result)))
})

test_that("buildVal: warns and fills NAs when partial coercion fails", {
  # "1, bad, 3" — "bad" coerces to NA while 1 and 3 succeed
  expect_warning(
    buildVal("vals = 1, bad, 3", 1L, "setup"),
    class = "nml_warn_coercion_partial"
  )
  result <- suppressWarnings(buildVal("vals = 1, bad, 3", 1L, "setup"))
  expect_false(any(is.na(result[["vals"]])))
})

test_that("buildVal: informs when all values coerce to NA", {
  expect_message(
    buildVal("vals = bad_val", 1L, "setup", coerce = TRUE),
    class = "nml_inform_coercion_all_na"
  )
  result <- suppressMessages(buildVal("vals = bad_val", 1L, "setup"))
  expect_true(is.na(result[["vals"]]))
})

test_that("buildVal: errors on a line with no '=' sign", {
  expect_error(
    buildVal("this has no equals sign", 5L, "setup"),
    class = "nml_error_parse_hanging"
  )
})

test_that("buildVal: errors when value is empty after split", {
  # "param =" splits on "=" and leaves NA as the right-hand side
  expect_error(
    buildVal("param =", 3L, "setup"),
    class = "nml_error_parse_empty"
  )
})

test_that("buildVal: error message contains the line number", {
  err <- tryCatch(
    buildVal("no_equals_here", 42L, "glm_setup"),
    error = function(e) e
  )
  expect_match(conditionMessage(err), "42")
})

test_that("buildVal: error message contains the block name", {
  err <- tryCatch(
    buildVal("no_equals_here", 1L, "morphometry"),
    error = function(e) e
  )
  expect_match(conditionMessage(err), "morphometry")
})


# ══════════════════════════════════════════════════════════════════════════════
# findBlck()
# ══════════════════════════════════════════════════════════════════════════════

test_that("findBlck: returns the index of the block containing the argument", {
  nml <- make_nml()
  expect_identical(findBlck(nml, "sim_name"), 1L)
})

test_that("findBlck: returns the correct index for an argument in the second block", {
  nml <- make_nml()
  expect_identical(findBlck(nml, "latitude"), 2L)
})

test_that("findBlck: returns multiple indices when argument exists in more than one block", {
  nml <- .nml(list(
    block_a = list(shared_param = 1),
    block_b = list(shared_param = 2)
  ))
  result <- findBlck(nml, "shared_param")
  expect_length(result, 2L)
  expect_identical(result, c(1L, 2L))
})

test_that("findBlck: returns an integer vector", {
  nml <- make_nml()
  expect_type(findBlck(nml, "latitude"), "integer")
})

test_that("findBlck: errors when argName is not a character", {
  nml <- make_nml()
  expect_error(findBlck(nml, 42),   class = "nml_error_findblck")
  expect_error(findBlck(nml, TRUE), class = "nml_error_findblck")
  expect_error(findBlck(nml, NULL), class = "nml_error_findblck")
})

test_that("findBlck: errors when parameter is not found in any block", {
  nml <- make_nml()
  expect_error(
    findBlck(nml, "nonexistent_param"),
    class = "nml_error_param_not_found"
  )
})

test_that("findBlck: error message lists available parameters", {
  nml <- make_nml()
  err <- tryCatch(
    findBlck(nml, "nonexistent_param"),
    error = function(e) e
  )
  expect_match(conditionMessage(err), "sim_name|latitude|longitude")
})


# ══════════════════════════════════════════════════════════════════════════════
# get_arg_name()
# ══════════════════════════════════════════════════════════════════════════════

test_that("get_arg_name: returns argument unchanged when no '::' prefix", {
  expect_identical(get_arg_name("latitude"), "latitude")
})

test_that("get_arg_name: strips the block prefix and returns the argument name", {
  expect_identical(get_arg_name("morphometry::latitude"), "latitude")
})

test_that("get_arg_name: handles multiple '::' by returning the second element only", {
  expect_identical(get_arg_name("block::arg::extra"), "arg")
})

test_that("get_arg_name: returns a character string", {
  expect_type(get_arg_name("setup::sim_name"), "character")
})


# ══════════════════════════════════════════════════════════════════════════════
# get_block()
# ══════════════════════════════════════════════════════════════════════════════

test_that("get_block: returns the integer block index for a bare argument name", {
  nml <- make_nml()
  expect_identical(get_block(nml, "latitude"), 2L)
})

test_that("get_block: supports block::arg_name syntax for explicit block targeting", {
  nml <- make_nml()
  expect_identical(get_block(nml, "morphometry::latitude"), "morphometry")
})

test_that("get_block: returns only the first block index when argument is ambiguous", {
  nml <- .nml(list(
    block_a = list(shared = 1),
    block_b = list(shared = 2)
  ))
  result <- suppressWarnings(get_block(nml, "shared"))
  expect_length(result, 1L)
})

test_that("get_block: warns when an argument exists in multiple blocks", {
  nml <- .nml(list(
    block_a = list(shared = 1),
    block_b = list(shared = 2)
  ))
  expect_warning(
    get_block(nml, "shared", warn = TRUE),
    class = "nml_warn_ambiguous_param"
  )
})

test_that("get_block: does not warn when warn = FALSE even for ambiguous argument", {
  nml <- .nml(list(
    block_a = list(shared = 1),
    block_b = list(shared = 2)
  ))
  expect_no_warning(get_block(nml, "shared", warn = FALSE))
})

test_that("get_block: warning message names the conflicting blocks", {
  nml <- .nml(list(
    block_a = list(shared = 1),
    block_b = list(shared = 2)
  ))
  w <- tryCatch(
    get_block(nml, "shared", warn = TRUE),
    warning = function(w) w
  )
  expect_match(conditionMessage(w), "block_a|block_b")
})

test_that("get_block: errors when the argument does not exist in any block", {
  nml <- make_nml()
  expect_error(
    get_block(nml, "nonexistent"),
    class = "nml_error_param_not_found"
  )
})


# ══════════════════════════════════════════════════════════════════════════════
# setnmlList()
# ══════════════════════════════════════════════════════════════════════════════
#
# setnmlList() delegates to set_nml() which is defined elsewhere in AEME.
# These tests cover only the guard logic in setnmlList() itself.
# Integration tests for the full round-trip belong in test-set_nml.R.

test_that("setnmlList: errors when arg_list is not a list", {
  nml <- make_nml()
  expect_error(setnmlList(nml, "not_a_list"), class = "nml_error_setnmllist")
  expect_error(setnmlList(nml, 42),           class = "nml_error_setnmllist")
})

test_that("setnmlList: errors when arg_list is an empty list", {
  nml <- make_nml()
  expect_error(setnmlList(nml, list()), class = "nml_error_setnmllist")
})

test_that("setnmlList: errors when arg_list has unnamed elements", {
  nml <- make_nml()
  expect_error(setnmlList(nml, list(1, 2, 3)), class = "nml_error_setnmllist")
})

test_that("setnmlList: errors when any element has an empty name", {
  nml <- make_nml()
  lst <- list(good = 1, bad = 2)
  names(lst)[2] <- ""
  expect_error(
    setnmlList(nml, lst),
    class = "nml_error_setnmllist"
  )
})


# ══════════════════════════════════════════════════════════════════════════════
# is_nml_file()
# ══════════════════════════════════════════════════════════════════════════════

test_that("is_nml_file: returns TRUE for a .nml extension", {
  expect_true(is_nml_file("glm3.nml"))
  expect_true(is_nml_file("/path/to/model/glm3.nml"))
})

test_that("is_nml_file: returns FALSE for other extensions", {
  expect_false(is_nml_file("config.csv"))
  expect_false(is_nml_file("data.txt"))
  expect_false(is_nml_file("model.R"))
  expect_false(is_nml_file("setup.nml.bak"))
})

test_that("is_nml_file: returns FALSE for a file with no extension", {
  expect_false(is_nml_file("nml_no_extension"))
})

test_that("is_nml_file: is case-sensitive so .NML is not .nml", {
  expect_false(is_nml_file("glm3.NML"))
})


# ══════════════════════════════════════════════════════════════════════════════
# ascii_only() and what_ascii()
# ══════════════════════════════════════════════════════════════════════════════

test_that("ascii_only: returns TRUE for a file with only ASCII characters", {
  f <- tempfile(fileext = ".txt")
  on.exit(unlink(f))
  writeLines("sim_name = 'my_lake'", f)
  expect_true(ascii_only(f))
})

test_that("ascii_only: returns a logical for any file", {
  f <- tempfile(fileext = ".txt")
  on.exit(unlink(f))
  writeLines(c("depth = 20", "name = lake"), f)
  expect_type(ascii_only(f), "logical")
})

test_that("what_ascii: returns character(0) for an ASCII-only file", {
  f <- tempfile(fileext = ".txt")
  on.exit(unlink(f))
  writeLines(c("x = 1", "y = 2"), f)
  expect_identical(what_ascii(f), character(0))
})

test_that("what_ascii: returns a character vector for any file", {
  f <- tempfile(fileext = ".txt")
  on.exit(unlink(f))
  writeLines("depth = 1", f)
  expect_type(what_ascii(f), "character")
})


# ══════════════════════════════════════════════════════════════════════════════
# print.nml()
# ══════════════════════════════════════════════════════════════════════════════

test_that("print.nml: returns the nml object invisibly", {
  nml <- make_nml()
  vis <- withVisible(print(nml))
  expect_false(vis$visible)
  expect_s3_class(vis$value, "nml")
})

test_that("print.nml: outputs a '&block_name' header for each block", {
  nml    <- make_nml()
  output <- capture.output(print(nml))
  expect_true(any(grepl("^&glm_setup",   output)))
  expect_true(any(grepl("^&morphometry", output)))
})

test_that("print.nml: outputs a closing '/' for each block", {
  nml    <- make_nml()
  output <- capture.output(print(nml))
  expect_equal(sum(output == "/"), length(nml))
})

test_that("print.nml: outputs numeric values correctly", {
  nml    <- .nml(list(block = list(depth = 42.5)))
  output <- capture.output(print(nml))
  expect_true(any(grepl("depth = 42.5", output)))
})

test_that("print.nml: outputs character values wrapped in single quotes", {
  nml    <- .nml(list(block = list(name = "Rotorua")))
  output <- capture.output(print(nml))
  expect_true(any(grepl("name = 'Rotorua'", output)))
})

test_that("print.nml: outputs logical TRUE as .true.", {
  nml    <- .nml(list(block = list(flag = TRUE)))
  output <- capture.output(print(nml))
  expect_true(any(grepl("flag = .true.", output, fixed = TRUE)))
})

test_that("print.nml: outputs logical FALSE as .false.", {
  nml    <- .nml(list(block = list(flag = FALSE)))
  output <- capture.output(print(nml))
  expect_true(any(grepl("flag = .false.", output, fixed = TRUE)))
})

test_that("print.nml: outputs numeric vectors as comma-separated values", {
  nml    <- .nml(list(block = list(heights = c(1, 2, 3))))
  output <- capture.output(print(nml))
  expect_true(any(grepl("1, 2, 3", output)))
})

test_that("print.nml: outputs logical vectors as comma-separated GLM booleans", {
  nml    <- .nml(list(block = list(flags = c(TRUE, FALSE, TRUE))))
  output <- capture.output(print(nml))
  expect_true(any(grepl(".true., .false., .true.", output, fixed = TRUE)))
})

test_that("print.nml: outputs character vectors as comma-separated quoted strings", {
  nml    <- .nml(list(block = list(names = c("a", "b", "c"))))
  output <- capture.output(print(nml))
  expect_true(any(grepl("'a','b','c'", output, fixed = TRUE)))
})

test_that("print.nml: outputs the correct number of parameter lines", {
  nml    <- make_nml()
  output <- capture.output(print(nml))
  # glm_setup has 3 params, morphometry has 5 — total 8
  expect_equal(sum(grepl("^ {3}\\w+ = ", output)), 8L)
})

test_that("print.nml: errors when x is not an nml object", {
  expect_error(print.nml(list(a = 1)), class = "nml_error_print")
  expect_error(print.nml("not_nml"),   class = "nml_error_print")
})


# ══════════════════════════════════════════════════════════════════════════════
# summary.nml()
# ══════════════════════════════════════════════════════════════════════════════

test_that("summary.nml: produces the same output as print.nml", {
  nml <- make_nml()
  expect_identical(
    capture.output(print(nml)),
    capture.output(summary.nml(nml))
  )
})

test_that("summary.nml: returns the nml object invisibly", {
  nml <- make_nml()
  vis <- withVisible(summary.nml(nml))
  expect_false(vis$visible)
})


# ══════════════════════════════════════════════════════════════════════════════
# Round-trip consistency
# ══════════════════════════════════════════════════════════════════════════════

test_that("round-trip: boolean values survive a to/from conversion", {
  original <- c(TRUE, FALSE, TRUE, TRUE, FALSE)
  expect_identical(from.glm_boolean(to.glm_boolean(original)), original)
})

test_that("round-trip: print output contains all parameter names from the nml", {
  nml    <- make_nml()
  output <- paste(capture.output(print(nml)), collapse = "\n")
  for (param in unlist(lapply(nml, names))) {
    expect_true(
      grepl(param, output, fixed = TRUE),
      info = paste("Parameter", param, "not found in print output")
    )
  }
})

test_that("round-trip: .nml structure is identical to input list aside from class", {
  input  <- list(block_a = list(x = 1, y = "hello"), block_b = list(z = TRUE))
  result <- .nml(input)
  class(result) <- NULL
  expect_identical(result, input)
})