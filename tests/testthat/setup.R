# Silence AEME's informational cli messages for the whole test run, so
# individual test files don't each need their own options(AEME.inform = FALSE).
options(AEME.inform = FALSE, ncore = 4L)

# Pre-install GLM once, up front, before any test file runs - fails fast
# with one clear message here rather than the same failure surfacing
# separately (or being masked) partway through 50+ tests.
#
# Deliberately does NOT stop() on failure: if GLM genuinely can't be
# obtained in this environment (no network, unsupported platform), the
# per-test skip_if_no_glm() / skip_if_models_unavailable() calls in each
# test still handle that correctly and skip individually - this file's job
# is only to warm the cache once, not to gate the whole suite.
tryCatch(
  invisible(install_glm_aed(version = getOption("AEME.glm_version", "3.9.108"))),
  error = function(e) {
    message(
      "Could not pre-install GLM at test setup (", conditionMessage(e), ") - ",
      "tests requiring GLM will skip individually via skip_if_no_glm()."
    )
  }
)

vers <- get_model_version("glm_aed")
