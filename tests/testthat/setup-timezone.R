# Pin the session timezone for the whole test run so datetime assertions that
# compare absolute instants are deterministic regardless of the developer's
# machine timezone. test-timezone.R deliberately overrides this locally with
# withr::local_timezone() to exercise the non-UTC paths.
withr::local_timezone("UTC", .local_envir = testthat::teardown_env())
Sys.setenv(TZ = "UTC")
