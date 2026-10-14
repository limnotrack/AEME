glm_extra_vars <- read.csv("data-raw/glm_extra_vars.csv",
                           stringsAsFactors = FALSE, na.strings = "")

stopifnot(
  !anyDuplicated(glm_extra_vars$var_aeme),
  !anyDuplicated(glm_extra_vars$glm_aed),
  all(glm_extra_vars$group %in% c("core", "sediment", "diag")),
  # must not clash with a regular AEME variable
  !any(glm_extra_vars$var_aeme %in% read.csv("data-raw/key_naming.csv")$name)
)

usethis::use_data(glm_extra_vars, overwrite = TRUE)
