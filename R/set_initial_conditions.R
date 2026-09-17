#' Set initial conditions for AEME models
#'
#' Configure the initial state - water depth, temperature and salinity
#' profile, and biogeochemical water-column pools - that [build_aeme()] writes
#' into each model's configuration. Values can be supplied once for every
#' model (`depth`, `profile`, `wq`) and/or overridden per model via
#' `model_init` (for example a GLM-AED-specific temperature profile).
#'
#' The structured specification is stored in
#' `configuration(aeme)$initial_conditions` as a list with a `default` entry
#' plus an optional entry per model, and is resolved at build time by merging
#' each model's overrides over the defaults. For backwards compatibility the
#' generic `depth`/`profile` are also written to `input(aeme)$init_depth` /
#' `input(aeme)$init_profile`, and scalar generic `wq` values into
#' `configuration(aeme)$model_controls$initial_wc`, so the generic controls
#' take effect immediately.
#'
#' @inheritParams build_aeme
#' @param depth numeric(1); initial water depth (m), i.e. the height of the
#'   lake surface above the lowest point of the hypsograph. `NULL` (default)
#'   leaves it unchanged.
#' @param profile data.frame; initial profile with a `depth` column
#'   (positive-down, 0 = surface) and a `temperature` column (degC); a `salt`
#'   column (ppt) is optional and defaults to 0. `NULL` (default) leaves the
#'   profile unchanged.
#' @param wq named list; initial water-column values keyed by AEME variable
#'   name (e.g. `list(CHM_oxy = 300, NIT_amm = 0.5)`). Each element is either
#'   a single number (constant with depth) or a data.frame with `depth` and
#'   `value` columns. Names must be present in `model_controls$var_aeme`.
#'   `NULL` (default) leaves water-quality initial values unchanged.
#' @param model_init named list; per-model overrides keyed by model name
#'   (`"dy_cd"`, `"glm_aed"`, `"gotm_wet"`, `"simstrat_aed2"`,
#'   `"simstrat_aed"`). Each element is itself a list with any of `depth`,
#'   `profile` and `wq`, following the same rules as the generic arguments.
#'   These are merged over the generic specification when that model is built.
#'   `NULL` (default) leaves per-model overrides unchanged.
#' @param from_obs logical; if `TRUE`, seed the generic profile and
#'   water-quality values from lake observations via [update_init()] before
#'   the explicit arguments are applied (the explicit arguments take
#'   precedence). Default `FALSE`.
#' @param reset logical; if `TRUE`, discard any existing
#'   `configuration(aeme)$initial_conditions` before applying the arguments.
#'   Default `FALSE`, i.e. new values are merged into the existing
#'   specification.
#'
#' @returns The `aeme` object with initial conditions set.
#' @export
#'
#' @seealso [get_initial_conditions()], [update_init()], [set_glm_init()],
#'   [set_gotm_init()], [set_simstrat_init()]
#'
#' @examples
#' aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
#' aeme <- readRDS(aeme_file)
#'
#' # Generic profile for all models
#' prof <- data.frame(depth = c(0, 5, 10), temperature = c(18, 14, 11))
#' aeme <- set_initial_conditions(aeme, depth = 10, profile = prof)
#'
#' # GLM-AED-specific override
#' aeme <- set_initial_conditions(
#'   aeme,
#'   model_init = list(
#'     glm_aed = list(profile = data.frame(depth = c(0, 10),
#'                                         temperature = c(20, 12)))
#'   )
#' )
set_initial_conditions <- function(aeme, depth = NULL, profile = NULL,
                                   wq = NULL, model_init = NULL,
                                   model_controls = NULL, from_obs = FALSE,
                                   reset = FALSE) {

  aeme <- check_aeme(aeme)

  if (is.null(model_controls)) {
    model_controls <- get_model_controls(aeme = aeme)
    if (is.null(model_controls)) {
      model_controls <- get_model_controls()
    }
  }

  if (isTRUE(from_obs)) {
    aeme <- update_init(aeme = aeme, model_controls = NULL)
    # update_init() may have rewritten model_controls on the object
    mc_obj <- get_model_controls(aeme = aeme)
    if (!is.null(mc_obj)) model_controls <- mc_obj
  }

  valid_models <- unname(list_models())

  # Validate the generic arguments -------------------------------------------
  if (!is.null(depth)) depth <- .validate_init_depth(depth)
  if (!is.null(profile)) {
    profile <- .validate_init_profile(profile, require_temp = TRUE,
                                      arg = "profile")
  }
  if (!is.null(wq)) wq <- .validate_init_wq(wq, model_controls, arg = "wq")

  # Validate the per-model overrides ---------------------------------------
  if (!is.null(model_init)) {
    model_init <- .validate_model_init(model_init, model_controls,
                                       valid_models = valid_models)
  }

  # Assemble / update the stored specification ---------------------------
  config <- configuration(aeme)
  spec <- config[["initial_conditions"]]
  if (isTRUE(reset) || is.null(spec)) {
    spec <- list(default = list())
  }
  if (is.null(spec[["default"]])) spec[["default"]] <- list()

  if (!is.null(depth))   spec[["default"]][["depth"]]   <- depth
  if (!is.null(profile)) spec[["default"]][["profile"]] <- profile
  if (!is.null(wq)) {
    spec[["default"]][["wq"]] <- .merge_init_wq(spec[["default"]][["wq"]], wq)
  }

  if (!is.null(model_init)) {
    for (m in names(model_init)) {
      spec[[m]] <- .merge_init_entry(spec[[m]], model_init[[m]])
    }
  }

  config[["initial_conditions"]] <- spec
  configuration(aeme) <- config

  # Backwards-compatible writes so the generic controls take effect now ----
  if (!is.null(depth) || !is.null(profile)) {
    inp <- input(aeme)
    if (!is.null(depth)) inp[["init_depth"]] <- depth
    if (!is.null(profile)) inp[["init_profile"]] <- profile
    input(aeme) <- inp
  }

  if (!is.null(wq)) {
    scalar_wq <- wq[vapply(wq, function(x) is.numeric(x) && length(x) == 1,
                           logical(1))]
    if (length(scalar_wq) > 0) {
      for (v in names(scalar_wq)) {
        model_controls[["initial_wc"]][model_controls[["var_aeme"]] == v] <-
          scalar_wq[[v]]
      }
      cfg <- configuration(aeme)
      cfg[["model_controls"]] <- new_model_controls(model_controls)
      configuration(aeme) <- cfg
    }
    depth_wq <- setdiff(names(wq), names(scalar_wq))
    if (length(depth_wq) > 0) {
      cli_inform_safe(c(
        "i" = paste0("Depth-resolved initial values stored for: ",
                     paste(depth_wq, collapse = ", "), "."),
        " " = paste("These are applied when the model is built; scalar",
                    "values also update `model_controls$initial_wc` now.")
      ))
    }
  }

  n_over <- length(setdiff(names(spec), "default"))
  cli_inform_safe(c(
    "v" = paste0("Initial conditions updated",
                 if (n_over > 0) {
                   paste0(" (", n_over, " model-specific override",
                          if (n_over > 1) "s" else "", ")")
                 } else "",
                 ".")
  ))

  return(aeme)
}

#' Get the stored initial-conditions specification
#'
#' Return the structured initial-conditions specification set by
#' [set_initial_conditions()], or, for a single `model`, that model's
#' resolved initial conditions (its overrides merged over the generic
#' defaults).
#'
#' @inheritParams build_aeme
#' @param model character(1); optional model name. If supplied, the resolved
#'   (merged) initial conditions for that model are returned instead of the
#'   full specification.
#'
#' @returns A list. When `model` is `NULL`, the stored specification (a list
#'   with a `default` element and an element per model with overrides), or
#'   `NULL` if none has been set. When `model` is supplied, a list with any
#'   of `depth`, `profile` and `wq`.
#' @export
#'
#' @seealso [set_initial_conditions()]
#'
#' @examples
#' aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
#' aeme <- readRDS(aeme_file)
#' aeme <- set_initial_conditions(aeme, depth = 10)
#' get_initial_conditions(aeme)
#' get_initial_conditions(aeme, model = "glm_aed")
get_initial_conditions <- function(aeme, model = NULL) {
  aeme <- check_aeme(aeme)
  spec <- configuration(aeme)[["initial_conditions"]]
  if (is.null(model)) {
    return(spec)
  }
  model <- match.arg(model, choices = unname(list_models()))
  .resolve_initial_conditions(spec, model = model)
}

#' Resolve a model's initial conditions from a stored specification
#'
#' Merge a model's overrides over the generic `default` entry.
#'
#' @param spec list; the stored specification (see [set_initial_conditions()]),
#'   or `NULL`.
#' @param model character(1); model name.
#'
#' @returns A list with any of `depth`, `profile` and `wq`; an empty list when
#'   `spec` is `NULL`.
#' @noRd
.resolve_initial_conditions <- function(spec, model) {
  if (is.null(spec)) return(list())
  .merge_init_entry(spec[["default"]], spec[[model]])
}

#' Shallow-merge two initial-condition entries (`depth`/`profile`/`wq`)
#'
#' `modifyList()` cannot be used here because `profile` and depth-resolved
#' `wq` values are data.frames, into which it would recurse.
#'
#' @param base,over lists with any of `depth`, `profile`, `wq`; either may be
#'   `NULL`.
#' @returns the merged list.
#' @noRd
.merge_init_entry <- function(base, over) {
  base <- base %||% list()
  over <- over %||% list()
  out <- base
  for (k in c("depth", "profile")) {
    if (!is.null(over[[k]])) out[[k]] <- over[[k]]
  }
  if (!is.null(base[["wq"]]) || !is.null(over[["wq"]])) {
    out[["wq"]] <- .merge_init_wq(base[["wq"]], over[["wq"]])
  }
  out
}

#' Merge two water-quality initial-condition lists by variable name
#'
#' @param base,over named lists of scalars / `depth`-`value` data.frames;
#'   either may be `NULL`.
#' @returns the merged named list (`over` wins per name).
#' @noRd
.merge_init_wq <- function(base, over) {
  base <- base %||% list()
  over <- over %||% list()
  base[names(over)] <- over
  base
}

#' Resolve a model's build-time initial conditions
#'
#' Combine the stored specification (see [set_initial_conditions()]) with the
#' generic build inputs, returning the profile, depth and `model_controls` to
#' hand to `build_<model>()`, plus any depth-resolved water-quality profiles
#' to apply to the written configuration files afterwards.
#'
#' @param spec `configuration(aeme)$initial_conditions`, or `NULL`.
#' @param model character(1); model name.
#' @param init_prof data.frame; generic initial profile
#'   (`depth`/`temperature`/`salt`).
#' @param init_depth numeric(1); generic initial depth.
#' @param model_controls data.frame of model controls.
#' @returns list with `init_prof`, `init_depth`, `model_controls` and
#'   `wq_prof` (named list of `depth`/`value` data.frames, possibly empty).
#' @noRd
.resolve_model_ic <- function(spec, model, init_prof, init_depth,
                              model_controls) {
  ic <- .resolve_initial_conditions(spec, model)

  ip <- init_prof
  if (!is.null(ic[["profile"]])) {
    prof <- ic[["profile"]]
    # A per-model profile override may carry only salinity (temperature all
    # NA); fall back to the generic temperature profile in that case.
    if (all(is.na(prof[["temperature"]])) &&
        !is.null(init_prof[["temperature"]])) {
      prof[["temperature"]] <- stats::approx(
        init_prof[["depth"]], init_prof[["temperature"]],
        xout = prof[["depth"]], rule = 2
      )[["y"]]
    }
    ip <- prof
  }

  id <- ic[["depth"]] %||% init_depth

  mc <- model_controls
  wq <- ic[["wq"]] %||% list()
  is_scalar <- vapply(wq, function(x) is.numeric(x) && length(x) == 1,
                      logical(1))
  for (v in names(wq)[is_scalar]) {
    if (v %in% mc[["var_aeme"]]) {
      mc[["initial_wc"]][mc[["var_aeme"]] == v] <- wq[[v]]
    }
  }

  list(init_prof = ip, init_depth = id, model_controls = mc,
       wq_prof = wq[!is_scalar])
}

#' Apply depth-resolved water-quality initial conditions to built config
#'
#' Called by [build_aeme()] after `build_<model>()` has written the model
#' directory. Scalar water-quality initials are folded into `model_controls`
#' by `.resolve_model_ic()` instead; this handles only the `depth`/`value`
#' data.frame entries, via the model-specific `set_*_init()` writers.
#'
#' @param model character(1); model name.
#' @param lake_dir filepath; the lake directory (holding the model subdirs).
#' @param wq_prof named list of `depth`/`value` data.frames (possibly empty).
#' @param use_bgc logical; whether the biogeochemical model is enabled.
#' @returns `NULL`, invisibly.
#' @noRd
.apply_wq_prof <- function(model, lake_dir, wq_prof, use_bgc) {
  if (length(wq_prof) == 0) return(invisible())
  vars <- paste(names(wq_prof), collapse = ", ")

  if (model %in% c("gotm_wet", "dy_cd")) {
    cli_inform_safe(c("!" = paste0(
      "Depth-resolved water-quality initial values are not supported for ",
      "{model}; ignored: {vars}."
    )))
    return(invisible())
  }
  if (!isTRUE(use_bgc)) {
    cli_inform_safe(c("!" = paste0(
      "Depth-resolved initial values for {model} ignored (use_bgc = FALSE): ",
      "{vars}."
    )))
    return(invisible())
  }

  res <- tryCatch({
    if (model == "glm_aed") {
      set_glm_init(path_glm = file.path(lake_dir, "glm_aed"), wq_init = wq_prof)
    } else {
      set_simstrat_init(path_simstrat = file.path(lake_dir, model),
                        wq_init = wq_prof)
    }
    TRUE
  }, error = function(e) e)

  if (inherits(res, "error")) {
    cli::cli_warn(c(
      "!" = "Could not apply depth-resolved initial values for {model}.",
      "i" = conditionMessage(res)
    ))
  }
  invisible()
}

#' Validate an initial water depth
#'
#' @param depth object supplied as `depth`
#' @returns the validated numeric(1)
#' @noRd
.validate_init_depth <- function(depth) {
  if (!is.numeric(depth) || length(depth) != 1 || !is.finite(depth) ||
      depth <= 0) {
    cli::cli_abort(
      "{.arg depth} must be a single positive number (metres).",
      class = "aeme_error_init_depth"
    )
  }
  depth
}

#' Validate an initial temperature / salinity profile
#'
#' @param profile object supplied as a profile
#' @param require_temp logical; require a `temperature` column
#' @param arg character; argument name for messages
#' @returns a cleaned data.frame ordered by `depth` with `depth`,
#'   `temperature` and `salt` columns (`salt` filled with 0 when absent)
#' @noRd
.validate_init_profile <- function(profile, require_temp = TRUE,
                                   arg = "profile") {
  if (!is.data.frame(profile)) {
    cli::cli_abort(
      "{.arg {arg}} must be a {.cls data.frame}, not {.cls {class(profile)[1]}}.",
      class = "aeme_error_init_profile"
    )
  }
  if (!"depth" %in% names(profile)) {
    cli::cli_abort(c("{.arg {arg}} must have a {.field depth} column."),
                   class = "aeme_error_init_profile")
  }
  has_temp <- "temperature" %in% names(profile)
  has_salt <- "salt" %in% names(profile)
  if (!has_temp && !has_salt) {
    cli::cli_abort(
      "{.arg {arg}} must have a {.field temperature} and/or {.field salt} column.",
      class = "aeme_error_init_profile"
    )
  }
  if (require_temp && !has_temp) {
    cli::cli_abort("{.arg {arg}} must have a {.field temperature} column.",
                   class = "aeme_error_init_profile")
  }
  cols <- c("depth", if (has_temp) "temperature", if (has_salt) "salt")
  for (cc in cols) {
    if (!is.numeric(profile[[cc]]) || anyNA(profile[[cc]])) {
      cli::cli_abort(
        "{.arg {arg}}${cc} must be numeric with no missing values.",
        class = "aeme_error_init_profile"
      )
    }
  }
  if (any(profile[["depth"]] < 0)) {
    cli::cli_abort("{.arg {arg}}$depth must be >= 0 (positive-down).",
                   class = "aeme_error_init_profile")
  }
  if (anyDuplicated(profile[["depth"]])) {
    cli::cli_abort("{.arg {arg}}$depth must not contain duplicate values.",
                   class = "aeme_error_init_profile")
  }
  out <- profile[order(profile[["depth"]]), cols, drop = FALSE]
  if (!("salt" %in% cols)) {
    out[["salt"]] <- 0
    cli_inform_safe(c("i" = "{.arg {arg}} has no {.field salt} column; using 0."))
  }
  if (!("temperature" %in% cols)) out[["temperature"]] <- NA_real_
  rownames(out) <- NULL
  out[, c("depth", "temperature", "salt")]
}

#' Validate a water-quality initial-conditions list
#'
#' @param wq object supplied as `wq`
#' @param model_controls data.frame of model controls (for `var_aeme`)
#' @param arg character; argument name for messages
#' @returns the normalised named list (scalars kept as length-1 numeric,
#'   profiles kept as data.frames with `depth` and `value`)
#' @noRd
.validate_init_wq <- function(wq, model_controls, arg = "wq") {
  if (!is.list(wq) || is.data.frame(wq)) {
    cli::cli_abort("{.arg {arg}} must be a named {.cls list}.",
                   class = "aeme_error_init_wq")
  }
  nms <- names(wq)
  if (is.null(nms) || any(!nzchar(nms))) {
    cli::cli_abort("Every element of {.arg {arg}} must be named.",
                   class = "aeme_error_init_wq")
  }
  if (anyDuplicated(nms)) {
    cli::cli_abort("{.arg {arg}} must not contain duplicate names.",
                   class = "aeme_error_init_wq")
  }
  known <- model_controls[["var_aeme"]]
  unknown <- setdiff(nms, known)
  if (length(unknown) > 0) {
    cli::cli_abort(
      c(paste0("Unknown {cli::qty(unknown)}variable name{?s} in {.arg {arg}}: ",
               "{.val {unknown}}."),
        "i" = "Names must be present in {.code model_controls$var_aeme}."),
      class = "aeme_error_init_wq"
    )
  }
  lapply(stats::setNames(nms, nms), function(v) {
    x <- wq[[v]]
    if (is.data.frame(x)) {
      if (!all(c("depth", "value") %in% names(x))) {
        cli::cli_abort(
          "{.arg {arg}}${v} must have {.field depth} and {.field value} columns.",
          class = "aeme_error_init_wq"
        )
      }
      if (!is.numeric(x[["depth"]]) || !is.numeric(x[["value"]]) ||
          anyNA(x[["depth"]]) || anyNA(x[["value"]])) {
        cli::cli_abort(
          "{.arg {arg}}${v} columns must be numeric with no missing values.",
          class = "aeme_error_init_wq"
        )
      }
      x <- x[order(x[["depth"]]), c("depth", "value"), drop = FALSE]
      rownames(x) <- NULL
      return(x)
    }
    if (is.numeric(x) && length(x) == 1 && is.finite(x)) {
      return(x)
    }
    cli::cli_abort(
      paste("{.arg {arg}}${v} must be a single number or a data.frame with",
            "{.field depth} and {.field value} columns."),
      class = "aeme_error_init_wq"
    )
  })
}

#' Validate the `model_init` per-model override list
#'
#' @param model_init object supplied as `model_init`
#' @param model_controls data.frame of model controls
#' @param valid_models character; permitted model names
#' @returns the normalised list, each entry containing validated
#'   `depth`/`profile`/`wq` elements
#' @noRd
.validate_model_init <- function(model_init, model_controls, valid_models) {
  if (!is.list(model_init) || is.data.frame(model_init)) {
    cli::cli_abort("{.arg model_init} must be a named {.cls list}.",
                   class = "aeme_error_model_init")
  }
  nms <- names(model_init)
  if (is.null(nms) || any(!nzchar(nms))) {
    cli::cli_abort("Every element of {.arg model_init} must be named by model.",
                   class = "aeme_error_model_init")
  }
  bad <- setdiff(nms, valid_models)
  if (length(bad) > 0) {
    cli::cli_abort(
      c("Unknown model name{?s} in {.arg model_init}: {.val {bad}}.",
        "i" = "Valid models: {.val {valid_models}}."),
      class = "aeme_error_model_init"
    )
  }
  allowed <- c("depth", "profile", "wq")
  lapply(stats::setNames(nms, nms), function(m) {
    entry <- model_init[[m]]
    if (!is.list(entry) || is.data.frame(entry)) {
      cli::cli_abort(
        "{.arg model_init}${m} must be a {.cls list} with any of {.val {allowed}}.",
        class = "aeme_error_model_init"
      )
    }
    extra <- setdiff(names(entry), allowed)
    if (length(extra) > 0) {
      cli::cli_abort(
        c("{.arg model_init}${m} has unsupported element{?s}: {.val {extra}}.",
          "i" = "Supported: {.val {allowed}}."),
        class = "aeme_error_model_init"
      )
    }
    out <- list()
    if (!is.null(entry[["depth"]])) {
      out[["depth"]] <- .validate_init_depth(entry[["depth"]])
    }
    if (!is.null(entry[["profile"]])) {
      out[["profile"]] <- .validate_init_profile(
        entry[["profile"]], require_temp = FALSE,
        arg = paste0("model_init$", m, "$profile")
      )
    }
    if (!is.null(entry[["wq"]])) {
      out[["wq"]] <- .validate_init_wq(
        entry[["wq"]], model_controls,
        arg = paste0("model_init$", m, "$wq")
      )
    }
    out
  })
}
