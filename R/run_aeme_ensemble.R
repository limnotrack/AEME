#' Run an ensemble of parameter sets
#'
#' @description
#' Runs a model once per supplied parameter set and stores every member's
#' output on the returned `Aeme` object as `output(aeme)$ens_001 ... ens_00k`,
#' alongside `output(aeme)$n_members`. That is the structure
#' [get_var()] reads with `ens_n`, and [write_aeme_parquet()] exports.
#'
#' This runs parameter sets you **supply**: it does not sample them. Drawing
#' sets from parameter bounds and extracting a posterior from a `pestpp-ies`
#' calibration are provided by the `aemetools` package, which passes its sets
#' through here. Sets that use zone ratios ([zone_ratio_param()]) work as they
#' are: [input_model_parameters()] expands them.
#'
#' Each member is one [input_model_parameters()] followed by one [run_aeme()],
#' so a member's cost is a single model run. A member that errors, times out or
#' produces no output is dropped with a warning and the survivors are
#' renumbered.
#'
#' A model can also fail *without* saying so. GLM, for instance, exits normally
#' when an unphysical parameter combination drains the lake: the water level goes
#' to zero or below and the rest of the output is `NA`, but the run reports
#' success. With `drop_incomplete = TRUE` (the default) a member is also dropped
#' when its water level (`LKE_lvlwtr`) reaches zero or is `NA` at any timestep,
#' spin-up included. This needs `LKE_lvlwtr` among the model's output variables;
#' if it was not written the check cannot be made and the member is kept.
#'
#' @inheritParams run_aeme
#' @param param_sets the parameter sets to run. Either a list of long `param`
#'   data.frames, one per member (such as the `aeme_param_sets` object returned
#'   by `aemetools::pest_posterior_params()`), or a single long data.frame with
#'   an `ensemble` column identifying each member. Each member needs the columns
#'   `model`, `file`, `name` and `value`, as in [parameters()] (a `group` and
#'   `index` are used where present). Names, or the `ensemble` ids, are kept as
#'   the member labels.
#' @param drop_incomplete logical; drop members whose water level reached zero
#'   or went `NA` (a lake that dried out or a model that stopped part-way), as
#'   well as members that errored. Default `TRUE`. `FALSE` keeps them, `NA`-padded.
#' @param parallel logical; run members in parallel, each worker in its own
#'   copy of the model directory. Default `FALSE`. The workers load the
#'   installed version of AEME.
#' @param ncore integer; number of workers when `parallel = TRUE`. `NULL`
#'   (default) uses one fewer than the cores available, and never more than the
#'   number of members.
#'
#' @return The `Aeme` object with the ensemble stored in [output()]. When
#'   `param_sets` was named, the labels of the surviving members are on
#'   `attr(output(aeme), "realisation")`.
#' @seealso [add_param()], [input_model_parameters()], [run_aeme()],
#'   [write_aeme_parquet()]
#' @export
#'
#' @importFrom parallel parLapply clusterExport stopCluster makeCluster
#' @importFrom parallel detectCores
#'
#' @examples
#' \dontrun{
#' aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
#' aeme <- readRDS(aeme_file)
#' path <- tempdir()
#' aeme <- build_aeme(path = path, aeme = aeme, model = "glm_aed",
#'                    ext_elev = 5)
#'
#' # Ten members, each scattering the default values by up to 10 %
#' data("aeme_parameters", package = "AEME")
#' param <- aeme_parameters[aeme_parameters$model == "glm_aed", ]
#' param_sets <- lapply(1:10, function(i) {
#'   p <- param
#'   p$value <- p$value * runif(nrow(p), 0.9, 1.1)
#'   p
#' })
#'
#' aeme <- run_aeme_ensemble(aeme, model = "glm_aed", path = path,
#'                           param_sets = param_sets)
#' output(aeme)$n_members
#' }
run_aeme_ensemble <- function(aeme, model, param_sets, path,
                              parallel = FALSE, ncore = NULL,
                              model_controls = NULL, timeout = Inf,
                              drop_incomplete = TRUE, keep_diag = TRUE,
                              extra_vars = "none", sed_layers = NULL,
                              zone_summary = FALSE) {

  aeme <- check_aeme(aeme)
  if (missing(model)) {
    model <- list_models(aeme)
  }
  model <- check_model(model = model, os_valid = TRUE)
  if (missing(param_sets)) {
    cli::cli_abort(c("x" = "{.arg param_sets} must be supplied.",
                     "i" = "Sample sets from parameter bounds with the
                     {.pkg aemetools} package, or build your own."))
  }
  if (missing(path)) {
    path <- get_aeme_path(aeme)
  }
  path <- check_path(path = path, must_exist = TRUE)
  if (is.null(model_controls)) {
    model_controls <- get_model_controls(aeme = aeme)
  }

  sets <- normalise_param_sets(param_sets)
  n <- length(sets)
  lake_dir <- get_lake_dir(aeme = aeme, path = path)
  if (is.null(ncore)) ncore <- parallel::detectCores() - 1
  ncore <- max(1L, min(as.integer(ncore), n))

  mod_list <- lapply(model, function(m) {

    members <- lapply(sets, function(s) s[s$model == m, , drop = FALSE])

    run_one <- function(mp, run_path) {
      tryCatch({
        a2 <- aeme
        cfg <- configuration(a2)
        cfg$path <- normalizePath(run_path, mustWork = FALSE)
        configuration(a2) <- cfg
        input_model_parameters(aeme = a2, model = m, param = mp,
                               path = run_path)
        a2 <- run_aeme(aeme = a2, model = m, path = run_path, ens_n = 1,
                       return_type = "aeme", model_controls = model_controls,
                       timeout = timeout, keep_diag = keep_diag,
                       extra_vars = extra_vars, sed_layers = sed_layers,
                       zone_summary = zone_summary)
        if (drop_incomplete && !run_completed(a2, m)) return(NULL)
        output(a2)[["ens_001"]][[m]]
      }, error = function(e) {
        structure(list(message = conditionMessage(e)),
                  class = "aeme_member_error")
      })
    }

    if (parallel && n > 1) {
      run_dirs <- make_ensemble_dirs(model = m, lake_dir = lake_dir,
                                     n = ncore)
      idx_list <- split(seq_len(n), rep(seq_len(ncore), length.out = n))

      cl <- parallel::makeCluster(ncore)
      on.exit(parallel::stopCluster(cl), add = TRUE)
      parallel::clusterExport(
        cl, varlist = c("members", "idx_list", "run_dirs", "run_one"),
        envir = environment())
      cli_inform_safe(c("i" = paste0("Running a ", m, " ensemble of ", n,
                                     " member", if (n == 1) "" else "s",
                                     " on ", ncore, " core",
                                     if (ncore == 1) "" else "s", ".")))

      res_by_worker <- parallel::parLapply(
        cl, seq_along(idx_list), function(k) {
          stats::setNames(
            lapply(idx_list[[k]], function(j) run_one(members[[j]],
                                                      run_dirs[k])),
            as.character(idx_list[[k]]))
        })
      # Rebuild in member order. A pre-sized list starts all-NULL, so a failed
      # member keeps its slot without `[[<-` deleting it.
      out_list <- vector("list", n)
      for (w in res_by_worker) {
        for (nm in names(w)) {
          if (!is.null(w[[nm]])) out_list[[as.integer(nm)]] <- w[[nm]]
        }
      }
    } else {
      out_list <- lapply(members, run_one, run_path = path)
    }

    names(out_list) <- names(members)
    out_list
  })
  names(mod_list) <- model

  assemble_ens_output(aeme = aeme, mod_list = mod_list, model = model)
}

#' Assemble per-member model output into the `output()$ens_*` structure
#'
#' Members that failed for any requested model are dropped and the survivors
#' renumbered `ens_001 .. ens_00k`. The surviving member labels are stashed on
#' `attr(., "realisation")`.
#'
#' @param mod_list named list (one element per model) of per-member output.
#' @noRd
assemble_ens_output <- function(aeme, mod_list, model) {

  is_ok <- function(x) !is.null(x) && !inherits(x, "aeme_member_error")

  n <- length(mod_list[[1]])
  ok <- vapply(seq_len(n), function(i) {
    all(vapply(model, function(m) is_ok(mod_list[[m]][[i]]), logical(1)))
  }, logical(1))
  keep <- which(ok)

  if (length(keep) < n) {
    dropped <- if (is.null(names(mod_list[[1]]))) which(!ok) else
      names(mod_list[[1]])[!ok]
    # Surface the first error message, if any, so a systematic problem (for
    # example a worker that cannot load AEME) is not mistaken for failed runs.
    errs <- unlist(lapply(mod_list, function(ml) {
      vapply(ml, function(x) if (inherits(x, "aeme_member_error")) x$message
             else NA_character_, character(1))
    }))
    errs <- unique(errs[!is.na(errs)])
    cli::cli_warn(c(
      "{n - length(keep)} of {n} ensemble member{?s} failed or ended early and
       {?was/were} dropped.",
      "i" = "Dropped: {.val {dropped}}.",
      if (length(errs)) c("x" = "Error: {errs[1]}")))
  }
  if (length(keep) == 0) {
    cli::cli_abort("No ensemble member ran successfully.")
  }

  outp <- list()
  for (i in seq_along(keep)) {
    outp[[sprintf("ens_%03d", i)]] <- stats::setNames(
      lapply(model, function(m) mod_list[[m]][[keep[i]]]), model)
  }
  outp$n_members <- length(keep)

  labels <- names(mod_list[[1]])
  if (!is.null(labels)) attr(outp, "realisation") <- labels[keep]

  output(aeme) <- outp
  aeme
}

#' Did a model run reach the end with a physical water level?
#'
#' A model that drains the lake can still exit with status 0 (GLM does), so the
#' only evidence is in the output: the level reaches zero or below and then goes
#' `NA`. Mirrors the dry-lake guard aemetools applies when scoring a run.
#'
#' @param aeme `Aeme` object with the member's output loaded as `ens_001`.
#' @param model character; the model that was run.
#' @return `FALSE` if the water level is non-positive or `NA` at any timestep.
#'   `TRUE` otherwise, including when the level was not written and so cannot be
#'   checked.
#' @noRd
run_completed <- function(aeme, model) {
  lvl <- tryCatch(
    get_var(aeme = aeme, model = model, var_sim = "LKE_lvlwtr", ens_n = 1,
            return_df = TRUE, remove_spin_up = FALSE),
    error = function(e) NULL)
  if (is.null(lvl) || nrow(lvl) == 0) return(TRUE)
  !(anyNA(lvl$value) || any(lvl$value <= 0))
}

#' Coerce `param_sets` to a list of long `param` data.frames
#'
#' Accepts a list of data.frames (including the `aeme_param_sets` object from
#' aemetools) or one long data.frame with an `ensemble` id column. Member order
#' follows first appearance, so ids such as `"1"`, `"2"`, ..., `"10"` do not
#' re-sort alphabetically.
#' @noRd
normalise_param_sets <- function(x) {

  req <- c("model", "file", "name", "value")

  if (is.data.frame(x)) {
    id <- intersect(c("ensemble", "realisation"), names(x))
    if (length(id) == 0) {
      cli::cli_abort(c(
        "A data.frame {.arg param_sets} needs an {.field ensemble} column
         identifying each member.",
        "i" = "Columns present: {.val {names(x)}}."
      ))
    }
    id <- id[[1]]
    x <- split(x[, setdiff(names(x), id), drop = FALSE],
               factor(x[[id]], levels = unique(x[[id]])))
  }

  if (!is.list(x) || length(x) == 0) {
    cli::cli_abort("{.arg param_sets} must be a non-empty list of parameter
                   data.frames, or a long data.frame with an {.field ensemble}
                   column.")
  }

  ok <- vapply(x, function(d) is.data.frame(d) && all(req %in% names(d)),
               logical(1))
  if (!all(ok)) {
    cli::cli_abort(c(
      "Every member of {.arg param_sets} must be a data.frame with columns
       {.val {req}}.",
      "x" = "Member{?s} {.val {which(!ok)}} {?is/are} malformed."
    ))
  }

  unclass(x)
}

#' Copy a model directory once per parallel worker
#'
#' The previous run's `output/` and any restart or netCDF files are left
#' behind; `output/` is re-created empty because GLM aborts at initialisation
#' when it is missing.
#'
#' @return character vector of `n` worker run paths, each a root that holds
#'   `<basename(lake_dir)>/<model>`.
#' @noRd
make_ensemble_dirs <- function(model, lake_dir, n) {

  model_dir <- file.path(lake_dir, model)
  fils <- list.files(model_dir, full.names = TRUE)
  fils <- fils[basename(fils) != "output"]
  fils <- fils[!grepl("restart|\\.nc$", basename(fils))]

  vapply(seq_len(n), function(k) {
    root <- file.path(tempdir(), paste0("n_", k))
    dir <- file.path(root, basename(lake_dir), model)
    unlink(dir, recursive = TRUE, force = TRUE)
    dir.create(dir, recursive = TRUE)
    file.copy(fils, dir, recursive = TRUE)
    dir.create(file.path(dir, "output"), showWarnings = FALSE)
    root
  }, character(1))
}
