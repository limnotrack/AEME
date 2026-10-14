#' Assess model sensitivity to initial conditions and required spin-up
#'
#' Build and run an ensemble of simulations that differ only in their initial
#' conditions, repeated across a range of spin-up lengths, then measure how
#' the ensemble spread at the start of the analysis period collapses as
#' spin-up is extended. The shortest spin-up at which the spread falls below
#' `tolerance` is the length beyond which the choice of initial condition no
#' longer materially affects the reported variables.
#'
#' Each ensemble member is a perturbation of the baseline initial conditions
#' (`get_initial_conditions(aeme, model = model)`, falling back to
#' `input(aeme)$init_profile` / `model_controls$initial_wc`). `perturb` is a
#' named list:
#' * `"temperature"` / `"salt"` - numeric vector of **additive** offsets
#'   applied to that column of the initial profile.
#' * any `model_controls$var_aeme` name - numeric vector of **multiplicative**
#'   factors applied to that water-quality initial value.
#'
#' The vectors are recycled to a common length, which becomes the ensemble
#' size; member `i` takes element `i` of every vector.
#'
#' One [build_aeme()] + [run_aeme()] is performed per (spin-up, member)
#' combination, each in its own subdirectory of `path`, with written output
#' restricted to `vars`. This can be a large number of model runs.
#'
#' @inheritParams build_aeme
#' @param model character(1); the model to assess.
#' @param spin_up numeric; spin-up lengths in days to test. Default
#'   `c(0, 30, 90, 180, 365)`.
#' @param perturb named list describing the initial-condition ensemble (see
#'   Details). Default `list(temperature = c(-3, 0, 3))`.
#' @param vars character; AEME variable names to report and restrict written
#'   model output to. Default `"HYD_temp"`.
#' @param path character; directory under which the per-run subdirectories are
#'   created. Default `tempdir()`.
#' @param metric character(1); which summary column the recommendation (and
#'   `tolerance`) applies to. One of `"spread"` (default; across-member SD in
#'   the variable's units), `"spread_cv"` (that SD divided by the ensemble
#'   mean, per depth - dimensionless, comparable across variables),
#'   `"drift"` or `"drift_cv"` (the same normalisation applied to the
#'   distance from the longest-spin-up ensemble mean). All four are always
#'   present in `summary`; `metric` only selects the one used for
#'   `recommended`. The `*_cv` forms are meant for strictly-positive
#'   variables (water-quality concentrations, salinity, K); a CV is
#'   ill-defined where the ensemble mean crosses zero and those depths are
#'   dropped from the depth-average.
#' @param tolerance numeric(1); threshold on `metric` (a named vector, one
#'   per variable, is allowed) used to pick the recommended spin-up. `NULL`
#'   (default) skips the recommendation.
#' @param build_args,run_args lists of extra arguments passed to
#'   [build_aeme()] and [run_aeme()] respectively.
#' @param verbose logical; print model progress. Default `FALSE`.
#'
#' @returns A list of class `aeme_spin_up` with:
#' * `data` - long data.frame: `spin_up`, `member`, `var`, `depth`, `value`
#'   (state at the start of the analysis period).
#' * `summary` - `spin_up`, `var`, and, each as a mean over depth of the
#'   per-depth quantity: `spread` (across-member SD), `spread_cv`
#'   (SD / |ensemble mean|), `drift` (|this spin-up's mean - longest
#'   spin-up's mean|) and `drift_cv` (that / |longest spin-up's mean|).
#' * `recommended` - named numeric (per `var`) shortest spin-up whose
#'   `metric` meets `tolerance`, and `overall` the maximum of those; `NULL`
#'   if `tolerance` is `NULL`.
#' * `model`, `vars`, `metric`, `tolerance`, `failures`.
#'
#' @seealso [set_initial_conditions()], [plot_spin_up()]
#' @export
#'
#' @examples
#' \dontrun{
#' aeme_dir <- system.file("extdata/lake/", package = "AEME")
#' aeme <- yaml_to_aeme(path = aeme_dir, "aeme.yaml")
#' res <- assess_spin_up(aeme, model = "glm_aed",
#'                       spin_up = c(0, 30, 90, 180),
#'                       perturb = list(temperature = c(-4, 0, 4)),
#'                       tolerance = 0.5)
#' res$recommended
#' plot_spin_up(res)
#' }
assess_spin_up <- function(aeme, model,
                           spin_up = c(0, 30, 90, 180, 365),
                           perturb = list(temperature = c(-3, 0, 3)),
                           vars = "HYD_temp",
                           metric = c("spread", "spread_cv", "drift", "drift_cv"),
                           model_controls = NULL,
                           path = tempdir(),
                           tolerance = NULL,
                           build_args = list(),
                           run_args = list(),
                           verbose = FALSE) {

  aeme <- check_aeme(aeme)
  metric <- match.arg(metric)
  model <- check_model(model = model)
  if (length(model) != 1) {
    cli::cli_abort("{.arg model} must be a single model name.",
                   class = "aeme_error_spin_up")
  }
  if (!is.numeric(spin_up) || length(spin_up) < 2 || any(spin_up < 0)) {
    cli::cli_abort(
      "{.arg spin_up} must be a numeric vector of at least two non-negative values.",
      class = "aeme_error_spin_up"
    )
  }
  spin_up <- sort(unique(spin_up))

  if (is.null(model_controls)) {
    model_controls <- get_model_controls(aeme = aeme)
    if (is.null(model_controls)) model_controls <- get_model_controls()
  }

  perturb <- .check_perturb(perturb, model_controls)
  n_mem <- max(vapply(perturb, length, integer(1)))
  perturb <- lapply(perturb, rep_len, n_mem)

  base <- .baseline_ic(aeme, model, model_controls)

  path <- check_path(path = path, create = TRUE)

  grid <- expand.grid(spin_up = spin_up, member = seq_len(n_mem),
                      KEEP.OUT.ATTRS = FALSE)
  cli_inform_safe(c("i" = paste0(
    "Assessing spin-up for {.val {model}}: {nrow(grid)} run",
    if (nrow(grid) != 1) "s" else "",
    " ({length(spin_up)} spin-up length{?s} x {n_mem} ensemble member{?s})."
  )))

  results <- vector("list", nrow(grid))
  failures <- list()

  for (r in seq_len(nrow(grid))) {
    s <- grid$spin_up[r]
    i <- grid$member[r]
    run_dir <- file.path(path, sprintf("spinup_%s_m%02d",
                                       formatC(s, width = 4, flag = "0"), i))
    dir.create(run_dir, showWarnings = FALSE, recursive = TRUE)

    member_ic <- .member_ic(base, perturb, i)

    a <- aeme
    tm <- time(a)
    tm$spin_up[[model]] <- s
    time(a) <- tm
    a <- set_initial_conditions(
      a, model_controls = model_controls,
      model_init = stats::setNames(list(member_ic), model)
    )

    res <- tryCatch({
      a <- do.call(build_aeme, c(list(aeme = a, model = model, path = run_dir,
                                      model_controls = model_controls,
                                      output_vars = vars), build_args))
      a <- do.call(run_aeme, c(list(aeme = a, model = model, path = run_dir,
                                    verbose = verbose), run_args))
      .state_at_start(a, model, vars)
    }, error = function(e) e)

    if (inherits(res, "error")) {
      failures[[length(failures) + 1L]] <- list(spin_up = s, member = i,
                                                message = conditionMessage(res))
      cli::cli_warn(c("!" = "Run failed (spin_up = {s}, member = {i}).",
                      "i" = conditionMessage(res)))
      next
    }
    res$spin_up <- s
    res$member <- i
    results[[r]] <- res
  }

  data <- do.call(rbind, results)
  if (is.null(data) || nrow(data) == 0) {
    cli::cli_abort("Every run failed; nothing to summarise.",
                   class = "aeme_error_spin_up")
  }
  rownames(data) <- NULL
  data <- data[, c("spin_up", "member", "var", "depth", "value")]

  summ <- .summarise_spin_up(data)
  recommended <- if (is.null(tolerance)) {
    NULL
  } else {
    .recommend_spin_up(summ, tolerance, vars, metric = metric)
  }

  structure(
    list(data = data, summary = summ, recommended = recommended,
         model = model, vars = vars, metric = metric, tolerance = tolerance,
         failures = failures),
    class = "aeme_spin_up"
  )
}

#' Validate the `perturb` specification
#' @noRd
.check_perturb <- function(perturb, model_controls) {
  if (!is.list(perturb) || is.data.frame(perturb) || length(perturb) == 0) {
    cli::cli_abort("{.arg perturb} must be a non-empty named {.cls list}.",
                   class = "aeme_error_spin_up")
  }
  nms <- names(perturb)
  if (is.null(nms) || any(!nzchar(nms))) {
    cli::cli_abort("Every element of {.arg perturb} must be named.",
                   class = "aeme_error_spin_up")
  }
  allowed <- c("temperature", "salt", model_controls[["var_aeme"]])
  bad <- setdiff(nms, allowed)
  if (length(bad) > 0) {
    cli::cli_abort(
      c("Unknown {.arg perturb} name{?s}: {.val {bad}}.",
        "i" = paste("Use {.val temperature}, {.val salt} or a",
                    "{.code model_controls$var_aeme} name.")),
      class = "aeme_error_spin_up"
    )
  }
  for (nm in nms) {
    if (!is.numeric(perturb[[nm]]) || length(perturb[[nm]]) == 0 ||
        anyNA(perturb[[nm]])) {
      cli::cli_abort("{.arg perturb}${nm} must be a non-empty numeric vector.",
                     class = "aeme_error_spin_up")
    }
  }
  perturb
}

#' Baseline initial conditions for the assessed model
#' @noRd
.baseline_ic <- function(aeme, model, model_controls) {
  ic <- get_initial_conditions(aeme, model = model)
  prof <- ic[["profile"]]
  if (is.null(prof) || all(is.na(prof[["temperature"]]))) {
    prof <- input(aeme)[["init_profile"]]
  }
  if (is.null(prof) || !is.data.frame(prof) || nrow(prof) == 0) {
    cli::cli_abort(
      c("No baseline initial temperature profile found.",
        "i" = paste("Set one with {.fn set_initial_conditions} or",
                    "{.fn update_init} before calling {.fn assess_spin_up}.")),
      class = "aeme_error_spin_up"
    )
  }
  if (!"salt" %in% names(prof)) prof[["salt"]] <- 0
  wc <- stats::setNames(model_controls[["initial_wc"]],
                        model_controls[["var_aeme"]])
  list(profile = prof[, c("depth", "temperature", "salt")], wc = wc)
}

#' Construct ensemble member `i`'s initial-condition override
#' @noRd
.member_ic <- function(base, perturb, i) {
  prof <- base[["profile"]]
  if (!is.null(perturb[["temperature"]])) {
    prof[["temperature"]] <- prof[["temperature"]] + perturb[["temperature"]][i]
  }
  if (!is.null(perturb[["salt"]])) {
    prof[["salt"]] <- pmax(prof[["salt"]] + perturb[["salt"]][i], 0)
  }
  wq_names <- setdiff(names(perturb), c("temperature", "salt"))
  wq <- list()
  for (v in wq_names) {
    b <- base[["wc"]][[v]]
    if (is.na(b)) next
    wq[[v]] <- unname(b * perturb[[v]][i])
  }
  out <- list(profile = prof)
  if (length(wq) > 0) out[["wq"]] <- wq
  out
}

#' Extract the modelled state at the first non-spin-up timestep
#' @noRd
.state_at_start <- function(aeme, model, vars) {
  parts <- lapply(vars, function(v) {
    df <- get_var(aeme = aeme, model = model, var_sim = v, return_df = TRUE,
                  remove_spin_up = TRUE)
    df <- df[df[["Date"]] == min(df[["Date"]]), , drop = FALSE]
    if (!"depth" %in% names(df)) df[["depth"]] <- 0
    data.frame(var = v, depth = df[["depth"]], value = df[["value"]])
  })
  do.call(rbind, parts)
}

#' Summarise ensemble spread and drift by spin-up length
#'
#' Everything is computed per depth first, then averaged over depth. `*_cv`
#' columns normalise by the ensemble mean at that depth and are `NA` where
#' the mean is (near) zero.
#' @noRd
.summarise_spin_up <- function(data) {
  longest <- max(data[["spin_up"]])
  ref <- stats::aggregate(value ~ var + depth,
                          data = data[data$spin_up == longest, ], FUN = mean)
  names(ref)[names(ref) == "value"] <- "ref_value"

  by_depth <- stats::aggregate(value ~ spin_up + var + depth, data = data,
                               FUN = function(x) stats::sd(x))
  names(by_depth)[names(by_depth) == "value"] <- "spread"
  by_depth <- merge(by_depth,
                    stats::aggregate(value ~ spin_up + var + depth, data = data,
                                     FUN = mean),
                    by = c("spin_up", "var", "depth"))
  by_depth <- merge(by_depth, ref, by = c("var", "depth"))
  by_depth[["drift"]] <- abs(by_depth[["value"]] - by_depth[["ref_value"]])

  eps <- .Machine[["double.eps"]] ^ 0.5
  safe_ratio <- function(num, den) {
    r <- num / abs(den)
    r[!is.finite(r) | abs(den) < eps] <- NA_real_
    r
  }
  by_depth[["spread_cv"]] <- safe_ratio(by_depth[["spread"]], by_depth[["value"]])
  by_depth[["drift_cv"]] <- safe_ratio(by_depth[["drift"]],
                                       by_depth[["ref_value"]])

  depth_mean <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
  out <- stats::aggregate(
    cbind(spread, drift, spread_cv, drift_cv) ~ spin_up + var,
    data = by_depth, FUN = depth_mean, na.action = stats::na.pass
  )
  out[order(out[["var"]], out[["spin_up"]]), ]
}

#' Shortest spin-up meeting the tolerance, per variable
#' @param metric character(1); name of the `summ` column to test.
#' @noRd
.recommend_spin_up <- function(summ, tolerance, vars, metric = "spread") {
  if (is.null(names(tolerance))) {
    tolerance <- stats::setNames(rep_len(tolerance, length(vars)), vars)
  }
  rec <- vapply(vars, function(v) {
    s <- summ[summ[["var"]] == v, ]
    val <- s[[metric]]
    ok <- s[["spin_up"]][!is.na(val) & val <= tolerance[[v]]]
    if (length(ok) == 0) NA_real_ else min(ok)
  }, numeric(1))
  c(rec, overall = if (all(is.na(rec))) NA_real_ else max(rec, na.rm = TRUE))
}

#' Plot a spin-up assessment
#'
#' The chosen summary metric at the start of the analysis period as a
#' function of spin-up length, one line per reported variable.
#'
#' @param x an `aeme_spin_up` object from [assess_spin_up()].
#' @param metric character(1); `"spread"`, `"spread_cv"`, `"drift"` or
#'   `"drift_cv"`. Defaults to the metric the assessment was run with
#'   (`x$metric`).
#' @param ... unused.
#'
#' @returns A `ggplot` object.
#' @seealso [assess_spin_up()]
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_hline labs
#' @export
plot_spin_up <- function(x, metric = NULL, ...) {
  if (!inherits(x, "aeme_spin_up")) {
    cli::cli_abort("{.arg x} must be an {.cls aeme_spin_up} object.")
  }
  metric <- match.arg(metric %||% x[["metric"]] %||% "spread",
                      c("spread", "spread_cv", "drift", "drift_cv"))
  df <- x[["summary"]]
  df[["y"]] <- df[[metric]]

  y_lab <- c(
    spread = "Ensemble spread (SD across members)",
    spread_cv = "Ensemble spread (CV: SD / mean)",
    drift = "Drift from longest spin-up",
    drift_cv = "Drift from longest spin-up (relative)"
  )[[metric]]

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[["spin_up"]],
                                        y = .data[["y"]],
                                        colour = .data[["var"]])) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::labs(x = "Spin-up length (days)", y = y_lab, colour = "Variable",
                  title = paste0("Spin-up assessment: ", x[["model"]]))

  if (!is.null(x[["tolerance"]]) && identical(metric, x[["metric"]])) {
    p <- p + ggplot2::geom_hline(yintercept = unname(x[["tolerance"]][1]),
                                 linetype = "dashed")
  }
  p
}

#' @export
print.aeme_spin_up <- function(x, ...) {
  cli::cli_h2("Spin-up assessment: {x$model}")
  cli::cli_text("Variables: {.val {x$vars}}")
  cli::cli_text("Spin-up lengths (days): {.val {sort(unique(x$data$spin_up))}}")
  cli::cli_text("Ensemble members: {.val {length(unique(x$data$member))}}")
  if (length(x$failures) > 0) {
    cli::cli_alert_warning("{length(x$failures)} run{?s} failed.")
  }
  print(x$summary, row.names = FALSE)
  if (!is.null(x$recommended)) {
    cli::cli_h3("Recommended spin-up ({x$metric} <= tolerance)")
    print(x$recommended)
  }
  invisible(x)
}
