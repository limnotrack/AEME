#' Set a value at a nested path in a list, creating levels as needed
#'
#' @param x list.
#' @param path character; keys from the outermost to the innermost level.
#' @param value value to set.
#' @noRd
set_nested <- function(x, path, value) {
  if (length(path) == 1) {
    x[[path]] <- value
    return(x)
  }
  x[[path[1]]] <- set_nested(x[[path[1]]], path[-1], value)
  x
}

#' Set `/`-delimited parameter names in a yaml/json configuration
#'
#' @param x list; parsed yaml/json config.
#' @param p data.frame; collapsed parameter rows for this file.
#' @param file character; file name used in warnings.
#' @return `x` with the values set.
#' @noRd
set_list_params <- function(x, p, file) {
  for (i in seq_len(nrow(p))) {
    path <- strsplit(p$name[i], "/")[[1]]
    warn_missing_list_keys(x, path, file)
    x <- set_nested(x, path, unlist(p$value[i]))
  }
  x
}

#' Warn about parameter files an `apply_params_*()` method can't handle
#' @noRd
warn_unhandled_files <- function(all_p, handled, model) {
  unhandled <- setdiff(unique(all_p$file), c(handled, "met", "inf", "wdr"))
  if (length(unhandled) > 0) {
    cli::cli_warn("Parameters for {.file {unhandled}} are not applied to \\
                  {.val {model}}: file not supported.")
  }
  invisible(unhandled)
}

#' Handle parameters whose target file is not in the configuration
#'
#' @param strict logical; abort if `TRUE`, otherwise warn and carry on.
#' @param msg character; cli message.
#' @noRd
config_missing <- function(strict, msg, env = parent.frame()) {
  if (strict) cli::cli_abort(msg, call = NULL, .envir = env)
  cli::cli_warn(c(msg, "i" = "These parameters are skipped."), .envir = env)
  invisible(NULL)
}

#' @rdname apply_parameters
#' @noRd
apply_params_gotm_wet <- function(config, all_p, strict = TRUE) {
  touched <- character(0)
  warn_unhandled_files(all_p, c("gotm.yaml", "fabm.yaml"), "gotm_wet")

  if ("gotm.yaml" %in% all_p$file) {
    x <- config[["hydrodynamic"]][["gotm"]]
    if (is.null(x)) {
      config_missing(strict,
                     "No {.file gotm.yaml} in the model configuration.")
    } else {
      config[["hydrodynamic"]][["gotm"]] <- set_list_params(
        x, all_p[all_p$file == "gotm.yaml", ], "gotm.yaml")
      touched <- c(touched, "gotm.yaml")
    }
  }
  if ("fabm.yaml" %in% all_p$file) {
    x <- config[["bgc"]][["fabm"]]
    if (is.null(x)) {
      config_missing(strict, c(
        "No {.file fabm.yaml} in the model configuration.",
        "i" = "Was the model built with {.code use_bgc = TRUE}?"))
    } else {
      config[["bgc"]][["fabm"]] <- set_list_params(
        x, all_p[all_p$file == "fabm.yaml", ], "fabm.yaml")
      touched <- c(touched, "fabm.yaml")
    }
  }
  list(config = config, touched = touched)
}

#' @rdname apply_parameters
#' @noRd
apply_params_simstrat <- function(config, all_p, model, strict = TRUE) {
  touched <- character(0)
  nml_label <- if (model == "simstrat_aed2") "aed2.nml" else "aed.nml"
  # Simstrat-AED2's group-indexed phyto/zoop nmls aren't supported: their
  # `%` syntax can't be parsed by AEME's generic nml reader (see
  # initialise_aed2()). Simstrat-AED's are CSV, the same as GLM-AED's.
  csv_files <- if (model == "simstrat_aed") {
    c("aed_phyto_pars.csv", "aed_zoop_pars.csv", "aed_macrophyte_pars.csv")
  } else {
    character(0)
  }
  warn_unhandled_files(all_p, c("simstrat.par", nml_label, csv_files), model)

  if ("simstrat.par" %in% all_p$file) {
    x <- config[["hydrodynamic"]]
    if (is.null(x)) {
      config_missing(strict,
                     "No {.file simstrat.par} in the model configuration.")
    } else {
      config[["hydrodynamic"]] <- set_list_params(
        x, all_p[all_p$file == "simstrat.par", ], "simstrat.par")
      touched <- c(touched, "simstrat.par")
    }
  }
  if (nml_label %in% all_p$file) {
    key <- tools::file_path_sans_ext(nml_label)
    nml <- config[["bgc"]][[key]]
    if (is.null(nml)) {
      config_missing(strict, c(
        "No {.file {nml_label}} in the model configuration.",
        "i" = "Was the model built with {.code use_bgc = TRUE}?"))
    } else {
      idx <- which(all_p$file == nml_label)
      arg_list <- lapply(idx, \(p) unlist(all_p$value[p]))
      names(arg_list) <- sapply(idx, \(p) gsub("/", "::", all_p$name[p]))
      warn_missing_nml_keys(nml, names(arg_list), nml_label)
      config[["bgc"]][[key]] <- set_nml(nml, arg_list = arg_list)
      touched <- c(touched, nml_label)
    }
  }
  for (f in csv_files[csv_files %in% all_p$file]) {
    key <- tools::file_path_sans_ext(f)
    df <- config[["bgc"]][[key]]
    if (is.null(df)) {
      config_missing(strict, c(
        "No {.file {f}} in the model configuration.",
        "i" = "Was the model built with {.code use_bgc = TRUE}?"))
    } else {
      config[["bgc"]][[key]] <- set_aed_csv_params(
        df, all_p[all_p$file == f, , drop = FALSE], f)
      touched <- c(touched, f)
    }
  }
  list(config = config, touched = touched)
}

#' @rdname apply_parameters
#' @noRd
apply_params_dy_cd <- function(config, all_p, strict = TRUE) {
  touched <- character(0)
  # DYRESM parameters are addressed by line number (the last `/` part of
  # `name`); the file is `dyresm3p1.par` ("par") or the lake's `.cfg` ("cfg")
  all_p$file[all_p$file %in% c("par", "dyresm3p1.par")] <- "par"
  all_p$file[all_p$file == "cfg" | grepl("\\.cfg$", all_p$file)] <- "cfg"
  warn_unhandled_files(all_p, c("par", "cfg"), "dy_cd")

  for (f in c("par", "cfg")) {
    if (!f %in% all_p$file) next
    lines <- config[["hydrodynamic"]][[f]]
    if (is.null(lines)) {
      config_missing(strict,
                     "No DYRESM {.val {f}} file in the model configuration.")
      next
    }
    for (p in which(all_p$file == f)) {
      value <- unlist(all_p$value[p])
      nme <- strsplit(all_p$name[p], "/")[[1]]
      lno <- as.numeric(nme[length(nme)])
      cmnt <- strsplit(trimws(lines[lno]), "#")[[1]]
      lines[lno] <- paste0(value, paste(" #", cmnt[2], collapse = " "))
    }
    config[["hydrodynamic"]][[f]] <- lines
    touched <- c(touched, f)
  }
  list(config = config, touched = touched)
}

#' Locate a model's configuration files, named by file name without extension
#'
#' [get_model_config_files()], except for GLM-AED where the `.nml` and `.csv`
#' files are found by listing the model directory. For GLM-AED
#' [get_model_config_files()] parses `aed.nml` to find the table paths, which
#' takes about as long as reading the file for a parameter update. Parameter
#' labels are literal file names, so listing is enough here. Where a file name
#' occurs more than once, the shortest path is used.
#'
#' @param model character; one model.
#' @param lake_dir character; the lake directory.
#' @return named character vector of paths.
#' @noRd
locate_config_files <- function(model, lake_dir) {
  if (model != "glm_aed") {
    return(get_model_config_files(path = lake_dir, model = model)[[model]])
  }
  f <- .find_model_files(lake_dir, "glm_aed", "\\.(nml|csv)$")
  f <- f[order(nchar(f))]
  nm <- tools::file_path_sans_ext(basename(f))
  stats::setNames(f[!duplicated(nm)], nm[!duplicated(nm)])
}

#' Read just the configuration `apply_parameters()` needs for a model
#'
#' Same structure as [read_model_config()], but only the configuration files
#' named in `labels` (the `file` column of the parameters being applied) are
#' read, plus the files `apply_parameters()` needs alongside them. This runs
#' once per model run in calibration and sensitivity analysis, so reading
#' every file of the model each time adds up. Files that are not read are left
#' out of the returned list; `write_params()` only writes the files
#' `apply_parameters()` touched, so they are not affected.
#'
#' @param model character; one model.
#' @param lake_dir character; the lake directory.
#' @param labels character or `NULL`; file labels of the parameters to apply.
#'   `NULL` reads every configuration file ([read_model_config()]).
#' @param files named character vector of paths from `locate_config_files()`;
#'   located if `NULL`.
#' @noRd
read_config_for_params <- function(model, lake_dir, labels = NULL,
                                   files = NULL) {
  if (is.null(labels)) return(read_model_config(model, lake_dir))
  if (is.null(files)) files <- locate_config_files(model, lake_dir)
  labels <- setdiff(unique(labels), c("met", "inf", "wdr"))

  read_file <- function(key) {
    f <- files[[key]]
    switch(
      tools::file_ext(f),
      nml = read_nml(f),
      csv = ,
      tsv = read_aed_param_csv(f),
      yaml = yaml::read_yaml(file = f),
      par = if (model %in% c("simstrat_aed2", "simstrat_aed")) {
        jsonlite::fromJSON(f, simplifyVector = FALSE)
      } else {
        readLines(f)
      },
      readLines(f)
    )
  }
  read_keys <- function(keys) {
    keys <- unique(keys[keys %in% names(files)])
    stats::setNames(lapply(keys, read_file), keys)
  }

  cfg <- list()
  switch(
    model,
    glm_aed = {
      glm_key <- find_glm_nml_key(names(files))
      cfg$hydrodynamic_file <- paste0(glm_key, ".nml")
      is_hydro <- grepl("^glm[0-9]+\\.nml$", labels)
      if (any(is_hydro)) cfg$hydrodynamic <- read_file(glm_key)
      keys <- tools::file_path_sans_ext(labels[!is_hydro])
      # Group-indexed aed2 nmls are located through aed2.nml, and the totals
      # re-derived after a phytoplankton change need both aed and its phyto
      # table
      if (any(c("aed2_phyto_pars", "aed2_zoop_pars") %in% keys)) {
        keys <- c(keys, "aed2")
      }
      if (any(c("aed", "aed_phyto_pars") %in% keys)) {
        keys <- c(keys, "aed", "aed_phyto_pars")
      }
      cfg$bgc <- read_keys(keys)
    },
    gotm_wet = {
      cfg$hydrodynamic <- list()
      if ("gotm.yaml" %in% labels) cfg$hydrodynamic$gotm <- read_file("gotm")
      cfg$bgc <- read_keys(if ("fabm.yaml" %in% labels) "fabm")
    },
    simstrat_aed2 = ,
    simstrat_aed = {
      if ("simstrat.par" %in% labels) cfg$hydrodynamic <- read_file("simstrat")
      cfg$bgc <- read_keys(tools::file_path_sans_ext(
        labels[labels != "simstrat.par"]))
    },
    dy_cd = {
      cfg$hydrodynamic <- list(par = readLines(files[["par"]]),
                               cfg = readLines(files[["cfg"]]))
    }
  )
  cfg
}

#' Write the configuration files changed by `apply_parameters()` back to disk
#'
#' Only the files listed in `res$touched` are written: round-tripping an
#' untouched file through its reader and writer can change it. Each file is
#' written to the location it was read from.
#'
#' @param res list returned by `apply_parameters()`.
#' @param lake_dir character; the lake directory containing the model
#'   directory.
#' @param model character; the model `res` was produced for.
#' @param files named character vector of paths from `locate_config_files()`;
#'   located if `NULL`.
#' @return Invisibly, the labels of the files written.
#' @noRd
write_params <- function(res, lake_dir, model, files = NULL) {
  if (is.null(files)) files <- locate_config_files(model, lake_dir)
  cfg <- res$config
  for (label in res$touched) {
    key <- tools::file_path_sans_ext(label)
    switch(
      model,
      glm_aed = {
        hydro_label <- cfg[["hydrodynamic_file"]]
        if (is.null(hydro_label)) hydro_label <- "glm3.nml"
        obj <- if (label == hydro_label) {
          cfg[["hydrodynamic"]]
        } else {
          cfg[["bgc"]][[key]]
        }
        if (tools::file_ext(label) == "csv") {
          write_aed_param_csv(obj, files[[key]])
        } else {
          write_nml(obj, files[[key]])
        }
      },
      gotm_wet = {
        obj <- if (label == "gotm.yaml") {
          cfg[["hydrodynamic"]][["gotm"]]
        } else {
          cfg[["bgc"]][["fabm"]]
        }
        write_yaml(obj, files[[key]])
      },
      simstrat_aed2 = ,
      simstrat_aed = {
        if (label == "simstrat.par") {
          jsonlite::write_json(cfg[["hydrodynamic"]], files[["simstrat"]],
                               pretty = TRUE, auto_unbox = TRUE, null = "null")
        } else if (tools::file_ext(label) == "csv") {
          write_aed_param_csv(cfg[["bgc"]][[key]], files[[key]])
        } else {
          write_nml(cfg[["bgc"]][[key]], files[[key]])
        }
      },
      dy_cd = writeLines(cfg[["hydrodynamic"]][[label]], files[[label]])
    )
  }
  invisible(res$touched)
}

#' Set parameters in an AED parameter table (phyto, zoop or macrophyte CSV)
#'
#' The tables have one row per parameter and one column per group, so each
#' parameter row selects a row by `name` and a column by `group`. Shared by
#' GLM-AED and Simstrat-AED, which read the same files.
#'
#' @param df data.frame; as read by [read_aed_param_csv()].
#' @param p data.frame; collapsed parameter rows for this file.
#' @param file character; the file label, e.g. `"aed_zoop_pars.csv"`. Zooplankton
#'   values are stored as character.
#' @return `df` with the values set.
#' @noRd
set_aed_csv_params <- function(df, p, file) {
  for (j in seq_len(nrow(p))) {
    col_idx <- which(grepl(p$group[j], names(df)))
    row_idx <- which(df[[1]] == p$name[j])
    if (file == "aed_zoop_pars.csv") {
      df[row_idx, col_idx] <- as.character(unlist(p$value[j]))
    } else {
      df[row_idx, col_idx] <- unlist(p$value[j])
    }
  }
  df
}
