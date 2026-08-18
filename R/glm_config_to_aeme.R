#' Reconstruct an Aeme object from a GLM-AED model configuration
#'
#' The inverse of [build_aeme()] for the GLM-AED model. Parses an existing
#' GLM hydrodynamic nml file (`glm3.nml`, `glm4.nml`, or any future
#' `glm<version>.nml`; see `find_glm_nml()`) and the sibling files it
#' references (meteorology, inflows, outflows, AED biogeochemistry) and
#' reassembles them into an `Aeme` object.
#'
#' @param nml_file character; path to a GLM hydrodynamic nml file (e.g.
#'   `glm3.nml`, `glm4.nml`), typically inside a `<id>_<name>/glm_aed/`
#'   directory written by [build_aeme()].
#' @param model_controls data.frame; model configuration, typically loaded
#'   via [get_model_controls()]. If `NULL` (default), one is generated with
#'   [get_model_controls()], using biogeochemistry state (see Details) to set
#'   `use_bgc`.
#' @param spin_up numeric; number of spin-up days assumed to have been
#'   subtracted from `time$start` when the nml file was written (see
#'   Details). Default `2`, matching [aeme_constructor()]'s own default.
#' @param read_params logical; also recover a `parameters` data frame (as set
#'   by [add_param()]) by cross-referencing every parameter known to
#'   [get_aeme_parameters()] against the value actually present in the GLM
#'   nml, `aed/aed.nml`, and the AED parameter CSVs. Default `FALSE`.
#'
#' @details
#' Several pieces of the original `Aeme` object cannot be recovered exactly
#' from the GLM-AED files alone, and are approximated:
#' \itemize{
#'   \item `lake$id` is taken from the `<id>_<name>` lake directory naming
#'   convention used by [build_aeme()] (the parent of `nml_file`'s
#'   directory). If the directory does not follow that convention,
#'   `"0001"` is used and a warning is issued.
#'   \item `lake$name` is whatever is stored in `morphometry$lake_name`,
#'   which [build_aeme()] always writes in lower case -- the original
#'   capitalisation is not recoverable.
#'   \item `lake$elevation` (and the hypsograph's elevation datum) is taken
#'   as `crest_elev`, i.e. the top of the hypsograph as written to the nml.
#'   If the hypsograph was extended with `ext_elev` at build time, this will
#'   not match the original `lake$elevation`.
#'   \item `time$start` has `spin_up` days added back on, since
#'   [build_aeme()] subtracts each model's spin-up period from `time$start`
#'   before writing it to the nml file. The true original spin-up is not
#'   stored anywhere in the GLM-AED files, so `spin_up` is a caller-supplied
#'   guess (default: `aeme_constructor()`'s own default of 2 days).
#'   \item `inflows()$factor` / `outflows()$factor` are assumed to be `1` --
#'   any factor applied at build time is already baked into the written
#'   `.csv` values and cannot be separated back out.
#'   \item `parameters` (when `read_params = TRUE`) only recovers scalar,
#'   numeric parameters known to AEME's parameter catalogue
#'   ([get_aeme_parameters()]); `min`/`max`/`group` come from that
#'   catalogue, not from the lake-specific files (which do not store them).
#'   Logical/character nml values (e.g. `glm_setup::non_avg`) and
#'   vector-valued/indexed nml parameters (e.g. per sediment zone) are
#'   skipped, since the catalogue has no lake-specific notion of a vector's
#'   true length and recovering only part of it would corrupt the field if
#'   written back via [input_model_parameters()].
#' }
#'
#' The returned object has `configuration()$calc_wbal`, `calc_wlev`, and
#' `ext_elev` set to `FALSE`/`FALSE`/`0`, rather than
#' [build_aeme()]'s own defaults of `TRUE`/`TRUE`/`0`. The loaded
#' `inflows()`/`outflows()`/hypsograph already reflect a finished water
#' balance and lake-level calculation; leaving `calc_wbal`/`calc_wlev` at
#' their `build_aeme()` defaults would make a subsequent
#' `build_aeme(aeme = ., use_aeme = TRUE)` call recompute and silently
#' overwrite those loaded values (`use_aeme = TRUE` only preserves the raw
#' nml text -- it does not by itself disable the water-balance/lake-level
#' recalculation that runs before the per-model config files are written).
#' Passing `calc_wbal`/`calc_wlev`/`ext_elev` explicitly to a later
#' [build_aeme()] call overrides these stored values as usual.
#'
#' The GLM version this configuration was read from (e.g. `"glm3.nml"` or
#' `"glm4.nml"`) is recorded in `configuration()$glm_aed$hydrodynamic_file`,
#' so a later [write_configuration()] or [build_aeme()] call writes it back
#' out under the same filename rather than assuming `glm3.nml`.
#'
#' @returns An `Aeme` object.
#' @export
#'
#' @importFrom dplyr mutate arrange desc
#' @importFrom cli cli_abort cli_warn
#'
#' @examples
#' aeme_dir <- system.file("extdata/lake/", package = "AEME")
#' path <- file.path(tempdir(), "glm_config_to_aeme_example")
#' aeme <- yaml_to_aeme(path = aeme_dir, "aeme.yaml")
#' model_controls <- get_model_controls()
#' aeme <- aeme |>
#'   build_aeme(path = path, model = "glm_aed", model_controls = model_controls,
#'              ext_elev = 5)
#' nml_file <- file.path(get_lake_dir(aeme, path), "glm_aed", "glm3.nml")
#' aeme2 <- glm_config_to_aeme(nml_file)

glm_config_to_aeme <- function(nml_file, model_controls = NULL, spin_up = 2,
                               read_params = FALSE) {
  # Set timezone temporarily to UTC
  withr::local_locale(c("LC_TIME" = "C"))
  withr::local_timezone("UTC")

  if (!file.exists(nml_file)) {
    cli::cli_abort("{.arg nml_file} {.file {nml_file}} does not exist.")
  }
  path_glm <- dirname(nml_file)
  nml <- read_nml(nml_file)

  # ---- lake ----
  elevation <- max(nml$morphometry$H)
  lake <- list(
    name      = nml$morphometry$lake_name,
    id        = .glm_cfg_lake_id(path_glm),
    latitude  = nml$morphometry$latitude,
    longitude = nml$morphometry$longitude,
    elevation = elevation,
    depth     = elevation - min(nml$morphometry$H),
    area      = max(nml$morphometry$A)
  )

  # ---- time ----
  nml_start <- as.POSIXct(nml$time$start, tz = "UTC")
  time <- list(
    start     = nml_start + spin_up * 86400,
    stop      = as.POSIXct(nml$time$stop, tz = "UTC"),
    time_step = nml$time$dt,
    spin_up   = list(dy_cd = 2, glm_aed = spin_up, gotm_wet = 2,
                     simstrat_aed2 = 2)
  )

  # ---- hypsograph ----
  hypsograph <- data.frame(
    elev = nml$morphometry$H,
    area = nml$morphometry$A
  ) |>
    dplyr::mutate(depth = elev - elevation) |>
    dplyr::arrange(dplyr::desc(elev))

  # ---- init profile ----
  init_depth <- nml$init_profiles$lake_depth
  init_profile <- data.frame(
    depth       = nml$init_profiles$the_depths,
    temperature = nml$init_profiles$the_temps,
    salt        = nml$init_profiles$the_sals
  )

  # ---- meteorology / Kw ----
  met <- .glm_cfg_read_meteo(nml, path_glm)
  Kw <- nml$light$Kw
  use_lw <- identical(nml$meteorology$lw_type, "LW_IN")

  input <- list(
    init_profile = init_profile,
    init_depth   = init_depth,
    hypsograph   = hypsograph,
    meteo        = met,
    use_lw       = use_lw,
    Kw           = Kw
  )

  # ---- inflows / outflows ----
  inflows  <- .glm_cfg_read_inflows(nml, path_glm)
  outflows <- .glm_cfg_read_outflows(nml, path_glm)

  # ---- configuration ----
  use_bgc <- !is.null(nml[["wq_setup"]])
  if (is.null(model_controls)) {
    model_controls <- get_model_controls(use_bgc = use_bgc)
  }
  model_cfg <- read_model_config(model = "glm_aed", lake_dir = path_glm)

  aeme <- aeme_constructor(
    lake     = lake,
    time     = time,
    input    = input,
    inflows  = inflows,
    outflows = outflows
  )

  cfg <- config_defaults()
  cfg$aeme_version = as.character(utils::packageVersion("AEME"))
  cfg$model_controls <- model_controls
  cfg$use_bgc <- use_bgc
  cfg$path <- dirname(dirname(path_glm))
  cfg$glm_aed <- model_cfg
  # The loaded inflows/outflows/hypsograph/init profile already reflect a
  # finished water-balance/lake-level calculation -- if build_aeme() were
  # left to redo that (its defaults), it would silently recompute and
  # overwrite the very values this function just recovered. A later
  # build_aeme(aeme = this, use_aeme = TRUE) call picks these up via
  # get_config_value() unless the caller explicitly overrides them.
  cfg$ext_elev <- 0
  cfg$calc_wbal <- FALSE
  cfg$calc_wlev <- FALSE
  configuration(aeme) <- cfg

  if (isTRUE(read_params)) {
    param_df <- tryCatch(
      .glm_cfg_read_params(glm_nml = nml, bgc = model_cfg$bgc),
      error = function(e) {
        cli::cli_warn(c("!" = "Could not recover {.arg parameters}: {conditionMessage(e)}"))
        NULL
      }
    )
    if (!is.null(param_df) && nrow(param_df) > 0) {
      aeme <- add_param(aeme, param_df)
    }
  }

  aeme
}

#' Determine a lake's id from its `<id>_<name>` directory naming convention
#' @param path_glm character; path to the `glm_aed` directory (the parent of
#'   the GLM hydrodynamic nml file).
#' @noRd
.glm_cfg_lake_id <- function(path_glm) {
  dir_name <- basename(dirname(path_glm))
  parts <- strsplit(dir_name, "_", fixed = TRUE)[[1]]
  if (length(parts) >= 2 && grepl("^[A-Za-z]*[0-9]+$", parts[1])) {
    return(parts[1])
  }
  cli::cli_warn(
    c("!" = "Could not determine lake {.arg id} from directory name {.val {dir_name}}.",
      "i" = "Defaulting to {.val {'0001'}}; set {.code lake(aeme)$id <- ...} manually if needed.")
  )
  "0001"
}

#' Read a GLM-AED meteorology file back into AEME's standard columns/units
#' @inheritParams .glm_cfg_lake_id
#' @param nml list; parsed GLM hydrodynamic nml (via [read_nml()]).
#' @noRd
.glm_cfg_read_meteo <- function(nml, path_glm) {
  met_file <- file.path(path_glm, nml$meteorology$meteo_fl)
  if (!file.exists(met_file)) {
    cli::cli_abort("Meteorological file {.file {met_file}} referenced by
                    the GLM nml file does not exist.")
  }
  met <- read.csv(met_file, stringsAsFactors = FALSE)

  use_lw <- identical(nml$meteorology$lw_type, "LW_IN")
  if (use_lw) {
    glm_names  <- c("time", "ShortWave", "LongWave", "AirTemp", "RelHum",
                    "WindSpeed", "Rain", "Snow", "AirPres")
    aeme_names <- c("Date", "MET_radswd", "MET_radlwd", "MET_tmpair",
                    "MET_humrel", "MET_wndspd", "MET_pprain", "MET_ppsnow",
                    "MET_prsttn")
  } else {
    glm_names  <- c("time", "ShortWave", "Cloud", "AirTemp", "RelHum",
                    "WindSpeed", "Rain", "Snow", "AirPres")
    aeme_names <- c("Date", "MET_radswd", "MET_cldcvr", "MET_tmpair",
                    "MET_humrel", "MET_wndspd", "MET_pprain", "MET_ppsnow",
                    "MET_prsttn")
  }
  idx <- match(names(met), glm_names)
  if (anyNA(idx)) {
    cli::cli_warn(
      c("!" = "Unrecognised meteorology column{?s} {.val {names(met)[is.na(idx)]}} left unchanged.")
    )
  }
  names(met)[!is.na(idx)] <- aeme_names[idx[!is.na(idx)]]

  met$Date <- as.Date(met$Date)
  # GLM stores rain/snow in m; AEME uses mm
  met$MET_pprain <- met$MET_pprain * 1000
  met$MET_ppsnow <- met$MET_ppsnow * 1000
  met
}

#' Read GLM-AED inflow files back into AEME's standard columns/units
#' @inheritParams .glm_cfg_read_meteo
#' @noRd
.glm_cfg_read_inflows <- function(nml, path_glm) {
  out <- list(data = NULL,
              factor = list(dy_cd = 1, glm_aed = 1, gotm_wet = 1,
                           simstrat_aed2 = 1))
  inf_block <- nml[["inflow"]]
  if (is.null(inf_block) || isTRUE(inf_block$num_inflows == 0) ||
      identical(inf_block$names_of_strms, "none")) {
    return(out)
  }
  names_inf <- inf_block$names_of_strms
  files_inf <- inf_block$inflow_fl

  env <- new.env(parent = emptyenv())
  data("key_naming", package = "AEME", envir = env)
  key <- env$key_naming

  inflow_data <- list()
  for (i in seq_along(names_inf)) {
    f <- file.path(path_glm, files_inf[i])
    if (!file.exists(f)) {
      cli::cli_warn("Inflow file {.file {f}} does not exist, skipping inflow {.val {names_inf[i]}}.")
      next
    }
    df <- read.csv(f, stringsAsFactors = FALSE)
    new_names <- rename_modelvars(names(df), type_input = "glm_aed",
                                  type_output = "var_aeme",
                                  warn_unmatched = TRUE)
    keep <- !is.na(new_names)
    df <- df[keep]
    names(df) <- new_names[keep]

    # Undo AED mass-unit conversion (aeme_val = glm_val * conversion_aed)
    for (col in setdiff(names(df), "Date")) {
      mult <- key$conversion_aed[match(col, key$var_aeme)]
      if (length(mult) == 1 && !is.na(mult)) {
        df[[col]] <- df[[col]] * mult
      }
    }
    # Undo m3/s -> m3/day conversion for flow (assumes inf_factor = 1)
    if ("HYD_flow" %in% names(df)) {
      df$HYD_flow <- df$HYD_flow * 86400
    }
    df$Date <- as.Date(df$Date)
    inflow_data[[names_inf[i]]] <- df
  }
  out$data <- if (length(inflow_data) > 0) inflow_data else NULL
  out
}

#' Read GLM-AED outflow files back into AEME's standard columns/units
#' @inheritParams .glm_cfg_read_meteo
#' @noRd
.glm_cfg_read_outflows <- function(nml, path_glm) {
  out <- list(data = NULL, elevation = list(),
              factor = list(dy_cd = 1, glm_aed = 1, gotm_wet = 1,
                           simstrat_aed2 = 1))
  outf_block <- nml[["outflow"]]
  if (is.null(outf_block) || isTRUE(outf_block$num_outlet == 0)) {
    return(out)
  }
  files_outf <- outf_block$outflow_fl
  elevs      <- outf_block$outl_elvs
  names_outf <- basename(files_outf)
  names_outf <- sub("^outflow_", "", names_outf)
  names_outf <- sub("\\.csv$", "", names_outf)

  outflow_data <- list()
  elevation <- list()
  for (i in seq_along(names_outf)) {
    f <- file.path(path_glm, files_outf[i])
    if (!file.exists(f)) {
      cli::cli_warn("Outflow file {.file {f}} does not exist, skipping outflow {.val {names_outf[i]}}.")
      next
    }
    df <- read.csv(f, stringsAsFactors = FALSE)
    # Undo m3/s -> m3/day conversion for flow (assumes outf_factor = 1)
    flow <- df$flow * 86400
    if (identical(names_outf[i], "wbal")) {
      # build_aeme()'s auto-computed water-balance outflow is always named
      # "wbal" and, uniquely among outflows, is written/read with a
      # `model`/`outflow` schema instead of `HYD_flow` -- make_wdr_glm()
      # special-cases the literal name "wbal" and expects exactly this
      # shape (see make_wdr_glm.R).
      outflow_data[["wbal"]] <- data.frame(
        Date = as.Date(df$time),
        model = "glm_aed",
        outflow = flow
      )
    } else {
      outflow_data[[names_outf[i]]] <- data.frame(
        Date     = as.Date(df$time),
        HYD_flow = flow
      )
    }
    elevation[[names_outf[i]]] <- elevs[i]
  }
  out$data <- if (length(outflow_data) > 0) outflow_data else NULL
  out$elevation <- elevation
  out
}

#' Recover a `parameters` data frame by cross-referencing AEME's parameter
#' catalogue against the values actually present in the GLM-AED config files
#' @param glm_nml list; parsed GLM hydrodynamic nml (via [read_nml()]).
#' @param bgc list or `NULL`; the `bgc` element of
#'   `read_model_config(model = "glm_aed", ...)`, i.e. a named list with
#'   (when biogeochemistry is enabled) an `aed` element (parsed `aed.nml`)
#'   and `aed_phyto_pars`/`aed_zoop_pars`/`aed_macrophyte_pars` data frames.
#' @noRd
.glm_cfg_read_params <- function(glm_nml, bgc) {
  catalog <- get_aeme_parameters(model = "glm_aed")

  # The parameter catalogue always tags the GLM hydrodynamic nml as
  # "glm3.nml" regardless of the GLM version actually used (see
  # find_glm_nml()) -- this key is purely an internal lookup matching that
  # fixed catalogue convention, not a claim about the source file's name
  nml_lookup <- list("glm3.nml" = glm_nml)
  if (!is.null(bgc[["aed"]])) {
    nml_lookup[["aed.nml"]] <- bgc[["aed"]]
  }

  extract_one <- function(i) {
    row <- catalog[i, ]
    val <- tryCatch({
      if (row$file %in% names(nml_lookup)) {
        # The catalogue's `index` marks a position within a vector-valued
        # nml field (e.g. per sediment zone), but the catalogue is generic
        # and has no notion of *this* lake's actual vector length (e.g.
        # n_zones); it may only describe a subset of the positions actually
        # present. Recovering just one indexed element and later writing it
        # back (via input_model_parameters()/set_nml()) would silently
        # truncate the whole field to length 1, corrupting the nml -- so
        # only recover genuinely scalar (unindexed) nml parameters.
        if (!is.na(row$index)) return(NA_real_)
        parts <- strsplit(row$name, "/", fixed = TRUE)[[1]]
        v <- get_nml_value(nml_lookup[[row$file]], parts[length(parts)])
        v
      } else if (grepl("\\.csv$", row$file)) {
        df <- bgc[[tools::file_path_sans_ext(row$file)]]
        if (is.null(df)) return(NA_real_)
        name_col <- names(df)[1]
        rr <- df[df[[name_col]] == row$name, , drop = FALSE]
        if (nrow(rr) == 0 || is.na(row$group)) return(NA_real_)
        col_idx <- grep(row$group, names(df))
        if (length(col_idx) == 0) return(NA_real_)
        rr[[col_idx[1]]]
      } else {
        NA_real_
      }
    }, error = function(e) NA_real_)
    # Only numeric parameters round-trip through the `parameters` data
    # frame's plain `value` column -- logical (e.g. glm_setup::non_avg) and
    # character nml values would silently be corrupted by as.numeric(), and
    # input_model_parameters() would later fail to write them back (it
    # requires the replacement value's type to match the nml's existing
    # value), so skip them rather than recover them incorrectly.
    if (is.null(val) || length(val) != 1 || is.logical(val) ||
        is.character(val)) {
      NA_real_
    } else {
      suppressWarnings(as.numeric(val))
    }
  }

  catalog$value <- vapply(seq_len(nrow(catalog)), extract_one, numeric(1))
  catalog[!is.na(catalog$value), ]
}
