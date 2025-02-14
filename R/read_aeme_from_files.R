#' Read an AEME object from files
#'
#' @param path Path to the directory containing the AEME files.
#' @param aeme An empty or template AEME object to populate with data.
#'
#' @returns An AEME object populated with data from the files.
#' @export
#'
#' @examples
#' aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
#' aeme <- readRDS(aeme_file)
#' path <- "test_write"
#' model_controls <- get_model_controls()
#' aeme <- build_aeme(path = path, aeme = aeme, model = "glm_aed",
#' model_controls = model_controls)
#' aeme <- run_aeme(aeme = aeme, model = "glm_aed", path = path)
#' write_aeme_to_files(aeme, path)
#' aeme_path <- get_lake_dir(aeme = aeme, path = path)
#' aeme2 <- read_aeme_from_files(aeme_path)

read_aeme_from_files <- function(path) {
  # Ensure the directory exists
  if (!dir.exists(path)) {
    stop("The specified directory does not exist.")
  }
  init_path <- dirname(path)
  lake_dirname <- basename(path)
  id <- strsplit(lake_dirname, "_")[[1]][1]
  name <- strsplit(lake_dirname, "_")[[1]][2]

  aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
  aeme <- readRDS(aeme_file)

  lke <- lake(aeme)
  lke$name <- name
  lke$id <- id
  lake(aeme) <- lke

  model <- list.dirs(path, recursive = FALSE) |>
    basename()
  # Get the names of the slots
  slot_names <- slotNames(aeme)

  # Iterate over each slot
  for (slot_name in slot_names) {
    if (slot_name %in% c("lake","parameters")) {
      # Read CSV files for simple slots
      file_path <- file.path(path, paste0(slot_name, ".csv"))
      if (file.exists(file_path)) {
        df <- read.csv(file_path, stringsAsFactors = FALSE)
        names(df) <- gsub("-", ".", names(df))  # Revert column name changes
        slot(aeme, slot_name) <- df
      }
    } else if (slot_name == "time") {
      # Read CSV file for time slot
      file_path <- file.path(path, paste0(slot_name, ".csv"))
      if (file.exists(file_path)) {
        df <- read.csv(file_path, stringsAsFactors = FALSE)
        inp <- list(
          start = as.POSIXct(df$start, tz = "UTC"),
          stop = as.POSIXct(df$stop, tz = "UTC"),
          time_step = as.numeric(df$time_step),
          spin_up = list(
            dy_cd = as.numeric(df[1, grepl("dy_cd", names(df))]),
            glm_aed = as.numeric(df[1, grepl("glm_aed", names(df))]),
            gotm_wet = as.numeric(df[1, grepl("gotm_wet", names(df))])
          )
        )
        slot(aeme, slot_name) <- inp
      }

    } else if (slot_name %in% c("observations", "input")) {
      # Read CSV files for observations and input slots
      slot_content <- list()
      files <- list.files(path, pattern = paste0("^", slot_name, "_"))
      for (file in files) {
        obs_name <- sub(paste0(slot_name, "_"), "", sub(".csv", "", file))
        df <- read.csv(file.path(path, file), stringsAsFactors = FALSE)
        if (ncol(df) == 1) {
          df <- unlist(df)
        }
        if ("Date" %in% colnames(df)) {
          df$Date <- as.Date(df$Date)
        }

        slot_content[[obs_name]] <- df
      }
      slot(aeme, slot_name) <- slot_content
    } else if (slot_name %in% c("inflows", "outflows", "water_balance")) {
      # Read CSV files for inflows, outflows, and water_balance slots
      slot_content <- list(data = list())
      files <- list.files(path, pattern = paste0("^", slot_name, "_"))
      for (file in files) {
        if (grepl("_method_use.csv", file)) {
          df <- read.csv(file.path(path, file), stringsAsFactors = FALSE)
          slot_content$method <- df$method
          slot_content$use <- df$use
        } else if (grepl("_factors.csv", file)) {
          df <- read.csv(file.path(path, file), stringsAsFactors = FALSE)
          slot_content$factor <- df
        } else {
          inf_name <- sub(paste0(slot_name, "_"), "", sub(".csv", "", file))
          df <- read.csv(file.path(path, file), stringsAsFactors = FALSE)
          slot_content$data[[inf_name]] <- df
        }
      }
      slot(aeme, slot_name) <- slot_content
    } else if (slot_name == "output") {
      # Read CSV file for output slot
      file_path <- file.path(path, paste0(slot_name, ".csv"))
      if (file.exists(file_path)) {
        outp <- output(aeme)

        df <- read.csv(file_path, stringsAsFactors = FALSE)
        n_ens <- max(df$ens)

        for (ens_n in 1:n_ens) {
          sub <- df[df$ens == ens_n, ]

          mods <- lapply(model, \(m) {
            vars <- sub |>
              dplyr::filter(model == m) |>
              dplyr::pull(var_sim) |>
              unique()
            vars_out <- lapply(vars, \(v) {
              # Subset the data an convert to vector if all depth is NA or matrix if not
              sub2 <- sub |>
                dplyr::filter(model == m, var_sim == v) |>
                dplyr::select(-model, -var_sim, -ens)

              if (all(is.na(sub2$depth))) {
                out <- sub2 |>
                  dplyr::pull(value)
              } else {
                out <- sub2 |>
                  tidyr::pivot_wider(names_from = Date, values_from = value) |>
                  dplyr::select(-depth) |>
                  as.matrix()
              }
              return(out)
            })
            names(vars_out) <- vars
            vars_out$Date <- sub |>
              dplyr::filter(model == m) |>
              dplyr::arrange(Date) |>
              dplyr::pull(Date) |>
              unique() |>
              as.Date()
            return(vars_out)
          })
          names(mods) <- model

          ens_lab <- paste0("ens_", sprintf("%03d", ens_n))

          outp[[ens_lab]] <- list(dy_cd = mods[["dy_cd"]], glm_aed = mods[["glm_aed"]],
                                  gotm_wet = mods[["gotm_wet"]])
        }

        outp$n_members <- sum(grepl("ens", names(outp)))

        slot(aeme, slot_name) <- outp
      }
    } else if (slot_name == "configuration") {
      # Read CSV for model controls
      config_file <- file.path(path, paste0(slot_name, "_model_controls.csv"))
      if (file.exists(config_file)) {
        model_controls <- read.csv(config_file, stringsAsFactors = FALSE)
      }

      aeme <- load_configuration(model = model, aeme = aeme,
                                model_controls = model_controls,
                                path = init_path)
    }
  }

  return(aeme)
}
