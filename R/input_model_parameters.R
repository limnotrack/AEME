#' Input model parameters
#'
#' @inheritParams build_aeme
#' @param param data.frame; parameters to input into the model
#' configuration files
#'
#' @return NULL
#' @export
#'

input_model_parameters <- function(aeme, model, param, path) {

  # Function checks ----
  if (!is.data.frame(param))
    stop("Parameter 'param' must be a data.frame.")
  if (!is.character(model))
    stop("Parameter 'model' must be a character string.")

  # Collapse parameters
  param <- collapse_params(param)

  # Load AEME data
  lke <- AEME::lake(aeme)
  lakename <- tolower(lke[["name"]])
  lake_dir <- get_lake_dir(aeme, path)
  inp <- input(aeme)
  obs <- observations(aeme)
  obs$lake$depth_mid <- (obs$lake$depth_to - obs$lake$depth_from) / 2

  if (!is.null(obs$level)) {
    z_max <- mean(obs$level[["value"]]) - min(inp$hypsograph$elev)
  } else {
    z_max <- max(inp$hypsograph$elev) - min(inp$hypsograph$elev)
  }

  # Check model parameters in supplied parameters
  for (m in model) {
    if (!m %in% param[["model"]]) {
      warning("No parameters in 'param' for ", m, ".")
    }
  }

  sapply(model, \(m) {

    # Path for model
    model_path <- file.path(lake_dir, m)

    all_p <- param[param$model == m, ] # Subset parameters to model specific
    if (nrow(all_p) == 0) return(NULL)

    # Scale met data ----
    if ("met" %in% all_p[["file"]]) {

      # Read in meteo file ----
      met_idx <- which(param$model == m & param$file == "met")

      met <- inp$meteo

      z_max <- max(inp$hypsograph$elev) - min(inp$hypsograph$elev)

      # Apply scaling factors ----
      for (v in met_idx) {
        value <- unlist(param[["value"]][v])
        if (param$name[v] == "MET_wndspd") {
          met[["MET_wnduvu"]] <- met[["MET_wnduvu"]] * value
          met[["MET_wnduvv"]] <- met[["MET_wnduvv"]] * value
        }
        met[[param$name[v]]] <- met[[param$name[v]]] * value
        if (param$name[v] == "MET_cldcvr") {
          met[[param$name[v]]][met[[param$name[v]]] < 0] <- 0
          met[[param$name[v]]][met[[param$name[v]]] > 1] <- 1
        }
      }

      if(m == "glm_aed") {
        make_metGLM(obs_met = met, path_glm = model_path,
                    use_lw = inp$use_lw)
      } else if(m == "gotm_wet") {
        make_metGOTM(df_met = met, path.gotm = model_path,
                     return_colname = FALSE, lat = lke$latitude,
                     lon = lke$longitude)
      } else if(m == "dy_cd") {
        # lakename <- strsplit(basename(lake_dir), "_")[[1]][2]
        make_DYmet(lakename = lakename, info = "test", obsMet = met,
                   filePath = model_path, infRain = FALSE, wndType = 0,
                   metHeight = 15, z_max = z_max, use_lw = inp$use_lw)
      }
    }

    # Scale wdr data ----
    if ("wdr" %in% all_p[["file"]]) {

      # Read in wdr data ----
      wdr_idx <- which(param$model == m & param$file == "wdr")
      col_id <- paste0(param$name[wdr_idx], "_", m)
      aeme_outf <- outflows(aeme)
      wdr <- aeme_outf[["data"]]
      for (c in names(wdr)) {
        value <- unlist(param[["value"]][wdr_idx])
        if (c == "wbal") {
          wdr[[c]][[col_id]] <- wdr[[c]][[col_id]] * value
        } else {
          wdr[[c]][["outflow"]] <- wdr[[c]][["outflow"]] * value
        }
      }
      if (m == "glm_aed") {
        make_wdrGLM(outf = wdr, path_glm = model_path, update_nml = FALSE)
      } else if(m == "gotm_wet") {
        make_wdrGOTM(outf = wdr, path_gotm = model_path, outf_factor = 1)
      } else if(m == "dy_cd") {
        make_DYwdr(lakename = lakename, wdrData = wdr, filePath = model_path,
                   info = "test")
      }
    }

    # Scale inf data ----
    if ("inf" %in% all_p[["file"]]) {

      # Read in inf data ----
      inf_idx <- which(param$model == m & param$file == "inf")
      col_id <- paste0(param$name[inf_idx], "_", m)
      aeme_inf <- inflows(aeme)
      inf <- aeme_inf[["data"]]
      inf_factor <- unlist(param[["value"]][inf_idx])

      if (m == "glm_aed") {
        make_infGLM(path_glm = model_path, list_inf = inf,
                    update_nml = FALSE, inf_factor = inf_factor)
      } else if(m == "gotm_wet") {
        cfg <- configuration(aeme)
        use_bgc <-!is.null(cfg[["gotm_wet"]][["ecosystem"]])
        make_infGOTM(inf_list = inf, inf_factor = inf_factor,
                     use_bgc = use_bgc, path_gotm = model_path,
                     update_gotm = FALSE)
      } else if(m == "dy_cd") {
        make_DYinf(lakename = lakename, infList = inf,
                   filePath = model_path, inf_factor = inf_factor)
      }
    }

    # Inputting model parameters ----
    #* DYRESM-CAEDYM ----
    if (m == "dy_cd") {
      # m <- "dy_cd"
      cfg_file <- list.files(file.path(lake_dir, m),
                             pattern = "cfg")
      cfg_files <- c("dyresm3p1.par", cfg_file)
      for (f in cfg_files) {
        if (tools::file_ext(f) %in% param$file | f %in% param$file) {
          cfg_file <- file.path(lake_dir, m, f)
          cfg <- readLines(cfg_file)
          idx <- which(grepl(tools::file_ext(f), param$file))
          for (p in idx) {
            value <- unlist(param$value[p])
            nme <- strsplit(param$name[p], "/")[[1]]
            lno <- as.numeric(nme[length(nme)])
            cmnt <- strsplit(trimws(cfg[lno]), "#")[[1]]
            cfg[lno] <- paste0(value, paste(" #", cmnt[2], collapse = " "))
          }
          writeLines(cfg, cfg_file)
        }
      }
    }
    #* GLM-AED ----
    if (m == "glm_aed") {
      # m <- "glm_aed"
      cfg_files <- c("glm3.nml", "aed2/aed2.nml", "aed2/aed2_phyto_pars.nml",
                     "aed2/aed2_zoop_pars.nml")
      for (f in cfg_files) {
        if (basename(f) %in% param$file) {
          idx <- which(param$file == basename(f))
          cfg_file <- file.path(lake_dir, m, f)
          nml <- read_nml(cfg_file)

          if (basename(f) %in% c("aed2_phyto_pars.nml", "aed2_zoop_pars.nml")) {
            aed_file <- file.path(lake_dir, m, "aed2/aed2.nml")
            aed <- read_nml(aed_file)
            grp <- ifelse(basename(f) == "aed2_phyto_pars.nml", "the_phytos",
                          "the_zoops")
            grp_idx <- get_nml_value(aed, grp)

            if (length(grp_idx) > 1) {
              wid <- param |>
                dplyr::slice(idx) |>
                dplyr::mutate(value = unlist(value)) |> 
                dplyr::select(name, value, group) |>
                tidyr::pivot_wider(names_from = "group", 
                                   values_from = "value") |>
                as.data.frame()

              names <- strsplit(get_nml_value(nml, "pd%p_name"), ",")[[1]]

              grp_idx <- grep(paste0(substr(names(wid)[-1], 1, 4),
                                     collapse = "|"), names)

              names(grp_idx) <- names(wid)[-1]
              # param[, c("value", "par", "group")]
              grps <- unique(param$group[idx])
              arg_list <- lapply(1:nrow(wid), \(p) {
                par <- strsplit(wid$name[p], "/")[[1]][2]
                vals <- get_nml_value(nml, par)
                for (v in 2:ncol(wid)) {
                  if (!is.na(wid[p, v])) {
                    vals[grp_idx[v-1]] <- wid[p, v]
                  }
                }
                vals
              })
              names(arg_list) <- sapply(1:nrow(wid), \(p) {
                nme <- gsub("/", "::", wid$name[p])
              })
            } else {
              arg_list <- lapply(idx, \(p) {
                par <- strsplit(param$name[p], "/")[[1]][2]
                vals <- get_nml_value(nml, par)
                vals[grp_idx] <- unlist(param$value[p])
                vals
              })
              names(arg_list) <- sapply(idx, \(p) {
                nme <- gsub("/", "::", param$name[p])
              })
            }
          } else {
            
            
            pnames <- sapply(idx, \(p) {
              nme <- gsub("/", "::", param$name[p])
            })
            arg_list <- lapply(idx, \(p) {
              unlist(param$value[p])
            })
            names(arg_list) <- pnames
          }

          # Set and write nml file
          nml <- set_nml(nml, arg_list = arg_list)
          write_nml(nml, cfg_file)
        }
      }
    }
    #* GOTM-WET ----
    if (m == "gotm_wet") {
      # m <- "gotm_wet"
      cfg_files <- c("gotm.yaml", "fabm.yaml")
      for (f in cfg_files) {
        if (f %in% param$file) {
          cfg_file <- file.path(lake_dir, m, f)
          yaml <- yaml::read_yaml(cfg_file)
          idx <- which(param$file == f)
          pnames <- lapply(idx, \(p) {
            list(name = strsplit(param$name[p], "/")[[1]],
                 value = unlist(param$value[p]))
            # nme[length(nme)]
          })
          for (i in pnames) {
            if (length(i[["name"]]) == 2) {
              yaml[[i[["name"]][1]]][[i[["name"]][2]]] <- i[["value"]]
            } else if (length(i[["name"]]) == 3) {
              yaml[[i[["name"]][1]]][[i[["name"]][2]]][[i[["name"]][3]]] <- i[["value"]]
            } else if (length(i[["name"]]) == 4) {
              yaml[[i[["name"]][1]]][[i[["name"]][2]]][[i[["name"]][3]]][[i[["name"]][4]]] <- i[["value"]]
            }
          }
          write_yaml(yaml, cfg_file)
        }
      }
    }

  })
  return(invisible())
}

#' Collapse model parameters
#' 
#' @param param_df data.frame; parameters to collapse
#'
#' @return data.frame; collapsed parameters
#' @noRd
#'
collapse_params <- function(param_df) {
  param_df |>
    dplyr::group_by(model, file, name, group) |>
    dplyr::summarise(
      value = list(value),
      min   = list(min),
      max   = list(max),
      .groups = "drop"
    ) |>
    tibble::as_tibble()
}
