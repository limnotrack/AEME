#' write initial concentrations to a GLM-AED simulation, using key file
#'
#' @param model_controls dataframe of loaded model controls
#' @param path_aed filepath; to AED files
#'
#' @return Written aed.nml files
#' @noRd
#'
#' @importFrom dplyr filter pull
#' @importFrom readr read_csv write_csv
#' 

initialise_aed <- function(model_controls, path_aed) {
  data("key_naming", package = "AEME", envir = environment())
  deriv_vars <- key_naming |>
    dplyr::filter(derived) |>
    dplyr::pull(var_aeme)
  this_ctrls <-  model_controls |>
    dplyr::filter(simulate,
                  !var_aeme %in% deriv_vars,
                  # variables with no init values
                  !var_aeme %in% c("DateTime",
                                   "HYD_flow", "HYD_temp", "HYD_dens",
                                   "LKE_lvlwtr",
                                   "RAD_par", "RAD_extc","RAD_secchi",
                                   "CHM_salt",
                                   "PHS_pip", "NIT_pin",
                                   "PHS_tp", "NIT_tn", "PHY_tchla", "CAR_toc")
    )
  
  aed_cfg <- file.path(path_aed, "aed.nml")
  aed_nml <- read_nml(aed_cfg)

  # --- Determine active AED modules -------------------------------------
  # Use the glm_aed-renamed names' prefixes, not var_aeme's own prefix --
  # AEME's var_aeme convention doesn't always match AED's internal module
  # naming (e.g. var_aeme "CHM_oxy" and "CAR_doc"/"CAR_poc" rename to
  # "OXY_oxy" and "OGM_doc"/"OGM_poc" respectively), exactly as
  # initialise_aed2() does for simstrat_aed2 names.
  if (nrow(this_ctrls) > 0) {
    glm_names <- rename_modelvars(input = this_ctrls$var_aeme,
                                  type_output = "glm_aed")
    prefixes <- sub("_.*$", "", glm_names)
    active_modules <- aed_prefixes_to_modules(prefixes)
  } else {
    active_modules <- character(0)
  }

  # NIT_tn/PHS_tp/CAR_toc are excluded from this_ctrls above (they're
  # aggregate totals, not state variables with their own initial
  # concentration), so they'd never be picked up by the prefix-based
  # detection -- but requesting one of them as output requires aed_totals
  # itself to be active (it's the module that actually computes
  # TOT_tn/TOT_tp/TOT_toc from the aed.nml TN_vars/TP_vars/TOC_vars lists).
  totals_vars <- c("NIT_tn", "PHS_tp", "CAR_toc")
  wants_totals <- any(model_controls$var_aeme %in% totals_vars &
                      model_controls$simulate)
  if (wants_totals) {
    active_modules <- union(active_modules, "aed_totals")
  }

  active_modules <- resolve_aed_active_modules(active_modules)

  if (length(active_modules) > 0) {
    aed_nml <- set_nml(aed_nml, arg_name = "models", arg_val = active_modules)
  }
  
  if (nrow(this_ctrls) == 0) {
    cli_inform_safe(c("i" = "No variables to initialise in AED"))
    write_nml(aed_nml, aed_cfg)
    return(invisible())
  }
  nme_chk <- rename_modelvars(input = this_ctrls$var_aeme,
                              type_output = "glm_aed")
  # Remove columns with no name - not necessary for GLM
  this_ctrls <- this_ctrls[nme_chk != "", ]
  
  if (sum(is.na(this_ctrls$initial_wc)) > 0) {
    na_vars <- this_ctrls$var_aeme[which(is.na(this_ctrls$initial_wc))]
    cli::cli_abort("Initial concentrations missing for: {paste(na_vars, collapse = ', ')}.
                  Please check your key file")
  }
  
  # open the pyto pars file
  # phy_nml <-  readLines(file.path(path_aed, "aed_phyto_pars.nml"))
  phy_csv_file <- basename(aed_nml[["aed_phytoplankton"]][["dbase"]])
  phy_csv_filepath <- file.path(path_aed, phy_csv_file)
  if (file.exists(phy_csv_filepath)) {
    # phy_cols <- read.csv(file.path(path_aed, phy_csv_file), nrows = 1,
    #                      header = FALSE, stringsAsFactors = FALSE)
    phy_vals <- readr::read_csv(file.path(path_aed, phy_csv_file), col_types = 
                                  readr::cols())
    phy_groups <- names(phy_vals)
    phy_groups <- phy_groups[!grepl("p_name", phy_groups)] |> 
      gsub("\\'", "", x = _)
    # carbon to chlorophyll ratios (mg C/mg chla)
    row_idx <- grepl("Xcc", phy_vals[[1]])
    phy_cc <- phy_vals |> 
      dplyr::filter(row_idx) |> 
      dplyr::select(-1) |>
      as.numeric()
    
  }
  zoo_csv_file <- basename(aed_nml[["aed_zooplankton"]][["dbase"]])
  zoo_csv_filepath <- file.path(path_aed, zoo_csv_file)
  if (file.exists(zoo_csv_filepath)) {
    # zoo_cols <- read.csv(file.path(path_aed, zoo_csv_file), nrows = 1,
    #                      header = FALSE, stringsAsFactors = FALSE)
    zoo_vals <- readr::read_csv(file.path(path_aed, zoo_csv_file), col_types = 
                                  readr::cols())
    zoo_groups <- names(zoo_vals)
    zoo_groups <- zoo_groups[!grepl("zoop_name", zoo_groups)]
  }
  
  # Macrophyte
  macrophyte_csv_file <- basename(aed_nml[["aed_macrophyte"]][["dbase"]])
  macrophyte_csv_filepath <- file.path(path_aed, macrophyte_csv_file)
  if (file.exists(macrophyte_csv_filepath)) {
    macrophyte_vals <- readr::read_csv(file.path(path_aed, macrophyte_csv_file), col_types = 
                                         readr::cols())
    macrophyte_groups <- names(macrophyte_vals)
    macrophyte_groups <- macrophyte_groups[!grepl("m_name", macrophyte_groups)]
  }
  
  # carbon to chlorophyll ratios (mg C/mg chla)
  # phy_cc <- get_line(phy_nml = phy_nml, "pd%Xcc") |>
  #   as.numeric()
  
  # iterate through the state variables
  for (i in 1:nrow(this_ctrls)) {
    
    var_name <- this_ctrls$var_aeme[i]
    
    # phytoplankton intialisation
    if (grepl("PHY_", var_name)) {
      
      phy_group <- gsub("^.*_","",this_ctrls$var_aeme[i])
      grp_idx <- which(phy_groups == gsub("PHY_","", var_name)) 
      col_idx <- which(phy_groups == gsub("PHY_","", var_name)) + 1
      row_idx <- grepl("p_initial", phy_vals[[1]])
      old_val <- phy_vals[row_idx, col_idx] |> as.numeric()
      
      # mols to grams then div by carbon:chl
      new_val <- this_ctrls$initial_wc[i] * 12.011 / phy_cc[grp_idx]
      phy_vals[row_idx, col_idx] <- new_val
      msg <- paste0(var_name, ": ", paste0(old_val, " replaced with ", new_val))
      if (round(old_val, 1) != round(new_val, 1)) {
        cli_inform_safe(c("i" = msg))
      }
      
      # Zooplankton initialisation
    } else if (grepl("ZOO_", var_name)) {
      
      # pH initialisation
    } else if (grepl("CHM_ph", var_name)) {
      # cli_inform_safe(c("i" = "Using default pH initialisation"))
    } else {
      
      nml_param_name <- paste0(gsub("^.*_","", var_name),
                               "_initial")
      nml_param_name <- ifelse(nml_param_name %in% c("ss1_initial","ss2_initial"),
                               "ss_initial", nml_param_name)
      if (nml_param_name == "ss_initial") next
      
      old_val <- get_nml_value(glm_nml = aed_nml,
                               arg_name = nml_param_name)
      
      # define the new value
      new_val <- (this_ctrls$initial_wc[i] / this_ctrls$conversion_aed[i]) |>
        round(4)
      
      
      # catch for ss special case
      if (grepl("ss_initial", nml_param_name)) {
        new_val <- paste0(this_ctrls |> dplyr::filter(var_aeme == "NCS_ss1") |>
                            dplyr::pull(initial_wc),
                          ",",
                          this_ctrls |> dplyr::filter(var_aeme == "NCS_ss2") |>
                            dplyr::pull(initial_wc))
      }
      
      # aed_nml[this.line] <- paste0(strsplit(aed_nml[this.line],
      #                                       " = ")[[1]][1], " = ", new_val)
      aed_nml <- set_nml(glm_nml = aed_nml, arg_name = nml_param_name,
                         arg_val = new_val)
      
      
      msg <- paste0(var_name, ": ", paste0(old_val, " replaced with ", new_val))
      if (round(old_val, 1) != round(new_val, 1)) {
        cli_inform_safe(c("i" = msg))
      }
    }
  }
  
  # Write files
  write_nml(aed_nml, file.path(path_aed, "aed.nml"))
  readr::write_csv(phy_vals, phy_csv_filepath)
  readr::write_csv(zoo_vals, zoo_csv_filepath)
  return(invisible())
}

#' Get line numbers in aed.nml files
#'
#' @param phy_nml aed nml file read in using `readLines()`
#' @param id_text string; vector to search for
#'
#' @return vector; of corresponding line numbers
#' @noRd
#'

get_line <- function(phy_nml, id_text) {
  
  phy_nml[which(grepl(id_text, phy_nml))] |>
    gsub(".*=", "", x = _) |>
    gsub(" ", "", x = _) |>
    gsub("'", "", x = _) |>
    strsplit(x = _, ",") |>
    unlist()
  
}
