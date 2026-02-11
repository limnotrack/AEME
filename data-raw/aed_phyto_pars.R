file <- "inst/extdata/glm_aed/aed2_phyto_pars.nml"
parse_aed2_phyto_nml <- function(file) {
  
  #-----------------------------
  # 1. Read file
  #-----------------------------
  lines <- readLines(file)
  
  # Locate phyto block
  start <- grep("&phyto_data", lines)
  if (length(start) == 0) {
    stop("No &phyto_data block found.")
  }
  
  # Find block end ("/")
  end <- grep("^\\s*/", lines)
  end <- end[end > start][1]
  
  if (is.na(end)) {
    stop("Could not find end of &phyto_data block.")
  }
  
  phyto_lines <- lines[(start + 1):(end - 1)]
  
  # Remove comments and empty lines
  phyto_lines <- phyto_lines |>
    stringr::str_trim() |>
    purrr::discard(~ .x == "" | stringr::str_starts(.x, "!"))
  
  #-----------------------------
  # 2. Parse parameter lines
  #-----------------------------
  parse_line <- function(x) {
    
    param <- stringr::str_match(x, "pd%([A-Za-z0-9_]+)")[, 2]
    
    values <- x |>
      stringr::str_split("=", simplify = FALSE) |>
      purrr::pluck(1, 2) |>
      stringr::str_replace_all("'", "") |>
      stringr::str_trim() |>
      stringr::str_split(",") |>
      purrr::pluck(1) |>
      stringr::str_trim()
    
    tibble::tibble(
      parameter_name = param,
      index = seq_along(values),
      value = suppressWarnings(type.convert(values, as.is = TRUE))
    )
  }
  
  lst <- purrr::map(phyto_lines, parse_line)
  df <- dplyr::bind_rows(lst[-(1)])
  
  #-----------------------------
  # 3. Extract group names
  #-----------------------------
  groups <- lst[[1]] |>
    # dplyr::filter(parameter_name == "p_name") |>
    dplyr::arrange(index) |>
    dplyr::pull(value)
  
  if (length(groups) == 0) {
    stop("p_name not found — cannot assign groups.")
  }
  
  df <- df |>
    dplyr::filter(parameter_name != "p_name") |>
    dplyr::mutate(group = groups[index])
  
  #-----------------------------
  # 4. Parameter descriptions
  #-----------------------------
  desc_lines <- lines[stringr::str_detect(lines, "^!.*")]
  
  desc_tbl <- desc_lines |>
    stringr::str_match("!\\s*([A-Za-z0-9_]+)\\s*:\\s*\\[[^\\]]+\\]\\s*-\\s*(.*)") |>
    as.data.frame() |>
    dplyr::filter(!is.na(V2)) |>
    dplyr::transmute(
      parameter_name = V2,
      description = V3
    )
  
  #-----------------------------
  # 5. Map parameter → var_sim
  #-----------------------------
  var_sim_map <- function(param) {
    
    var_ret <- "PHY_tchla"
    
    if (param %in% c(
      "simDINUptake","simDONUptake","simNFixation",
      "simINDynamics","N_o","K_N","X_ncon","X_nmin",
      "X_nmax","R_nuptake","k_nfix","R_nfix"
    )) {
      var_ret <- paste0(c(var_ret, "NIT_tn"), collapse = "|")
    }
    
    if (param %in% c(
      "simDIPUptake","simIPDynamics","P_0","K_P",
      "X_pcon","X_pmin","X_pmax","R_puptake"
    )) {
      var_ret <- paste0(c(var_ret, "PHS_tp"), collapse = "|")
    }
    
    if (param %in% c(
      "simSiUptake","Si_0","K_Si","X_sicon"
    )) {
      var_ret <- paste0(c(var_ret, "SIL_rsi"), collapse = "|")
    }
    
    # if (param %in% c(
    #   "salTol","S_bep","S_maxsp","S_opt"
    # )) return("SAL")
    
    return(var_ret)
  }
  
  df <- df |>
    dplyr::mutate(
      var_sim = purrr::map_chr(parameter_name, var_sim_map)
    )
  
  #-----------------------------
  # 6. Join descriptions
  #-----------------------------
  priority_groups <- c("green", "diatom", "cyano")
  df <- df |>
    dplyr::left_join(desc_tbl, by = "parameter_name") |>
    dplyr::select(
      parameter_name,
      group,
      value,
      description,
      var_sim
    ) |>
    dplyr::mutate(
      group = factor(
        group,
        levels = c(
          priority_groups,
          setdiff(unique(group), priority_groups)
        )
      )
    ) |>
    dplyr::arrange(group, parameter_name) |> 
    dplyr::mutate(group = as.character(group))
  
  return(df)
}

aed_phyto_pars <- parse_aed2_phyto_nml(file)
usethis::use_data(aed_phyto_pars, overwrite = TRUE)
