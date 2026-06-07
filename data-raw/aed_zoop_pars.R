parse_aed2_zoop_nml <- function(file) {
  
  #-----------------------------
  # 1. Read file
  #-----------------------------
  lines <- readLines(file)
  
  start <- grep("&zoop_params", lines)
  if (length(start) == 0) {
    stop("No &zoop_params block found.")
  }
  
  end <- grep("^\\s*/", lines)
  end <- end[end > start][1]
  
  if (is.na(end)) {
    stop("Could not find end of &zoop_params block.")
  }
  
  zoop_lines <- lines[(start + 1):(end - 1)] |>
    stringr::str_trim() |>
    purrr::discard(~ .x == "" | stringr::str_starts(.x, "!"))
  
  #-----------------------------
  # 2. Parse parameter lines
  #-----------------------------
  parse_line <- function(x) {
    
    param <- stringr::str_match(
      x,
      "zoop_param%([A-Za-z0-9_()%]+)"
    )[, 2]
    
    values <- x |>
      stringr::str_split("=", simplify = FALSE) |>
      purrr::pluck(1, 2) |>
      stringr::str_replace_all("'", "") |>
      stringr::str_trim() |>
      stringr::str_split(",") |>
      purrr::pluck(1) |>
      stringr::str_trim()
    
    is_char <- any(stringr::str_detect(values, "[^0-9.]+"))
    value_num <- suppressWarnings(as.numeric(values))
    value_char <- if (is_char) values else NA_character_
    
    tibble::tibble(
      parameter_name = param,
      index = seq_along(values),
      # value = if (is_char) NA_real_ else as.numeric(values),
      value = value_num,
      is_char = is_char,
      char_value = value_char
    )
  }
  
  # df <- purrr::map_dfr(zoop_lines, parse_line)
  lst <- purrr::map(zoop_lines, parse_line)
  df <- dplyr::bind_rows(lst[-(1)])
  
  #-----------------------------
  # 3. Extract group names
  #-----------------------------
  groups <- lst[[1]] |>
    dplyr::arrange(index) |>
    dplyr::pull(char_value)
  
  if (length(groups) == 0) {
    stop("zoop_name not found.")
  }
  
  df <- df |>
    dplyr::filter(parameter_name != "zoop_name") |>
    dplyr::mutate(group = groups[index])
  
  #-----------------------------
  # 4. Extract descriptions
  #-----------------------------
  desc_lines <- lines[stringr::str_detect(lines, "^!.*")]
  
  desc_tbl <- desc_lines |>
    stringr::str_match("!\\s*([A-Za-z0-9_()%]+)\\s*:\\s*\\[[^\\]]+\\]\\s*-\\s*(.*)") |>
    as.data.frame() |>
    dplyr::filter(!is.na(V2)) |>
    dplyr::transmute(
      parameter_name = V2,
      description = V3
    )
  
  #-----------------------------
  # 5. Map var_sim
  #-----------------------------
  var_sim_map <- function(param) {
    
    if (stringr::str_detect(param, "^prey")) return("PREY")
    
    if (param %in% c("Smin_zoo","Smax_zoo","Sint_zoo","saltfunc_zoo"))
      return("SAL")
    
    if (param %in% c("DOmin_zoo"))
      return("OXY")
    
    return("ZOO")
  }
  
  df <- df |>
    dplyr::mutate(
      # var_sim = purrr::map_chr(parameter_name, var_sim_map),
      var_sim = paste0("ZOO_", group)
    )
  
  #-----------------------------
  # 6. Clean prey parameter names
  #-----------------------------
  df <- df |>
    dplyr::mutate(
      parameter_name = stringr::str_replace_all(
        parameter_name,
        "prey\\((\\d+)\\)%",
        "prey\\1_"
      )
    )
  
  # Example:
  # prey(1)%zoop_prey -> prey1_zoop_prey
  
  #-----------------------------
  # 7. Join descriptions
  #-----------------------------
  df <- df |>
    dplyr::left_join(desc_tbl, by = "parameter_name") |>
    dplyr::select(
      parameter_name,
      group,
      value,
      description,
      var_sim
    ) |>
    dplyr::arrange(group, parameter_name)
  
  return(df)
}

aed2_zoop_pars_file <- "inst/extdata/glm_aed/aed2_zoop_pars.nml"
aed2_zoop_pars <- parse_aed2_zoop_nml(file = aed2_zoop_pars_file)

aed_zoop_pars <- AEME::read_aed_param_csv("inst/extdata/glm_aed/aed_zoop_pars.csv")
