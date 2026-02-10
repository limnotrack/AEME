library(AEME)
# Grab from LER-WQ library ----
ler_wq <- read.csv("https://raw.githubusercontent.com/aemon-j/LakeEnsemblR.WQ/main/data/LakeEnsemblR_WQ_dictionary.csv")

glm_dir <- "inst/extdata/glm_aed/"

# GLM nml file
glm_file <- file.path(glm_dir, "glm3.nml")
glm_pars <- AEME::read_nml(glm_file)
glm_text <- readLines(glm_file)
glm_text <- glm_text[grepl("!", glm_text)]
glm_p <- lapply(names(glm_pars), \(n) {
  lapply(names(glm_pars[[n]]), \(p) {
    df <- data.frame(
      model = "glm_aed",
      file = "glm3.nml",
      name = paste0(n, "/", p)
    )
    logical <- is.logical(glm_pars[[n]][[p]])
    logical_val <- ifelse(logical, as.logical(glm_pars[[n]][[p]]), NA)
    value <- ifelse(is.numeric(as.numeric(glm_pars[[n]][[p]])), as.numeric(glm_pars[[n]][[p]]), NA)
    char <- is.character(glm_pars[[n]][[p]]) & is.na(value)
    char_val <- ifelse(char, glm_pars[[n]][[p]], NA)

    desc <- glm_text[grepl(p, glm_text)][1]
    desc <- strsplit(desc, "      ")[[1]][2]

    data.frame(model = "glm_aed", file = "glm3.nml",
               module = n, par = p, #description = desc,
               name = paste0(n , "/", p), value = value,
               default = value, logical, logical_val, char,
               char_val) |>
      dplyr::mutate(
        model = "glm_aed",
        index = dplyr::case_when(
          grepl("sed_heat_Ksoil|sed_temp_depth|sed_temp_mean|sed_temp_amplitude|
                sed_temp_peak_doy|zone_heights|sed_reflectivity|sed_roughness", par) ~ 1,
          .default = NA_integer_ 
        ),
        min = default - (0.5 * abs(default)),
        max = default + (0.5 * abs(default)),
        group = NA_character_
      )
  }) |>
    dplyr::bind_rows()
 }) |>
    dplyr::bind_rows()

aed_file <- file.path(glm_dir, "aed2.nml")
aed2_pars <- AEME::read_nml(aed_file)
aed2_phy_idx <- AEME::get_nml_value(aed2_pars, "the_phytos")


phy_file <- file.path(glm_dir, "aed2_phyto_pars.nml")
phy_pars <- AEME::read_nml(phy_file)

zoop_file <- file.path(glm_dir, "aed2_zoop_pars.nml")
zoop <- AEME::read_nml(zoop_file)
z_lines <- readLines(zoop_file)
strsplit(z_lines[5], "] - ")[[1]][2]

def_pars <- lapply(zoop$zoop_params, \(p) {
  p[[1]]
})
sel_pars <- c("zoop_param%zoop_initial", "zoop_param%min_zoo",
              "zoop_param%Rgrz_zoo", "zoop_param%fassim_zoo",
              "zoop_param%Kgrz_zoo", "zoop_param%theta_grz_zoo",
              "zoop_param%Rresp_zoo", "zoop_param%Rmort_zoo",
              "zoop_param%ffecal_zoo", "zoop_param%fexcr_zoo",
              "zoop_param%ffecal_sed", "zoop_param%theta_resp_zoo",
              "zoop_param%Tstd_zoo", "zoop_param%Topt_zoo",
              "zoop_param%Tmax_zoo", "zoop_param%saltfunc_zoo",
              "zoop_param%Smin_zoo", "zoop_param%Smax_zoo",
              "zoop_param%Sint_zoo", "zoop_param%INC_zoo", "zoop_param%IPC_zoo",
              "zoop_param%DOmin_zoo", "zoop_param%Cmin_grz_zoo")

zoo_pars <- lapply(sel_pars, \(p) {
  desc = strsplit(z_lines[grep(gsub("zoop_param%", "", p), z_lines)], "] - ")[[1]][2]
  data.frame(parameter = p, note = desc, default = def_pars[[p]], domain = "water")
}) |>
  dplyr::bind_rows() |>
  dplyr::mutate(module = "zooplankton",
                path = paste0("zoop_params/", parameter)) |>
  dplyr::mutate(parameter = gsub("zoop_param%", "", parameter),
                process = dplyr::case_when(
                  grepl("initial", note, ignore.case = TRUE) ~ "initial_conditions",
                  grepl("std", note, ignore.case = TRUE) ~ "standard",
                  grepl("resp|oxy|temp|fecal", note, ignore.case = TRUE) ~ "respiration",
                  grepl("graz", note, ignore.case = TRUE) ~ "grazing",
                  grepl("assim", note, ignore.case = TRUE) ~ "assimilation",
                  grepl("resp", note, ignore.case = TRUE) ~ "respiration",
                  grepl("mort", note, ignore.case = TRUE) ~ "mortality",
                  grepl("fexcr", note, ignore.case = TRUE) ~ "respiration",
                )
  )

# AED C, N, O, P ----
aed <- ler_wq |>
  dplyr::mutate(
    module = dplyr::case_when(
      module == "phosphorous" ~ "phosphorus",
      .default = module
    ),
    group = NA, file = "aed2.nml"
  ) |>
  dplyr::filter(model %in% c("aed2") &
                  module %in% c("carbon", "nitrogen", "oxygen",
                                "phosphorus")) |>
  dplyr::mutate(
    process = dplyr::case_when(
      grepl("frp_initial", parameter) ~ "initial_conditions",
      .default = process
    )
  ) |> 
  dplyr::filter(parameter != "pon_initial ") |> 
  dplyr::distinct(module, domain, process, parameter, path, .keep_all = TRUE)

head(aed)
tail(aed, 10)
# View(aed)

# Phytoplankton ----
phy <- c("cyanobacteria", "diatoms", "greens")

phy_pars <- lapply(phy, \(p) {

  sub <- ler_wq |>
    dplyr::mutate(
      parameter = dplyr::case_when(
        grepl("fT_method", parameter) ~ "fT_Method",
        .default = parameter
      )
    ) |>
    dplyr::mutate(
      path = dplyr::case_when(
        grepl("fT_Method", parameter) ~ paste0("phyto_data/pd%", "fT_Method"),
        .default = path
      )
    ) |>
    dplyr::filter(module == "phytoplankton" & model == "aed2") |>
    dplyr::filter(parameter != "p_name") |>
    dplyr::mutate(group = p, file = "aed2_phyto_pars.nml")

  phy_names <- AEME::get_nml_value(phy_pars, "pd%p_name") |>
    strsplit(",") |>
    unlist()
  idx <- grep(substr(p, 1, 4), phy_names)

  for (i in 1:nrow(sub)) {
    vals <- AEME::get_nml_value(phy_pars, paste0("pd%", sub$parameter[i]))
    sub$default[i] <- vals[idx]
  }
  sub
}) |>
  dplyr::bind_rows()

# View(phy_pars)

# Zooplankton ----
zoop <- c("cladocerans")
zoo_pars <- zoo_pars |>
  dplyr::mutate(group = "cladocerans", file = "aed2_zoop_pars.nml",
                module = "zooplankton", default = as.character(default))

# zoo_pars <- lapply(zoop, \(p) {
#   sub <- ler_wq |>
#     dplyr::filter(module == "zooplankton" & model == "aed2") |>
#     dplyr::mutate(group = p, file = "aed2_zoop_pars.nml")
#   sub
# }) |>
#   dplyr::bind_rows()

glm_aed_parameters <- dplyr::bind_rows(aed, phy_pars, zoo_pars) |>
  dplyr::mutate(default = as.numeric(default), value = default)

glm_aed_parameters <- glm_aed_parameters |>
  dplyr::filter(!grepl("noncohesive", process, ignore.case = TRUE),
                # Filter out switches ----
                !grepl("fT_Method|lightModel|salTol|simDINUptake|simDONUptake|simNFixation|simINDynamics|simDIPUptake|simIPDynamics|simSiUptake", parameter, ignore.case = TRUE),
                ) |>
  dplyr::mutate(
    model = "glm_aed",
    min = default - (0.5 * abs(default)),
    max = default + (0.5 * abs(default))
  ) |>
  dplyr::mutate(
    default = dplyr::case_when(
      parameter == "Fsed_oxy" ~ -50,
      parameter == "Ksed_oxy" ~ 100,
      parameter == "Fsed_frp" ~ 0.05,
      .default = default
    ),
    min = dplyr::case_when(
      parameter == "Fsed_oxy" ~ -100,
      parameter == "Ksed_oxy" ~ 10,
      .default = min
    ),
    max = dplyr::case_when(
      parameter == "Fsed_oxy" ~ 0,
      parameter == "Ksed_oxy" ~ 100,
      .default = max
    ),
    value = dplyr::case_when(
      parameter == "Fsed_oxy" ~ -50,
      parameter == "Ksed_oxy" ~ 100,
      parameter == "Fsed_frp" ~ 0.05,
      .default = value
    )#,
    # logical = !is.na(logical_val),
    # char = !is.na(char_val)
  ) |>
  dplyr::rename(
    name = path,
    par = parameter,
    description = note
  ) |>
  dplyr::select(model, file, name, value, min, max, dplyr::everything()) |>
  dplyr::bind_rows(glm_p)
dim(glm_aed_parameters)

glm_aed_parameters[glm_aed_parameters$name == "aed2_organic_matter/poc_initial", ]$par

df2 <- glm_aed_parameters |>
  dplyr::distinct(model, file, name, par, group, .keep_all = TRUE)

head(glm_aed_parameters)
tail(glm_aed_parameters)
glm_aed_parameters |>
  dplyr::filter(process == "initial_conditions")

glm_aed_parameters |>
  dplyr::filter(module == "phytoplankton")

# glm_aed_parameters |>
#   dplyr::filter(!par %in% param$par)

param_names <- param_colnames()
glm_aed_parameters  <- glm_aed_parameters |>
  dplyr::select(dplyr::any_of(param_names)) |> 
  tibble::as_tibble()


#GLM-AED parameters

aed_file <- file.path(glm_dir, "aed.nml")
aed_nml <- AEME::read_nml(aed_file)

# Recursively extract parameters from the lists into a dataframe
aed_pars <- lapply(names(aed_nml), function(section) {
  print(section)
  lst <- lapply(names(aed_nml[[section]]), function(param) {
    print(param)
    value <- aed_nml[[section]][[param]]
    if (is.list(value)) {
      # If the parameter is a list, extract each sub-parameter
      sub_params <- lapply(names(value), function(sub_param) {
        sub_value <- value[[sub_param]]
        data.frame(
          model = "glm_aed",
          file = "aed.nml",
          name = paste0(section, "/", param, "/", sub_param),
          value = I(list(sub_value)),
          module = section,
          group = NA,
          index = NA
        )
      })
      df2 <- do.call(rbind, sub_params)
      return(df2)
    } else {
      # If the parameter is not a list, extract directly
      df2 <- data.frame(
        model = "glm_aed",
        file = "aed.nml",
        name = paste0(section, "/", param),
        value = value,
        module = gsub("aed_", "", section),
        group = NA,
        index = NA,
        var_sim = dplyr::case_when(
          grepl("oxy", param, ignore.case = TRUE) ~ "CHM_oxy",
          grepl("amm", param, ignore.case = TRUE) ~ "NIT_amm|NIT_tn",
          grepl("nitrogen", param, ignore.case = TRUE) ~ "NIT_tn",
          grepl("nit", param, ignore.case = TRUE) ~ "NIT_nit|NIT_tn",
          grepl("n2o", param, ignore.case = TRUE) ~ "NIT_nit|NIT_tn",
          grepl("frp", param, ignore.case = TRUE) ~ "PHS_frp|PHS_tp",
          .default = NA
        )
        
      )
    }
    if (nrow(df2) > 1) {
      df2$index <- seq(1, nrow(df2))
    }
    return(df2)
  })
  df <- do.call(rbind, lst)
  if (any(is.na(as.numeric(df$value)))) {
    df <- df |> 
      dplyr::mutate(
        char = is.character(value),
        char_val = ifelse(char, as.character(value), NA),
        value = as.numeric(as.character(value)),
        min = value - (0.5 * abs(value)),
        max = value + (0.5 * abs(value))
      )
  } else {
    df <- df |> 
      dplyr::mutate(
        min = value - (0.5 * abs(value)),
        max = value + (0.5 * abs(value))
      )
  }
  return(df)
})
aed_pars_df <- dplyr::bind_rows(aed_pars)
aed_pars_df[grepl("fsed_don", aed_pars_df$name), ]
aed_pars_df2 <- aed_pars_df |>
  dplyr::filter(is.na(index) | index == 1) |> 
  dplyr::select(model, file, name, value, min, max, module, group, index,
                char, char_val) |> 
  dplyr::mutate(
    min = dplyr::case_when(
      grepl("theta", name) ~ 1, .default = min
    ),
    max = dplyr::case_when(
      grepl("theta", name) ~ 1.12, .default = max
    ),
    var_sim = dplyr::case_when(
      grepl("oxy", name, ignore.case = TRUE) ~ "CHM_oxy",
      grepl("amm", name, ignore.case = TRUE) ~ "NIT_amm|NIT_tn",
      grepl("nitrogen", name, ignore.case = TRUE) ~ "NIT_tn",
      grepl("nit", name, ignore.case = TRUE) ~ "NIT_nit|NIT_tn",
      grepl("n2o", name, ignore.case = TRUE) ~ "NIT_nit|NIT_tn",
      grepl("frp", name, ignore.case = TRUE) ~ "PHS_frp|PHS_tp",
      grepl("rsi", name, ignore.case = TRUE) ~ "SIL_rsi",
      grepl("rsi", name, ignore.case = TRUE) ~ "SIL_rsi",
      .default = NA
    )
  )

aed_pars_df2 |> 
  dplyr::select(name, var_sim)


# GLM-AED phytoplankton parameters
phy_csv_file <- basename(aed_nml[["aed_phytoplankton"]][["dbase"]])
phy_csv_filepath <- file.path(glm_dir, phy_csv_file)
phy_param <- readr::read_csv(phy_csv_filepath, show_col_types = FALSE)
# strip "'" from colnames
colnames(phy_param) <- gsub("'", "", colnames(phy_param))
phy_long <- phy_param |> 
  tidyr::pivot_longer(cols = -c(p_name), names_to = "group", values_to = "value") |> 
  dplyr::mutate(model = "glm_aed",
                name = gsub("'", "", p_name),
                file = phy_csv_file,
                module = "phytoplankton"
                ) |> 
  dplyr::select(model, file, name, value, module, group) |> 
  dplyr::mutate(min = value - (0.5 * abs(value)),
                max = value + (0.5 * abs(value)),
                var_sim = paste0("PHY_", group, "|PHY_tchla")
                )

zoo_csv_file <- basename(aed_nml[["aed_zooplankton"]][["dbase"]])
zoo_csv_filepath <- file.path(glm_dir, zoo_csv_file)
zoo_param <- readr::read_csv(zoo_csv_filepath, show_col_types = FALSE)
# strip "'" from colnames
colnames(zoo_param) <- gsub("'", "", colnames(zoo_param))
zoo_long <- zoo_param |>
  tidyr::pivot_longer(cols = -c(zoop_name), names_to = "group", values_to = "value") |> 
  dplyr::mutate(model = "glm_aed",
                name = gsub("'", "", zoop_name),
                file = zoo_csv_file,
                module = "zooplankton",
                value_num = as.numeric(value),
                char = ifelse(is.na(value_num), TRUE, FALSE),
                char_val = ifelse(char, value, NA),
                value = ifelse(char, NA, value_num)
                
  ) |> 
  dplyr::mutate(min = value - (0.5 * abs(value)),
                max = value + (0.5 * abs(value))) |> 
  dplyr::select(model, file, name, value, min, max, module, group, char, 
                char_val) 
  
macrophyte_csv_file <- basename(aed_nml[["aed_macrophyte"]][["dbase"]])
macrophyte_csv_filepath <- file.path(glm_dir, macrophyte_csv_file)
macrophyte_param <- readr::read_csv(macrophyte_csv_filepath, show_col_types = FALSE)
# strip "'" from colnames
colnames(macrophyte_param) <- gsub("'", "", colnames(macrophyte_param))
macrophyte_long <- macrophyte_param |>
  tidyr::pivot_longer(cols = -c(m_name), names_to = "group", values_to = "value") |> 
  dplyr::mutate(model = "glm_aed",
                name = gsub("'", "", m_name),
                file = macrophyte_csv_file,
                module = "macrophyte",
  ) |> 
  dplyr::mutate(min = value - (0.5 * abs(value)),
                max = value + (0.5 * abs(value))) |> 
  dplyr::select(model, file, name, value, min, max, module, group)


glm_aed_parameters <- dplyr::bind_rows(glm_aed_parameters,
                                        aed_pars_df2,
                                        phy_long,
                                        zoo_long,
                                        macrophyte_long) 

na_params <- which(is.na(glm_aed_parameters$value))
if (length(na_params) > 0) {
  print("Parameters with NA values:")
  print(glm_aed_parameters$name[na_params])
  glm_aed_parameters <- glm_aed_parameters |> 
    dplyr::filter(!is.na(value))
}

usethis::use_data(glm_aed_parameters, overwrite = TRUE)

