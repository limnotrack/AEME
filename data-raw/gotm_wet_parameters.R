gotm_dir <- system.file("extdata/gotm_wet/", package = "AEME")

gotm_file <- file.path(gotm_dir, "gotm.yaml")

gotm <- yaml::read_yaml(gotm_file)
gotm_text <- readLines(gotm_file)

# GOTM yaml file ----
gotm_pars <- lapply(names(gotm), \(n) {
  pars <- gotm[[n]]
  if (length(pars) == 1) {
    return(NULL)
  }
  print(n)
  df <- lapply(names(pars), \(p) {
    if (is.list(pars[[p]])) {
      df2 <- lapply(names(pars[[p]]), \(m) {
        print(m)
        if (is.list(pars[[p]][[m]])) {
          df3 <- lapply(names(pars[[p]][[m]]), \(o) {
            txt1 <- which(grepl(p, gotm_text))[1]
            txt2_ind <- which(grepl(m, gotm_text))
            txt2 <- txt2_ind[txt2_ind > txt1][1]
            txt3_ind <- which(grepl(o, gotm_text))
            txt <- gotm_text[txt3_ind[txt3_ind > txt2][1]]
            desc <- strsplit(txt, "#")[[1]][2]
            def <- strsplit(desc, "default = |default=")[[1]][2] |>
              gsub("]", "", x = _) |>
              as.numeric()

            fraction <- grepl("fraction", desc)
            logical <- is.logical(pars[[p]][[m]][[o]])
            logical_val <- ifelse(logical, as.logical(pars[[p]][[m]][[o]]), NA)
            value <- ifelse(is.numeric(as.numeric(pars[[p]][[m]][[o]])), as.numeric(pars[[p]][[m]][[o]]), NA)
            char <- is.character(pars[[p]][[m]][[o]]) & is.na(value)
            char_val <- ifelse(char, pars[[p]][[m]][[o]], NA)
            data.frame(module = n, par = m, subpar = o,
                       name = paste0(n, "/", m, "/", o),
                       description = desc, default = def, value = value, fraction,
                       logical, logical_val, char, char_val)
          }) |>
            do.call(rbind, args = _)
          return(df3)
        } else {
          txt1 <- which(grepl(n, gotm_text))[1]
          txt2_ind <- which(grepl(p, gotm_text))
          txt <- gotm_text[txt2_ind[txt2_ind > txt1][1]]
          desc <- strsplit(txt, "#")[[1]][2]
          def <- strsplit(desc, "default = |default=")[[1]][2] |>
            gsub("]", "", x = _) |>
            as.numeric()

          fraction <- grepl("fraction", desc)
          logical <- is.logical(pars[[p]][[m]])
          logical_val <- ifelse(logical, as.logical(pars[[p]][[m]]), NA)
          value <- ifelse(is.numeric(as.numeric(pars[[p]][[m]])), as.numeric(pars[[p]][[m]]), NA)
          char <- is.character(pars[[p]][[m]]) & is.na(value)
          char_val <- ifelse(char, pars[[p]][[m]], NA)
          data.frame(module = n, par = m,
                     name = paste0(n, "/", p, "/", m),
                     description = desc, default = def, value = value, fraction,
                     logical, logical_val, char, char_val)
        }
      }) |>
        dplyr::bind_rows()
    } else {
      txt1 <- which(grepl(n, gotm_text))[1]
      txt2_ind <- which(grepl(p, gotm_text))
      txt <- gotm_text[txt2_ind[txt2_ind > txt1][1]]
      desc <- strsplit(txt, "#")[[1]][2]
      def <- strsplit(desc, "default = |default=")[[1]][2] |>
        gsub("]", "", x = _) |>
        as.numeric()

      fraction <- grepl("fraction", desc)
      logical <- is.logical(pars[[p]])
      logical_val <- ifelse(logical, as.logical(pars[[p]]), NA)
      value <- ifelse(is.numeric(as.numeric(pars[[p]])), as.numeric(pars[[p]]), NA)
      char <- is.character(pars[[p]]) & is.na(value)
      char_val <- ifelse(char, pars[[p]], NA)
      data.frame(module = n, par = p,
                 name = paste0(n, "/", p),
                 description = desc, default = def, value = value, fraction,
                 logical, logical_val, char, char_val)
    }
  }) |>
    dplyr::bind_rows()
  df |>
    dplyr::mutate(
      default = dplyr::case_when(
        !is.na(value) ~ value,
        .default = default
      ),
      min = default - (0.25 * abs(default)),
      max = default + (0.25 * abs(default))
    )
}) |>
  dplyr::bind_rows() |>
  dplyr::mutate(model = "gotm_wet", file = "gotm.yaml")

gotm_pars |>
  dplyr::filter(par == "k_min")

# FABM yaml file ----
fabm_file <- file.path(gotm_dir, "fabm.yaml")

fabm <- yaml::read_yaml(fabm_file)
fabm_text <- readLines(fabm_file)

# Water column ----
wc <- "abiotic_water"

wc_pars <- names(fabm$instances$abiotic_water$parameters)
# rem_pars <- c("diagnostics")
# wc_pars <- wc_pars[!wc_pars %in% rem_pars]

out <- lapply(wc, \(w) {
  pars <- fabm$instances[[w]]$parameters
  df <- lapply(wc_pars, \(p) {
    txt <- grep(p, fabm_text, value = TRUE) [1]
    desc <- strsplit(txt, "#")[[1]][2]
    def <- strsplit(desc, "default = ")[[1]][2] |>
      as.numeric()

    fraction <- grepl("fraction", desc)
    logical <- is.logical(pars[[p]])
    logical_val <- ifelse(logical, as.logical(pars[[p]]), NA)
    value <- ifelse(is.numeric(as.numeric(pars[[p]])), as.numeric(pars[[p]]), NA)
    char <- is.character(pars[[p]]) & is.na(value)
    char_val <- ifelse(char, pars[[p]], NA)
    data.frame(module = w, par = p,
               name = paste0("instances/", w, "/parameters/", p),
               description = desc, default = def, value = value, fraction,
               logical, logical_val, char, char_val)

    # value <- pars[[p]]
    # data.frame(module = w, par = p,
    #            name = paste0("instances/", w, "/parameters/", p),
    #            description = desc, default = def, value)
  }) |>
    do.call(rbind, args = _)
  df |>
    dplyr::mutate(
      min = default - (0.25 * abs(default)),
      max = default + (0.25 * abs(default))
    )
}) |>
  do.call(rbind, args = _)

wc_sa <- out |>
  dplyr::mutate(
    model = "gotm_wet",
    file = "fabm.yaml"
  ) |>
  dplyr::select(model, file, name, value, min, max, dplyr::everything())

#* Water column initial conditions ----
wc_ic <- names(fabm$instances$abiotic_water$initialization)

out <- lapply(wc, \(w) {
  pars <- fabm$instances[[w]]$initialization
  df <- lapply(wc_ic, \(p) {
    txt <- grep(p, fabm_text, value = TRUE) [1]
    desc <- strsplit(txt, "#")[[1]][2]
    def <- strsplit(desc, "default = ")[[1]][2] |>
      as.numeric()

    fraction <- grepl("fraction", desc)
    logical <- is.logical(pars[[p]])
    logical_val <- ifelse(logical, as.logical(pars[[p]]), NA)
    value <- ifelse(is.numeric(as.numeric(pars[[p]])), as.numeric(pars[[p]]), NA)
    char <- is.character(pars[[p]]) & is.na(value)
    char_val <- ifelse(char, pars[[p]], NA)
    data.frame(module = w, par = p,
               name = paste0("instances/", w, "/parameters/", p),
               description = desc, default = def, value = value, fraction,
               logical, logical_val, char, char_val)

    # value <- pars[[p]]
    # def <- ifelse(is.na(def), value, def)
    # data.frame(module = w, par = p,
    #            name = paste0("instances/", w, "/initialization/", p),
    #            description = desc, default = def, value)
  }) |>
    do.call(rbind, args = _)
  df |>
    dplyr::mutate(
      min = default - (0.5 * abs(default)),
      max = default + (0.5 * abs(default))
    )
}) |>
  do.call(rbind, args = _)

wc_ic_sa <- out |>
  dplyr::mutate(
    model = "gotm_wet",
    file = "fabm.yaml",
  ) |>
  dplyr::select(model, file, name, value, min, max, dplyr::everything())


# Sediment ----

sed <- "abiotic_sediment"

sed_pars <- names(fabm$instances$abiotic_sediment$parameters)
rem_pars <- c("hypsograph", "diagnostics")
sed_pars <- sed_pars[!sed_pars %in% rem_pars]

out <- lapply(sed, \(s) {
  pars <- fabm$instances[[s]]$parameters
  df <- lapply(sed_pars, \(p) {
    txt <- grep(p, fabm_text, value = TRUE) [1]
    desc <- strsplit(txt, "#")[[1]][2]
    def <- strsplit(desc, "default = ")[[1]][2] |>
      as.numeric()
    value <- pars[[p]]
    data.frame(module = s, par = p,
               name = paste0("instances/", s, "/parameters/", p),
               description = desc, default = def, value)
  }) |>
    do.call(rbind, args = _)
  df |>
    dplyr::mutate(
      min = default - (0.25 * abs(default)),
      max = default + (0.25 * abs(default))
    )
}) |>
  do.call(rbind, args = _)

sed_sa <- out |>
  dplyr::mutate(
    model = "gotm_wet",
    file = "fabm.yaml"
  ) |>
  dplyr::select(model, file, name, value, min, max, dplyr::everything())

# Sediment IC ----

sed <- "abiotic_sediment"

sed_pars <- names(fabm$instances$abiotic_sediment$initialization)

out <- lapply(sed, \(s) {
  pars <- fabm$instances[[s]]$initialization
  df <- lapply(sed_pars, \(p) {
    txt <- grep(p, fabm_text, value = TRUE) [1]
    desc <- strsplit(txt, "#")[[1]][2]
    def <- strsplit(desc, "default = ")[[1]][2] |>
      as.numeric()
    value <- pars[[p]]
    def <- ifelse(is.na(def), value, def)
    data.frame(module = s, par = p,
               name = paste0("instances/", s, "/initialization/", p),
               description = desc, default = def, value)
  }) |>
    do.call(rbind, args = _)
  df |>
    dplyr::mutate(
      min = default - (0.5 * abs(default)),
      max = default + (0.5 * abs(default))
    )
}) |>
  do.call(rbind, args = _)

sed_ic_sa <- out |>
  dplyr::mutate(
    model = "gotm_wet",
    file = "fabm.yaml"
  ) |>
  dplyr::select(model, file, name, value, min, max, dplyr::everything())


# Resuspended sediment ----

sed <- "resus_sed"

sed_pars <- names(fabm$instances$resus_sed$parameters)
rem_pars <- c( "diagnostics")
sed_pars <- sed_pars[!sed_pars %in% rem_pars]

out <- lapply(sed, \(s) {
  pars <- fabm$instances[[s]]$parameters
  df <- lapply(sed_pars, \(p) {
    txt <- grep(p, fabm_text, value = TRUE)
    if (p == "qResus") {
      txt <- txt[12]
    }
    desc <- strsplit(txt, "#")[[1]][2]
    def <- strsplit(desc, "default = ")[[1]][2] |>
      as.numeric()
    value <- pars[[p]]
    data.frame(module = s, par = p,
               name = paste0("instances/", s, "/parameters/", p),
               description = desc, default = def, value)
  }) |>
    do.call(rbind, args = _)
  df |>
    dplyr::mutate(
      min = default - (0.25 * abs(default)),
      max = default + (0.25 * abs(default))
    )
}) |>
  do.call(rbind, args = _)

resus_sa <- out |>
  dplyr::mutate(
    model = "gotm_wet",
    file = "fabm.yaml"
  ) |>
  dplyr::select(model, file, name, value, min, max, dplyr::everything())

# Phytoplankton ----
phy <- c("cyanobacteria", "diatoms", "greens")

phyto_pars <- names(fabm$instances$cyanobacteria$parameters)
# rem_pars <- c("qLightMethod", "cLOptRef", "lSi", "hSiAss", "cSiD", "lNfix",
#               "cNFixMax", "fMuNFix", "qTrans", "cVSwim", "fLVMmin",
#               "fNutLimVMdown", "fNutLimVMup", "diagnostics")
#
# phyto_pars <- phyto_pars[!phyto_pars %in% rem_pars]

out <- lapply(phy, \(ph) {
  pars <- fabm$instances[[ph]]$parameters
  df <- lapply(phyto_pars, \(p) {
    txt <- grep(p, fabm_text, value = TRUE) [1]
    desc <- strsplit(txt, "#")[[1]][2]
    def <- strsplit(desc, "default = ")[[1]][2] |>
      as.numeric()
    fraction <- grepl("fraction", desc)
    logical <- grepl("toggle", desc)
    logical_val <- ifelse(logical, as.logical(pars[[p]]), NA)
    value <- ifelse(is.numeric(as.numeric(pars[[p]])), as.numeric(pars[[p]]), NA)
    char <- is.character(pars[[p]]) & is.na(value)
    char_val <- ifelse(char, pars[[p]], NA)
    data.frame(module = ph, par = p,
               name = paste0("instances/", ph, "/parameters/", p),
               description = desc, default = def, value = value, fraction,
               logical, logical_val, char, char_val)
  }) |>
    do.call(rbind, args = _)
  df |>
    dplyr::mutate(
      min = default - (0.25 * abs(default)),
      max = default + (0.25 * abs(default))
    ) |>
    dplyr::mutate(
      min = dplyr::case_when(
        fraction ~ 0,
        .default = min
      ),
      max = dplyr::case_when(
        fraction ~ 1,
        .default = max
      )
    )
}) |>
  do.call(rbind, args = _)

phy_sa <- out |>
  dplyr::mutate(
    model = "gotm_wet",
    file = "fabm.yaml"
  ) |>
  dplyr::select(model, file, name, value, min, max, dplyr::everything())|>
  dplyr::select(-fraction)


# Phytoplankton IC ----
phy <- c("cyanobacteria", "diatoms", "greens")

phyto_pars <- names(fabm$instances$cyanobacteria$initialization)

out <- lapply(phy, \(ph) {
  pars <- fabm$instances[[ph]]$initialization
  df <- lapply(phyto_pars, \(p) {
    txt <- grep(p, fabm_text, value = TRUE) [1]
    desc <- strsplit(txt, "#")[[1]][2]
    def <- strsplit(desc, "default = ")[[1]][2] |>
      as.numeric()
    fraction <- grepl("fraction", desc)
    logical <- grepl("toggle", desc)
    logical_val <- ifelse(logical, as.logical(pars[[p]]), NA)
    value <- ifelse(is.numeric(as.numeric(pars[[p]])), as.numeric(pars[[p]]), NA)
    char <- is.character(pars[[p]]) & is.na(value)
    char_val <- ifelse(char, pars[[p]], NA)
    def <- value
    data.frame(module = ph, par = p,
               name = paste0("instances/", ph, "/initialization/", p),
               description = desc, default = def, value = value, fraction,
               logical, logical_val, char, char_val)
  }) |>
    do.call(rbind, args = _)
  df |>
    dplyr::mutate(
      min = default - (0.5 * abs(default)),
      max = default + (0.5 * abs(default))
    ) |>
    dplyr::mutate(
      min = dplyr::case_when(
        fraction ~ 0,
        .default = min
      ),
      max = dplyr::case_when(
        fraction ~ 1,
        .default = max
      )
    )
}) |>
  do.call(rbind, args = _)

phy_ic_sa <- out |>
  dplyr::mutate(
    model = "gotm_wet",
    file = "fabm.yaml"
  ) |>
  dplyr::select(model, file, name, value, min, max, dplyr::everything())|>
  dplyr::select(-fraction)


# Zooplankton ----
zoop <- c("cladocerans")

zoop_pars <- names(fabm$instances$cladocerans$parameters)
# rem_pars <- c("lSi1", "lSi2", "lSi3", "nPrey", "prey_suffix1", "prey_suffix2",
#               "prey_suffix3", "qODB", "qTrans", "diagnostics" )
# zoop_pars <- zoop_pars[!zoop_pars %in% rem_pars]

out2 <- lapply(zoop, \(zp) {
  pars <- fabm$instances[[zp]]$parameters
  df <- lapply(zoop_pars, \(p) {
    txt <- grep(p, fabm_text, value = TRUE) [1]
    desc <- strsplit(txt, "#")[[1]][2]
    def <- strsplit(desc, "default = ")[[1]][2] |>
      as.numeric()
    fraction <- grepl("fraction", desc)
    logical <- grepl("toggle", desc)
    logical_val <- ifelse(logical, as.logical(pars[[p]]), NA)
    char <- is.character(pars[[p]])
    char_val <- ifelse(char, pars[[p]], NA)
    value <- ifelse(is.numeric(pars[[p]]), as.numeric(pars[[p]]), NA)
    data.frame(module = zp, par = p,
               name = paste0("instances/", zp, "/parameters/", p),
               description = desc, default = def, value = value, fraction,
               logical, logical_val, char, char_val)
  }) |>
    do.call(rbind, args = _)
  df |>
    dplyr::mutate(
      min = default - (0.25 * abs(default)),
      max = default + (0.25 * abs(default))
    ) |>
    dplyr::mutate(
      min = dplyr::case_when(
        fraction ~ 0,
        .default = min
      ),
      max = dplyr::case_when(
        fraction ~ 1,
        .default = max
      )
    )
}) |>
  do.call(rbind, args = _)

zoop_sa <- out2 |>
  dplyr::mutate(
    model = "gotm_wet",
    file = "fabm.yaml"
  ) |>
  dplyr::select(model, file, name, value, min, max, dplyr::everything()) |>
  dplyr::select(-fraction)

# Zooplankton IC ----
zoop <- c("cladocerans")

zoop_pars <- names(fabm$instances$cladocerans$initialization)

out2 <- lapply(zoop, \(zp) {
  pars <- fabm$instances[[zp]]$initialization
  df <- lapply(zoop_pars, \(p) {
    txt <- grep(p, fabm_text, value = TRUE) [1]
    desc <- strsplit(txt, "#")[[1]][2]
    def <- strsplit(desc, "default = ")[[1]][2] |>
      as.numeric()
    fraction <- grepl("fraction", desc)
    value <- pars[[p]]
    data.frame(module = zp, par = p,
               name = paste0("instances/", zp, "/initialization/", p),
               description = desc, default = def, value = value, fraction)
  }) |>
    do.call(rbind, args = _)
  df |>
    dplyr::mutate(
      min = default - (0.25 * abs(default)),
      max = default + (0.25 * abs(default))
    ) |>
    dplyr::mutate(
      min = dplyr::case_when(
        fraction ~ 0,
        .default = min
      ),
      max = dplyr::case_when(
        fraction ~ 1,
        .default = max
      )
    )
}) |>
  do.call(rbind, args = _)

zoop_ic_sa <- out2 |>
  dplyr::mutate(
    model = "gotm_wet",
    file = "fabm.yaml"
  ) |>
  dplyr::select(model, file, name, value, min, max, dplyr::everything()) |>
  dplyr::select(-fraction)

# Combine ----
gotm_wet_parameters <- dplyr::bind_rows(
  wc_sa,
  wc_ic_sa,
  sed_sa,
  sed_ic_sa,
  resus_sa,
  phy_sa,
  phy_ic_sa,
  zoop_sa,
  zoop_ic_sa
) |>
  # dplyr::select(model, file, module, name, value, min, max, default,
  #               description, par) |>
  dplyr::mutate(
    module = dplyr::case_when(
      module == "cladocerans" ~ "zooplankton",
      module == "cyanobacteria" ~ "phytoplankton",
      module == "diatoms" ~ "phytoplankton",
      module == "greens" ~ "phytoplankton",
      TRUE ~ module
    )
  )

# Grab from LER-WQ library ----
ler_wq <- read.csv("https://raw.githubusercontent.com/aemon-j/LakeEnsemblR.WQ/main/data/LakeEnsemblR_WQ_dictionary.csv")

wet <- ler_wq |>
  dplyr::mutate(
    module = dplyr::case_when(
      module == "phosphorous" ~ "phosphorus",
      .default = module
    )
  ) |>
  dplyr::filter(model %in% c("wet") &
                  module %in% c("carbon", "nitrogen", "oxygen", "phosphorus",
                                "phytoplankton", "zooplankton")) |>
  dplyr::filter(!grepl("burial\\/", path)) |>
  dplyr::arrange(module, domain, model) |>
  dplyr::mutate(
    model = "gotm_wet",
    file = "fabm.yaml",
    default = as.numeric(default),
    value = default,
    min = default - (0.25 * abs(default)),
    max = default + (0.25 * abs(default)),
    path = paste0("instances/", path)
  ) |>
  dplyr::rename(
    name = path,
    par = parameter,
    description = note
  ) |>
  dplyr::select(model, file, module, process, subprocess, name, value, min, max,
                default, description, par) |>
  dplyr::mutate(logical = FALSE)

# Phytoplankton II ----
phy <- c("cyanobacteria", "diatoms", "greens")

phy_pars <- lapply(phy, \(p) {
  sub <- wet |>
    dplyr::filter(module == "phytoplankton") |>
    dplyr::mutate(name = gsub("\\{group_name\\}", p, name),
                  group = p) |>
    dplyr::mutate(
      value = dplyr::case_when(
        par == "lSi" ~ 1, # lSi is a boolean
        par == "lNfix" ~ 0, # lNfix is a boolean
        .default = value
      ),
      default = dplyr::case_when(
        par == "lSi" ~ 1, # lSi is a boolean
        par == "lNfix" ~ 0, # lNfix is a boolean
        .default = value
      ),
      logical = dplyr::case_when(
        par %in% c("lSi", "lNfix") ~ TRUE, # lSi & lNfix is a boolean
        .default = FALSE
      )
    )

  for (i in 1:nrow(sub)) {
    if (is.na(sub$value[i])) {
      sub$default[i] <- gotm_wet_parameters$value[gotm_wet_parameters$name == sub$name[i]]
      sub$value[i] <- sub$default[i]
      sub$max[i] <- sub$default[i] * 1.5
      sub$min[i] <- sub$default[i] * 0.5
    }
  }
  sub
}) |>
  do.call(rbind, args = _)


# Zooplankton II ----
zoop <- c("cladocerans")
gotm_zoo <- gotm_wet_parameters |>
  dplyr::filter(module == "zooplankton")

zoo_pars <- lapply(zoop, \(p) {
  sub <- wet |>
    dplyr::filter(module == "zooplankton" & par %in% gotm_zoo$par) |>
    dplyr::mutate(name = gsub("\\{group_name\\}", p, name),
                  group = p,
                  value = dplyr::case_when(
                    par == "nPrey" ~ 3,
                    .default = value
                  ),
                  default = dplyr::case_when(
                    par == "nPrey" ~ 3,
                    .default = default
                  )
    )

  add <- gotm_zoo |>
    dplyr::filter(!par %in% sub$par) |>
    dplyr::mutate(
      process = dplyr::case_when(
        grepl("cClearPrey|GutOccPrey", par) ~ "grazing",
        grepl("cVswim", par) ~ "grazing",
        par == "kDConsMaxZoo" ~ "growth"
      )
    )

  sub <- dplyr::bind_rows(sub, add)

  for (i in 1:nrow(sub)) {
    if (is.na(sub$value[i])) {
      sub$default[i] <- gotm_wet_parameters$value[gotm_wet_parameters$name == sub$name[i]]
      sub$value[i] <- sub$default[i]
      sub$max[i] <- sub$default[i] * 1.5
      sub$min[i] <- sub$default[i] * 0.5
    }
  }
  sub |>
    dplyr::mutate(group = p)
}) |>
  do.call(rbind, args = _)

# WET II ----
# Combine ----
wet <- wet |>
  dplyr::filter(!module %in% c("phytoplankton", "zooplankton")) |>
  dplyr::bind_rows(phy_pars, zoo_pars)

wet |>
  dplyr::filter(is.na(value))

for (i in 1:nrow(wet)) {
  if (is.na(wet$default[i])) {

    if (wet$process[i] == "initial_conditions" & !is.na(wet$process[i])) {
      wet$default[i] <- gotm_wet_parameters$value[gotm_wet_parameters$par == wet$par[i]]
      wet$value[i] <- wet$default[i]
    }
  }
}
summary(wet)
wet |>
  dplyr::filter(is.na(min))

wet |>
  dplyr::filter(min == max)

gotm_wet_parameters <- wet |>
  dplyr::filter(!is.na(value) & (!logical | is.na(logical)) &
                  !grepl("nPrey", par)) |>
  dplyr::filter(min != max) |>
  dplyr::mutate(
    min = dplyr::case_when(
      is.na(min) ~ value - (0.5 * abs(value)),
      # par == "fDOMS" ~ 0,
      .default = min
    ),
    max = dplyr::case_when(
      is.na(max) ~ value + (0.5 * abs(value)),
      # par == "fDOMS" ~ 0.5,
      .default = max
    )
  )
gotm_wet_parameters <- dplyr::bind_rows(gotm_pars, gotm_wet_parameters)

summary(gotm_wet_parameters)
# View(gotm_wet_parameters)

param_names <- AEME:::get_param_names()
gotm_wet_parameters <- gotm_wet_parameters |>
  dplyr::select(dplyr::all_of(param_names))
head(gotm_wet_parameters)
dim(gotm_wet_parameters)
tail(gotm_wet_parameters)
gotm_wet_parameters |>
  dplyr::filter(module == "light_extinction")

usethis::use_data(gotm_wet_parameters, overwrite = TRUE)
