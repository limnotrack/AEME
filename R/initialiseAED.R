### write initial concentrations to a GLM-AED simulation, using key file

initialiseAED <- function(mod_ctrls, path_aed, config_dir) {
  
  this_ctrls <-  mod_ctrls |>
    dplyr::filter(simulate == 1,
                  !name %in% c("DateTime","HYD_flow","HYD_temp","HYD_dens",
                               "RAD_par","RAD_extc","RAD_secchi",
                               "CHM_salt",
                               "PHS_pip", "NIT_pin",
                               "PHS_tp","NIT_tn","PHY_tchla")
                  )
  
  if (sum(is.na(this_ctrls$initial_wc)) > 0) {
    stop("incomplete initialisation, please check your key file")
  }
  
  aed_cfg <- file.path(path_aed, "aed2.nml")
  aed_phyto <- file.path(path_aed, "aed2_phyto_pars.nml")

  # open the aed2.nml
  aed_nml <- readLines(file.path(path_aed, "aed2.nml"))
  
  # open the pyto pars file
  phy_nml <-  readLines(file.path(path_aed, "aed2_phyto_pars.nml"))
  
  # group names
  phy.groups <- get_line(phy_nml = phy_nml, "pd%p_name")
  
  # carbon to chlorophyll ratios (mg C/mg chla)
  phy.cc <- get_line(phy_nml = phy_nml, "pd%Xcc") |> 
    as.numeric()

  # iterate through the state variables
  for (i in 1:nrow(this_ctrls)) {
    
    this.name <- this_ctrls$name[i]
    
    # phytoplankton intialisation
    if (grepl("PHY_", this.name)) {
      
      this.phy <- gsub("^.*_","",this_ctrls$name[i]) 
      this.col <- which(phy.groups == gsub("PHY_","", this.name))
      
      vals.init <- get_line(phy_nml = phy_nml, "pd%p_initial") |>
        as.numeric()
      
      # mols to grams then div by carbon:chl
      init.new <- this_ctrls$initial_wc[i] * 12.011 / phy.cc[this.col] 
      vals.init[this.col] <- init.new
      
      # write the new initial values
      this.row <- which(grepl("pd%p_initial", phy_nml))
      phy_nml[this.row] <- paste0(strsplit(phy_nml[this.row],"=")[[1]][1],
                                  "=", paste0(sprintf("%13s", 
                                                      vals.init), collapse=","))
      
      
      message(paste0(this.name, " ", paste0(vals.init[this.col], 
                                            " replaced with ", init.new)))
      
      # other initialisation
    } else {
      
      nm.init <- paste0(gsub("^.*_","", this.name),
                       "_initial")
      nm.init <- ifelse(nm.init %in% c("ss1_initial","ss2_initial"),
                        "ss_initial", nm.init)
      
       # find the line
      this.line <- which(grepl(nm.init, aed_nml))
      
      og <- aed_nml[this.line]
      
      # define the new value
      val.new <- (this_ctrls$initial_wc[i] / this_ctrls$conversion_aed[i]) |>
        round(4)
      
      # catch for ss special case
      if(grepl("ss_initial", nm.init)) { 
        val.new <- paste0(this_ctrls |> dplyr::filter(name == "NCS_ss1") |> 
                            dplyr::pull(initial_wc),
                          ",",
                          this_ctrls |> dplyr::filter(name == "NCS_ss2") |>
                            dplyr::pull(initial_wc))}
      
      aed_nml[this.line] <- paste0(strsplit(aed_nml[this.line],
                                            " = ")[[1]][1], " = ", val.new)
      
      message(paste0(og, " replaced with ", val.new))
      
    }
  }

  f <- file(file.path(path_aed, "aed2.nml"), "w")
  writeLines(aed_nml, f)
  close(f)
}

get_line <- function(phy_nml, id_text) {
  
  phy_nml[which(grepl(id_text, phy_nml))] |>
    gsub(".*=", "", x = _) |> 
    gsub(" ", "", x = _) |> 
    gsub("'", "", x = _) |>
    strsplit(x = _, ",") |> 
    unlist()
  
}
