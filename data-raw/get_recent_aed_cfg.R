devtools::load_all()
json_file <- "https://raw.githubusercontent.com/AquaticEcoDynamics/efi-workshop/refs/heads/main/case_studies/aed_case7.json"

aed <- jsonlite::fromJSON(json_file)
aed <- .nml(aed)
aed

write_nml(aed, "inst/extdata/glm_aed/aed.nml")

# nml_file <- "https://raw.githubusercontent.com/AquaticEcoDynamics/glm-aed/refs/heads/main/glm-examples/Sparkling/glm3.nml"
# nml <- read_nml(nml_file)

csv_file <- "https://raw.githubusercontent.com/AquaticEcoDynamics/efi-workshop/refs/heads/main/case_studies/aed_macrophyte_pars.csv" 
macrophyte_pars <- readLines(csv_file)
writeLines(macrophyte_pars, "inst/extdata/glm_aed/aed_macrophyte_pars.csv")
csv_file <- "https://raw.githubusercontent.com/AquaticEcoDynamics/efi-workshop/refs/heads/main/case_studies/aed_phyto_pars.csv"
phyto_pars <- readLines(csv_file)
writeLines(phyto_pars, "inst/extdata/glm_aed/aed_phyto_pars.csv")
csv_file <- "https://raw.githubusercontent.com/AquaticEcoDynamics/efi-workshop/refs/heads/main/case_studies/aed_zoop_pars.csv"
zoop_pars <- readLines(csv_file)
writeLines(zoop_pars, "inst/extdata/glm_aed/aed_zoop_pars.csv")

