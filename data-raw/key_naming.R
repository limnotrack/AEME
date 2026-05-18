key_naming <- read.csv("data-raw/key_naming.csv")

# check units are valid
for (i in 1:nrow(key_naming)) {
  units::as_units(key_naming$units[i])
}

key_naming <- key_naming |> 
  dplyr::rename(var_aeme = name)

usethis::use_data(key_naming, overwrite = TRUE)
