## code to prepare `aeme` goes here

# 1. Load raw data
aeme_yaml <- "inst/extdata/lake/aeme.yaml"
aeme <- yaml_to_aeme(file = aeme_yaml)

# 3. Save to inst/extdata/
saveRDS(aeme, file = "inst/extdata/aeme.rds")
