## code to prepare `Aeme` object goes here

aeme_dir <- system.file("extdata/lake/", package = "AEME")
aeme <- yaml_to_aeme(path = aeme_dir, "aeme.yaml")
saveRDS(aeme, file = "inst/extdata/aeme.rds")

aeme_file <- system.file("extdata/lernzmp/LID45819.rds", package = "AEME")
aeme <- readRDS(aeme_file)
write_aeme_to_files(aeme = aeme, path = "inst/extdata/")
