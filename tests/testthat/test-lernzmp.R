test_that("building lernzmp example works", {
  lernzmp_exam <- system.file("extdata/lernzmp", package = "AEME")
  metadata <- read.csv(file.path(lernzmp_exam, "LERNZmp_lake_metadata.csv"))
  testthat::expect_equal(nrow(metadata), 1275)
  metadata <- metadata |> 
    dplyr::filter(aeme_file %in% c("LID11133", "LID40102"))
  aeme <- readRDS(file.path(lernzmp_exam, "LID11133.rds"))
  testthat::expect_true(is(aeme, "Aeme"))

  model <- c("glm_aed", "gotm_wet") # models to build
  path <- tempdir() # directory in which the model configuration will be built
  
  aeme <- build_aeme(aeme = aeme, model = model, path = path,
                     use_aeme = TRUE, use_bgc = TRUE)
  testthat::expect_true(is(aeme, "Aeme"))
  mod_files <- list.files(path, recursive = TRUE)
  testthat::expect_true(any(grepl("glm_aed/glm3.nml", mod_files)))
  testthat::expect_true(any(grepl("bcs/outflow_wbal.csv", mod_files)))
  
  aeme <- run_aeme(aeme = aeme, model = model, path = path)
  lke <- lake(aeme)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "output", "output.nc"))
  testthat::expect_true(all(file_chk))
})
