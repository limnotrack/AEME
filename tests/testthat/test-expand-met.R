test_that("can expand met", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  aeme_file <- list.files(aeme_dir, recursive = TRUE, pattern = "aeme",
                         full.names = TRUE)
  aeme_yaml <- yaml::read_yaml(aeme_file)
  met_file <- list.files(aeme_dir, recursive = TRUE, pattern = "meteo",
                         full.names = TRUE)
  met <- read.csv(met_file) |>
    dplyr::mutate(Date = as.Date(Date))

  ex_met <- expand_met(met = met, lat = aeme_yaml$lake$latitude,
                       lon = aeme_yaml$lake$longitude,
                       elev = aeme_yaml$lake$elevation)

  testthat::expect_equal(ncol(ex_met), 20)


})
