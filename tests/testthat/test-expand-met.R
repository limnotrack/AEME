test_that("can expand met", {
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

  met2 <- met |>
    dplyr::mutate(
      MET_wndspd = sqrt((MET_wnduvu^2 + MET_wnduvv^2)),
      MET_wnddir = atan2(MET_wnduvv, MET_wnduvu) * 180 / pi
    ) |>
    dplyr::select(-MET_wnduvu, -MET_wnduvv)

  ex_met2 <- expand_met(met = met2, lat = aeme_yaml$lake$latitude,
                        lon = aeme_yaml$lake$longitude,
                        elev = aeme_yaml$lake$elevation)
  testthat::expect_equal(ncol(ex_met2), 20)
  testthat::expect_true(all(round(ex_met2$MET_wnduvu, 2) == round(ex_met$MET_wnduvu, 2)))


  met3 <- met |>
    dplyr::mutate(
      MET_wndspd = sqrt((MET_wnduvu^2 + MET_wnduvv^2))
    ) |>
    dplyr::select(-MET_wnduvu, -MET_wnduvv)

  ex_met3 <- expand_met(met = met3, lat = aeme_yaml$lake$latitude,
                        lon = aeme_yaml$lake$longitude,
                        elev = aeme_yaml$lake$elevation)
  testthat::expect_equal(ncol(ex_met3), 20)
  testthat::expect_true(all(round(abs(ex_met3$MET_wnduvu), 2) == round(ex_met$MET_wndspd, 2)))

})
