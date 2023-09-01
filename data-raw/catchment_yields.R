catchment_yields <- data.frame(Type = c("Forested", "Low", "Moderate",
                                        "Intensive"),
                               "TN_kg/ha/y" = c(4, 10, 20, 40),
                               "TP_kg/ha/y" = c(0.2, 0.5, 1.5, 3),
                               "TSS_kg/ha/y" = c(100, 200, 500, 1000)
                               )
colnames(catchment_yields) <- c("Type", "TN_kg/ha/y", "TP_kg/ha/y", "TSS_kg/ha/y")

usethis::use_data(catchment_yields, overwrite = TRUE)
