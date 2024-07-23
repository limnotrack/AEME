test_that("aeme object can be read from yaml file", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- yaml_to_aeme(path = path, file = "aeme.yaml")
  testthat::expect_s4_class(aeme, "aeme")
  testthat::expect_output(print(aeme))
})

test_that("aeme object can be constructed from add-in", {

  # insert_aeme()

  aeme_list <- list(
    # Define lake list
    lake = list(
      name = character(), # name of the lake
      id = numeric(), # id number for the lake
      latitude = numeric(), # latitude
      longitude = numeric(), # longitude
      elevation = numeric(), # elevation of lake surface above sea level [m]
      depth = numeric(), # depth of the lake [m]
      area = numeric() # surface area of the lake [m2]
    ),
    # Define time list
    time = list(
      start = as.POSIXct("2020-01-01 00:00:00"), # start date
      stop = as.POSIXct("2020-12-31 00:00:00"), # stop date
      time_step = 3600, # time step in seconds
      spin_up = list( # spin-up period for each model
        "dy_cd" = 365,
        "glm_aed" = 365,
        "gotm_wet" = 365
        )
      ),
    # Define observations list
    observations = list(
      lake = data.frame(), # dataframe of lake observations in the AEME format
      level = data.frame() # dataframe of lake level observations in the AEME format
    ),
    # Define input list
    input = list(
      init_depth = numeric(), # initial depth of the lake [m]
      hypsograph = data.frame(), # dataframe of hypsograph data in the AEME format
      meteo = data.frame(), # dataframe of meteorological data in the AEME format
      use_lw = TRUE, # Logical use incoming longwave radiation
      Kw = numeric() # Light attenuation coefficient [m-1]
      ),
    # Define inflows list
    inflows = list(
      data = list(
        inflow1 = data.frame() # dataframe of inflow data in the AEME format
      ),
      factor = list( # scaling factors for inflows for each model
        "dy_cd" = 1,
        "glm_aed" = 1,
        "gotm_wet" = 1
        )
      ),
    # Define outflows list
    outflows = list(
      data = list(
        outflow1 = data.frame() # dataframe of outflow data in the AEME format
      ),
      factor = list( # scaling factor for outflows for each model
        "dy_cd" = 1,
        "glm_aed" = 1,
        "gotm_wet" = 1
        )
      ),
    # Define water balance list
    water_balance = list(
      method = 2, # Method for calculating water balance. 1 = none, 2 = outflows, 3 = inflows and outflows
      use = "mod", # Use observations or modelled data for water balance. Can be 'obs' or 'mod'.
      data = list(
        model = data.frame(), # dataframe with modelled water balance data
        wbal = data.frame() # Calculated water balance with build_aeme()
        )
      )
    )

  aeme <- aeme_constructor(lake = aeme_list$lake, time = aeme_list$time,
                           observations = aeme_list$observations,
                           input = aeme_list$input,
                           inflows = aeme_list$inflows,
                           outflows = aeme_list$outflows,
                           water_balance = aeme_list$water_balance)

  testthat::expect_s4_class(aeme, "aeme")

})

test_that("aeme object can be built with partial information", {
  aeme_list <- list(
    # Define lake list
    lake = list(
      name = character(), # name of the lake
      id = numeric(), # id number for the lake
      latitude = numeric(), # latitude
      longitude = numeric(), # longitude
      elevation = numeric(), # elevation of lake surface above sea level [m]
      depth = numeric(), # depth of the lake [m]
      area = numeric() # surface area of the lake [m2]
    ),
    # Define time list
    time = list(
      start = as.POSIXct("2020-01-01 00:00:00"), # start date
      stop = as.POSIXct("2020-12-31 00:00:00") # stop date
    ),
    # Define input list
    input = list(
      init_depth = numeric(), # initial depth of the lake [m]
      hypsograph = data.frame(), # dataframe of hypsograph data in the AEME format
      meteo = data.frame(), # dataframe of meteorological data in the AEME format
      use_lw = TRUE, # Logical use incoming longwave radiation
      Kw = numeric() # Light attenuation coefficient [m-1]
    ),
    # Define water balance list
    water_balance = list(
      use = "mod" # Use observations or modelled data for water balance. Can be 'obs' or 'mod'.
    )
  )

  aeme <- aeme_constructor(lake = aeme_list$lake, time = aeme_list$time,
                           input = aeme_list$input,
                           water_balance = aeme_list$water_balance)
  testthat::expect_s4_class(aeme, "aeme")

  wbal <- water_balance(aeme)

  testthat::expect_true(wbal$use == aeme_list$water_balance$use)
  testthat::expect_true(wbal$method == 2)


})


test_that("aeme object errors when non alpha numeric chars present", {
  aeme_list <- list(
    # Define lake list
    lake = list(
      name = character(), # name of the lake
      id = "LID 123", # id number for the lake
      latitude = numeric(), # latitude
      longitude = numeric(), # longitude
      elevation = numeric(), # elevation of lake surface above sea level [m]
      depth = numeric(), # depth of the lake [m]
      area = numeric() # surface area of the lake [m2]
    ),
    # Define time list
    time = list(
      start = as.POSIXct("2020-01-01 00:00:00"), # start date
      stop = as.POSIXct("2020-12-31 00:00:00") # stop date
    ),
    # Define input list
    input = list(
      init_depth = numeric(), # initial depth of the lake [m]
      hypsograph = data.frame(), # dataframe of hypsograph data in the AEME format
      meteo = data.frame(), # dataframe of meteorological data in the AEME format
      use_lw = TRUE, # Logical use incoming longwave radiation
      Kw = numeric() # Light attenuation coefficient [m-1]
    ),
    # Define water balance list
    water_balance = list(
      use = "mod" # Use observations or modelled data for water balance. Can be 'obs' or 'mod'.
    )
  )

  testthat::expect_error({
    aeme_constructor(lake = aeme_list$lake, time = aeme_list$time,
                     input = aeme_list$input, water_balance = aeme_list$water_balance)
  })
})

test_that("parameters can be added to an aeme object", {

  params <- data.frame(
    model = c("glm_aed"),
    file = c("glm3.nml"),
    name = c("light/Kw"),
    value = c(0.58),
    min = c(0.1),
    max = c(1.0),
    module = "light",
    group = NA,
    par = "Kw",
    logical = FALSE,
    logical_val = NA,
    char = FALSE,
    char_val = NA
  )

  aeme_list <- list(
    # Define lake list
    lake = list(
      name = character(), # name of the lake
      id = numeric(), # id number for the lake
      latitude = numeric(), # latitude
      longitude = numeric(), # longitude
      elevation = numeric(), # elevation of lake surface above sea level [m]
      depth = numeric(), # depth of the lake [m]
      area = numeric() # surface area of the lake [m2]
    ),
    # Define time list
    time = list(
      start = as.POSIXct("2020-01-01 00:00:00"), # start date
      stop = as.POSIXct("2020-12-31 00:00:00") # stop date
    ),
    # Define input list
    input = list(
      init_depth = numeric(), # initial depth of the lake [m]
      hypsograph = data.frame(), # dataframe of hypsograph data in the AEME format
      meteo = data.frame(), # dataframe of meteorological data in the AEME format
      use_lw = TRUE, # Logical use incoming longwave radiation
      Kw = numeric() # Light attenuation coefficient [m-1]
    ),
    # Define water balance list
    water_balance = list(
      use = "mod" # Use observations or modelled data for water balance. Can be 'obs' or 'mod'.
    ),
    # Define parameters list
    parameters = params[, 1:5]
  )

  testthat::expect_error(aeme_constructor(lake = aeme_list$lake, time = aeme_list$time,
                                          input = aeme_list$input,
                                          water_balance = aeme_list$water_balance,
                                          parameters = aeme_list$parameters))
  aeme_list$parameters <- params

  aeme <- aeme_constructor(lake = aeme_list$lake, time = aeme_list$time,
                           input = aeme_list$input,
                           water_balance = aeme_list$water_balance,
                           parameters = aeme_list$parameters)
  testthat::expect_s4_class(aeme, "aeme")

  pars <- parameters(aeme)

  testthat::expect_true(all(pars[, 1:7] == params[, 1:7]))


})
