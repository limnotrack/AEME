#' Insert default aeme list code snippet.
#'
#' Call this function as an addin to insert a default `aeme` list at the
#' cursor position.
#'
#' @export

insert_aeme <- function() {
  txt <-
  "aeme <- list(
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
      start = as.Date(\"2020-01-01\"), # start date
      stop = as.Date(\"2020-12-31\"), # stop date
      time_step = 3600, # time step in seconds
      spin_up = list( # spin-up period for each model
        \"dy_cd\" = 365,
        \"glm_aed\" = 365,
        \"gotm_wet\" = 365
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
        \"dy_cd\" = 1,
        \"glm_aed\" = 1,
        \"gotm_wet\" = 1
        )
      ),
    # Define outflows list
    outflows = list(
      data = list(
        outflow1 = data.frame() # dataframe of outflow data in the AEME format
      ),
      factor = list( # scaling factor for outflows for each model
        \"dy_cd\" = 1,
        \"glm_aed\" = 1,
        \"gotm_wet\" = 1
        )
      ),
    # Define water balance list
    water_balance = list(
      method = integer(), # Method for calculating water balance. 1 = none, 2 = outflows, 3 = inflows and outflows
      use = character(), # Use observations or modelled data for water balance. Can be 'obs' or 'mod'.
      data = list(
        model = data.frame(), # dataframe with modelled water balance data
        wbal = data.frame() # Calculated water balance with build_ensemble()
        )
      )
    )"

  rstudioapi::insertText(txt)
}
