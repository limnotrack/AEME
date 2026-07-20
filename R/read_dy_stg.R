read_dy_stg <- function(file) {
  # read all lines and trim
  lines_raw <- readLines(file, warn = FALSE)
  lines <- lines_raw |>
    trimws() |>
    (\(x) x[x != ""])()   # drop empty lines
  
  # helper: remove inline comments (anything after '#') and trim
  clean <- function(s) trimws(strsplit(s, "#", fixed = TRUE)[[1]][1])
  
  # helper: parse numeric value from a line (after removing comments)
  num_line <- function(i) {
    v <- clean(lines[i])
    if (v == "") return(NA_real_)
    n <- suppressWarnings(as.numeric(v))
    if (is.na(n)) stop("Expected numeric at line ", i, " but got: '", lines[i], "'")
    n
  }
  
  # helper: test whether line (cleaned) is numeric-looking
  is_numeric_line <- function(i) {
    v <- clean(lines[i])
    if (v == "") return(FALSE)
    # if first token is numeric consider it numeric line
    t <- strsplit(v, "\\s+")[[1]][1]
    !is.na(suppressWarnings(as.numeric(t)))
  }
  
  i <- 1L
  # skip initial non-numeric title lines
  while (i <= length(lines) && !is_numeric_line(i)) i <- i + 1L
  if (i > length(lines)) stop("No numeric content found in file.")
  
  # parse header numeric fields sequentially
  latitude      <- num_line(i); i <- i + 1L
  surface_elev  <- num_line(i); i <- i + 1L
  n_inflows     <- as.integer(num_line(i)); i <- i + 1L
  
  if (is.na(n_inflows) || n_inflows < 0) stop("Invalid number of inflows.")
  
  # parse inflow lines
  inflows_raw_idx <- i:(i + n_inflows - 1L)
  if (max(inflows_raw_idx) > length(lines)) stop("File ended before reading all inflows.")
  inflows <- inflows_raw_idx |>
    lapply(function(k) {
      toks <- strsplit(clean(lines[k]), "\\s+")[[1]]
      toks <- toks[toks != ""]
      if (length(toks) < 5) stop("Bad inflow line at ", k, ": ", lines[k])
      if (length(toks) > 5) toks <- c(toks[1:4], paste(toks[5:length(toks)], collapse = " "))
      list(
        type = toks[1],
        height = as.numeric(toks[2]),
        d1 = as.numeric(toks[3]),
        d2 = as.numeric(toks[4]),
        name = toks[5]
      )
    }) |>
    (\(lst) {
      df <- do.call(rbind, lapply(lst, function(x) data.frame(
        type = x$type,
        height = x$height,
        d1 = x$d1,
        d2 = x$d2,
        name = x$name,
        stringsAsFactors = FALSE
      )))
      df$height <- as.numeric(df$height)
      df$d1     <- as.numeric(df$d1)
      df$d2     <- as.numeric(df$d2)
      df
    })()
  
  i <- i + n_inflows
  
  # next numeric values: base elevation, crest elevation, n_outlets
  while (i <= length(lines) && !is_numeric_line(i)) i <- i + 1L
  base_elev <- num_line(i); i <- i + 1L
  
  while (i <= length(lines) && !is_numeric_line(i)) i <- i + 1L
  crest_elev <- num_line(i); i <- i + 1L
  
  while (i <= length(lines) && !is_numeric_line(i)) i <- i + 1L
  n_outlets <- as.integer(num_line(i)); i <- i + 1L
  
  if (is.na(n_outlets) || n_outlets < 0) stop("Invalid number of outlets.")
  
  # --- Fixed outlet-reading logic to allow multiple numbers on one line ---
  outlet_heights <- numeric(0)
  if (n_outlets > 0) {
    while (length(outlet_heights) < n_outlets && i <= length(lines)) {
      # skip non-numeric/comment-only lines
      if (!is_numeric_line(i)) { i <- i + 1L; next }
      line_clean <- clean(lines[i])
      if (line_clean == "") { i <- i + 1L; next }
      
      # split into tokens and coerce to numeric where possible
      toks <- strsplit(line_clean, "\\s+")[[1]]
      toks <- toks[toks != ""]
      nums <- suppressWarnings(as.numeric(toks))
      nums <- nums[!is.na(nums)]
      if (length(nums) > 0) {
        needed <- n_outlets - length(outlet_heights)
        take <- if (length(nums) <= needed) nums else nums[1:needed]
        outlet_heights <- c(outlet_heights, take)
        if (length(nums) > needed) {
          warning("More outlet numeric values found on the same line than needed; extra values ignored.")
        }
      }
      i <- i + 1L
    }
    if (length(outlet_heights) < n_outlets) stop("File ended before reading all outlet heights.")
  }
  
  # next: number of bathymetry records
  while (i <= length(lines) && !is_numeric_line(i)) i <- i + 1L
  if (i > length(lines)) stop("No bathymetry count found.")
  n_bathy <- as.integer(num_line(i)); i <- i + 1L
  if (is.na(n_bathy) || n_bathy < 0) stop("Invalid number of bathymetry records.")
  
  # skip possible header line
  if (i <= length(lines) && grepl("elev", lines[i], ignore.case = TRUE)) {
    i <- i + 1L
  }
  
  # read bathymetry rows
  bathy_idx <- i:(i + n_bathy - 1L)
  if (max(bathy_idx) > length(lines)) stop("File ended before reading all bathymetry rows.")
  bathy_text <- bathy_idx |>
    vapply(function(k) clean(lines[k]), FUN.VALUE = "") |>
    paste(collapse = "\n")
  
  bathymetry <- read.table(text = bathy_text, header = FALSE, stringsAsFactors = FALSE)
  if (ncol(bathymetry) < 2) stop("Bathymetry table must have at least two columns.")
  names(bathymetry)[1:2] <- c("elev", "area")
  if (ncol(bathymetry) > 2) warning("Bathymetry table has more than 2 columns; extras kept.")
  
  # return structured list
  list(
    latitude = latitude,
    surface_elev = surface_elev,
    n_inflows = n_inflows,
    inflows = inflows,
    base_elev = base_elev,
    crest_elev = crest_elev,
    n_outlets = n_outlets,
    outlet_heights = outlet_heights,
    n_bathy = n_bathy,
    bathymetry = bathymetry
  )
}
