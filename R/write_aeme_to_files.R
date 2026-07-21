#' Write an AEME object to files
#'
#' @inheritParams build_aeme
#' @param include_output logical, include output files. Default is FALSE. The
#' output files can be large and take up a lot of space.
#'
#' @returns A vector of file paths to the written files
#' @export
#' 
#' @importFrom methods slotNames slot
#'
#' @examples
#' aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
#' aeme <- readRDS(aeme_file)
#' model <- "glm_aed"
#' path <- file.path(tempdir(), "test_write")
#' model_controls <- get_model_controls()
#' aeme <- build_aeme(path = path, aeme = aeme, model = model,
#' model_controls = model_controls, ext_elev = 5)
#' aeme <- run_aeme(aeme = aeme, model = "glm_aed", path = path)
#' write_aeme_to_files(aeme, path)

write_aeme_to_files <- function(aeme, path, include_output = FALSE) {

  aeme <- check_aeme(aeme)
  path <- check_path(path = path, create = TRUE)
  # Create lake directory
  lake_dir <- get_lake_dir(aeme, path)
  model <- list_models(aeme = aeme)
  # Ensure the output directory exists
  if (!dir.exists(lake_dir)) {
    dir.create(lake_dir, recursive = TRUE)
  }

  # Get the names of the slots
  slot_names <- methods::slotNames(aeme)
  out_files <- c()

  # Iterate over each slot
  for (slot_name in slot_names) {
    # Get the slot content
    slot_content <- methods::slot(aeme, slot_name)
    if (slot_name %in% c("lake", "time", "parameters")) {
      df <- as.data.frame(slot_content)
      names(df) <- gsub("\\.", "-", names(df))
      fname <- file.path(lake_dir, paste0(slot_name, ".csv"))
      out_files <- c(out_files, fname)
      write.csv(df, fname, row.names = FALSE)
      next
    }

    # configuration
    if (slot_name %in% c("configuration") & length(model) > 0) {
      for (m in model) {
        if (is.null(slot_content[[m]])) {
          next
        }
        write_configuration(model = m, aeme = aeme, path = path)
      }
      df <- slot_content$model_controls
      fname <- file.path(lake_dir, paste0(slot_name, "_model_controls.csv"))
      out_files <- c(out_files, fname)
      write.csv(df, fname, row.names = FALSE)
      next
    }

    # observations, input
    if (slot_name %in% c("observations", "input")) {
      for (obs in names(slot_content)) {
        df <- slot_content[[obs]]
        if (is.null(obs)) {
          next
        }
        # if (is.data.frame(df)) {
        #   df <- data.frame(obs = df)
        #   names(df) <- obs
        # }
        fname <- file.path(lake_dir, paste0(slot_name, "_", obs, ".csv"))
        out_files <- c(out_files, fname)
        write.csv(df, fname, row.names = FALSE)
      }
      next
    }

    # inflows, outflows
    if (slot_name %in% c("inflows", "outflows", "water_balance")) {
      for (inf in names(slot_content$data)) {
        df <- slot_content$data[[inf]]
        if (is.null(df)) {
          next
        }
        fname <- file.path(lake_dir, paste0(slot_name, "_", inf, ".csv"))
        out_files <- c(out_files, fname)
        write.csv(df, fname, row.names = FALSE)
      }
      if (slot_name == "water_balance") {
        df <- data.frame(method = slot_content$method, use = slot_content$use)
        fname <- file.path(lake_dir, paste0(slot_name, "_method_use.csv"))
        out_files <- c(out_files, fname)
        write.csv(df, fname, row.names = FALSE)
      }
      if (slot_name %in% c("inflows", "outflows")){
        factors <- as.data.frame(slot_content$factor)
        fname <- file.path(lake_dir, paste0(slot_name, "_factors.csv")) 
        out_files <- c(out_files, fname)
        write.csv(factors, fname, row.names = FALSE)
      }
      next
    }

    # output
    if (slot_name %in% c("output")) {

      if (!include_output) {
        next
      }

      lke <- lake(aeme)
      max_depth <- lke$depth
      h <- model_layer_structure |>
        dplyr::filter(zi <= max_depth) |>
        dplyr::pull(h)

      out_depths <- cumsum(h)
      out_depths <- c(0, out_depths)
      out_depths <- out_depths[out_depths <= max_depth]

      out_vars <- get_output_vars(aeme = aeme, model = model)
      outp <- output(aeme)
      n_members <- outp$n_members

      v = out_vars[2]
      ens_df <- lapply(1:n_members, \(ens) {
        out_df <- lapply(out_vars, \(v) {
          out <- AEME::get_var(aeme = aeme, model = model, var_sim = v,
                               return_df = TRUE, ens_n = ens) 

          if (all(is.na(out$depth))) {
            out <- out |>
              dplyr::mutate(depth = NA)
          }
          out <- out |>
            dplyr::mutate(#depth = depth - max(depth), 
                          value = round(value, 4),
                          model = toggle_models(Model, to = "code")
            ) |>
            dplyr::select(Date, model, var_sim, depth, value) |>
            dplyr::mutate(ens = ens)
        }) |>
          dplyr::bind_rows()

        return(out_df)
      }) |>
        dplyr::bind_rows()
      
      fname <- file.path(lake_dir, paste0(slot_name, ".csv"))
      out_files <- c(out_files, fname)
      write.csv(ens_df, fname, row.names = FALSE)

    }
  }
  return(invisible(out_files))
}
