#' Construct a "grouped" (non depth x time) model output variable
#'
#' Some GLM-AED output variables have dimensions AEME has no fixed
#' convention for (e.g. `nzones`, `particle`, `sed_layers`, `lon`, `lat`),
#' unlike the package's usual `(time)`-vector / `(z, time)`-matrix output
#' shapes. Rather than force such a variable into the depth x time
#' convention -- which would silently misinterpret the extra axis as depth
#' or time -- [read_glm_output()] keeps it as its own labelled array via
#' this constructor, so the actual index/coordinate values for every
#' dimension are always available alongside the data, ready to be
#' interpreted properly later.
#'
#' @param value array; the variable's data, with dimensions in the same
#'   order as `dim_names`.
#' @param dim_names character; the name of each dimension of `value`, in
#'   order (e.g. `c("nzones", "time")`).
#' @param dim_values named list; one element per entry in `dim_names`,
#'   giving the coordinate/index values along that dimension (e.g. zone
#'   numbers, or a `Date` vector for a `"time"` dimension).
#'
#' @return An object of class `aeme_grouped_var`.
#' @export
new_grouped_var <- function(value, dim_names, dim_values) {
  structure(
    list(value = value, dim_names = dim_names, dim_values = dim_values),
    class = "aeme_grouped_var"
  )
}

#' @export
print.aeme_grouped_var <- function(x, ...) {
  dims_txt <- paste0(
    x$dim_names, " [", vapply(x$dim_values, length, integer(1)), "]",
    collapse = " x "
  )
  cat("<aeme_grouped_var>", dims_txt, "\n")
  invisible(x)
}

#' Convert a grouped variable to a long-format data frame
#'
#' @param x an `aeme_grouped_var` object (see [new_grouped_var()]).
#' @param ... unused.
#'
#' @return data.frame with one row per combination of the variable's
#'   dimension values, a column per dimension (named after that dimension
#'   and holding its coordinate/index values -- a `"time"` dimension
#'   becomes a `Date` column), and a `value` column.
#' @export
as.data.frame.aeme_grouped_var <- function(x, ...) {
  grid <- do.call(
    expand.grid,
    c(x$dim_values, list(KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE))
  )
  grid$value <- as.vector(x$value)
  names(grid)[names(grid) == "time"] <- "Date"
  grid
}

#' Plot a grouped variable as a line plot, coloured by its non-time
#' dimensions (e.g. one line per zone)
#'
#' A thin wrapper around [as.data.frame.aeme_grouped_var()] that plots the
#' resulting long-format data frame with ggplot2 -- one coloured line per
#' combination of the variable's dimensions other than `"time"` (e.g. one
#' line per sediment zone for a GLM-AED `(nzones, time)` AED flux variable).
#'
#' @param x an `aeme_grouped_var` object (see [new_grouped_var()]).
#' @param var_sim character; variable name, used for the plot title/y-axis
#'   label. Default `NULL` (no title/label).
#' @param ylim numeric vector of length 2; y-axis limits. Default `NULL`
#'   (ranged to the data).
#' @param ... unused.
#'
#' @return A ggplot2 object (the long-format data frame instead, with a
#'   warning, if `x` has no `"time"` dimension).
#' @export
#' @method plot aeme_grouped_var
#'
#' @importFrom ggplot2 ggplot aes geom_line coord_cartesian labs theme_bw
plot.aeme_grouped_var <- function(x, var_sim = NULL, ylim = NULL, ...) {
  df <- as.data.frame(x)
  .plot_grouped_ggplot(df, var_sim = var_sim, ylim = ylim)
}
