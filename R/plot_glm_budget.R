#' Box-and-arrow figure of the C, N and P budget of a lake
#'
#' Draws the budget from [diag_glm_budget()] as a lake with its compartments
#' (inorganic, plankton, dissolved and particulate organic matter), the
#' exchange with the atmosphere above and the sediment below, and the inflow
#' and outflow, one panel per element. The number on each arrow is the mean
#' flux between the two boxes and the width of the arrow scales with it; the
#' number in each box is its mean inventory. Anything between the compartments
#' that the diagnostics do not account for is shown as a residual, in red: on
#' the lake as a whole (how much of the change in the inventory is
#' unexplained) and, with `show_residual = TRUE`, on each compartment.
#'
#' Fluxes between the same two boxes are added, so a pair of arrows in
#' opposite directions is only drawn once, the net of the two, in the
#' direction of the larger. Internal conversions within a box (such as
#' nitrification) are printed inside it.
#'
#' @param budget a `glm_budget` from [diag_glm_budget()], or an Aeme object, in
#'   which case the budget is calculated with the other arguments.
#' @param element character; any of `"C"`, `"N"` and `"P"`. Default is the
#'   elements in `budget`.
#' @param period character; the period of `budget` to draw. Default is the
#'   first.
#' @param units character; `"kg_yr"` (default, kg of the element per year) or
#'   `"mmol_m2_d"` (mmol per m2 of lake surface per day).
#' @param show_residual logical; also show the residual of each compartment?
#' @param ... passed to [diag_glm_budget()] when `budget` is an Aeme object.
#'
#' @return a ggplot object.
#' @seealso [diag_glm_budget()], [calc_glm_fluxes()]
#' @export
#' @examples
#' \dontrun{
#' plot_glm_budget(aeme)
#' bud <- diag_glm_budget(aeme, element = "N", period = "year")
#' plot_glm_budget(bud, period = "2021", units = "mmol_m2_d")
#' }
plot_glm_budget <- function(budget, element = NULL, period = NULL,
                            units = c("kg_yr", "mmol_m2_d"),
                            show_residual = FALSE, ...) {
  units <- match.arg(units)
  if (!inherits(budget, "glm_budget")) {
    budget <- diag_glm_budget(budget, ...)
  }
  all_el <- unique(budget$fluxes$element)
  element <- element %||% all_el
  element <- intersect(c("C", "N", "P"), element)
  if (length(element) == 0 || !all(element %in% all_el)) {
    cli::cli_abort("{.arg element} must be among {.val {all_el}}.")
  }
  period <- period %||% budget$fluxes$period[1]
  if (!period %in% budget$fluxes$period) {
    cli::cli_abort("{.arg period} must be one of {.val
                   {unique(budget$fluxes$period)}}.")
  }
  area <- attr(budget, "surface_area")
  lay <- glm_budget_layout()

  panels <- lapply(element, function(el) {
    glm_budget_panel(budget, el, period, units, area, lay, show_residual)
  })
  pick <- function(nm) dplyr::bind_rows(lapply(panels, `[[`, nm))
  boxes <- pick("boxes")
  arrows <- pick("arrows")
  labels <- pick("labels")

  cols <- c(C = "#2f8a46", N = "#2a6cb8", P = "#c26a10")
  fills <- c(C = "#dcf1df", N = "#dcebfb", P = "#fde8cf")
  strip <- pick("strip")
  lvl <- strip$panel
  boxes$panel <- factor(boxes$panel, lvl)
  arrows$panel <- factor(arrows$panel, lvl)
  labels$panel <- factor(labels$panel, lvl)
  strip$panel <- factor(strip$panel, lvl)

  lake_box <- lay$lake
  ext <- lay$external
  p <- ggplot2::ggplot() +
    ggplot2::geom_rect(
      data = lake_box[rep(1, nrow(strip)), ] |>
        dplyr::mutate(panel = factor(lvl, lvl),
                      element = strip$element),
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                   fill = element),
      colour = "grey55", linewidth = 0.4, alpha = 0.35) +
    ggplot2::geom_rect(
      data = dplyr::cross_join(ext, dplyr::distinct(strip, panel)),
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = "grey93", colour = "grey55", linewidth = 0.4) +
    ggplot2::geom_text(
      data = dplyr::cross_join(ext, dplyr::distinct(strip, panel)),
      ggplot2::aes(x = (xmin + xmax) / 2, y = (ymin + ymax) / 2,
                   label = label),
      size = 3.4, colour = "grey25") +
    ggplot2::geom_rect(
      data = boxes,
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                   fill = element, colour = element),
      linewidth = 0.7) +
    ggplot2::geom_path(
      data = arrows,
      ggplot2::aes(x = x, y = y, group = id, linewidth = width,
                   colour = colour),
      arrow = grid::arrow(length = grid::unit(0.16, "cm"), type = "closed"),
      lineend = "butt", linejoin = "mitre") +
    ggplot2::geom_text(
      data = labels,
      ggplot2::aes(x = x, y = y, label = label, hjust = hjust,
                   colour = colour, fontface = face),
      size = 2.9, lineheight = 0.9) +
    ggplot2::geom_text(
      data = strip,
      ggplot2::aes(x = 6.25, y = 9.5, label = title),
      size = 4.4, fontface = "bold", colour = "grey15") +
    ggplot2::facet_wrap(~panel, nrow = 1) +
    ggplot2::scale_fill_manual(values = fills, guide = "none") +
    ggplot2::scale_colour_manual(
      values = c(cols, resid = "#c13b3b", grey = "grey40"), guide = "none") +
    ggplot2::scale_linewidth_identity() +
    ggplot2::coord_fixed(xlim = c(-2.4, 14.6), ylim = c(-0.2, 9.9),
                         expand = FALSE) +
    ggplot2::labs(caption = glm_budget_caption(units, period)) +
    ggplot2::theme_void() +
    ggplot2::theme(strip.text = ggplot2::element_blank(),
                   panel.spacing = grid::unit(0.5, "lines"),
                   plot.caption = ggplot2::element_text(
                     size = 8, colour = "grey35", hjust = 0),
                   plot.margin = ggplot2::margin(6, 6, 6, 6))
  p
}

#' Caption for the budget figure
#' @noRd
glm_budget_caption <- function(units, period) {
  u <- if (units == "kg_yr") {
    "Arrows: mean flux, kg of the element per year. Boxes: mean inventory, kg."
  } else {
    "Arrows: mean flux, mmol per m2 of lake surface per day. Boxes: mean inventory, mmol per m2."
  }
  paste0(u, " Red: not explained by the diagnostics. Period: ", period, ".")
}

#' Geometry of the budget figure
#'
#' All coordinates are on a common canvas of about 17 by 10 units so that the
#' panels of the elements share it.
#'
#' @return list with `lake` (the water column), `external` (atmosphere and
#'   sediment bars), `boxes` (compartment centres) and `arrows`, a named
#'   list of waypoints and label positions keyed by `"from>to"`.
#' @noRd
glm_budget_layout <- function() {
  w <- 2.6
  h <- 1.8
  centre <- list(inorganic = c(4.0, 5.95), phyto = c(8.5, 5.95),
                 dom = c(4.0, 3.05), pom = c(8.5, 3.05))
  box <- function(nm) {
    c(xmin = centre[[nm]][1] - w / 2, xmax = centre[[nm]][1] + w / 2,
      ymin = centre[[nm]][2] - h / 2, ymax = centre[[nm]][2] + h / 2)
  }
  arr <- function(x, y, lx, ly, hjust = 0.5) {
    list(x = x, y = y, lx = lx, ly = ly, hjust = hjust)
  }
  list(
    lake = tibble::tibble(xmin = 2.1, xmax = 10.4, ymin = 1.9, ymax = 7.1),
    external = tibble::tibble(xmin = c(2.1, 2.1), xmax = c(10.4, 10.4),
                              ymin = c(7.7, 0.2), ymax = c(8.5, 1.0),
                              label = c("Atmosphere", "Sediment")),
    width = w, height = h,
    centre = centre,
    box = box,
    arrows = list(
      # within the lake (top row y 5.05-6.85, bottom row y 2.15-3.95)
      "inorganic>phyto" = arr(c(5.4, 7.1), c(6.4, 6.4), 6.25, 6.75),
      "phyto>inorganic" = arr(c(7.1, 5.4), c(5.5, 5.5), 6.25, 5.15),
      "phyto>pom" = arr(c(9.1, 9.1), c(5.05, 3.95), 9.2, 4.5, 0),
      "phyto>dom" = arr(c(7.4, 5.4), c(5.05, 3.95), 6.4, 4.5),
      "pom>dom" = arr(c(7.1, 5.4), c(3.3, 3.3), 6.25, 3.65),
      "dom>pom" = arr(c(5.4, 7.1), c(2.7, 2.7), 6.25, 2.35),
      "dom>inorganic" = arr(c(3.4, 3.4), c(3.95, 5.05), 3.5, 4.5, 0),
      # atmosphere
      "atmosphere>inorganic" = arr(c(3.4, 3.4), c(7.7, 6.85), 3.3, 7.3, 1),
      "inorganic>atmosphere" = arr(c(4.7, 4.7), c(6.85, 7.7), 4.8, 7.3, 0),
      "atmosphere>phyto" = arr(c(8.5, 8.5), c(7.7, 6.85), 8.6, 7.3, 0),
      # sediment
      "sediment>dom" = arr(c(3.4, 3.4), c(1.0, 2.15), 3.3, 1.58, 1),
      "sediment>pom" = arr(c(8.0, 8.0), c(1.0, 2.15), 8.1, 1.58, 0),
      "sediment>inorganic" = arr(c(2.1, 0.9, 0.9, 2.7), c(0.6, 0.6, 5.95, 5.95),
                                 0.8, 3.4, 1),
      "sediment>phyto" = arr(c(10.4, 11.6, 11.6, 9.8), c(0.6, 0.6, 5.95, 5.95),
                             11.7, 3.4, 0),
      # across the lake boundary
      "inflow>lake" = arr(c(0.1, 2.1), c(4.5, 4.5), 1.5, 4.95),
      "lake>outflow" = arr(c(10.4, 12.6), c(4.5, 4.5), 11.5, 5.25)
    )
  )
}

#' Draw data for one element of the budget figure
#' @noRd
glm_budget_panel <- function(budget, el, period, units, area, lay,
                             show_residual) {
  unit_col <- if (units == "kg_yr") "kg_yr" else "mmol_m2_d"
  fmt <- function(x) {
    ifelse(is.na(x), "-", ifelse(abs(x) >= 100, format(round(x), big.mark = ",",
                                                       trim = TRUE),
                                 format(signif(x, 2), trim = TRUE,
                                        scientific = FALSE)))
  }
  names_el <- glm_element_names()[[el]]
  cmp_names <- list(
    inorganic = c(N = "Inorganic N", P = "FRP + adsorbed P", C = "DIC")[[el]],
    phyto = "Plankton", dom = "Dissolved organic",
    pom = "Particulate organic")

  fl <- budget$fluxes[budget$fluxes$element == el &
                        budget$fluxes$period == period, ]
  fl$value <- fl[[unit_col]]

  pools <- budget$pools[budget$pools$element == el &
                          budget$pools$period == period, ]
  pools$value <- if (units == "kg_yr") pools$kg else pools$mmol / area

  cl <- NULL
  if (!is.null(budget$closure)) {
    cl <- budget$closure[budget$closure$element == el &
                           budget$closure$period == period, ]
    cl$value <- cl[[unit_col]]
  }
  cmp <- NULL
  if (!is.null(budget$compartments)) {
    cmp <- budget$compartments[budget$compartments$element == el &
                                 budget$compartments$period == period &
                                 budget$compartments$term == "residual", ]
    cmp$value <- cmp[[unit_col]]
  }

  # net flux per ordered pair of boxes, as drawn
  pair <- fl |>
    dplyr::filter(from != to) |>
    dplyr::summarise(value = sum(value), .by = c(from, to)) |>
    dplyr::mutate(key = paste(from, to, sep = ">"))
  # opposite directions are shown once, as the net
  pair <- pair |>
    dplyr::mutate(rev_key = paste(to, from, sep = ">")) |>
    dplyr::left_join(dplyr::select(pair, rev_key = key, rev_value = value),
                     by = "rev_key") |>
    dplyr::mutate(rev_value = dplyr::coalesce(rev_value, 0))
  # a pair is drawn in the direction that has an arrow in the layout
  keep <- pair$key %in% names(lay$arrows)
  pair <- pair[keep, ]
  # inflow and outflow from the closure
  ext <- tibble::tibble(key = character(), value = numeric())
  if (!is.null(cl) && any(cl$term == "inflow")) {
    ext <- tibble::tibble(
      key = c("inflow>lake", "lake>outflow"),
      value = c(cl$value[cl$term == "inflow"], cl$value[cl$term == "outflow"]))
  }
  draw <- dplyr::bind_rows(dplyr::select(pair, key, value), ext)
  # collapse pairs that exist in both directions in the layout
  draw <- draw[is.finite(draw$value) & draw$value != 0, ]
  max_abs <- max(abs(draw$value), na.rm = TRUE)
  if (!is.finite(max_abs) || max_abs == 0) max_abs <- 1

  arrows <- lapply(seq_len(nrow(draw)), function(i) {
    a <- lay$arrows[[draw$key[i]]]
    v <- draw$value[i]
    x <- a$x
    y <- a$y
    if (v < 0) {
      x <- rev(x)
      y <- rev(y)
    }
    list(
      path = tibble::tibble(x = x, y = y, id = paste(el, draw$key[i]),
                            width = 0.4 + 2.4 * sqrt(abs(v) / max_abs),
                            colour = el),
      label = tibble::tibble(x = a$lx, y = a$ly, hjust = a$hjust,
                             label = paste0(glm_budget_process(
                               draw$key[i], fl), "\n", fmt(abs(v))),
                             colour = "grey", face = "plain"))
  })
  arrow_df <- dplyr::bind_rows(lapply(arrows, `[[`, "path"))
  label_df <- dplyr::bind_rows(lapply(arrows, `[[`, "label"))

  # boxes with name, inventory and internal cycling
  internal <- fl |>
    dplyr::filter(from == to) |>
    dplyr::summarise(value = sum(value), .by = c(from, process))
  box_df <- dplyr::bind_rows(lapply(names(lay$centre), function(nm) {
    b <- lay$box(nm)
    tibble::tibble(xmin = b[["xmin"]], xmax = b[["xmax"]], ymin = b[["ymin"]],
                   ymax = b[["ymax"]], element = el)
  }))
  box_labels <- dplyr::bind_rows(lapply(names(lay$centre), function(nm) {
    inv <- pools$value[pools$compartment == nm]
    lines <- c(cmp_names[[nm]],
               if (length(inv) == 1) fmt(inv) else "not simulated")
    int <- internal[internal$from == nm & abs(internal$value) > 0, ]
    if (nrow(int) > 0) {
      int <- int[which.max(abs(int$value)), ]
      lines <- c(lines, paste0(tolower(int$process), " ", fmt(int$value)))
    }
    show_r <- isTRUE(show_residual) && !is.null(cmp)
    out <- tibble::tibble(x = lay$centre[[nm]][1],
                          y = lay$centre[[nm]][2] + if (show_r) 0.2 else 0,
                          hjust = 0.5, label = paste(lines, collapse = "\n"),
                          colour = el, face = "bold")
    if (show_r) {
      r <- cmp$value[cmp$compartment == nm]
      if (length(r) == 1 && is.finite(r)) {
        out <- dplyr::bind_rows(out, tibble::tibble(
          x = lay$centre[[nm]][1], y = lay$centre[[nm]][2] - 0.62,
          hjust = 0.5, label = paste0("unexplained ", fmt(r)),
          colour = "resid", face = "plain"))
      }
    }
    out
  }))

  title <- paste0(names_el, " (", if (units == "kg_yr") paste0("kg ", el, "/yr")
                  else paste0("mmol ", el, "/m2/d"), ")")
  resid_lab <- NULL
  if (!is.null(cl) && any(cl$term == "residual")) {
    r <- cl$value[cl$term == "residual"]
    # relative to the largest flux, as the storage change can be close to zero
    big <- max(abs(cl$value[!cl$term %in% c("explained", "d_storage",
                                            "residual")]), na.rm = TRUE)
    pct <- if (is.finite(big) && big != 0) {
      paste0(" (", round(100 * abs(r) / big), "% of the largest boundary flux)")
    } else ""
    resid_lab <- tibble::tibble(
      x = 6.25, y = 8.95, hjust = 0.5,
      label = paste0("Not explained by the diagnostics: ", fmt(r), pct),
      colour = "resid", face = "bold")
  }

  list(boxes = box_df, arrows = arrow_df,
       labels = dplyr::bind_rows(label_df, box_labels, resid_lab),
       strip = tibble::tibble(panel = names_el, element = el, title = title)) |>
    glm_budget_set_panel(names_el)
}

#' Set the panel name on each table of a panel
#' @noRd
glm_budget_set_panel <- function(x, panel) {
  x$boxes$panel <- panel
  x$arrows$panel <- panel
  x$labels$panel <- panel
  x
}

#' Label of a drawn arrow: the processes behind it, or inflow/outflow
#' @noRd
glm_budget_process <- function(key, fl) {
  if (key == "inflow>lake") return("Inflow")
  if (key == "lake>outflow") return("Outflow")
  ends <- strsplit(key, ">", fixed = TRUE)[[1]]
  p <- fl[fl$from == ends[1] & fl$to == ends[2] & fl$value != 0, ]
  p <- p[order(-abs(p$value)), ]
  p <- unique(p$process)
  if (length(p) == 0) return(key)
  if (length(p) > 2) p <- c(p[1], "other")
  paste(p, collapse = " + ")
}
