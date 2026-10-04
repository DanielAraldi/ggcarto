map_footprint <- function(source, destination) {
  extent <- source$extent
  horizontal <- seq(extent[["xmin"]], extent[["xmax"]], length.out = 101L)
  vertical <- seq(extent[["ymin"]], extent[["ymax"]], length.out = 101L)
  boundary <- rbind(
    cbind(horizontal, extent[["ymin"]]),
    cbind(extent[["xmax"]], vertical[-1L]),
    cbind(rev(horizontal)[-1L], extent[["ymax"]]),
    cbind(extent[["xmin"]], rev(vertical)[-1L])
  )
  polygon <- sf::st_sfc(sf::st_polygon(list(boundary)), crs = source$crs)
  geographic <- sf::st_transform(polygon, 4326, partial = FALSE)
  longitude <- sf::st_coordinates(geographic)[, "X"]
  longitude <- (longitude + 180) %% 360 - 180
  if (any(!is.finite(longitude)) || any(abs(diff(longitude)) > 180)) {
    gc_abort(
      "Inset footprints crossing the antimeridian are not supported.",
      "inset_projection"
    )
  }
  transformed <- tryCatch(
    sf::st_transform(polygon, destination$crs, partial = FALSE),
    error = function(error) {
      gc_abort(
        "The inset footprint cannot be transformed to the target CRS.",
        "inset_projection"
      )
    }
  )
  coordinates <- sf::st_coordinates(transformed)
  if (any(sf::st_is_empty(transformed)) || any(!is.finite(coordinates))) {
    gc_abort(
      "The inset footprint falls outside the target projection.",
      "inset_projection"
    )
  }
  if (
    isTRUE(sf::st_is_longlat(destination$crs)) &&
      any(abs(diff(coordinates[, "X"])) > 180)
  ) {
    gc_abort(
      "Inset footprints crossing the antimeridian are not supported.",
      "inset_projection"
    )
  }
  transformed
}

footprint_node <- function(footprint, context, col, fill, lwd) {
  coordinates <- sf::st_coordinates(footprint)
  extent <- context$extent
  grob <- grid::polygonGrob(
    x = (coordinates[, "X"] - extent[["xmin"]]) /
      (extent[["xmax"]] - extent[["xmin"]]),
    y = (coordinates[, "Y"] - extent[["ymin"]]) /
      (extent[["ymax"]] - extent[["ymin"]]),
    gp = grid::gpar(col = col, fill = fill, lwd = lwd),
    name = "inset-footprint"
  )
  gc_get_element(
    grob,
    "custom",
    width = "100%",
    height = "100%",
    metadata = list(obstacle = FALSE)
  )
}

#' Compose a geographically linked secondary map
#'
#' Create a locator or detail map with an independent geographic frame and a
#' projected footprint linking it to an explicit reference map.
#'
#' @param plot Single-panel coord_sf ggplot for the secondary map.
#' @param reference Main map, supplied as an [gc_frame()] or single-panel
#'   coord_sf ggplot. Only its displayed extent and CRS are retained.
#' @param mode `"locator"` highlights the reference extent inside the secondary
#'   map. `"detail"` highlights the secondary extent in the containing main
#'   frame when this inset is supplied in its `overlays`.
#' @param highlight Logical; whether to draw the footprint.
#' @param highlight_col,highlight_fill,highlight_lwd Footprint outline color,
#'   fill color and positive grid line width.
#' @param overlays Additional elements belonging to the secondary map, such as
#'   its own [gc_scale_bar()]. Their coordinates refer to its own fitted panel.
#' @param width,height Logical outer dimensions. Width defaults to 30 percent
#'   of the parent map. Automatic height follows the secondary map aspect.
#' @param ... Outer viewport properties passed to [gc_frame()], including
#'   positioning, padding, border and background. Use [gc_place()] to position
#'   the returned inset explicitly inside a main frame.
#'
#' @details
#' Requires sf. The footprint is sampled along the displayed rectangle edges
#' before transformation between CRSs, rather than merely transforming four
#' corners. It is drawn as a clipped overlay; neither plot's geographic limits,
#' data, theme nor coordinate settings are changed. Antimeridian-crossing
#' transformed footprints and invalid projection domains are rejected.
#' Each inset has its own map context: its scale is independent of the main
#' map. In a main frame's overlays, the reference must match that frame's
#' built extent and CRS. This also prevents accidentally linking an inset to
#' another map. In locator mode it can be positioned elsewhere in a scene;
#' detail-mode parent highlighting requires insertion in the main frame.
#' No connector lines or automatic cartographic feature selection are added.
#'
#' @returns An `gc_inset` inheriting from `gc_frame`, `gc_viewport` and `gc_node`.
#'   `reference_context` stores the main extent and CRS; `footprint` holds the
#'   transformed sf polygon when highlighting is enabled. Inputs are unchanged.
#' @seealso [gc_frame()], [gc_scale_bar()], [gc_place()]
#' @examples
#' if (requireNamespace("sf", quietly = TRUE)) {
#'   area <- sf::st_as_sfc(sf::st_bbox(c(
#'     xmin = 0, ymin = 0, xmax = 100000, ymax = 50000
#'   ), crs = sf::st_crs(32119)))
#'   overview <- ggplot2::ggplot() +
#'     ggplot2::geom_sf(data = area) +
#'     ggplot2::coord_sf(expand = FALSE, datum = NA)
#'   detail <- overview
#'   detail$coordinates <- ggplot2::coord_sf(
#'     xlim = c(20000, 40000), ylim = c(10000, 30000),
#'     expand = FALSE, datum = NA
#'   )
#'   inset <- gc_inset(overview, reference = detail, mode = "locator")
#'   gc_render(gc_frame(detail, overlays = list(
#'     gc_place(inset, right = 8, top = 8)
#'   )))
#' }
#' @export
gc_inset <- function(
  plot,
  reference,
  mode = "locator",
  highlight = TRUE,
  highlight_col = "#C63F30",
  highlight_fill = "#D84B3933",
  highlight_lwd = 1,
  overlays = list(),
  width = "30%",
  height = "auto",
  ...
) {
  if (length(mode) != 1L || is.na(mode) || !mode %in% c("locator", "detail")) {
    gc_abort("mode must be locator or detail.", property = "mode")
  }
  if (!is.logical(highlight) || length(highlight) != 1L || is.na(highlight)) {
    gc_abort("highlight must be TRUE or FALSE.", property = "highlight")
  }
  scalar_number(highlight_lwd, "highlight_lwd", positive = TRUE)
  cartographic_colour(highlight_col, "highlight_col")
  cartographic_colour(highlight_fill, "highlight_fill")
  reference_context <- if (inherits(reference, "gc_frame")) {
    reference$map_context
  } else {
    map_panel_context(reference)
  }
  context <- map_panel_context(plot)
  footprint <- NULL
  parent_highlight <- NULL
  if (highlight) {
    source <- if (mode == "locator") reference_context else context
    destination <- if (mode == "locator") context else reference_context
    footprint <- map_footprint(source, destination)
    highlight_node <- footprint_node(
      footprint,
      destination,
      highlight_col,
      highlight_fill,
      highlight_lwd
    )
    if (mode == "locator") {
      overlays <- c(list(highlight_node), overlays)
    } else {
      parent_highlight <- highlight_node
    }
  }
  inset <- gc_frame(
    plot,
    overlays = overlays,
    width = width,
    height = height,
    ...
  )
  if (inset$height$kind == "auto" && is.null(inset$aspect_ratio)) {
    inset$aspect_ratio <- context$aspect_ratio
  }
  if (is.null(inset$max_height)) {
    inset$max_height <- gc_length("100%")
  }
  if (is.null(inset$max_width)) {
    inset$max_width <- gc_length("100%")
  }
  inset$reference_context <- reference_context
  inset$footprint <- footprint
  inset$parent_highlight <- parent_highlight
  inset$inset_mode <- mode
  class(inset) <- c("gc_inset", class(inset))
  inset
}
