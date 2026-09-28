require_cartography <- function() {
  if (!requireNamespace("sf", quietly = TRUE)) {
    l_abort(
      "Cartographic functions require the optional sf package.",
      "missing_dependency"
    )
  }
}

same_map_context <- function(first, second) {
  isTRUE(first$crs == second$crs) &&
    isTRUE(all.equal(as.numeric(first$extent), as.numeric(second$extent)))
}

cartographic_colour <- function(value, property) {
  valid <- length(value) == 1L &&
    (is.character(value) || is.numeric(value) || identical(value, NA))
  if (valid && is.numeric(value) && !is.na(value) && !is.finite(value)) {
    valid <- FALSE
  }
  if (valid) {
    valid <- tryCatch(
      {
        grDevices::col2rgb(value)
        TRUE
      },
      error = function(error) FALSE
    )
  }
  if (!valid) {
    l_abort(
      paste(property, "must be a single grid colour or NA."),
      property = property
    )
  }
  invisible(value)
}

map_panel_context <- function(plot) {
  require_cartography()
  if (!inherits(plot, "ggplot") || !inherits(plot$coordinates, "CoordSf")) {
    l_abort("plot must be a ggplot using coord_sf().", property = "plot")
  }
  built <- ggplot2::ggplot_build(plot)
  panels <- built$layout$panel_params
  if (length(panels) != 1L) {
    l_abort("Cartographic frames require exactly one panel.", property = "plot")
  }
  panel <- panels[[1L]]
  crs <- sf::st_crs(panel$crs)
  ranges <- c(panel$x_range, panel$y_range)
  if (
    is.na(crs) ||
      length(ranges) != 4L ||
      any(!is.finite(ranges)) ||
      diff(panel$x_range) <= 0 ||
      diff(panel$y_range) <= 0
  ) {
    l_abort("The map needs a known CRS and finite, increasing panel ranges.")
  }
  extent <- sf::st_bbox(
    c(
      xmin = panel$x_range[[1L]],
      ymin = panel$y_range[[1L]],
      xmax = panel$x_range[[2L]],
      ymax = panel$y_range[[2L]]
    ),
    crs = crs
  )
  aspect <- built$plot$coordinates$aspect(panel)
  scalar_number(aspect, "map aspect", positive = TRUE)
  list(extent = extent, crs = crs, aspect_ratio = 1 / aspect)
}

#' Frame a geographic panel and its cartographic overlays
#'
#' Extract one coord_sf panel and fit it inside a logical viewport without
#' stretching its geographic proportions. Overlays share the displayed panel,
#' rather than the surrounding page, as their positioning context.
#'
#' @param plot A single-panel ggplot using [ggplot2::coord_sf()] with a known
#'   CRS. Set the projection, limits and expansion on this plot before framing.
#' @param overlays List of grobs or lplot nodes placed over the map panel.
#' @param width,height Logical dimensions of the outer layout viewport.
#' @param ... Additional outer viewport properties passed to [l_viewport()],
#'   including padding, border, background and positioning constraints.
#'
#' @details
#' Requires the optional sf package. The source plot and its theme are preserved.
#' Titles, legends and axes are not included in the extracted panel; extract
#' them separately with [l_get_element()]. The actual built panel ranges,
#' including coord_sf expansion, define the extent, not the data bounding box.
#' Geographic and projected CRSs are supported by the frame. The inner panel
#' is centered, preserves the coord_sf aspect ratio and clips overlays at its
#' edges. Padding and borders belong to the outer viewport, outside the map.
#' No device dimensions are stored. Each draw fits the panel again.
#'
#' @returns An `l_frame` inheriting from `l_viewport` and `l_node`, with a
#'   `map_context` containing the displayed extent, CRS and aspect ratio.
#' @seealso [l_scale_bar()], [l_inset()], [l_viewport()], [l_place()], [l_get_element()]
#' @examples
#' if (requireNamespace("sf", quietly = TRUE)) {
#'   area <- sf::st_as_sfc(sf::st_bbox(c(
#'     xmin = 0, ymin = 0, xmax = 100000, ymax = 50000
#'   ), crs = sf::st_crs(32119)))
#'   plot <- ggplot2::ggplot() +
#'     ggplot2::geom_sf(data = area) +
#'     ggplot2::coord_sf(expand = FALSE, datum = NA)
#'   l_render(l_frame(plot, padding = 12))
#' }
#' @export
l_frame <- function(
  plot,
  overlays = list(),
  width = "auto",
  height = "auto",
  ...
) {
  context <- map_panel_context(plot)
  if (
    !is.list(overlays) ||
      inherits(overlays, "l_node") ||
      grid::is.grob(overlays)
  ) {
    l_abort(
      "overlays must be a list of graphics objects.",
      property = "overlays"
    )
  }
  overlays <- unlist(
    lapply(overlays, function(overlay) {
      if (inherits(overlay, "l_inset")) {
        if (!same_map_context(overlay$reference_context, context)) {
          l_abort(
            "The inset reference does not match its containing frame.",
            "inset_reference"
          )
        }
        if (!is.null(overlay$parent_highlight)) {
          return(list(overlay$parent_highlight, overlay))
        }
      }
      list(overlay)
    }),
    recursive = FALSE
  )
  panel <- l_get_element(plot, "panel", width = "100%", height = "100%")
  inner <- l_viewport(
    c(list(panel), overlays),
    width = "100%",
    max_height = "100%",
    aspect_ratio = context$aspect_ratio,
    x = "50%",
    y = "50%",
    anchor = "center",
    overflow = "hidden"
  )
  inner$map_panel <- context
  frame <- l_viewport(list(inner), width = width, height = height, ...)
  frame$map_context <- context
  frame$map_plot <- panel$source
  class(frame) <- c("l_frame", class(frame))
  frame
}

map_metres_per_unit <- function(context) {
  if (isTRUE(sf::st_is_longlat(context$crs))) {
    l_abort(
      "Scale bars require a projected CRS; angular coordinates are not distances.",
      "unsupported_crs"
    )
  }
  unit <- tolower(context$crs$units_gdal %||% "")
  factor <- switch(
    unit,
    metre = 1,
    meter = 1,
    m = 1,
    kilometre = 1000,
    kilometer = 1000,
    km = 1000,
    foot = 0.3048,
    feet = 0.3048,
    ft = 0.3048,
    "us survey foot" = 1200 / 3937,
    "us-ft" = 1200 / 3937,
    NULL
  )
  if (is.null(factor)) {
    l_abort(
      paste0("Unsupported projected CRS unit: ", unit, "."),
      "unsupported_crs"
    )
  }
  factor
}

scale_bar_grob <- function(specification, distance, width, context) {
  positions <- seq(0, 1, length.out = specification$segments + 1L)
  values <- positions * distance
  labels <- format(signif(values, 8), trim = TRUE, scientific = FALSE)
  labels[[length(labels)]] <- paste(
    labels[[length(labels)]],
    specification$unit
  )
  justification <- c("left", rep("center", length(labels) - 2L), "right")
  label_grobs <- lapply(seq_along(labels), function(index) {
    l_text(
      labels[[index]],
      x = positions[[index]],
      y = 0.76,
      just = justification[[index]],
      fontsize = specification$fontsize,
      fontfamily = specification$fontfamily,
      col = specification$col,
      name = paste0("scale-label-", index)
    )
  })
  label_widths <- vapply(
    label_grobs,
    function(grob) {
      measure_grob(grob, context)[["width"]]
    },
    numeric(1)
  )
  starts <- positions *
    width -
    label_widths * c(0, rep(0.5, length(labels) - 2L), 1)
  ends <- starts + label_widths
  if (any(ends[-length(ends)] + 2 > starts[-1L])) {
    l_warn(
      "Scale labels overlap; reduce segments/fontsize or enlarge the map.",
      "scale_labels"
    )
  }
  if (specification$design == "bar") {
    body <- lapply(seq_len(specification$segments), function(index) {
      l_rect(
        x = (index - 0.5) / specification$segments,
        y = 0.28,
        width = 1 / specification$segments,
        height = 0.24,
        fill = if (index %% 2L) {
          specification$fill
        } else {
          specification$fill_secondary
        },
        col = specification$col,
        lwd = specification$lwd,
        name = paste0("scale-segment-", index)
      )
    })
  } else {
    body <- list(grid::segmentsGrob(
      x0 = 0,
      x1 = 1,
      y0 = 0.28,
      y1 = 0.28,
      gp = grid::gpar(col = specification$col, lwd = specification$lwd),
      name = "scale-line"
    ))
  }
  ticks <- seq(
    0,
    1,
    length.out = specification$segments * specification$subdivisions + 1L
  )
  major <- (seq_along(ticks) - 1L) %% specification$subdivisions == 0L
  if (specification$design == "ticks" || specification$subdivisions > 1L) {
    body[[length(body) + 1L]] <- grid::segmentsGrob(
      x0 = ticks,
      x1 = ticks,
      y0 = 0.16,
      y1 = ifelse(major, 0.4, 0.28),
      gp = grid::gpar(col = specification$col, lwd = specification$lwd),
      name = "scale-ticks"
    )
  }
  l_template(children = c(body, label_grobs), name = "scale-bar")
}

bind_scale_bar <- function(node, context) {
  map <- context$map_panel
  if (is.null(map)) {
    l_abort(
      "Place l_scale_bar() directly in the overlays of l_frame() or l_inset().",
      "unbound_scale",
      node$id
    )
  }
  if (
    node$width$kind != "auto" ||
      !is.null(node$min_width) ||
      !is.null(node$max_width) ||
      !is.null(node$aspect_ratio) ||
      (!is.null(node$left) && !is.null(node$right)) ||
      node$collision %in% c("shrink", "avoid-and-shrink")
  ) {
    l_abort(
      "Scale width is geographic; do not override width, constrain it or shrink it.",
      "scale_constraint",
      node$id,
      "width"
    )
  }
  specification <- node$scale_specification
  metres_per_unit <- c(m = 1, km = 1000, ft = 0.3048, mi = 1609.344)
  extent_metres <- as.numeric(map$extent[["xmax"]] - map$extent[["xmin"]]) *
    map_metres_per_unit(map)
  maximum <- extent_metres *
    specification$max_fraction /
    metres_per_unit[[specification$unit]]
  distance <- specification$distance
  if (is.null(distance)) {
    magnitude <- 10^floor(log10(maximum))
    candidates <- c(1, 2, 5, 10) * magnitude
    distance <- max(candidates[candidates <= maximum * (1 + 1e-12)])
  }
  fraction <- distance * metres_per_unit[[specification$unit]] / extent_metres
  if (!is.finite(fraction) || fraction <= 0 || fraction > 1) {
    l_abort(
      "The scale distance must be positive and no wider than the map extent.",
      "scale_distance",
      node$id
    )
  }
  width <- context$width * fraction
  padding <- resolve_edges(node$padding, context, node, "padding")
  border <- resolve_border(node$border, context, node)
  node$width <- l_length(
    width + sum(padding[c("left", "right")]) + 2 * border$width
  )
  node$content <- scale_bar_grob(specification, distance, width, context)
  node$scale_info <- list(
    distance = distance,
    unit = specification$unit,
    distance_m = distance * metres_per_unit[[specification$unit]],
    extent_width_m = extent_metres,
    width = width
  )
  node
}

#' Construct a map-bound projected-distance scale bar
#'
#' Declare a scale whose physical width is resolved from its containing map
#' panel on every draw. Use it directly in the overlays of [l_frame()] or
#' [l_inset()], optionally positioned with [l_place()].
#'
#' @param distance Positive distance in `unit`, or `NULL` to choose a 1, 2 or 5
#'   times a power of ten automatically.
#' @param unit Display unit: `"km"`, `"m"`, `"ft"` (international feet) or `"mi"`.
#' @param max_fraction Maximum fraction of the panel width for automatic
#'   distance selection, greater than zero and at most one. Explicit distances
#'   ignore this preference but cannot exceed the displayed map width.
#' @param segments Number of labeled main intervals, from 1 to 20.
#' @param subdivisions Number of tick subdivisions per interval, from 1 to 20.
#' @param design `"bar"` for alternating filled segments or `"ticks"` for a line.
#' @param height Logical border-box height. The default is 9 mm.
#' @param col,fill,fill_secondary Outline/text and alternating segment colors.
#' @param lwd Positive grid line width.
#' @param fontsize Positive label font size in points.
#' @param fontfamily Grid font family.
#' @param ... Additional node properties accepted by [l_place()], such as
#'   left, bottom, margin, padding, background or id. The default position is
#'   bottom-left. Width is determined geographically and cannot be overridden.
#'
#' @details
#' Measures projected distance, not geodesic ground distance. The containing
#' frame must use a projected CRS in metres, kilometres, international feet
#' or US survey feet. Angular, missing and unsupported CRS units are rejected;
#' projection distortion is not corrected. sf remains an optional dependency.
#' Text and stroke sizes remain physical; geometry follows the map panel.
#' Labels that do not fit produce an `lplot_scale_labels` warning without
#' falsifying the distance. Width constraints, aspect ratios and collision
#' shrinking are rejected. Collision avoidance can move the bar without
#' changing its distance. Padding/borders are outside its calibrated length.
#' A standalone scale cannot be rendered or measured without a frame.
#'
#' @returns An `l_scale_bar` inheriting from `l_element` and `l_node`.
#'   Resolved scale nodes expose a `scale_bar` list with distance, units,
#'   distance in metres, displayed extent width in metres and bar width in
#'   logical pixels. The original declaration is not modified.
#' @seealso [l_frame()], [l_inset()], [l_place()], [l_resolve()]
#' @examples
#' if (requireNamespace("sf", quietly = TRUE)) {
#'   area <- sf::st_as_sfc(sf::st_bbox(c(
#'     xmin = 0, ymin = 0, xmax = 100000, ymax = 50000
#'   ), crs = sf::st_crs(32119)))
#'   plot <- ggplot2::ggplot() +
#'     ggplot2::geom_sf(data = area) +
#'     ggplot2::coord_sf(expand = FALSE, datum = NA)
#'   scale <- l_scale_bar(50, "km", segments = 2, left = 12, bottom = 8)
#'   l_render(l_frame(plot, overlays = list(scale)))
#' }
#' @export
l_scale_bar <- function(
  distance = NULL,
  unit = "km",
  max_fraction = 0.25,
  segments = 4,
  subdivisions = 1,
  design = "bar",
  height = "9mm",
  col = "#203C43",
  fill = col,
  fill_secondary = "white",
  lwd = 1,
  fontsize = 8,
  fontfamily = "",
  ...
) {
  if (!is.null(distance)) {
    scalar_number(distance, "distance", positive = TRUE)
  }
  if (
    length(unit) != 1L || is.na(unit) || !unit %in% c("km", "m", "ft", "mi")
  ) {
    l_abort("unit must be km, m, ft or mi.", property = "unit")
  }
  scalar_number(max_fraction, "max_fraction", positive = TRUE)
  if (max_fraction > 1) {
    l_abort("max_fraction cannot exceed one.")
  }
  for (property in c("segments", "subdivisions")) {
    value <- get(property)
    scalar_number(value, property, positive = TRUE)
    if (value != floor(value) || value > 20) {
      l_abort(paste(property, "must be an integer from 1 to 20."))
    }
  }
  if (length(design) != 1L || is.na(design) || !design %in% c("bar", "ticks")) {
    l_abort("design must be bar or ticks.", property = "design")
  }
  scalar_number(fontsize, "fontsize", positive = TRUE)
  scalar_number(lwd, "lwd", positive = TRUE)
  if (
    !is.character(fontfamily) || length(fontfamily) != 1L || is.na(fontfamily)
  ) {
    l_abort("fontfamily must be a single string.", property = "fontfamily")
  }
  for (property in c("col", "fill", "fill_secondary")) {
    cartographic_colour(get(property), property)
  }
  node <- new_l_node("element", grid::nullGrob(), "scale_bar", height = height)
  node$adapter <- list(measure = function(element, context) {
    c(width = element$scale_info$width, height = 9 * 96 / 25.4)
  })
  node$anchor <- "bottom-left"
  node$scale_specification <- list(
    distance = distance,
    unit = unit,
    max_fraction = max_fraction,
    segments = segments,
    subdivisions = subdivisions,
    design = design,
    col = col,
    fill = fill,
    fill_secondary = fill_secondary,
    lwd = lwd,
    fontsize = fontsize,
    fontfamily = fontfamily
  )
  class(node) <- c("l_scale_bar", "l_element", "l_node")
  update_node(node, list(...))
}
