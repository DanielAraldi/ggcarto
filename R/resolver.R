has_coordinates <- function(node) {
  any(vapply(
    node[c("x", "y", "top", "right", "bottom", "left")],
    Negate(is.null),
    logical(1)
  ))
}

resolve_property <- function(
  value,
  context,
  axis,
  node,
  property,
  fallback = NULL
) {
  if (is.null(value)) {
    return(fallback)
  }
  result <- tryCatch(
    resolve_length(value, context, axis),
    ggcarto_error = function(condition) {
      gc_abort(condition$message, node = node$id, property = property)
    }
  )
  if (!is.finite(result)) {
    gc_abort(
      paste0(property, " must resolve to a finite length."),
      node = node$id,
      property = property
    )
  }
  result
}

resolve_edges <- function(edges, context, node, property) {
  result <- vapply(
    names(edges),
    function(side) {
      resolve_property(
        edges[[side]],
        context,
        if (side %in% c("left", "right")) "x" else "y",
        node,
        property
      )
    },
    numeric(1)
  )
  if (any(result < 0)) {
    gc_abort(
      paste0(property, " cannot be negative."),
      node = node$id,
      property = property
    )
  }
  result
}

resolve_border <- function(border, context, node) {
  if (is.null(border)) {
    return(list(width = 0, color = NA, line_type = 1))
  }
  if (is.character(border)) {
    border <- list(color = border)
  }
  if (
    !is.list(border) ||
      !all(names(border) %in% c("color", "colour", "width", "line_type"))
  ) {
    gc_abort(
      "border must be a color or a list of color, width and line_type.",
      node = node$id,
      property = "border"
    )
  }
  width <- resolve_property(
    border$width %||% "1px",
    context,
    "x",
    node,
    "border"
  )
  if (width < 0) {
    gc_abort(
      "Border width cannot be negative.",
      node = node$id,
      property = "border"
    )
  }
  list(
    width = width,
    color = border$color %||% border$colour %||% "black",
    line_type = border$line_type %||% 1
  )
}

anchor_fraction <- function(anchor) {
  index <- match(anchor, anchors) - 1L
  c(x = (index %% 3L) / 2, y = (index %/% 3L) / 2)
}

adapter_dimensions <- function(node, grob, context) {
  measured_node <- node
  measured_node$content <- grob
  dimensions <- node$adapter$measure(measured_node, context)
  if (is.list(dimensions)) {
    dimensions <- unlist(dimensions[c("width", "height")])
  }
  if (is.null(names(dimensions)) && length(dimensions) == 2L) {
    names(dimensions) <- c("width", "height")
  }
  dimensions[c("width", "height")]
}

grob_dimensions <- function(node, grob, context) {
  dimensions <- measure_grob(grob, context)
  if (inherits(grob, "gtable") && !identical(node$type, "legend")) {
    if (any(grid::unitType(grob$widths) == "null")) {
      dimensions[["width"]] <- context$width
    }
    if (any(grid::unitType(grob$heights) == "null")) {
      dimensions[["height"]] <- context$height
    }
  }
  if (identical(node$type, "x_axis")) {
    dimensions[["width"]] <- context$width
  }
  if (identical(node$type, "y_axis")) dimensions[["height"]] <- context$height
  dimensions
}

measure_content <- function(node, grob, context) {
  if (node$kind %in% c("plot", "viewport") || identical(node$type, "panel")) {
    return(c(width = context$width, height = context$height))
  }
  dimensions <- if (is.function(node$adapter$measure)) {
    adapter_dimensions(node, grob, context)
  } else {
    grob_dimensions(node, grob, context)
  }
  if (
    length(dimensions) != 2L ||
      !all(is.finite(dimensions)) ||
      any(dimensions < 0)
  ) {
    gc_abort(
      paste0(
        "Measurer must return finite, nonnegative width ",
        "and height in logical pixels."
      ),
      "invalid_measurement",
      node$id
    )
  }
  dimensions
}

size_limits <- function(node, context) {
  result <- list()
  for (axis in c("width", "height")) {
    minimum <- paste0("min_", axis)
    maximum <- paste0("max_", axis)
    coordinate <- if (axis == "width") "x" else "y"
    result[[minimum]] <- resolve_property(
      node[[minimum]],
      context,
      coordinate,
      node,
      minimum,
      0
    )
    result[[maximum]] <- resolve_property(
      node[[maximum]],
      context,
      coordinate,
      node,
      maximum,
      Inf
    )
    if (result[[minimum]] < 0 || result[[maximum]] < result[[minimum]]) {
      gc_abort(
        paste0("Invalid min/max constraints for ", axis, "."),
        node = node$id,
        property = axis
      )
    }
  }
  result
}

axis_sides <- function(dimension) {
  if (dimension == "width") c("left", "right") else c("top", "bottom")
}

preferred_size <- function(
  node,
  dimension,
  intrinsic,
  context,
  margin,
  padding,
  border,
  area
) {
  coordinate <- if (dimension == "width") "x" else "y"
  sides <- axis_sides(dimension)
  insets <- lapply(sides, function(side) {
    resolve_property(node[[side]], context, coordinate, node, side)
  })
  if (node[[dimension]]$kind != "auto") {
    return(resolve_property(
      node[[dimension]],
      context,
      coordinate,
      node,
      dimension
    ))
  }
  if (all(lengths(insets) > 0L)) {
    return(context[[dimension]] - sum(unlist(insets)) - sum(margin[sides]))
  }
  if (node$kind %in% c("plot", "viewport") || identical(node$type, "panel")) {
    return(area[[dimension]] - sum(margin[sides]))
  }
  intrinsic[[dimension]] + sum(padding[sides]) + 2 * border$width
}

apply_aspect_ratio <- function(node, dimensions, limits) {
  ratio <- node$aspect_ratio
  if (
    node$width$kind != "auto" &&
      node$height$kind != "auto" &&
      abs(dimensions[["width"]] - ratio * dimensions[["height"]]) > 1e-7
  ) {
    gc_abort(
      "Explicit dimensions contradict aspect_ratio.",
      node = node$id,
      property = "aspect_ratio"
    )
  }
  preferred <- if (node$width$kind == "auto" && node$height$kind != "auto") {
    dimensions[["height"]] * ratio
  } else {
    dimensions[["width"]]
  }
  minimum <- max(limits$min_width, limits$min_height * ratio)
  maximum <- min(limits$max_width, limits$max_height * ratio)
  if (minimum > maximum) {
    gc_abort(
      "Aspect ratio conflicts with min/max constraints.",
      node = node$id,
      property = "aspect_ratio"
    )
  }
  dimensions[["width"]] <- max(minimum, min(preferred, maximum))
  dimensions[["height"]] <- dimensions[["width"]] / ratio
  dimensions
}

resolve_size <- function(
  node,
  intrinsic,
  context,
  margin,
  padding,
  border,
  area
) {
  dimensions <- c(width = 0, height = 0)
  limits <- size_limits(node, context)
  for (dimension in names(dimensions)) {
    preferred <- preferred_size(
      node,
      dimension,
      intrinsic,
      context,
      margin,
      padding,
      border,
      area
    )
    if (preferred < 0) {
      gc_abort(
        paste0("Negative resolved ", dimension, "."),
        node = node$id,
        property = dimension
      )
    }
    dimensions[[dimension]] <- max(
      limits[[paste0("min_", dimension)]],
      min(preferred, limits[[paste0("max_", dimension)]])
    )
  }
  if (!is.null(node$aspect_ratio)) {
    dimensions <- apply_aspect_ratio(node, dimensions, limits)
  }
  dimensions
}

placement_area <- function(node, context, safe_area) {
  area <- c(x = 0, y = 0, width = context$width, height = context$height)
  if (!has_coordinates(node)) {
    area[["x"]] <- safe_area[["left"]]
    area[["y"]] <- safe_area[["top"]]
    area[["width"]] <- context$width - sum(safe_area[c("left", "right")])
    area[["height"]] <- context$height - sum(safe_area[c("top", "bottom")])
  }
  if (any(area[c("width", "height")] < 0)) {
    gc_abort(
      "Safe area exceeds the containing viewport.",
      node = node$id,
      property = "safe_area"
    )
  }
  area
}

check_axis_constraints <- function(node, axis, near, far, coordinate) {
  if (!is.null(coordinate) && (!is.null(near) || !is.null(far))) {
    sides <- axis_sides(if (axis == "x") "width" else "height")
    gc_abort(
      paste0(
        axis,
        " cannot be combined with ",
        paste(sides, collapse = "/"),
        "."
      ),
      "contradictory_constraints",
      node$id,
      axis
    )
  }
  invisible(node)
}

check_opposing_insets <- function(
  node,
  dimension,
  near,
  far,
  span,
  available
) {
  if (
    !is.null(near) &&
      !is.null(far) &&
      abs(near + far + span[[1L]] + span[[2L]] - available) > 1e-7
  ) {
    gc_abort(
      paste0("Opposing insets contradict ", dimension, "."),
      "contradictory_constraints",
      node$id,
      dimension
    )
  }
  invisible(node)
}

axis_position <- function(
  axis,
  near,
  far,
  coordinate,
  extent,
  fraction,
  margin,
  context,
  area
) {
  dimension <- if (axis == "x") "width" else "height"
  sides <- axis_sides(dimension)
  if (!is.null(coordinate)) {
    return(
      coordinate -
        fraction * extent +
        (1 - fraction) * margin[[sides[[1L]]]] -
        fraction * margin[[sides[[2L]]]]
    )
  }
  if (!is.null(near)) {
    return(near + margin[[sides[[1L]]]])
  }
  if (!is.null(far)) {
    return(context[[dimension]] - far - extent - margin[[sides[[2L]]]])
  }
  area[[axis]] +
    margin[[sides[[1L]]]] +
    fraction * (area[[dimension]] - sum(margin[sides]) - extent)
}

resolve_position <- function(node, dimensions, context, margin, area) {
  fraction <- anchor_fraction(node$anchor)
  position <- c(x = 0, y = 0)
  for (axis in c("x", "y")) {
    dimension <- if (axis == "x") "width" else "height"
    sides <- axis_sides(dimension)
    near <- resolve_property(
      node[[sides[[1L]]]],
      context,
      axis,
      node,
      sides[[1L]]
    )
    far <- resolve_property(
      node[[sides[[2L]]]],
      context,
      axis,
      node,
      sides[[2L]]
    )
    coordinate <- resolve_property(node[[axis]], context, axis, node, axis)
    check_axis_constraints(node, axis, near, far, coordinate)
    extent <- dimensions[[dimension]]
    check_opposing_insets(
      node,
      dimension,
      near,
      far,
      c(extent, sum(margin[sides])),
      context[[dimension]]
    )
    position[[axis]] <- axis_position(
      axis,
      near,
      far,
      coordinate,
      extent,
      fraction[[axis]],
      margin,
      context,
      area
    )
  }
  c(position, dimensions)
}

resolve_node <- function(node, context, safe_area) {
  if (inherits(node, "gc_scale_bar")) {
    node <- bind_scale_bar(node, context)
  }
  margin <- resolve_edges(node$margin, context, node, "margin")
  padding <- resolve_edges(node$padding, context, node, "padding")
  border <- resolve_border(node$border, context, node)
  area <- placement_area(node, context, safe_area)
  content <- with_grid_context(context, prepare_content(node, context))
  intrinsic <- measure_content(node, content, context)
  dimensions <- resolve_size(
    node,
    intrinsic,
    context,
    margin,
    padding,
    border,
    area
  )
  box <- resolve_position(node, dimensions, context, margin, area)
  list(
    id = node$id,
    node = node,
    box = box,
    margin = margin,
    padding = padding,
    border = border,
    intrinsic = intrinsic,
    content = content,
    children = list(),
    scale = 1,
    collision_candidates = list(),
    scale_bar = node$scale_info
  )
}

content_box <- function(resolved) {
  padding <- resolved$padding
  border <- resolved$border$width
  c(
    x = unname(padding[["left"]] + border),
    y = unname(padding[["top"]] + border),
    width = max(
      0,
      resolved$box[["width"]] - sum(padding[c("left", "right")]) - 2 * border
    ),
    height = max(
      0,
      resolved$box[["height"]] - sum(padding[c("top", "bottom")]) - 2 * border
    )
  )
}

validate_resolved <- function(resolved, context) {
  box <- resolved$box
  margin <- resolved$margin
  tolerance <- 1e-7
  outside <- box[["x"]] - margin[["left"]] < -tolerance ||
    box[["y"]] - margin[["top"]] < -tolerance ||
    box[["x"]] + box[["width"]] + margin[["right"]] >
      context$width + tolerance ||
    box[["y"]] + box[["height"]] + margin[["bottom"]] >
      context$height + tolerance
  if (outside) {
    gc_warn(
      "Box exceeds its containing viewport.",
      "overflow",
      resolved$id,
      "box"
    )
  }
  content <- content_box(resolved)
  fixed_content <- isTRUE(
    resolved$node$type %in% c(text_types, "credits", "annotation")
  )
  if (
    fixed_content &&
      resolved$node$overflow != "hidden" &&
      any(
        resolved$intrinsic * resolved$scale >
          content[c("width", "height")] + tolerance
      )
  ) {
    gc_warn(
      paste0(
        "Intrinsic content exceeds its box; no implicit ",
        "clipping or text reflow is applied."
      ),
      "overflow",
      resolved$id,
      "content"
    )
  }
  invisible(resolved)
}

flow_insets <- function(flow, cursor, safe_area) {
  list(
    left = gc_length(
      if (flow == "row") cursor + safe_area[["left"]] else safe_area[["left"]]
    ),
    top = gc_length(
      if (flow == "column") {
        cursor + safe_area[["top"]]
      } else {
        safe_area[["top"]]
      }
    )
  )
}

flow_advance <- function(result, flow, gap) {
  dimension <- if (flow == "row") "width" else "height"
  result$box[[dimension]] + sum(result$margin[axis_sides(dimension)]) + gap
}

resolve_flow <- function(parent, context, safe_area, gap) {
  flow <- parent$node$flow
  sequential <- flow %in% c("row", "column")
  cursor <- 0
  children <- vector("list", length(parent$node$children))
  names(children) <- names(parent$node$children)
  for (index in seq_along(parent$node$children)) {
    original <- parent$node$children[[index]]
    child <- original
    automatic <- sequential && !has_coordinates(child)
    if (automatic) {
      insets <- flow_insets(flow, cursor, safe_area)
      child$left <- insets$left
      child$top <- insets$top
    }
    result <- resolve_node(child, context, safe_area)
    result$node <- original
    if (automatic) {
      cursor <- cursor + flow_advance(result, flow, gap)
    }
    children[[index]] <- result
  }
  children
}

resolve_descendants <- function(child, context) {
  validate_resolved(child, context)
  if (!length(child$node$children)) {
    return(child)
  }
  inner <- content_box(child)
  if (any(inner[c("width", "height")] <= 0)) {
    gc_warn(
      "No positive content area for child nodes.",
      "overflow",
      child$id,
      "padding"
    )
    return(child)
  }
  child_context <- new_layout_context(
    inner[["width"]],
    inner[["height"]],
    context$dpi,
    context$root_width,
    context$root_height
  )
  child$children <- resolve_children(child, child_context)
  child
}

resolve_children <- function(parent, context) {
  context$map_panel <- parent$node$map_panel
  safe_area <- resolve_edges(
    parent$node$safe_area,
    context,
    parent$node,
    "safe_area"
  )
  gap <- resolve_property(
    parent$node$gap,
    context,
    if (parent$node$flow == "column") "y" else "x",
    parent$node,
    "gap"
  )
  if (gap < 0) {
    gc_abort("gap cannot be negative.", node = parent$id, property = "gap")
  }
  children <- resolve_flow(parent, context, safe_area, gap)
  if (
    any(vapply(
      children,
      function(child) child$node$collision != "none",
      logical(1)
    ))
  ) {
    children <- resolve_collisions(children, context, safe_area)
  }
  lapply(children, resolve_descendants, context = context)
}

root_context <- function(node, width, height, dpi) {
  defaults <- c(width = 800, height = 600)
  if (grDevices::dev.cur() != 1L) {
    defaults <- c(
      width = grid::convertWidth(
        gc_unit(1, "npc"),
        "inches",
        valueOnly = TRUE
      ) *
        96,
      height = grid::convertHeight(
        gc_unit(1, "npc"),
        "inches",
        valueOnly = TRUE
      ) *
        96
    )
  } else {
    reference <- new_layout_context(
      defaults[["width"]],
      defaults[["height"]],
      dpi
    )
    for (dimension in names(defaults)) {
      declaration <- node[[dimension]]
      if (declaration$kind != "auto") {
        defaults[[dimension]] <- resolve_length(
          declaration,
          reference,
          if (dimension == "width") "x" else "y"
        )
      }
    }
  }
  new_layout_context(
    width %||% defaults[["width"]],
    height %||% defaults[["height"]],
    dpi
  )
}

#' Resolve a scene into boxes for one rendering context
#'
#' Evaluate logical lengths, intrinsic sizes, constraints and collision
#' policies without drawing. The result is an inspection snapshot for a
#' particular size; keep the original scene for later rendering or export.
#'
#' @param object An ggcarto scene, viewport, element, ggplot or grid grob.
#'   Non-viewport objects are wrapped in a root viewport for resolution.
#' @param width,height Optional positive numeric root dimensions in logical
#'   pixels. Each `NULL` dimension uses the current grid viewport if a device
#'   is open. Without a device, scene reference dimensions are used, falling
#'   back to 800 by 600 for automatic dimensions.
#' @param dpi Positive output-density metadata. Logical pixels remain 1/96 inch;
#'   this value does not redefine lengths or change the current device.
#'
#' @details
#' The root content box accounts for padding and borders; child coordinates
#' are local to their parent's content box. Implicit node IDs are assigned
#' deterministically. Conflicting constraints raise errors. Unresolved box
#' overflow and collision placement emit `ggcarto_overflow` and
#' `ggcarto_collision`
#' warnings rather than being silently repaired or clipped.
#'
#' Measurement uses the current graphics device's font metrics. If no device
#' is open, a temporary null PDF device is opened and closed. The caller's
#' viewport stack and scene declarations are preserved. Results may differ
#' between devices with different font metrics.
#'
#' @returns An `gc_layout` list with `root` and `context` fields. The context
#'   contains dimensions, root dimensions and DPI. Resolved nodes contain
#'   `id`, the logical `node`, a named `box` (`x`, `y`, `width`, `height`),
#'   `margin`, `padding`, `border`, prepared `content`, `intrinsic` dimensions,
#'   a content `scale`, `collision_candidates` and resolved `children`. Box
#'   measurements are logical pixels. This is not an input to [gc_render()].
#' @seealso [gc_measure()], [gc_render()], [gc_place()], [gc_length()]
#' @examples
#' scene <- gc_viewport(list(
#'   gc_place(gc_rect(fill = "#95CEC0"),
#'     left = "10%", top = 10,
#'     width = "50%", height = 40
#'   )
#' ))
#' small <- gc_resolve(scene, width = 300, height = 120)
#' large <- gc_resolve(scene, width = 600, height = 240)
#' small$root$children[[1]]$box
#' large$root$children[[1]]$box
#' @export
gc_resolve <- function(object, width = NULL, height = NULL, dpi = 96) {
  node <- as_l_node(object)
  if (node$kind != "viewport") {
    node <- gc_viewport(list(node), width = node$width, height = node$height)
  }
  node <- assign_scene_ids(node)
  if (anyDuplicated(scene_ids(node))) {
    gc_abort("Node IDs must be unique within a scene.", property = "id")
  }
  context <- root_context(node, width, height, dpi)
  with_grid_context(context, {
    root <- list(
      id = node$id,
      node = node,
      box = c(x = 0, y = 0, width = context$width, height = context$height),
      margin = resolve_edges(node$margin, context, node, "margin"),
      padding = resolve_edges(node$padding, context, node, "padding"),
      border = resolve_border(node$border, context, node),
      content = NULL,
      scale = 1,
      intrinsic = c(width = context$width, height = context$height),
      collision_candidates = list()
    )
    inner <- content_box(root)
    if (any(inner[c("width", "height")] <= 0)) {
      gc_abort("Root padding/border leaves no content area.", node = node$id)
    }
    child_context <- new_layout_context(
      inner[["width"]],
      inner[["height"]],
      dpi,
      context$width,
      context$height
    )
    root$children <- resolve_children(root, child_context)
    structure(list(root = root, context = context), class = "gc_layout")
  })
}

#' Measure a graphics object's constrained and intrinsic size
#'
#' Prepare and measure one node in an explicit layout context without drawing.
#' Use this for sizing annotations and checking the effect of responsive
#' styles; use [gc_resolve()] to inspect all children of a complete scene.
#'
#' @param object An ggcarto element, scene node, ggplot or grid grob.
#' @param viewport A list with positive numeric `width` and `height` in logical
#'   pixels. Optional `root_width` and `root_height` control root-relative
#'   units;
#'   otherwise each defaults to its corresponding local dimension.
#' @inheritParams gc_resolve
#'
#' @details
#' Measurement includes the node's dimension constraints, padding and border
#' in the reported box size, but not its outer margins. The `intrinsic` field
#' describes content before those layout additions. Custom adapter measurers
#' receive prepared styled content; see [gc_register_element()].
#'
#' Plots, panels and viewports use available or declared dimensions rather than
#' inferring bounds around absolutely positioned children. Generic gTrees may
#' not provide intrinsic child bounds; give templates explicit layout sizes.
#' Measurement does not wrap text or run full sibling collision placement.
#'
#' The current device supplies font metrics. If none is open, a temporary null
#' PDF device is used and closed. No file is written and the caller's viewport
#' stack is restored. Results are context-specific, not cached into the node.
#'
#' @returns A list with numeric `width` and `height`, named numeric `intrinsic`
#'   dimensions and `units = "px"`.
#' @seealso [gc_resolve()], [gc_get_element()], [gc_style()], [gc_template()]
#' @examples
#' label <- gc_get_element(gc_text("Survey area"), "annotation",
#'   style = list(font_size = "clamp(8pt, 2vmin, 18pt)", padding = 4)
#' )
#' gc_measure(label, viewport = list(width = 300, height = 200))
#' gc_measure(label, viewport = list(width = 1200, height = 800))
#' gc_measure(gc_rect(), viewport = list(width = 200, height = 100))
#' @export
gc_measure <- function(
  object,
  viewport = list(width = 800, height = 600),
  dpi = 96
) {
  node <- as_l_node(object)
  context <- new_layout_context(
    viewport$width,
    viewport$height,
    dpi,
    viewport$root_width %||% viewport$width,
    viewport$root_height %||% viewport$height
  )
  with_grid_context(context, {
    resolved <- resolve_node(
      node,
      context,
      c(top = 0, right = 0, bottom = 0, left = 0)
    )
    list(
      width = resolved$box[["width"]],
      height = resolved$box[["height"]],
      intrinsic = resolved$intrinsic,
      units = "px"
    )
  })
}
