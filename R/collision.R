collision_box <- function(resolved) {
  box <- resolved$box
  margin <- resolved$margin
  c(
    x = box[["x"]] - margin[["left"]],
    y = box[["y"]] - margin[["top"]],
    width = box[["width"]] + sum(margin[c("left", "right")]),
    height = box[["height"]] + sum(margin[c("top", "bottom")])
  )
}

boxes_overlap <- function(first, second) {
  first[["x"]] < second[["x"]] + second[["width"]] - 1e-7 &&
    first[["x"]] + first[["width"]] > second[["x"]] + 1e-7 &&
    first[["y"]] < second[["y"]] + second[["height"]] - 1e-7 &&
    first[["y"]] + first[["height"]] > second[["y"]] + 1e-7
}

collision_obstacle <- function(resolved) {
  override <- resolved$node$metadata$obstacle
  if (!is.null(override)) {
    return(isTRUE(override))
  }
  resolved$node$kind != "plot" &&
    !isTRUE(
      resolved$node$type %in% c("panel", "plot_background", "panel_background")
    )
}

fits_area <- function(box, area) {
  box[["x"]] >= area[["x"]] - 1e-7 &&
    box[["y"]] >= area[["y"]] - 1e-7 &&
    box[["x"]] + box[["width"]] <= area[["x"]] + area[["width"]] + 1e-7 &&
    box[["y"]] + box[["height"]] <= area[["y"]] + area[["height"]] + 1e-7
}

collision_anchors <- function(node) {
  if (!is.null(node$candidates)) {
    return(unique(c(node$anchor, node$candidates)))
  }
  if (node$anchor == "top-right") {
    return(c(
      "top-right",
      "center-right",
      "top-left",
      "bottom-right",
      "bottom-center",
      "bottom-left",
      "top-center",
      "center-left",
      "center"
    ))
  }
  unique(c(node$anchor, anchors))
}

shrink_factors <- function(resolved, context) {
  node <- resolved$node
  if (
    !node$responsive ||
      !node$collision %in% c("shrink", "avoid-and-shrink") ||
      (is.null(node$min_width) && is.null(node$min_height))
  ) {
    return(1)
  }
  inner <- content_box(resolved)
  if (any(inner[c("width", "height")] <= 0)) {
    return(1)
  }
  limits <- size_limits(node, context)
  fixed <- resolved$box[c("width", "height")] - inner[c("width", "height")]
  minimum <- max(
    0.1,
    (limits$min_width - fixed[["width"]]) / inner[["width"]],
    (limits$min_height - fixed[["height"]]) / inner[["height"]]
  )
  if (minimum >= 1) {
    return(1)
  }
  unique(c(seq(1, minimum, by = -0.05), minimum))
}

collision_fixed <- function(child) {
  child$node$collision == "none" ||
    (has_coordinates(child$node) && !child$node$allow_move)
}

collision_viable <- function(candidate, region, accepted) {
  box <- collision_box(candidate)
  fits_area(box, region) &&
    !any(vapply(
      accepted,
      function(other) boxes_overlap(box, collision_box(other)),
      logical(1)
    ))
}

collision_attempt <- function(anchor, scale, box, accepted) {
  list(anchor = anchor, scale = scale, box = box, accepted = accepted)
}

collision_candidate <- function(current, base, anchor, factor, context, area) {
  inner <- content_box(current)
  dimensions <- current$box[c("width", "height")] -
    inner[c("width", "height")] * (1 - factor)
  base$anchor <- anchor
  current$box <- resolve_position(
    base,
    dimensions,
    context,
    current$margin,
    area
  )
  current$scale <- factor
  current
}

search_collision <- function(current, movable, accepted, context, areas) {
  node <- current$node
  can_move <- node$collision %in% c("avoid", "avoid-and-shrink")
  base <- if (can_move) movable else node
  region <- if (can_move) areas$movable else areas$original
  choices <- if (can_move) collision_anchors(node) else node$anchor
  attempts <- list()
  for (factor in shrink_factors(current, context)) {
    for (anchor in choices) {
      candidate <- collision_candidate(
        current,
        base,
        anchor,
        factor,
        context,
        region
      )
      found <- collision_viable(candidate, region, accepted)
      attempts[[length(attempts) + 1L]] <- collision_attempt(
        anchor,
        factor,
        candidate$box,
        found
      )
      if (found) {
        return(list(current = candidate, found = TRUE, attempts = attempts))
      }
    }
  }
  list(current = current, found = FALSE, attempts = attempts)
}

place_collision <- function(current, fixed, accepted, context, safe_area) {
  node <- current$node
  movable <- node
  for (property in c("x", "y", "left", "right", "top", "bottom")) {
    movable[property] <- list(NULL)
  }
  area <- placement_area(movable, context, safe_area)
  areas <- list(
    movable = area,
    original = if (has_coordinates(node)) {
      placement_area(node, context, safe_area)
    } else {
      area
    }
  )
  found <- collision_viable(current, areas$original, accepted)
  attempts <- list(collision_attempt(node$anchor, 1, current$box, found))
  if (!found && !fixed) {
    search <- search_collision(current, movable, accepted, context, areas)
    current <- search$current
    found <- search$found
    attempts <- c(attempts, search$attempts)
  }
  current$collision_candidates <- attempts
  if (!found) {
    gc_warn(
      "No permitted collision-free placement; keeping the preferred box.",
      "collision",
      current$id,
      "collision"
    )
  }
  current
}

resolve_collisions <- function(children, context, safe_area) {
  fixed <- vapply(children, collision_fixed, logical(1))
  priority <- vapply(children, function(child) child$node$priority, numeric(1))
  indices <- order(!fixed, -priority, seq_along(children))
  accepted <- list()
  for (index in indices) {
    current <- children[[index]]
    if (current$node$collision != "none") {
      current <- place_collision(
        current,
        fixed[[index]],
        accepted,
        context,
        safe_area
      )
    }
    children[[index]] <- current
    if (collision_obstacle(current)) {
      accepted[[length(accepted) + 1L]] <- current
    }
  }
  children
}
