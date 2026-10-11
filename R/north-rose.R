north_rose_designs <- c(
  "classic",
  "eight_point",
  "sixteen_point",
  "thirty_two_point",
  "stellar",
  "concentric",
  "compass",
  "nautical",
  "minimal",
  "geometric",
  "ornamental",
  "asymmetric"
)

north_rose_polar <- function(angle, radius) {
  list(x = 0.5 + sin(angle) * radius, y = 0.5 + cos(angle) * radius)
}

north_rose_polygon <- function(angle, radius, colour, name) {
  position <- north_rose_polar(angle, radius)
  grid::polygonGrob(
    x = position$x,
    y = position$y,
    name = name,
    gp = grid::gpar(fill = colour)
  )
}

north_rose_ring <- function(radius, name) {
  grid::circleGrob(
    r = gc_unit(radius, "npc"),
    name = name,
    gp = grid::gpar(fill = NA)
  )
}

north_rose_rays <- function(count, inner, outer, name) {
  angles <- (seq_len(count) - 1) * 2 * pi / count
  start <- north_rose_polar(angles, inner)
  end <- north_rose_polar(angles, outer)
  grid::segmentsGrob(
    x0 = start$x,
    y0 = start$y,
    x1 = end$x,
    y1 = end$y,
    name = name
  )
}

north_rose_star <- function(
  count,
  radii,
  fill,
  fill_secondary,
  inner = 0.07,
  offset = 0,
  prefix = "point"
) {
  radii <- rep_len(radii, count)
  shapes <- list()
  for (index in seq_len(count)) {
    direction <- (index - 1) * 2 * pi / count + offset
    for (side in c(-1, 1)) {
      shapes[[length(shapes) + 1L]] <- north_rose_polygon(
        c(direction, direction + side * pi / count, direction),
        c(0, inner, radii[[index]]),
        if (side == -1) fill else fill_secondary,
        sprintf(
          "%s-%02d-%s",
          prefix,
          index,
          if (side == -1) "primary" else "secondary"
        )
      )
    }
  }
  shapes
}

north_rose_radii16 <- rep(c(0.34, 0.19, 0.26, 0.19), 4)

north_rose_classic <- function(points, fill, fill_secondary) {
  north_rose_star(4, 0.33, fill, fill_secondary, inner = 0.09)
}

north_rose_eight_point <- function(points, fill, fill_secondary) {
  north_rose_star(8, c(0.34, 0.24), fill, fill_secondary)
}

north_rose_sixteen_point <- function(points, fill, fill_secondary) {
  north_rose_star(16, north_rose_radii16, fill, fill_secondary)
}

north_rose_thirty_two_point <- function(points, fill, fill_secondary) {
  north_rose_star(
    32,
    rep(c(0.34, 0.15, 0.21, 0.15, 0.27, 0.15, 0.21, 0.15), 4),
    fill,
    fill_secondary,
    inner = 0.06
  )
}

north_rose_stellar <- function(points, fill, fill_secondary) {
  north_rose_star(points, c(0.36, 0.29), fill, fill_secondary, inner = 0.025)
}

north_rose_concentric <- function(points, fill, fill_secondary) {
  c(
    list(
      north_rose_ring(0.36, "outer-ring"),
      north_rose_ring(0.28, "middle-ring"),
      north_rose_ring(0.19, "inner-ring"),
      north_rose_rays(32, 0.34, 0.36, "ring-divisions")
    ),
    north_rose_star(8, c(0.32, 0.24), fill, fill_secondary)
  )
}

north_rose_compass <- function(points, fill, fill_secondary) {
  c(
    list(
      north_rose_ring(0.36, "case"),
      north_rose_ring(0.32, "dial"),
      north_rose_rays(32, 0.295, 0.32, "dial-ticks"),
      north_rose_rays(4, 0.265, 0.32, "cardinal-ticks")
    ),
    north_rose_star(
      2,
      c(0.29, 0.25),
      fill,
      fill_secondary,
      inner = 0.055,
      prefix = "needle"
    ),
    list(grid::circleGrob(
      r = gc_unit(0.024, "npc"),
      name = "pivot",
      gp = grid::gpar(fill = fill_secondary)
    ))
  )
}

north_rose_nautical <- function(points, fill, fill_secondary) {
  c(
    list(
      north_rose_ring(0.365, "outer-ring"),
      north_rose_ring(0.34, "inner-ring"),
      north_rose_rays(32, 0.08, 0.365, "rhumb-lines")
    ),
    north_rose_star(
      16,
      north_rose_radii16 * 0.92,
      fill,
      fill_secondary,
      inner = 0.065
    ),
    list(
      north_rose_ring(0.09, "hub-ring"),
      north_rose_polygon(
        c(-0.035, 0, 0.035, 0),
        c(0.355, 0.39, 0.355, 0.365),
        fill,
        "north-mark"
      )
    )
  )
}

north_rose_minimal <- function(points, fill, fill_secondary) {
  list(
    north_rose_rays(4, 0.045, 0.30, "fine-rays"),
    north_rose_polygon(
      c(-0.07, 0, 0.07),
      c(0.27, 0.33, 0.27),
      fill,
      "north-tip"
    ),
    north_rose_ring(0.018, "center")
  )
}

north_rose_geometric <- function(points, fill, fill_secondary) {
  diamonds <- lapply(seq_len(8), function(index) {
    north_rose_polygon(
      (index - 1) * pi / 4 + c(0, -0.17, 0, 0.17),
      c(if (index %% 2) 0.34 else 0.25, 0.14, 0.065, 0.14),
      if (index %% 2) fill else fill_secondary,
      sprintf("diamond-%02d", index)
    )
  })
  c(
    diamonds,
    list(north_rose_polygon(
      (0:3) * pi / 2,
      rep(0.042, 4),
      fill_secondary,
      "center-diamond"
    ))
  )
}

north_rose_ornament <- function(index, fill_secondary) {
  direction <- (index - 1) * pi / 4
  position <- north_rose_polar(direction, 0.345)
  list(
    grid::circleGrob(
      x = position$x,
      y = position$y,
      r = gc_unit(0.016, "npc"),
      name = sprintf("bead-%02d", index),
      gp = grid::gpar(fill = fill_secondary)
    ),
    north_rose_polygon(
      direction + c(0, -0.12, 0, 0.12),
      c(0.32, 0.27, 0.20, 0.27),
      fill_secondary,
      sprintf("petal-%02d", index)
    )
  )
}

north_rose_ornamental <- function(points, fill, fill_secondary) {
  c(
    list(
      north_rose_ring(0.36, "outer-ring"),
      north_rose_ring(0.33, "inner-ring")
    ),
    unlist(
      lapply(seq_len(8), north_rose_ornament, fill_secondary = fill_secondary),
      recursive = FALSE
    ),
    north_rose_star(8, c(0.29, 0.21), fill, fill_secondary, inner = 0.11),
    list(
      north_rose_ring(0.065, "jewel-ring"),
      north_rose_polygon((0:3) * pi / 2, rep(0.04, 4), fill, "jewel")
    )
  )
}

north_rose_asymmetric <- function(points, fill, fill_secondary) {
  diamonds <- lapply(seq_len(4), function(index) {
    north_rose_polygon(
      (index - 1) * pi / 2 + pi / 4 + c(0, -0.22, 0, 0.22),
      c(0.23, 0.12, 0.065, 0.12),
      fill_secondary,
      sprintf("intercardinal-%02d", index)
    )
  })
  c(
    diamonds,
    north_rose_star(
      4,
      c(0.36, 0.30, 0.32, 0.30),
      fill,
      fill_secondary,
      inner = 0.10,
      prefix = "cardinal"
    )
  )
}

north_rose_body <- function(design, points, fill, fill_secondary) {
  builder <- switch(
    design,
    classic = north_rose_classic,
    eight_point = north_rose_eight_point,
    sixteen_point = north_rose_sixteen_point,
    thirty_two_point = north_rose_thirty_two_point,
    stellar = north_rose_stellar,
    concentric = north_rose_concentric,
    compass = north_rose_compass,
    nautical = north_rose_nautical,
    minimal = north_rose_minimal,
    geometric = north_rose_geometric,
    ornamental = north_rose_ornamental,
    asymmetric = north_rose_asymmetric
  )
  builder(points, fill, fill_secondary)
}

check_design <- function(design, designs, message) {
  if (!is.character(design) || !is_choice(design, designs)) {
    gc_abort(message, property = "design")
  }
  invisible(design)
}

north_rose_colour_index <- function(colour) {
  !is.numeric(colour) ||
    is.na(colour) ||
    (is.finite(colour) && colour <= .Machine$integer.max)
}

north_rose_colour_valid <- function(colour) {
  is_colour_value(colour) &&
    north_rose_colour_index(colour) &&
    colour_parses(colour)
}

north_rose_check_colours <- function(colours) {
  for (property in names(colours)) {
    if (!north_rose_colour_valid(colours[[property]])) {
      gc_abort(
        paste(property, "must be a scalar grid colour or NA."),
        property = property
      )
    }
  }
  invisible(colours)
}

north_rose_labels <- function(labels, design) {
  if (isTRUE(labels)) {
    if (design %in% c("classic", "compass", "minimal")) {
      return(c("N", "E", "S", "W"))
    }
    return(c("N", "NE", "E", "SE", "S", "SW", "W", "NW"))
  }
  if (identical(labels, FALSE) || is.null(labels)) {
    return(character())
  }
  if (
    !is.character(labels) ||
      !length(labels) %in% c(4, 8, 16, 32) ||
      anyNA(labels)
  ) {
    gc_abort(
      "labels must be TRUE, FALSE, NULL, or 4, 8, 16 or 32 strings.",
      property = "labels"
    )
  }
  labels
}

north_rose_fontfamily <- function(fontfamily, design) {
  fontfamily <- fontfamily %||%
    if (design %in% c("nautical", "ornamental")) "serif" else ""
  if (!is_single_string(fontfamily)) {
    gc_abort(
      "fontfamily must be a single string or NULL.",
      property = "fontfamily"
    )
  }
  fontfamily
}

north_rose_label_gpar <- function(
  col,
  fontsize,
  fontface,
  fontfamily,
  label_gp
) {
  parameters <- template_gpar(list(
    col = col,
    fontsize = fontsize,
    fontface = fontface,
    fontfamily = fontfamily
  ))
  if (!is.null(label_gp)) {
    overrides <- template_gpar(label_gp)
    parameters[names(overrides)] <- overrides
  }
  parameters
}

north_rose_content <- function(body, labels, label_parameters) {
  if (!length(labels)) {
    return(list(body))
  }
  position <- north_rose_polar(
    (seq_along(labels) - 1) * 2 * pi / length(labels),
    0.425
  )
  list(
    body,
    gc_text(
      labels,
      x = position$x,
      y = position$y,
      name = "labels",
      gp = label_parameters
    )
  )
}

#' Create customizable compass roses
#'
#' Construct a north-oriented compass rose from native grid primitives, without
#' drawing or opening a graphics device. Twelve presets cover cartographic,
#' nautical, technical and decorative styles.
#'
#' @param design Exact preset name: `"classic"` (four points), `"eight_point"`,
#'   `"sixteen_point"`, `"thirty_two_point"`, `"stellar"`, `"concentric"`,
#'   `"compass"`, `"nautical"`, `"minimal"`, `"geometric"`, `"ornamental"`
#'   or `"asymmetric"`. Abbreviations and numeric indices are not accepted.
#' @param angle Finite counterclockwise rotation in degrees. Rotates the entire
#'   rose, including labels; zero points north toward the top of the viewport.
#' @param points Number of tips for `"stellar"`: 8, 16 or 32. Other presets have
#'   fixed geometry and ignore this setting after validation.
#' @param labels `TRUE` for automatic labels (N, E, S, W for `"classic"`,
#'   `"compass"` and `"minimal"`; N, NE, E, SE, S, SW, W, NW otherwise).
#'   `FALSE` or `NULL` omits labels. A character vector of length 4, 8, 16 or 32
#'   supplies equally spaced labels clockwise from north. Empty strings hide
#'   individual labels. Label count does not change the geometry.
#' @param col Outline and default label colour, as a scalar grid colour or `NA`.
#' @param fill,fill_secondary Primary and secondary scalar grid fill colours.
#'   Use `NA` for unfilled shapes. Line-only parts do not use fills.
#' @param lwd Positive grid line width. The `"minimal"` preset uses 65 percent
#'   of this value. Font sizes and line widths do not scale with the viewport.
#' @param fontface,fontfamily Native grid font face and family. When `NULL`,
#'   `fontfamily` uses `"serif"` for `"nautical"` and `"ornamental"`, and the
#'   device default family for the other presets.
#' @param gp Additional inherited graphical parameters as a [grid::gpar()] or
#'   uniquely named list. Explicit `col` and `lwd` take precedence. Preset fills
#'   use `fill` and `fill_secondary`, not `gp$fill`.
#' @param label_gp Label-specific graphical overrides, including colour and
#'   typography, as a [grid::gpar()] or uniquely named list.
#' @param name Optional grid grob name, distinct from a scene node ID.
#' @inheritParams gc_scale_bar
#' @inheritParams gc_north_arrow
#'
#' @details
#' The rose occupies a centered square with side equal to the shorter dimension
#' of its available viewport. Circles stay circular even in rectangular layout
#' boxes. Coordinates and geometry are resolved by grid at draw time; no device
#' dimensions are captured during construction. An outer `vp` is preserved and
#' its rotation adds to `angle`. Inputs and graphical parameter lists are not
#' modified. Native grid inheritance, including multiplicative alpha, applies.
#'
#' The four point-count presets use split triangular tips. `"stellar"` uses
#' long narrow tips; `"concentric"` uses three rings; `"compass"` has a divided
#' dial, two-ended needle and pivot; `"nautical"` combines rhumb lines, rings
#' and sixteen tips. `"minimal"` has thin cardinal rays; `"geometric"` uses
#' detached diamonds; `"ornamental"` adds beads, petals and a central jewel;
#' `"asymmetric"` combines broad cardinal tips with smaller intercardinal
#' diamonds and an emphasized north tip.
#'
#' Supply explicit sizes with [gc_place()] or [gc_get_element()]: a generic
#' gTree
#' has no automatic child bounding-box measurement. Start with a 120 by 120
#' logical-pixel box and enlarge it for dense or long labels. Text is not
#' automatically shrunk, wrapped or collision-checked, and parent clipping is
#' not overridden. Label centers are at 42.5 percent of the square's side from
#' the center. Leave additional room for long labels and rotation.
#'
#' This is graphical content, not a geographic calculation. It does not inspect
#' a map, calculate true north or apply magnetic declination. Supply an angle
#' computed by [gc_north_angle()] for the map and reference location when
#' required.
#' No spatial packages are needed to construct or render the rose.
#'
#' @returns A native grid `gTree`, usable with [grid::grid.draw()],
#'   [gc_template()],
#'   [gc_place()], [gc_get_element()] and [gc_save()].
#' @seealso [gc_north_angle()], [gc_template()], [gc_unit()], [gc_place()],
#'   [gc_get_element()]
#' @examples
#' rose <- gc_north_rose("eight_point", fill = "#197C80")
#' scene <- gc_viewport(list(
#'   gc_place(rose, right = 12, top = 12, width = 120, height = 120)
#' ), width = 240, height = 180, background = "white")
#' gc_render(scene)
#' gc_north_rose("stellar", points = 32, labels = FALSE)
#' gc_north_rose("classic", labels = c("N", "L", "S", "O"), angle = 15)
#' @export
gc_north_rose <- function(
  design = "classic",
  angle = 0,
  points = 16,
  labels = TRUE,
  col = "#203C43",
  fill = col,
  fill_secondary = "white",
  lwd = 1,
  fontsize = 11,
  fontface = "bold",
  fontfamily = NULL,
  gp = NULL,
  label_gp = NULL,
  name = NULL,
  vp = NULL
) {
  check_design(
    design,
    north_rose_designs,
    paste("design must be one of:", toString(north_rose_designs))
  )
  angle <- scalar_number(angle, "angle")
  points <- scalar_number(points, "points")
  if (!points %in% c(8, 16, 32)) {
    gc_abort("points must be 8, 16 or 32.", property = "points")
  }
  lwd <- scalar_number(lwd, "lwd", positive = TRUE)
  fontsize <- scalar_number(fontsize, "fontsize", positive = TRUE)
  north_rose_check_colours(
    list(col = col, fill = fill, fill_secondary = fill_secondary)
  )
  labels <- north_rose_labels(labels, design)
  fontfamily <- north_rose_fontfamily(fontfamily, design)
  parameters <- template_gpar(gp, list(col = col, lwd = lwd))
  label_parameters <- north_rose_label_gpar(
    col,
    fontsize,
    fontface,
    fontfamily,
    label_gp
  )
  body <- gc_template(
    children = north_rose_body(design, points, fill, fill_secondary),
    name = "body",
    gp = if (design == "minimal") grid::gpar(lwd = lwd * 0.65) else NULL
  )
  gc_template(
    gc_template(
      children = north_rose_content(body, labels, label_parameters),
      name = "rose",
      vp = grid::viewport(
        width = gc_unit(1, "snpc"),
        height = gc_unit(1, "snpc"),
        angle = angle,
        name = "north-rose-square"
      )
    ),
    gp = parameters,
    name = name,
    vp = vp
  )
}
