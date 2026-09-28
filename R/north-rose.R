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

north_rose_body <- function(design, points, fill, fill_secondary) {
  shapes <- list()
  add <- function(grob) {
    shapes[[length(shapes) + 1L]] <<- grob
  }
  polygon <- function(angle, radius, colour, name) {
    position <- north_rose_polar(angle, radius)
    add(grid::polygonGrob(
      x = position$x,
      y = position$y,
      name = name,
      gp = grid::gpar(fill = colour)
    ))
  }
  ring <- function(radius, name) {
    add(grid::circleGrob(
      r = l_unit(radius, "npc"),
      name = name,
      gp = grid::gpar(fill = NA)
    ))
  }
  rays <- function(count, inner, outer, name) {
    angles <- (seq_len(count) - 1) * 2 * pi / count
    start <- north_rose_polar(angles, inner)
    end <- north_rose_polar(angles, outer)
    add(grid::segmentsGrob(
      x0 = start$x,
      y0 = start$y,
      x1 = end$x,
      y1 = end$y,
      name = name
    ))
  }
  star <- function(count, radii, inner = 0.07, offset = 0, prefix = "point") {
    radii <- rep_len(radii, count)
    for (index in seq_len(count)) {
      direction <- (index - 1) * 2 * pi / count + offset
      for (side in c(-1, 1)) {
        polygon(
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
  }
  radii16 <- rep(c(0.34, 0.19, 0.26, 0.19), 4)
  switch(
    design,
    classic = star(4, 0.33, inner = 0.09),
    eight_point = star(8, c(0.34, 0.24)),
    sixteen_point = star(16, radii16),
    thirty_two_point = star(
      32,
      rep(c(0.34, 0.15, 0.21, 0.15, 0.27, 0.15, 0.21, 0.15), 4),
      inner = 0.06
    ),
    stellar = star(points, c(0.36, 0.29), inner = 0.025),
    concentric = {
      ring(0.36, "outer-ring")
      ring(0.28, "middle-ring")
      ring(0.19, "inner-ring")
      rays(32, 0.34, 0.36, "ring-divisions")
      star(8, c(0.32, 0.24))
    },
    compass = {
      ring(0.36, "case")
      ring(0.32, "dial")
      rays(32, 0.295, 0.32, "dial-ticks")
      rays(4, 0.265, 0.32, "cardinal-ticks")
      star(2, c(0.29, 0.25), inner = 0.055, prefix = "needle")
      add(grid::circleGrob(
        r = l_unit(0.024, "npc"),
        name = "pivot",
        gp = grid::gpar(fill = fill_secondary)
      ))
    },
    nautical = {
      ring(0.365, "outer-ring")
      ring(0.34, "inner-ring")
      rays(32, 0.08, 0.365, "rhumb-lines")
      star(16, radii16 * 0.92, inner = 0.065)
      ring(0.09, "hub-ring")
      polygon(
        c(-0.035, 0, 0.035, 0),
        c(0.355, 0.39, 0.355, 0.365),
        fill,
        "north-mark"
      )
    },
    minimal = {
      rays(4, 0.045, 0.30, "fine-rays")
      polygon(c(-0.07, 0, 0.07), c(0.27, 0.33, 0.27), fill, "north-tip")
      ring(0.018, "center")
    },
    geometric = {
      for (index in seq_len(8)) {
        direction <- (index - 1) * pi / 4
        polygon(
          direction + c(0, -0.17, 0, 0.17),
          c(if (index %% 2) 0.34 else 0.25, 0.14, 0.065, 0.14),
          if (index %% 2) fill else fill_secondary,
          sprintf("diamond-%02d", index)
        )
      }
      polygon((0:3) * pi / 2, rep(0.042, 4), fill_secondary, "center-diamond")
    },
    ornamental = {
      ring(0.36, "outer-ring")
      ring(0.33, "inner-ring")
      for (index in seq_len(8)) {
        direction <- (index - 1) * pi / 4
        position <- north_rose_polar(direction, 0.345)
        add(grid::circleGrob(
          x = position$x,
          y = position$y,
          r = l_unit(0.016, "npc"),
          name = sprintf("bead-%02d", index),
          gp = grid::gpar(fill = fill_secondary)
        ))
        polygon(
          direction + c(0, -0.12, 0, 0.12),
          c(0.32, 0.27, 0.20, 0.27),
          fill_secondary,
          sprintf("petal-%02d", index)
        )
      }
      star(8, c(0.29, 0.21), inner = 0.11)
      ring(0.065, "jewel-ring")
      polygon((0:3) * pi / 2, rep(0.04, 4), fill, "jewel")
    },
    asymmetric = {
      for (index in seq_len(4)) {
        direction <- (index - 1) * pi / 2 + pi / 4
        polygon(
          direction + c(0, -0.22, 0, 0.22),
          c(0.23, 0.12, 0.065, 0.12),
          fill_secondary,
          sprintf("intercardinal-%02d", index)
        )
      }
      star(4, c(0.36, 0.30, 0.32, 0.30), inner = 0.10, prefix = "cardinal")
    }
  )
  shapes
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
#' @param fontsize Positive label font size in points.
#' @param fontface,fontfamily Native grid font face and family. When `NULL`,
#'   `fontfamily` uses `"serif"` for `"nautical"` and `"ornamental"`, and the
#'   device default family for the other presets.
#' @param gp Additional inherited graphical parameters as a [grid::gpar()] or
#'   uniquely named list. Explicit `col` and `lwd` take precedence. Preset fills
#'   use `fill` and `fill_secondary`, not `gp$fill`.
#' @param label_gp Label-specific graphical overrides, including colour and
#'   typography, as a [grid::gpar()] or uniquely named list.
#' @param name Optional grid grob name, distinct from a scene node ID.
#' @param vp Optional outer grid viewport, viewport name or viewport path.
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
#' Supply explicit sizes with [l_place()] or [l_get_element()]: a generic gTree
#' has no automatic child bounding-box measurement. Start with a 120 by 120
#' logical-pixel box and enlarge it for dense or long labels. Text is not
#' automatically shrunk, wrapped or collision-checked, and parent clipping is
#' not overridden. Label centers are at 42.5 percent of the square's side from
#' the center. Leave additional room for long labels and rotation.
#'
#' This is graphical content, not a geographic calculation. It does not inspect
#' a map, calculate true north or apply magnetic declination. Supply an angle
#' computed by [l_north_angle()] for the map and reference location when required.
#' No spatial packages are needed to construct or render the rose.
#'
#' @returns A native grid `gTree`, usable with [grid::grid.draw()], [l_template()],
#'   [l_place()], [l_get_element()] and [l_save()].
#' @seealso [l_north_angle()], [l_template()], [l_unit()], [l_place()], [l_get_element()]
#' @examples
#' rose <- l_north_rose("eight_point", fill = "#197C80")
#' scene <- l_viewport(list(
#'   l_place(rose, right = 12, top = 12, width = 120, height = 120)
#' ), width = 240, height = 180, background = "white")
#' l_render(scene)
#' l_north_rose("stellar", points = 32, labels = FALSE)
#' l_north_rose("classic", labels = c("N", "L", "S", "O"), angle = 15)
#' @export
l_north_rose <- function(
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
  if (
    !is.character(design) ||
      length(design) != 1L ||
      is.na(design) ||
      !design %in% north_rose_designs
  ) {
    l_abort(
      paste(
        "design must be one of:",
        paste(north_rose_designs, collapse = ", ")
      ),
      property = "design"
    )
  }
  angle <- scalar_number(angle, "angle")
  points <- scalar_number(points, "points")
  if (!points %in% c(8, 16, 32)) {
    l_abort("points must be 8, 16 or 32.", property = "points")
  }
  lwd <- scalar_number(lwd, "lwd", positive = TRUE)
  fontsize <- scalar_number(fontsize, "fontsize", positive = TRUE)
  colours <- list(col = col, fill = fill, fill_secondary = fill_secondary)
  for (property in names(colours)) {
    colour <- colours[[property]]
    valid <- length(colour) == 1L &&
      (is.character(colour) || is.numeric(colour) || identical(colour, NA)) &&
      (!is.numeric(colour) ||
        is.na(colour) ||
        (is.finite(colour) && colour <= .Machine$integer.max)) &&
      tryCatch(
        {
          grDevices::col2rgb(colour)
          TRUE
        },
        error = function(error) FALSE
      )
    if (!valid) {
      l_abort(
        paste(property, "must be a scalar grid colour or NA."),
        property = property
      )
    }
  }
  if (isTRUE(labels)) {
    labels <- if (design %in% c("classic", "compass", "minimal")) {
      c("N", "E", "S", "W")
    } else {
      c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
    }
  } else if (identical(labels, FALSE) || is.null(labels)) {
    labels <- character()
  } else if (
    !is.character(labels) ||
      !length(labels) %in% c(4, 8, 16, 32) ||
      anyNA(labels)
  ) {
    l_abort(
      "labels must be TRUE, FALSE, NULL, or 4, 8, 16 or 32 strings.",
      property = "labels"
    )
  }
  fontfamily <- fontfamily %||%
    if (design %in% c("nautical", "ornamental")) "serif" else ""
  if (
    !is.character(fontfamily) || length(fontfamily) != 1L || is.na(fontfamily)
  ) {
    l_abort(
      "fontfamily must be a single string or NULL.",
      property = "fontfamily"
    )
  }
  parameters <- template_gpar(gp, list(col = col, lwd = lwd))
  label_parameters <- template_gpar(list(
    col = col,
    fontsize = fontsize,
    fontface = fontface,
    fontfamily = fontfamily
  ))
  if (!is.null(label_gp)) {
    overrides <- template_gpar(label_gp)
    label_parameters[names(overrides)] <- overrides
  }
  body <- l_template(
    children = north_rose_body(design, points, fill, fill_secondary),
    name = "body",
    gp = if (design == "minimal") grid::gpar(lwd = lwd * 0.65) else NULL
  )
  content <- list(body)
  if (length(labels)) {
    position <- north_rose_polar(
      (seq_along(labels) - 1) * 2 * pi / length(labels),
      0.425
    )
    content <- c(
      content,
      list(l_text(
        labels,
        x = position$x,
        y = position$y,
        name = "labels",
        gp = label_parameters
      ))
    )
  }
  l_template(
    l_template(
      children = content,
      name = "rose",
      vp = grid::viewport(
        width = l_unit(1, "snpc"),
        height = l_unit(1, "snpc"),
        angle = angle,
        name = "north-rose-square"
      )
    ),
    gp = parameters,
    name = name,
    vp = vp
  )
}
