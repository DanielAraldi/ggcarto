#' Create customizable north arrows
#'
#' Build a north arrow from native grid primitives without drawing it. Choose
#' one of twelve designs or supply your own graphical composition.
#'
#' @param design One of `"classic"` (counterchanged split needle), `"ornate"`
#'   (ringed compass), `"minimal"` (fletched shaft), `"fleur_de_lis"`, `"bold"`
#'   (swallowtail block arrow), `"fine_line"` (graduated technical arrow),
#'   `"circle"` (needle in a ticked ring), `"double"` (two opposing tips),
#'   `"triangle"` (split triangle), `"cross"`, `"pennant"` or `"art_deco"`.
#'   Every design except `"fine_line"` uses both `fill` and `fill_secondary`.
#' @param angle Finite rotation in degrees counterclockwise. Zero points up.
#'   Rotates the symbol and label together, inside `vp`.
#' @param label A character string; `NULL` or `""` omits the label.
#' @param col Outline and default label colour.
#' @param fill Primary fill colour. Defaults to `col`; use `NA` for no fill.
#' @param fill_secondary Secondary fill colour for split or inset shapes.
#' @param lwd Positive line width in grid units (multiples of 1/96 inch).
#' @param fontsize Positive label size in points, independent of layout size.
#' @param fontface,fontfamily Label font face and family, as in [gc_text()].
#' @param label_x,label_y Label position as finite numeric `npc` coordinates or
#'   scalar grid units. The label is centered at this position before rotation.
#' @param arrow Native [grid::arrow()] specification for the `"minimal"`
#'   design; `NULL` removes its tip. Controls tip length, angle, ends and type.
#' @param children Optional list or [grid::gList()] of grobs replacing the
#'   preset body. The label, graphical parameters and rotation still apply.
#' @param gp Additional group graphical parameters, as in [gc_template()].
#'   Explicit `col` and `lwd` take precedence. Preset fills are controlled by
#'   `fill` and `fill_secondary`, not `gp$fill`. Custom children retain their
#'   own explicit graphical parameters.
#' @param label_gp Optional graphical parameters overriding label typography
#'   and colour, as a [grid::gpar()] object or uniquely named list.
#' @param name Optional grid grob name.
#' @param vp Optional outer grid viewport, viewport name or viewport path.
#'
#' @details
#' The return value is graphical content, not a scene node. Supply explicit
#' dimensions with [gc_place()], or wrap it with
#' `gc_get_element(arrow, "north_arrow", width = 40, height = 68)`.
#' Preset geometry uses normalized parent coordinates. A box near 40 by 68
#' logical pixels is a useful starting point; changing its proportions changes
#' the symbol proportions. Circles use grid's isotropic `snpc` radius.
#' Text size, line width and the minimal design's tip length remain physical
#' sizes. Leave enough space for long labels or rotation; the constructor does
#' not measure bounds, shrink content or override parent clipping.
#'
#' This function does not infer a CRS or calculate true or magnetic north.
#' Supply `angle` from [gc_north_angle()] for your map and reference location when
#' geographic orientation is needed. Additional rotation in `vp` is cumulative.
#' Custom children should point up before rotation and use the same local
#' coordinate system. They can contain arbitrary grid geometry, including
#' their own viewports and physical units. Construction does not open a device
#' or modify the input grobs or graphical parameters.
#'
#' @returns A native grid `gTree`, suitable for [grid::grid.draw()],
#'   [gc_template()], [gc_get_element()], [gc_place()] and [gc_save()].
#' @seealso [gc_north_angle()], [gc_template()], [gc_text()], [gc_unit()], [grid::arrow()]
#' @examples
#' north <- gc_north_arrow("classic", fill = "#197C80", angle = 12)
#' scene <- gc_viewport(list(
#'   gc_place(north, right = 12, top = 12, width = 40, height = 68)
#' ), width = 200, height = 140)
#' gc_render(scene)
#' gc_north_arrow("minimal", arrow = grid::arrow(
#'   length = gc_unit(2, "mm"), type = "open"
#' ), label_gp = list(col = "#197C80"))
#' custom <- gc_north_arrow(children = list(grid::polygonGrob(
#'   x = c(0.2, 0.5, 0.8, 0.5), y = c(0.15, 0.78, 0.15, 0.3),
#'   gp = grid::gpar(fill = "#197C80")
#' )))
#' grid::is.grob(custom)
#' @export
gc_north_arrow <- function(
  design = c(
    "classic",
    "ornate",
    "minimal",
    "fleur_de_lis",
    "bold",
    "fine_line",
    "circle",
    "double",
    "triangle",
    "cross",
    "pennant",
    "art_deco"
  ),
  angle = 0,
  label = "N",
  col = "#203C43",
  fill = col,
  fill_secondary = "white",
  lwd = 1.5,
  fontsize = 11,
  fontface = "bold",
  fontfamily = "",
  label_x = 0.5,
  label_y = 0.92,
  arrow = grid::arrow(length = gc_unit(3, "mm"), type = "closed"),
  children = NULL,
  gp = NULL,
  label_gp = NULL,
  name = NULL,
  vp = NULL
) {
  designs <- eval(formals(gc_north_arrow)$design)
  if (missing(design)) {
    design <- designs[[1]]
  }
  if (
    !is.character(design) ||
      length(design) != 1L ||
      is.na(design) ||
      !design %in% designs
  ) {
    gc_abort(
      paste0("design must be one of: ", paste(designs, collapse = ", "), "."),
      property = "design"
    )
  }
  angle <- scalar_number(angle, "angle")
  lwd <- scalar_number(lwd, "lwd", positive = TRUE)
  fontsize <- scalar_number(fontsize, "fontsize", positive = TRUE)
  if (
    !is.null(label) &&
      (!is.character(label) || length(label) != 1L || is.na(label))
  ) {
    gc_abort(
      "label must be NULL or a single character string.",
      property = "label"
    )
  }
  for (property in c("label_x", "label_y")) {
    value <- get(property)
    if (grid::is.unit(value)) {
      if (length(value) != 1L || !is.finite(as.numeric(value))) {
        gc_abort(
          "Label coordinates must be finite scalar grid units.",
          property = property
        )
      }
    } else {
      scalar_number(value, property)
    }
  }
  for (property in c("col", "fill", "fill_secondary")) {
    value <- get(property)
    valid <- length(value) == 1L &&
      (is.character(value) || is.numeric(value) || identical(value, NA))
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
      gc_abort(
        paste0(property, " must be a single grid colour or NA."),
        property = property
      )
    }
  }
  if (!is.null(arrow) && !inherits(arrow, "arrow")) {
    gc_abort(
      "arrow must be NULL or a grid::arrow() specification.",
      property = "arrow"
    )
  }
  group_gp <- template_gpar(gp, list(col = col, lwd = lwd))
  text_gp <- template_gpar(
    list(fontsize = fontsize, fontface = fontface, fontfamily = fontfamily),
    label_gp %||% list()
  )
  if (is.null(children)) {
    children <- north_arrow_body(design, fill, fill_secondary, arrow)
  }
  body <- gc_template(children = children, name = "body")
  parts <- list(body)
  if (!is.null(label) && nzchar(label)) {
    parts <- c(
      parts,
      list(gc_text(
        label,
        x = label_x,
        y = label_y,
        gp = text_gp,
        name = "label"
      ))
    )
  }
  gc_template(
    gc_template(
      children = parts,
      vp = grid::viewport(angle = angle),
      name = "symbol"
    ),
    gp = group_gp,
    vp = vp,
    name = name
  )
}

north_arrow_body <- function(design, fill, fill_secondary, arrow) {
  # Geometry is designed for the recommended 40 by 68 box: radial lengths are
  # in snpc-like units and vertical offsets are scaled by this aspect ratio.
  aspect <- 40 / 68
  centre <- c(0.5, 0.44)
  polygon <- function(x, y, colour = fill, name) {
    grid::polygonGrob(x, y, gp = grid::gpar(fill = colour), name = name)
  }
  line <- function(x, y, name) grid::linesGrob(x, y, name = name)
  circle <- function(radius, name, y = centre[[2]], colour = NA) {
    grid::circleGrob(
      x = 0.5,
      y = y,
      r = gc_unit(radius, "snpc"),
      gp = grid::gpar(fill = colour),
      name = name
    )
  }
  point <- function(radius, degrees) {
    theta <- degrees * pi / 180
    c(
      centre[[1]] + radius * sin(theta),
      centre[[2]] + radius * cos(theta) * aspect
    )
  }
  # A compass point split along its axis. Degrees run clockwise from north;
  # opposite points therefore receive counterchanged fills.
  kite <- function(
    degrees,
    length,
    width,
    name,
    colours = c(fill, fill_secondary)
  ) {
    tip <- point(length, degrees)
    left <- point(width, degrees - 90)
    right <- point(width, degrees + 90)
    list(
      polygon(
        c(tip[[1]], left[[1]], centre[[1]]),
        c(tip[[2]], left[[2]], centre[[2]]),
        colours[[1]],
        paste0(name, "-left")
      ),
      polygon(
        c(tip[[1]], right[[1]], centre[[1]]),
        c(tip[[2]], right[[2]], centre[[2]]),
        colours[[2]],
        paste0(name, "-right")
      )
    )
  }
  ticks <- function(degrees, inner, outer, name) {
    from <- vapply(degrees, function(angle) point(inner, angle), numeric(2))
    to <- vapply(degrees, function(angle) point(outer, angle), numeric(2))
    grid::segmentsGrob(from[1, ], from[2, ], to[1, ], to[2, ], name = name)
  }
  hub <- function(radius = 0.06) {
    circle(radius, "hub", colour = fill_secondary)
  }
  switch(
    design,
    classic = c(
      kite(0, 0.66, 0.26, "north"),
      kite(180, 0.6, 0.26, "south"),
      list(hub())
    ),
    ornate = c(
      list(
        circle(0.46, "outer-ring"),
        circle(0.38, "inner-ring"),
        ticks(seq(22.5, 337.5, by = 45), 0.38, 0.46, "ring-ticks")
      ),
      kite(45, 0.4, 0.07, "north-east", c(fill_secondary, fill)),
      kite(135, 0.4, 0.07, "south-east", c(fill_secondary, fill)),
      kite(225, 0.4, 0.07, "south-west", c(fill_secondary, fill)),
      kite(315, 0.4, 0.07, "north-west", c(fill_secondary, fill)),
      kite(90, 0.48, 0.1, "east"),
      kite(270, 0.48, 0.1, "west"),
      kite(0, 0.66, 0.13, "north"),
      kite(180, 0.6, 0.13, "south"),
      list(hub(0.05))
    ),
    minimal = list(
      line(c(0.38, 0.5, 0.62), c(0.06, 0.16, 0.06), "lower-fletching"),
      line(c(0.38, 0.5, 0.62), c(0.12, 0.22, 0.12), "upper-fletching"),
      grid::segmentsGrob(
        x0 = 0.5,
        x1 = 0.5,
        y0 = 0.06,
        y1 = 0.76,
        arrow = arrow,
        gp = grid::gpar(fill = fill),
        name = "shaft"
      ),
      circle(0.06, "hub", y = 0.48, colour = fill_secondary)
    ),
    fleur_de_lis = list(
      grid::xsplineGrob(
        x = c(0.48, 0.38, 0.2, 0.12, 0.23, 0.37, 0.44),
        y = c(0.32, 0.62, 0.66, 0.49, 0.42, 0.54, 0.3),
        shape = c(0, 1, 1, 1, 1, 1, 0),
        open = FALSE,
        gp = grid::gpar(fill = fill),
        name = "west-petal"
      ),
      grid::xsplineGrob(
        x = 1 - c(0.48, 0.38, 0.2, 0.12, 0.23, 0.37, 0.44),
        y = c(0.32, 0.62, 0.66, 0.49, 0.42, 0.54, 0.3),
        shape = c(0, 1, 1, 1, 1, 1, 0),
        open = FALSE,
        gp = grid::gpar(fill = fill),
        name = "east-petal"
      ),
      polygon(
        c(0.5, 0.37, 0.5),
        c(0.82, 0.58, 0.28),
        name = "central-petal-west"
      ),
      polygon(
        c(0.5, 0.63, 0.5),
        c(0.82, 0.58, 0.28),
        fill_secondary,
        "central-petal-east"
      ),
      polygon(
        c(0.46, 0.34, 0.5, 0.5),
        c(0.3, 0.1, 0.18, 0.3),
        fill_secondary,
        "foot-west"
      ),
      polygon(
        c(0.54, 0.66, 0.5, 0.5),
        c(0.3, 0.1, 0.18, 0.3),
        name = "foot-east"
      ),
      polygon(
        c(0.31, 0.69, 0.69, 0.31),
        c(0.29, 0.29, 0.36, 0.36),
        fill_secondary,
        "band"
      ),
      polygon(
        c(0.5, 0.45, 0.5, 0.55),
        c(0.355, 0.325, 0.295, 0.325),
        name = "band-jewel"
      )
    ),
    bold = list(
      polygon(
        c(0.5, 0.9, 0.66, 0.66, 0.5, 0.34, 0.34, 0.1),
        c(0.84, 0.5, 0.5, 0.08, 0.18, 0.08, 0.5, 0.5),
        name = "solid-arrow"
      ),
      polygon(
        c(0.5, 0.76, 0.66, 0.5, 0.34, 0.24),
        c(0.74, 0.54, 0.54, 0.66, 0.54, 0.54),
        fill_secondary,
        "chevron"
      ),
      polygon(
        c(0.47, 0.53, 0.53, 0.47),
        c(0.26, 0.26, 0.6, 0.6),
        fill_secondary,
        "stripe"
      )
    ),
    fine_line = list(
      line(c(0.5, 0.5), c(0.04, 0.82), "shaft"),
      line(c(0.28, 0.5, 0.72), c(0.6, 0.82, 0.6), "open-tip"),
      line(c(0.36, 0.5, 0.64), c(0.58, 0.72, 0.58), "inner-tip"),
      circle(0.14, "reference-ring"),
      grid::segmentsGrob(
        x0 = 0.5 - c(0.06, 0.1, 0.14, 0.18),
        x1 = 0.5 + c(0.06, 0.1, 0.14, 0.18),
        y0 = c(0.28, 0.21, 0.14, 0.07),
        y1 = c(0.28, 0.21, 0.14, 0.07),
        name = "graduated-ticks"
      )
    ),
    circle = c(
      list(
        circle(0.44, "outer-ring"),
        circle(0.36, "inner-ring"),
        ticks(seq(0, 315, by = 45), 0.36, 0.44, "ring-ticks")
      ),
      kite(0, 0.66, 0.14, "north"),
      kite(180, 0.34, 0.14, "south"),
      list(hub(0.05))
    ),
    double = list(
      line(c(0.5, 0.5), c(0.24, 0.64), "shaft"),
      line(c(0.3, 0.7), c(0.44, 0.44), "crossbar"),
      polygon(
        c(0.5, 0.76, 0.5, 0.24),
        c(0.84, 0.56, 0.62, 0.56),
        name = "north-tip"
      ),
      polygon(
        c(0.5, 0.76, 0.5, 0.24),
        c(0.04, 0.32, 0.26, 0.32),
        fill_secondary,
        "south-tip"
      ),
      circle(0.12, "hub-ring", colour = fill_secondary),
      circle(0.04, "hub-dot", colour = fill)
    ),
    triangle = list(
      polygon(c(0.12, 0.5, 0.5), c(0.14, 0.84, 0.14), name = "west-half"),
      polygon(
        c(0.5, 0.5, 0.88),
        c(0.14, 0.84, 0.14),
        fill_secondary,
        "east-half"
      ),
      polygon(
        c(0.3, 0.5, 0.5),
        c(0.2, 0.58, 0.2),
        fill_secondary,
        "inner-west"
      ),
      polygon(c(0.5, 0.5, 0.7), c(0.2, 0.58, 0.2), name = "inner-east"),
      polygon(
        c(0.12, 0.88, 0.88, 0.12),
        c(0.04, 0.04, 0.09, 0.09),
        name = "base"
      )
    ),
    cross = c(
      kite(90, 0.44, 0.1, "east", c(fill_secondary, fill)),
      kite(270, 0.44, 0.1, "west", c(fill_secondary, fill)),
      list(circle(0.22, "ring")),
      kite(0, 0.66, 0.12, "north"),
      kite(180, 0.6, 0.12, "south"),
      list(hub(0.05))
    ),
    pennant = list(
      line(c(0.5, 0.5), c(0.12, 0.8), "mast"),
      polygon(c(0.5, 0.87, 0.5), c(0.8, 0.8, 0.5), name = "flag"),
      polygon(
        c(0.53, 0.73, 0.53),
        c(0.76, 0.76, 0.6),
        fill_secondary,
        "flag-stripe"
      ),
      circle(0.04, "knot", y = 0.5, colour = fill_secondary),
      polygon(c(0.3, 0.7, 0.6, 0.4), c(0.06, 0.06, 0.12, 0.12), name = "plinth")
    ),
    art_deco = list(
      polygon(
        c(0.5, 0.16, 0.34, 0.34, 0.66, 0.66, 0.84),
        c(0.8, 0.5, 0.5, 0.14, 0.14, 0.5, 0.5),
        name = "stepped-arrow"
      ),
      polygon(
        c(0.5, 0.36, 0.5, 0.64),
        c(0.69, 0.51, 0.37, 0.51),
        fill_secondary,
        "inset-diamond"
      ),
      polygon(
        c(0.5, 0.45, 0.5, 0.55),
        c(0.6, 0.51, 0.44, 0.51),
        name = "inset-core"
      ),
      polygon(
        c(0.39, 0.42, 0.42, 0.39),
        c(0.18, 0.18, 0.4, 0.4),
        fill_secondary,
        "west-groove"
      ),
      polygon(
        c(0.58, 0.61, 0.61, 0.58),
        c(0.18, 0.18, 0.4, 0.4),
        fill_secondary,
        "east-groove"
      ),
      line(
        c(0.26, 0.2, 0.2, 0.14, 0.14, 0.08),
        c(0.14, 0.14, 0.28, 0.28, 0.42, 0.42),
        "west-steps"
      ),
      line(
        c(0.74, 0.8, 0.8, 0.86, 0.86, 0.92),
        c(0.14, 0.14, 0.28, 0.28, 0.42, 0.42),
        "east-steps"
      ),
      grid::segmentsGrob(
        x0 = c(0.26, 0.18),
        x1 = c(0.74, 0.82),
        y0 = c(0.08, 0.04),
        y1 = c(0.08, 0.04),
        name = "base"
      )
    )
  )
}
