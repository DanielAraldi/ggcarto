#' Create customizable north arrows
#'
#' Build a north arrow from native grid primitives without drawing it. Choose
#' one of twelve designs or supply your own graphical composition.
#'
#' @param design One of `"classic"` (split diamond), `"ornate"` (ornamented
#'   compass), `"minimal"` (modern shaft), `"fleur_de_lis"`, `"bold"`,
#'   `"fine_line"`, `"circle"`, `"double"` (two opposing tips), `"triangle"`
#'   (split triangle), `"cross"`, `"pennant"` or `"art_deco"`.
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
  polygon <- function(x, y, colour = fill, name) {
    grid::polygonGrob(x, y, gp = grid::gpar(fill = colour), name = name)
  }
  line <- function(x, y, name) grid::linesGrob(x, y, name = name)
  circle <- function(radius, name, y = 0.44) {
    grid::circleGrob(
      x = 0.5,
      y = y,
      r = gc_unit(radius, "snpc"),
      gp = grid::gpar(fill = NA),
      name = name
    )
  }
  diamond <- function(
    top = 0.78,
    bottom = 0.12,
    middle = 0.34,
    left = 0.23,
    right = 0.77
  ) {
    list(
      polygon(c(0.5, left, 0.5), c(top, middle, bottom), name = "west-half"),
      polygon(
        c(0.5, right, 0.5),
        c(top, middle, bottom),
        fill_secondary,
        name = "east-half"
      )
    )
  }
  switch(
    design,
    classic = diamond(),
    ornate = c(
      list(
        circle(0.26, "compass-ring"),
        polygon(
          c(0.12, 0.5, 0.88, 0.5),
          c(0.44, 0.53, 0.44, 0.35),
          fill_secondary,
          "east-west"
        ),
        line(c(0.22, 0.78), c(0.61, 0.27), "diagonal-up"),
        line(c(0.22, 0.78), c(0.27, 0.61), "diagonal-down")
      ),
      diamond(
        top = 0.8,
        bottom = 0.08,
        middle = 0.44,
        left = 0.34,
        right = 0.66
      ),
      list(polygon(
        c(0.44, 0.5, 0.56, 0.5),
        c(0.44, 0.48, 0.44, 0.4),
        fill_secondary,
        "center-jewel"
      ))
    ),
    minimal = list(grid::segmentsGrob(
      x0 = 0.5,
      x1 = 0.5,
      y0 = 0.14,
      y1 = 0.72,
      arrow = arrow,
      gp = grid::gpar(fill = fill),
      name = "shaft"
    )),
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
        gp = grid::gpar(fill = fill_secondary),
        name = "east-petal"
      ),
      polygon(
        c(0.5, 0.38, 0.5, 0.62),
        c(0.8, 0.58, 0.28, 0.58),
        name = "central-petal"
      ),
      polygon(
        c(0.46, 0.36, 0.5, 0.64, 0.54),
        c(0.3, 0.14, 0.2, 0.14, 0.3),
        name = "foot"
      ),
      polygon(
        c(0.33, 0.67, 0.67, 0.33),
        c(0.3, 0.3, 0.35, 0.35),
        fill_secondary,
        "band"
      )
    ),
    bold = list(polygon(
      c(0.5, 0.9, 0.64, 0.64, 0.36, 0.36, 0.1),
      c(0.8, 0.48, 0.48, 0.12, 0.12, 0.48, 0.48),
      name = "solid-arrow"
    )),
    fine_line = list(
      line(c(0.5, 0.5), c(0.12, 0.78), "shaft"),
      line(c(0.28, 0.5, 0.72), c(0.61, 0.78, 0.61), "open-tip"),
      line(c(0.34, 0.66), c(0.18, 0.18), "base-tick"),
      line(c(0.4, 0.6), c(0.3, 0.3), "mid-tick")
    ),
    circle = c(
      list(circle(0.37, "outer-ring")),
      diamond(top = 0.76, bottom = 0.2, middle = 0.33, left = 0.3, right = 0.7)
    ),
    double = list(
      line(c(0.5, 0.5), c(0.22, 0.66), "shaft"),
      polygon(c(0.25, 0.5, 0.75), c(0.54, 0.8, 0.54), name = "north-tip"),
      polygon(
        c(0.25, 0.5, 0.75),
        c(0.34, 0.08, 0.34),
        fill_secondary,
        "south-tip"
      )
    ),
    triangle = list(
      polygon(c(0.15, 0.5, 0.5), c(0.16, 0.8, 0.16), name = "west-half"),
      polygon(
        c(0.5, 0.5, 0.85),
        c(0.16, 0.8, 0.16),
        fill_secondary,
        "east-half"
      )
    ),
    cross = list(
      polygon(
        c(0.12, 0.5, 0.88, 0.5),
        c(0.4, 0.49, 0.4, 0.31),
        fill_secondary,
        "east-west"
      ),
      polygon(
        c(0.5, 0.37, 0.5, 0.63),
        c(0.8, 0.4, 0.08, 0.4),
        name = "north-south"
      ),
      line(c(0.5, 0.5), c(0.08, 0.8), "meridian")
    ),
    pennant = list(
      line(c(0.5, 0.5), c(0.12, 0.8), "mast"),
      polygon(c(0.5, 0.87, 0.5), c(0.8, 0.8, 0.5), name = "flag"),
      line(c(0.34, 0.66), c(0.12, 0.12), "base")
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
      line(c(0.2, 0.2, 0.1), c(0.18, 0.39, 0.39), "west-step"),
      line(c(0.8, 0.8, 0.9), c(0.18, 0.39, 0.39), "east-step"),
      line(c(0.26, 0.74), c(0.08, 0.08), "base")
    )
  )
}
