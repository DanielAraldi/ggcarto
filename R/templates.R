template_gpar <- function(gp = NULL, overrides = list()) {
  for (parameters in list(gp, overrides)) {
    if (is.null(parameters)) {
      next
    }
    if (
      !is.list(parameters) ||
        (length(parameters) &&
          (is.null(names(parameters)) ||
            anyNA(names(parameters)) ||
            any(!nzchar(names(parameters))) ||
            anyDuplicated(names(parameters))))
    ) {
      gc_abort(
        "Graphical parameters must be a gpar or a uniquely named list.",
        property = "gp"
      )
    }
  }
  if (!inherits(gp, "gpar")) {
    gp <- do.call(grid::gpar, gp %||% list())
  }
  if (!inherits(overrides, "gpar")) {
    overrides <- do.call(grid::gpar, overrides)
  }
  gp[names(overrides)] <- overrides
  gp
}

#' Create native grid units
#'
#' Construct a unit object by delegating directly to [grid::unit()]. The
#' arguments, validation, vectorization and return value follow grid's native
#' behavior. No ggcarto-specific units or conversions are added.
#'
#' @param x Numeric vector of values interpreted in `units`.
#' @param units Character vector of grid unit names, recycled according to
#'   [grid::unit()] rules. Examples include `"npc"`, `"native"`, `"mm"`, `"cm"`,
#'   `"inches"`, `"points"`, `"lines"`, `"char"`, `"null"`, `"strwidth"`,
#'   `"strheight"`, `"grobwidth"` and `"grobheight"`. See [grid::unit()] for the
#'   complete list and supported aliases.
#' @param data Optional auxiliary data required by some units, passed unchanged
#'   to [grid::unit()]. Supply text or an expression for string-based units, a
#'   grob or [grid::gPath()] for grob-based units, or a list for multiple values,
#'   following grid's recycling rules. Defaults to `NULL`.
#'
#' @details
#' This function creates a unit specification, not a converted numeric length.
#' Relative units and font or grob measurements are evaluated by grid in the
#' applicable viewport and graphics-device context. Use [grid::convertUnit()],
#' [grid::convertWidth()] or [grid::convertHeight()] for explicit conversions.
#' Construction does not draw or open a graphics device.
#'
#' Unit objects work with [gc_rect()], [gc_text()], native grid grobs and
#' viewports, including the children of [gc_template()]. Native grid arithmetic,
#' indexing and [grid::unit.c()] remain available. With `"npc"`, `(0, 0)` is
#' the bottom-left corner and `(1, 1)` is the top-right corner of the viewport.
#' The `"null"` unit is meaningful for relative sizing in [grid::grid.layout()].
#'
#' These are not ggcarto layout lengths. Use [gc_length()] or layout strings for
#' [gc_place()] and [gc_viewport()] constraints. For example, `gc_unit(0.5, "npc")`
#' describes half a native grid viewport, whereas `gc_length("50%")` describes
#' half the relevant ggcarto parent dimension. CSS-like `"px"`, `"%"`, `"vw"`,
#' `"auto"` and `"clamp(...)"` are not grid unit names. Invalid input raises
#' grid's own errors, not ggcarto-specific conditions.
#'
#' @returns A native grid `unit` object, identical to the result of
#'   `grid::unit(x, units, data = data)`. It is not an `gc_length` or a grob.
#' @seealso [grid::unit()], [grid::convertUnit()], [grid::unit.c()],
#'   [gc_length()], [gc_rect()], [gc_text()], [gc_template()]
#' @examples
#' spacing <- gc_unit(5, "mm")
#' grid::is.unit(spacing)
#' gc_unit(c(0.25, 0.75), "npc")
#' gc_unit(c(1, 5), c("cm", "mm"))
#' gc_unit(1, "strwidth", data = "Survey area")
#' outline <- gc_rect(
#'   width = gc_unit(20, "mm"), height = gc_unit(10, "mm"),
#'   fill = NA, col = "black"
#' )
#' grid::is.grob(outline)
#' gc_unit(1, "grobwidth", data = outline)
#' gc_unit(1, "npc") - spacing
#' @export
gc_unit <- function(x, units, data = NULL) {
  grid::unit(x, units, data = data)
}

#' Compose reusable graphical templates
#'
#' Combine native grid grobs into a single graphical object without drawing it.
#' Templates can contain rectangles, text, nested templates and other grid
#' primitives, and can be placed as one element in an ggcarto scene.
#'
#' @param ... Child grobs in drawing order. Later children are drawn on top of
#'   earlier children. These arguments are content, not graphical parameters.
#' @param children Optional list or [grid::gList()] of grobs appended after the
#'   children supplied in `...`. An empty template is allowed. Each supplied
#'   child must be a grob; strings, `NULL`, ggplots and ggcarto nodes are not grobs.
#' @param name Optional character name for the grid grob. This is distinct from
#'   a scene node's `id` in [gc_place()].
#' @param gp `NULL`, a [grid::gpar()] object, or a uniquely named list of grid
#'   graphical parameters. Unspecified child properties inherit from this group.
#' @param vp Optional grid viewport, viewport name or viewport path. A viewport
#'   controls local coordinates, dimensions, justification, scales and clipping.
#' @param childrenvp Optional native grid viewport tree for the children, as
#'   accepted by [grid::grobTree()].
#' @param cl Optional character vector of additional classes for the gTree.
#'   Intended for grid extensions; normally leave this as `NULL`.
#'
#' @details
#' Templates preserve native grid drawing order and inheritance. Explicit child
#' settings such as `col` take precedence over inherited settings; parameters
#' such as `alpha`, `cex` and `lex` can be multiplicative across the hierarchy.
#' Neither the input grobs nor their graphical parameters are modified.
#'
#' Internal coordinates follow grid conventions: with `npc` units, `(0, 0)` is
#' the bottom-left corner and `(1, 1)` is the top-right corner. Use [gc_place()]
#' for outer positioning in ggcarto's top-left coordinate system. Templates have
#' no automatic child bounding-box measurement: supply explicit layout sizes
#' or opposing insets. Enlarging the box does not scale fixed font sizes.
#'
#' A template is graphical content, not a layout container. Use [gc_viewport()]
#' when children need independent constraints, margins or collision policies.
#' Wrap a template in [gc_get_element()] to attach semantic metadata and styles.
#' These constructors do not draw, open devices or register global state.
#'
#' @returns A native grid `gTree` inheriting from `grob`. It can be passed to
#'   [grid::grid.draw()], [gc_place()], [gc_get_element()] or [gc_save()].
#' @seealso [gc_rect()], [gc_text()], [grid::grobTree()], [grid::viewport()]
#' @examples
#' badge <- gc_template(
#'   gc_rect(fill = "white", col = "#203C43"),
#'   gc_text("N", fontsize = 12, fontface = "bold"),
#'   gp = list(col = "#203C43")
#' )
#' scene <- gc_viewport(list(
#'   gc_place(badge, right = 12, top = 12, width = 40, height = 50)
#' ), width = 240, height = 120)
#' gc_render(scene, width = 240, height = 120)
#' nested <- gc_template(children = list(badge, gc_text("Survey", y = 0.1)))
#' grid::is.grob(nested)
#' @export
gc_template <- function(
  ...,
  children = NULL,
  name = NULL,
  gp = NULL,
  vp = NULL,
  childrenvp = NULL,
  cl = NULL
) {
  if (!is.null(children) && !is.list(children)) {
    gc_abort("children must be a list of grobs.", property = "children")
  }
  children <- c(list(...), children)
  if (!all(vapply(children, grid::is.grob, logical(1)))) {
    gc_abort(
      "Template children must be grobs.",
      "unsupported_source",
      property = "children"
    )
  }
  if (!is.null(gp)) {
    gp <- template_gpar(gp)
  }
  do.call(
    grid::grobTree,
    c(
      unname(children),
      list(name = name, gp = gp, vp = vp, childrenvp = childrenvp, cl = cl)
    )
  )
}

#' Create rectangles with native grid geometry
#'
#' Construct one or more rectangles as a grob, using grid units and graphical
#' parameters. The default rectangle is centered and fills its current grid
#' viewport. Construction does not draw or open a graphics device.
#'
#' @param x,y Numeric coordinates in `default.units`, or [gc_unit()] / [grid::unit()]
#'   objects.
#'   Vectors are supported according to [grid::rectGrob()] recycling rules.
#' @param width,height Rectangle dimensions as numbers in `default.units` or
#'   grid unit objects. These are internal geometry, not ggcarto layout lengths.
#' @param just Character or numeric justification relative to `(x, y)`, such
#'   as `"centre"` or `c("left", "bottom")`.
#' @param hjust,vjust Optional numeric horizontal and vertical justification,
#'   overriding the corresponding component of `just`. Zero means left/bottom,
#'   0.5 means center and one means right/top.
#' @param default.units Grid unit used for numeric geometry, by default `"npc"`.
#'   Existing unit objects, including the defaults, retain their own units.
#' @inheritParams gc_template
#' @param ... Uniquely named [grid::gpar()] parameters overriding `gp`. Common
#'   parameters are `fill`, `col`, `alpha`, `lwd`, `lty`, `lineend`, `linejoin`,
#'   `linemitre` and `lex`. Values are validated by grid, not by a CSS parser.
#'
#' @details
#' Use `fill = NA` for an unfilled rectangle and `col = NA` for no outline.
#' Styles follow grid inheritance and device capabilities; an unspecified fill
#' does not guarantee a white background. The supplied `gp` is not modified.
#'
#' Numbers default to normalized parent coordinates, with the origin at the
#' bottom-left. For example, `width = 0.5` occupies half the current viewport;
#' `width = gc_unit(20, "mm")` remains a physical 20 mm. Layout strings such
#' as `"50%"` or `"8px"` belong to [gc_place()], not this constructor.
#' Using `"native"` coordinates requires appropriate grid viewport scales and
#' does not automatically connect the rectangle to a map's projection.
#'
#' The grob has no padding, rounded corners or independent collision policy.
#' Use a containing scene node for layout properties. Several vectorized
#' rectangles remain one grob and one layout element.
#'
#' @returns A native grid `rect` grob, suitable for [gc_template()] or direct
#'   use with [gc_place()] and [grid::grid.draw()].
#' @seealso [gc_unit()], [gc_text()], [gc_template()], [grid::rectGrob()], [grid::gpar()]
#' @examples
#' swatches <- gc_rect(
#'   x = c(0.25, 0.75), width = 0.4, height = 0.7,
#'   fill = c("#95CEC0", "#194E70"), col = NA
#' )
#' scene <- gc_viewport(list(
#'   gc_place(swatches, left = 10, top = 10, width = 180, height = 60)
#' ))
#' gc_render(scene, width = 200, height = 80)
#' outline <- gc_rect(
#'   width = gc_unit(20, "mm"),
#'   height = gc_unit(10, "mm"), fill = NA, col = "black", lty = "dashed"
#' )
#' grid::is.grob(outline)
#' @export
gc_rect <- function(
  x = gc_unit(0.5, "npc"),
  y = gc_unit(0.5, "npc"),
  width = gc_unit(1, "npc"),
  height = gc_unit(1, "npc"),
  just = "centre",
  hjust = NULL,
  vjust = NULL,
  default.units = "npc",
  name = NULL,
  gp = NULL,
  vp = NULL,
  ...
) {
  grid::rectGrob(
    x = x,
    y = y,
    width = width,
    height = height,
    just = just,
    hjust = hjust,
    vjust = vjust,
    default.units = default.units,
    name = name,
    gp = template_gpar(gp, list(...)),
    vp = vp
  )
}

#' Create text annotations with native grid typography
#'
#' Construct a text grob with native grid alignment, rotation, mathematical
#' expressions and vectorized labels. No drawing or device creation occurs
#' until the grob is rendered.
#'
#' @param label Text to display, including character vectors and mathematical
#'   expressions such as `expression(alpha^2)`. Use `"\n"` for explicit lines.
#' @inheritParams gc_rect
#' @param rot Text rotation in degrees; for example, 90 gives vertical text.
#' @param check.overlap Logical; if `TRUE`, grid may omit overlapping labels
#'   within this grob at draw time, in input order. It does not reposition labels
#'   or detect collisions with other ggcarto nodes.
#' @param ... Uniquely named [grid::gpar()] parameters overriding `gp`, including
#'   `col`, `fontsize`, `fontfamily`, `fontface`, `lineheight`, `alpha` and `cex`.
#'   Use grid names, not [gc_style()] names such as `color` or `font_size`.
#'
#' @details
#' `fontsize` is numeric, in points. `fontface` accepts values such as `"plain"`,
#' `"bold"`, `"italic"` and `"bold.italic"`. A `fontface` shortcut overrides a
#' `font` already stored in `gp`, and vice versa. Font availability and glyph
#' metrics depend on the output device. Graphical parameters are not mutated.
#'
#' `lineheight` controls relative spacing for explicit line breaks. There is
#' no automatic line wrapping, Markdown or HTML interpretation. Assigning a
#' layout width does not turn a long label into a paragraph.
#'
#' Internal coordinates use grid's bottom-left origin and default `npc` units.
#' Internal `just` and the outer [gc_place()] `anchor` are independent. For a
#' fixed font size, place the grob directly. For responsive typography, wrap it
#' in [gc_get_element()] and use `style = list(font_size = "clamp(...)")`;
#' responsive length strings cannot be passed as grid `fontsize` values.
#'
#' @returns A native grid `text` grob. [gc_measure()] can inspect its intrinsic
#'   size, and [gc_template()] can combine it with other graphical primitives.
#' @seealso [gc_unit()], [grid::textGrob()], [grid::gpar()], [gc_get_element()], [gc_style()]
#' @examples
#' note <- gc_text("Source: survey\nLocal coordinates",
#'   x = 0, just = "left",
#'   gp = list(fontsize = 9, col = "#50666C"), lineheight = 1.3
#' )
#' gc_measure(note)
#' labels <- gc_text(c("West", "East"),
#'   x = c(0.2, 0.8),
#'   fontsize = 10, fontface = c("plain", "bold")
#' )
#' gc_render(gc_viewport(list(
#'   gc_place(labels, left = 0, top = 0, width = 240, height = 60)
#' )), width = 240, height = 60)
#' formula <- gc_text(expression(alpha^2 + beta^2), fontsize = 12)
#' grid::is.grob(formula)
#' @export
gc_text <- function(
  label,
  x = gc_unit(0.5, "npc"),
  y = gc_unit(0.5, "npc"),
  just = "centre",
  hjust = NULL,
  vjust = NULL,
  rot = 0,
  check.overlap = FALSE,
  default.units = "npc",
  name = NULL,
  gp = NULL,
  vp = NULL,
  ...
) {
  grid::textGrob(
    label = label,
    x = x,
    y = y,
    just = just,
    hjust = hjust,
    vjust = vjust,
    rot = rot,
    check.overlap = check.overlap,
    default.units = default.units,
    name = name,
    gp = template_gpar(gp, list(...)),
    vp = vp
  )
}
