test_that("gc_unit preserves the grid signature and unit construction", {
  caller <- grDevices::dev.cur()
  devices <- grDevices::dev.list()
  expect_identical(formals(gc_unit), formals(grid::unit))
  for (units in c(
    "npc",
    "native",
    "mm",
    "cm",
    "inches",
    "points",
    "lines",
    "char",
    "null"
  )) {
    expect_identical(gc_unit(2, units), grid::unit(2, units))
    expect_identical(
      gc_unit(c(0, 0.5, 2), units),
      grid::unit(c(0, 0.5, 2), units)
    )
  }
  expect_identical(
    gc_unit(c(1, 2, 3, 4), c("cm", "mm")),
    grid::unit(c(1, 2, 3, 4), c("cm", "mm"))
  )
  expect_true(grid::is.unit(gc_unit(5, "mm")))
  expect_false(inherits(gc_unit(5, "mm"), "gc_length"))
  expect_identical(grDevices::dev.cur(), caller)
  expect_identical(grDevices::dev.list(), devices)
})

test_that("gc_unit forwards auxiliary data without changing grid semantics", {
  label <- gc_text("Survey", name = "unit-label")
  original <- label
  caller <- grDevices::dev.cur()
  devices <- grDevices::dev.list()
  for (arguments in list(
    list(1, "strwidth", data = "Survey area"),
    list(1, "strheight", data = expression(alpha^2)),
    list(c(1, 2), "strwidth", data = list("West", "East")),
    list(1, "grobwidth", data = label),
    list(1, "grobheight", data = grid::gPath("unit-label")),
    list(c(1, 2), c("grobwidth", "grobheight"), data = list(label, label))
  )) {
    expect_identical(do.call(gc_unit, arguments), do.call(grid::unit, arguments))
  }
  expect_identical(label, original)
  expect_identical(grDevices::dev.cur(), caller)
  expect_identical(grDevices::dev.list(), devices)
})

test_that("gc_unit retains native arithmetic and primitive compatibility", {
  width <- gc_unit(1, "npc") - gc_unit(5, "mm")
  native_width <- grid::unit(1, "npc") - grid::unit(5, "mm")
  expect_identical(width, native_width)
  expect_identical(2 * gc_unit(c(1, 2), "cm"), 2 * grid::unit(c(1, 2), "cm"))
  expect_identical(
    grid::unit.c(gc_unit(1, "cm"), gc_unit(0.5, "npc"))[2],
    grid::unit.c(grid::unit(1, "cm"), grid::unit(0.5, "npc"))[2]
  )
  expect_identical(
    gc_rect(width = width, height = gc_unit(10, "mm"), name = "outline"),
    grid::rectGrob(
      width = native_width,
      height = grid::unit(10, "mm"),
      name = "outline"
    )
  )
  expect_identical(
    gc_text("Note", x = gc_unit(5, "mm"), name = "note"),
    grid::textGrob("Note", x = grid::unit(5, "mm"), name = "note")
  )
})

test_that("gc_unit defers conversion to the current viewport", {
  relative <- gc_unit(0.5, "npc")
  native <- gc_unit(50, "native")
  original <- relative
  grDevices::pdf(NULL, width = 8, height = 6)
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_equal(
    grid::convertWidth(gc_unit(25.4, "mm"), "inches", valueOnly = TRUE),
    1
  )
  for (width in c(2, 4)) {
    grid::pushViewport(grid::viewport(
      width = grid::unit(width, "inches"),
      height = grid::unit(2, "inches"),
      xscale = c(0, 100)
    ))
    expect_equal(
      grid::convertWidth(relative, "inches", valueOnly = TRUE),
      width / 2
    )
    expect_equal(grid::convertHeight(relative, "inches", valueOnly = TRUE), 1)
    expect_equal(grid::convertX(native, "inches", valueOnly = TRUE), width / 2)
    grid::popViewport()
  }
  expect_identical(relative, original)
})

test_that("gc_unit leaves invalid-input errors to grid", {
  for (arguments in list(
    list(),
    list(x = 1),
    list(units = "mm"),
    list(numeric(), "mm"),
    list(1, "not-a-unit"),
    list(1, "px"),
    list(1, "%"),
    list(1, "auto"),
    list(1, "strwidth"),
    list(1, "grobwidth", data = 42)
  )) {
    expected <- tryCatch(do.call(grid::unit, arguments), error = identity)
    actual <- tryCatch(do.call(gc_unit, arguments), error = identity)
    expect_s3_class(expected, "error")
    expect_s3_class(actual, "error")
    if (inherits(expected, "missingArgError")) {
      expect_s3_class(actual, "missingArgError")
    } else {
      expect_identical(class(actual), class(expected))
    }
    expect_identical(conditionMessage(actual), conditionMessage(expected))
  }
})

test_that("primitive defaults match native grid constructors", {
  caller <- grDevices::dev.cur()
  devices <- grDevices::dev.list()
  expect_identical(gc_rect(name = "rect"), grid::rectGrob(name = "rect"))
  expect_identical(
    gc_text("Label", name = "text"),
    grid::textGrob("Label", name = "text")
  )
  expect_identical(
    gc_template(name = "template"),
    grid::grobTree(name = "template")
  )
  expect_identical(grDevices::dev.cur(), caller)
  expect_identical(grDevices::dev.list(), devices)
})

test_that("primitives retain native geometry, vectorization and text controls", {
  viewport <- grid::viewport(angle = 15, name = "local")
  rectangle_args <- list(
    x = c(2, 4),
    y = grid::unit(c(3, 6), "mm"),
    width = grid::unit(1, "cm"),
    height = c(1, 2),
    just = c("left", "bottom"),
    hjust = c(0, 1),
    vjust = 0,
    default.units = "cm",
    name = "rectangles",
    vp = viewport,
    gp = grid::gpar(fill = c("white", "red"), col = NA, alpha = 0.7)
  )
  expect_identical(
    do.call(gc_rect, rectangle_args),
    do.call(grid::rectGrob, rectangle_args)
  )
  text_args <- list(
    label = expression(alpha, beta^2),
    x = c(1, 2),
    y = grid::unit(c(4, 8), "mm"),
    just = c("right", "top"),
    hjust = c(1, 0),
    vjust = 1,
    rot = 30,
    check.overlap = TRUE,
    default.units = "cm",
    name = "labels",
    vp = viewport,
    gp = grid::gpar(
      fontsize = c(10, 14),
      fontfamily = "serif",
      fontface = "bold"
    )
  )
  expect_identical(
    do.call(gc_text, text_args),
    do.call(grid::textGrob, text_args)
  )
})

test_that("graphical shortcuts override gp without mutating it", {
  validated <- grid::gpar(fontface = c(first = "italic", second = "bold"))
  original_validated <- validated
  expect_identical(template_gpar(overrides = validated), validated)
  expect_identical(validated, original_validated)
  theme <- grid::gpar(col = "red", fontface = "bold", fontsize = 9)
  original <- theme
  label <- gc_text("Note", gp = theme, col = "blue", fontface = "italic")
  expect_identical(theme, original)
  expect_equal(label$gp$col, "blue")
  expect_equal(label$gp$fontsize, 9)
  expect_equal(unname(label$gp$font), 3L)
  expect_equal(
    unname(gc_text("Note", gp = list(fontface = "italic"), font = 2)$gp$font),
    2L
  )
  parameters <- list(
    fill = "white",
    col = "black",
    alpha = 0.6,
    lwd = 2,
    lty = "dashed",
    lineend = "round",
    linejoin = "bevel",
    linemitre = 4,
    lex = 1.2
  )
  expect_identical(
    do.call(gc_rect, parameters)$gp,
    do.call(grid::gpar, parameters)
  )
  parameters <- list(
    col = "blue",
    fontsize = 11,
    fontfamily = "serif",
    fontface = "bold.italic",
    lineheight = 1.4,
    cex = 0.9
  )
  expect_identical(
    gc_text("Note", gp = parameters)$gp,
    do.call(grid::gpar, parameters)
  )
  expect_identical(gc_rect(gp = list(fill = "red"))$gp, grid::gpar(fill = "red"))
  expect_identical(
    gc_template(gp = parameters)$gp,
    do.call(grid::gpar, parameters)
  )
})

test_that("templates retain child order, nesting and native viewport controls", {
  background <- gc_rect(name = "background", fill = "white")
  label <- gc_text("N", name = "label")
  nested <- gc_template(grid::segmentsGrob(name = "segment"), name = "nested")
  viewport <- grid::viewport(angle = 10)
  child_viewports <- grid::vpList(grid::viewport(name = "child"))
  template <- gc_template(
    background,
    children = list(label, nested),
    name = "template",
    gp = list(col = "red"),
    vp = viewport,
    childrenvp = child_viewports,
    cl = "badge"
  )
  expect_identical(
    template,
    grid::grobTree(
      background,
      label,
      nested,
      name = "template",
      gp = grid::gpar(col = "red"),
      vp = viewport,
      childrenvp = child_viewports,
      cl = "badge"
    )
  )
  expect_identical(template$childrenOrder, c("background", "label", "nested"))
  expect_null(label$gp$col)
  expect_identical(
    gc_template(children = grid::gList(background, label), name = "list"),
    grid::grobTree(background, label, name = "list")
  )
})

test_that("invalid children and malformed graphical parameter lists are rejected", {
  expect_error(gc_template(1), class = "ggcarto_unsupported_source")
  expect_error(gc_template(gc_viewport()), class = "ggcarto_unsupported_source")
  expect_error(gc_template(children = "label"), class = "ggcarto_error")
  expect_error(
    gc_template(children = list(gc_rect(), NULL)),
    class = "ggcarto_error"
  )
  for (bad in list(
    "red",
    list("red"),
    list(col = "red", col = "blue"),
    stats::setNames(list("red"), NA_character_)
  )) {
    expect_error(gc_text("Label", gp = bad), class = "ggcarto_error")
    expect_error(gc_rect(gp = bad), class = "ggcarto_error")
    expect_error(gc_template(gp = bad), class = "ggcarto_error")
  }
  expect_error(
    gc_text(
      "Label",
      0.5,
      0.5,
      "centre",
      NULL,
      NULL,
      0,
      FALSE,
      "npc",
      NULL,
      NULL,
      NULL,
      "red"
    ),
    class = "ggcarto_error"
  )
  expect_error(gc_rect(col = "red", col = "blue"), class = "ggcarto_error")
  expect_error(gc_text("Label", font = 2, fontface = "italic"))
})

test_that("templates enter extraction and layout through the existing grob path", {
  template <- gc_template(
    gc_rect(fill = "white", col = "black"),
    gc_text("N", fontsize = 12, fontface = "bold")
  )
  original <- template
  element <- gc_get_element(template, "north_arrow", width = 40, height = 60)
  expect_identical(element$content, template)
  scene <- gc_viewport(
    list(
      gc_place(template, left = 10, top = 10, width = 40, height = 60),
      gc_place(element, right = 10, bottom = 10)
    ),
    width = 200,
    height = 120
  )
  expect_s3_class(gc_resolve(scene, width = 200, height = 120), "gc_layout")
  expect_true(grid::is.grob(prepared(element)))
  expect_gt(gc_measure(gc_text("Measured", fontsize = 12))$width, 0)
  expect_identical(template, original)
})

test_that("template themes render like native grid and preserve child overrides", {
  skip_if_not_installed("ragg")
  caller <- grDevices::dev.cur()
  devices <- grDevices::dev.list()
  build <- function(template, rectangle, text, unit) {
    template(
      rectangle(
        x = unit(0.25, "npc"),
        width = unit(0.5, "npc"),
        gp = grid::gpar(col = NA)
      ),
      rectangle(
        x = unit(0.75, "npc"),
        width = unit(0.5, "npc"),
        gp = grid::gpar(fill = "#194E70", col = NA)
      ),
      template(text(
        "Theme",
        gp = grid::gpar(col = "white", fontface = "bold")
      )),
      gp = grid::gpar(fill = "#2B8E98", fontsize = 12)
    )
  }
  template <- build(gc_template, gc_rect, gc_text, gc_unit)
  native <- build(grid::grobTree, grid::rectGrob, grid::textGrob, grid::unit)
  original <- template
  capture <- function(grob, size) {
    image <- ragg::agg_capture(width = size[[1]], height = size[[2]], res = 96)
    on.exit(grDevices::dev.off())
    scene <- gc_viewport(list(gc_place(
      grob,
      left = 10,
      right = 10,
      top = 10,
      bottom = 10
    )))
    gc_render(scene)
    image()
  }
  for (size in list(c(200, 120), c(400, 240))) {
    actual <- capture(template, size)
    expect_identical(actual, capture(native, size))
    expect_true(any(tolower(actual) == "#2b8e98"))
    expect_true(any(tolower(actual) == "#194e70"))
  }
  expect_identical(template, original)
  expect_identical(grDevices::dev.cur(), caller)
  expect_identical(grDevices::dev.list(), devices)
})
