box_in <- function(node, width = 800, height = 600, ...) {
  gc_resolve(gc_viewport(list(node), ...), width, height)$root$children[[1L]]$box
}

test_that("anchors and opposing insets have deterministic geometry", {
  node <- gc_place(
    grid::rectGrob(),
    x = "50%",
    y = "20px",
    width = 200,
    height = 30,
    anchor = "top-center"
  )
  expect_equal(box_in(node), c(x = 300, y = 20, width = 200, height = 30))
  for (anchor in anchors) {
    fraction <- anchor_fraction(anchor)
    box <- box_in(gc_place(
      grid::rectGrob(),
      width = 100,
      height = 50,
      anchor = anchor
    ))
    expect_equal(unname(box[c("x", "y")]), unname(fraction * c(700, 550)))
  }
  inset <- gc_place(
    grid::rectGrob(),
    left = 10,
    right = 20,
    top = 30,
    bottom = 40,
    margin = 5
  )
  expect_equal(box_in(inset), c(x = 15, y = 35, width = 760, height = 520))
  expect_error(
    box_in(gc_place(inset, width = 300)),
    class = "ggcarto_contradictory_constraints"
  )
  expect_error(
    box_in(gc_place(inset, x = 20)),
    class = "ggcarto_contradictory_constraints"
  )
})

test_that("constraints, aspect ratio and overflow produce useful diagnostics", {
  node <- gc_place(
    grid::rectGrob(),
    width = "50%",
    height = 100,
    min_width = 450,
    max_width = 500
  )
  expect_equal(box_in(node)[["width"]], 450)
  expect_error(box_in(gc_place(node, max_width = 400)), class = "ggcarto_error")
  expect_equal(
    box_in(gc_place(grid::rectGrob(), width = 200, aspect_ratio = 2))[[
      "height"
    ]],
    100
  )
  expect_error(box_in(gc_place(node, aspect_ratio = 1)), class = "ggcarto_error")
  expect_warning(
    box_in(gc_place(grid::rectGrob(), width = 900, height = 10)),
    class = "ggcarto_overflow"
  )
  expect_error(
    box_in(gc_place(grid::rectGrob(), width = "min(-10px, 30px)")),
    class = "ggcarto_error"
  )
  expect_error(box_in(gc_place(node, top = "auto")), class = "ggcarto_error")
})

test_that("padding, border, safe area and flow are separate box concepts", {
  node <- gc_place(
    grid::rectGrob(),
    width = 20,
    height = 10,
    anchor = "bottom-right"
  )
  scene <- gc_viewport(
    list(node),
    padding = 10,
    border = list(width = 2),
    safe_area = 8
  )
  layout <- gc_resolve(scene, 200, 100)
  expect_equal(
    layout$root$children[[1]]$box,
    c(x = 148, y = 58, width = 20, height = 10)
  )
  explicit <- gc_place(node, right = 0, bottom = 0)
  expect_equal(box_in(explicit, 200, 100, safe_area = 8)[["x"]], 180)
  row <- gc_viewport(
    list(gc_place(node, anchor = "top-left"), node),
    flow = "row",
    gap = 5
  )
  expect_equal(gc_resolve(row, 200, 100)$root$children[[2]]$box[["x"]], 25)
  column <- gc_viewport(list(node, node), flow = "column", gap = 3)
  expect_equal(gc_resolve(column, 200, 100)$root$children[[2]]$box[["y"]], 13)
})

test_that("responsive resizing and nesting never rewrite logical specifications", {
  element <- gc_place(
    grid::rectGrob(),
    x = "50%",
    y = "50%",
    width = "10vw",
    height = 10,
    anchor = "center"
  )
  child <- gc_viewport(list(element), padding = 4)
  scene <- gc_join(list(gc_place(
    child,
    left = "10%",
    top = "10%",
    width = "60%",
    height = "80%"
  )))
  original <- scene
  for (dimensions in list(
    c(256, 256),
    c(512, 512),
    c(1024, 768),
    c(1920, 1080)
  )) {
    layout <- gc_resolve(scene, dimensions[[1]], dimensions[[2]])
    parent <- layout$root$children[[1]]
    nested <- parent$children[[1]]
    expect_equal(parent$box[["x"]], dimensions[[1]] * 0.1)
    expect_equal(nested$box[["width"]], dimensions[[1]] * 0.1)
    expect_equal(
      nested$box[["x"]] + nested$box[["width"]] / 2,
      (dimensions[[1]] * 0.6 - 8) / 2
    )
    expect_identical(scene, original)
  }
  moved <- gc_join(list(gc_place(
    child,
    left = "20%",
    width = "60%",
    height = "80%"
  )))
  expect_equal(
    gc_resolve(scene, 800, 600)$root$children[[1]]$children[[1]]$box,
    gc_resolve(moved, 800, 600)$root$children[[1]]$children[[1]]$box
  )
})

test_that("intrinsic measurement is contextual and uses custom measurers", {
  caller <- grDevices::dev.cur()
  devices <- grDevices::dev.list()
  title <- gc_get_element(example_plot(), "title")
  size <- gc_measure(title)
  expect_gt(size$width, 0)
  expect_gt(size$height, 0)
  registry <- gc_register_element(
    "test",
    extract = function(plot) grid::rectGrob(),
    measure = function(element, context) {
      c(width = context$width / 2, height = 20)
    }
  )
  element <- gc_get_element(NULL, "test", registry = registry)
  expect_equal(gc_measure(element, list(width = 200, height = 100))$width, 100)
  expect_equal(gc_measure(element, list(width = 400, height = 100))$width, 200)
  expect_identical(grDevices::dev.cur(), caller)
  expect_identical(grDevices::dev.list(), devices)
})

test_that("custom measurers normalize dimensions and reject invalid results", {
  caller <- grDevices::dev.cur()
  devices <- grDevices::dev.list()
  for (dimensions in list(
    c(40, 20),
    c(height = 20, width = 40),
    list(height = 20, width = 40)
  )) {
    registry <- gc_register_element(
      "measured",
      extract = function(source) gc_rect(),
      measure = function(element, context) dimensions
    )
    element <- gc_get_element(NULL, "measured", registry = registry)
    measured <- gc_measure(element, list(width = 200, height = 100))
    expect_equal(measured$width, 40)
    expect_equal(measured$height, 20)
  }
  for (dimensions in list(
    c(-1, 20),
    c(40, Inf),
    c(NA_real_, 20),
    list(width = 40),
    numeric()
  )) {
    registry <- gc_register_element(
      "measured",
      extract = function(source) gc_rect(),
      measure = function(element, context) dimensions
    )
    element <- gc_get_element(NULL, "measured", registry = registry)
    expect_error(gc_measure(element), class = "ggcarto_invalid_measurement")
  }
  expect_identical(grDevices::dev.cur(), caller)
  expect_identical(grDevices::dev.list(), devices)
})

test_that("border diagnostics retain the node and offending property", {
  for (border in list(2, list(unknown = "red"), list(width = -1))) {
    node <- gc_place(gc_rect(), id = "border-case", border = border)
    condition <- tryCatch(box_in(node), ggcarto_error = identity)
    expect_s3_class(condition, "ggcarto_error")
    expect_identical(condition$node, "border-case")
    expect_identical(condition$property, "border")
  }
  node <- gc_place(gc_rect(), width = 40, height = 20, border = "red")
  layout <- gc_resolve(gc_viewport(list(node)), 200, 100)
  expect_equal(layout$root$children[[1]]$border$width, 1)
  expect_identical(layout$root$children[[1]]$border$color, "red")
})

test_that("aspect ratios derive automatic widths and reject incompatible limits", {
  node <- gc_place(gc_rect(), height = 40, aspect_ratio = 2)
  original <- node
  expect_equal(box_in(node)[c("width", "height")], c(width = 80, height = 40))
  expect_identical(node, original)
  constrained <- gc_place(
    gc_rect(),
    id = "ratio-case",
    aspect_ratio = 2,
    min_width = 200,
    max_height = 50
  )
  condition <- tryCatch(box_in(constrained), ggcarto_error = identity)
  expect_s3_class(condition, "ggcarto_error")
  expect_identical(condition$node, "ratio-case")
  expect_identical(condition$property, "aspect_ratio")
})

test_that("explicit layout contexts preserve an already open device and viewport", {
  for (size in list(c(5, 5), c(8, 4))) {
    grDevices::pdf(NULL, width = size[[1]], height = size[[2]])
    device <- grDevices::dev.cur()
    tryCatch(
      {
        grid::grid.newpage()
        grid::pushViewport(grid::viewport(
          name = "caller",
          width = 0.5,
          height = 0.75
        ))
        viewport <- grid::current.viewport()
        devices <- grDevices::dev.list()
        scene <- collision_scene()
        layout <- gc_resolve(scene, width = 200, height = 120)
        expect_equal(
          layout$root$children[[2]]$box,
          c(x = 140, y = 45, width = 60, height = 30)
        )
        title <- gc_get_element(
          example_plot(),
          "title",
          style = list(font_size = "12pt")
        )
        expect_gt(gc_measure(title, list(width = 200, height = 120))$width, 0)
        bad <- gc_place(grid::rectGrob(), left = 0, right = 0, width = 300)
        expect_error(
          gc_measure(bad, list(width = 200, height = 120)),
          class = "ggcarto_contradictory_constraints"
        )
        expect_identical(grDevices::dev.cur(), device)
        expect_identical(grDevices::dev.list(), devices)
        expect_identical(grid::current.viewport(), viewport)
      },
      finally = grDevices::dev.off(device)
    )
  }
})
