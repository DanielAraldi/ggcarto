test_that("placement returns copies and preserves unspecified constraints", {
  original <- gc_viewport(width = 256, height = 128)
  placed <- gc_place(original, left = "10%", top = "2mm")
  expect_null(original$left)
  expect_identical(placed$width, original$width)
  expect_identical(placed$left, gc_length("10%"))
  expect_null(gc_place(placed, left = NULL)$left)
  expect_identical(gc_place(placed, width = "auto")$width, gc_length("auto"))
})

test_that("joining retains child-local declarations", {
  child <- gc_viewport(list(gc_place(
    grid::rectGrob(),
    x = "50%",
    y = "50%",
    anchor = "center"
  )))
  joined <- gc_join(list(
    gc_place(child, width = "60%"),
    gc_place(child, left = "60%", width = "40%")
  ))
  expect_identical(joined$children[[1]]$children[[1]]$x, child$children[[1]]$x)
  expect_identical(joined$children[[2]]$children[[1]]$y, child$children[[1]]$y)
  expect_false(anyDuplicated(scene_ids(joined)) > 0)
  expect_identical(
    joined,
    gc_join(list(
      gc_place(child, width = "60%"),
      gc_place(child, left = "60%", width = "40%")
    ))
  )
})

test_that("box declarations validate input and normalize edges", {
  expect_identical(
    normalize_edges(c("2px", "4px")),
    normalize_edges(list(
      top = "2px",
      bottom = "2px",
      left = "4px",
      right = "4px"
    ))
  )
  expect_identical(gc_viewport(width = 2, units = "cm")$width, gc_length("2cm"))
  for (properties in list(
    list(anchor = "north"),
    list(width = -1),
    list(priority = Inf),
    list(overflow = "silent"),
    list(unknown = 1),
    list(responsive = NA)
  )) {
    expect_error(
      do.call(gc_place, c(list(grid::rectGrob()), properties)),
      class = "ggcarto_error"
    )
  }
  expect_error(gc_viewport(list(1)), class = "ggcarto_unsupported_source")
})

test_that("edges accept single lengths and reject invalid declarations", {
  edges <- normalize_edges(gc_length(5))
  expect_named(edges, c("top", "right", "bottom", "left"))
  expect_identical(format_length(edges$left), "5px")
  expect_error(
    normalize_edges(list(up = 1)),
    "edge names",
    class = "ggcarto_error"
  )
  expect_error(normalize_edges(1:5), "one to four", class = "ggcarto_error")
})

test_that("node properties, IDs and candidates are validated", {
  node <- as_l_node(gc_text("Label"))
  expect_error(
    update_node(node, list(1)),
    "unique names",
    class = "ggcarto_error"
  )
  condition <- expect_error(
    gc_place(gc_text("Label"), id = ""),
    class = "ggcarto_error"
  )
  expect_identical(condition$property, "id")
  condition <- expect_error(
    gc_place(gc_text("Label"), candidates = "nowhere"),
    class = "ggcarto_error"
  )
  expect_identical(condition$property, "candidates")
})

test_that("viewports wrap single grobs and reject invalid children", {
  scene <- gc_viewport(gc_text("Label"))
  expect_length(scene$children, 1L)
  expect_error(gc_viewport(1), "list of scene nodes", class = "ggcarto_error")
  label <- gc_place(gc_text("Label"), id = "label")
  condition <- expect_error(
    gc_viewport(list(label, label)),
    class = "ggcarto_error"
  )
  expect_identical(condition$property, "id")
  joined <- gc_join(list(gc_text("Label")), position = list(left = 7))
  expect_identical(format_length(joined$left), "7px")
})

test_that("printing summarises nodes without drawing them", {
  scene <- gc_viewport(list(gc_text("Label")), width = 300, height = 100)
  expect_output(
    returned <- print(scene),
    "<gc_viewport> viewport root [300px x 100px]",
    fixed = TRUE
  )
  expect_identical(returned, scene)
  expect_output(print(scene), "Children: 1")
  expect_output(
    print(as_l_node(gc_text("Label"))),
    "(unassigned) [auto x auto]",
    fixed = TRUE
  )
})
