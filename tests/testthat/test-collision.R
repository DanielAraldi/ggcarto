test_that("candidate order is deterministic and independent of drawing order", {
  scene <- collision_scene()
  original <- scene
  first <- gc_resolve(scene, width = 200, height = 120)
  second <- gc_resolve(scene, width = 200, height = 120)
  moved <- first$root$children[[2]]
  expect_identical(moved$box, c(x = 140, y = 45, width = 60, height = 30))
  expect_identical(
    moved$collision_candidates,
    second$root$children[[2]]$collision_candidates
  )
  expect_false(boxes_overlap(first$root$children[[1]]$box, moved$box))
  expect_identical(scene, original)
  scene$children[[2]] <- gc_place(scene$children[[2]], z_index = -100)
  expect_identical(
    gc_resolve(scene, width = 200, height = 120)$root$children[[2]]$box,
    moved$box
  )
})

test_that("fixed placement wins unless displacement is explicitly enabled", {
  scene <- collision_scene()
  scene$children[[2]] <- gc_place(scene$children[[2]], top = 0, right = 0)
  expect_warning(
    layout <- gc_resolve(scene, width = 200, height = 120),
    class = "ggcarto_collision"
  )
  expect_identical(layout$root$children[[2]]$box[["y"]], 0)
  scene$children[[2]] <- gc_place(scene$children[[2]], allow_move = TRUE)
  expect_identical(
    gc_resolve(scene, width = 200, height = 120)$root$children[[2]]$box[["y"]],
    45
  )
  scene <- collision_scene("none")
  expect_no_warning(gc_resolve(scene, width = 200, height = 120))
})

test_that("priority resists displacement; plot backgrounds are not obstacles", {
  node <- gc_place(
    grid::rectGrob(),
    width = 40,
    height = 20,
    collision = "avoid",
    anchor = "top-right"
  )
  scene <- gc_viewport(list(
    example_plot(),
    node,
    gc_place(node, priority = 20)
  ))
  layout <- gc_resolve(scene, 400, 300)
  expect_identical(layout$root$children[[3]]$box[["y"]], 0)
  expect_gt(layout$root$children[[2]]$box[["y"]], 0)
})

test_that("shrink requires responsive permission and respects minimums", {
  node <- gc_place(
    grid::rectGrob(),
    width = 180,
    height = 60,
    collision = "shrink",
    min_width = 80,
    min_height = 20
  )
  scene <- gc_viewport(list(node), safe_area = 10)
  layout <- gc_resolve(scene, 120, 100)
  result <- layout$root$children[[1]]
  expect_lte(result$box[["width"]], 100)
  expect_gte(result$box[["width"]], 80)
  expect_lt(result$scale, 1)
  node <- gc_place(node, responsive = FALSE)
  seen <- new.env(parent = emptyenv())
  seen$classes <- list()
  unchanged <- withCallingHandlers(
    gc_resolve(gc_viewport(list(node)), 120, 100),
    warning = function(condition) {
      seen$classes[[length(seen$classes) + 1L]] <- class(condition)
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(vapply(
    seen$classes,
    function(classes) "ggcarto_collision" %in% classes,
    logical(1)
  )))
  expect_identical(unchanged$root$children[[1]]$box[["width"]], 180)
  expect_identical(unchanged$root$children[[1]]$scale, 1)
})

test_that("explicit candidates lead the anchor search", {
  node <- list(anchor = "top-left", candidates = c("center", "top-left"))
  expect_identical(collision_anchors(node), c("top-left", "center"))
})

test_that("shrinking is skipped without room or reachable minimums", {
  unreachable <- gc_place(
    gc_text("Shrink"),
    left = 0,
    top = 0,
    width = 40,
    height = 20,
    collision = "shrink",
    min_width = 80
  )
  layout <- gc_resolve(gc_viewport(list(unreachable)), 200, 100)
  expect_identical(shrink_factors(layout$root$children[[1]], layout$context), 1)
  padded <- gc_place(
    gc_text("Shrink"),
    left = 0,
    top = 0,
    width = 20,
    height = 20,
    padding = 10,
    collision = "shrink",
    min_width = 5
  )
  layout <- suppressWarnings(gc_resolve(gc_viewport(list(padded)), 200, 100))
  expect_identical(shrink_factors(layout$root$children[[1]], layout$context), 1)
})
