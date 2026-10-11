test_that("logical lengths resolve without mutation", {
  length <- gc_length("50%")
  original <- length
  expect_identical(resolve_length(length, new_layout_context(800, 600)), 400)
  expect_identical(resolve_length(length, new_layout_context(200, 100)), 100)
  expect_identical(length, original)
  context <- new_layout_context(200, 100, root_width = 800, root_height = 600)
  expect_identical(resolve_length("10vw", context), 80)
  expect_identical(resolve_length("10vh", context), 60)
  expect_identical(resolve_length("10vmin", context), 60)
  expect_identical(resolve_length("10vmax", context), 80)
  expect_identical(resolve_length("50%", context, "y"), 50)
})

test_that("physical lengths use 96 logical pixels per inch at any dpi", {
  for (dpi in c(72, 96, 300)) {
    context <- new_layout_context(800, 600, dpi)
    for (length in c("1in", "25.4mm", "2.54cm", "72pt", "96px")) {
      expect_identical(resolve_length(length, context), 96)
    }
  }
})

test_that("expressions are parsed safely and resolved recursively", {
  context <- new_layout_context(800, 600)
  expect_identical(resolve_length("clamp(120px, 35%, 360px)", context), 280)
  expect_identical(resolve_length("min(90%, max(20px, 600px))", context), 600)
  expect_identical(
    resolve_length(gc_clamp("10px", "200%", "300px"), context),
    300
  )
  expect_true(is.na(resolve_length("auto", context)))
  expect_identical(resolve_length("auto", context, intrinsic = 42), 42)
  for (invalid in c(
    "12em",
    "12",
    "min()",
    "clamp(1px, 2px)",
    "min(auto, 2px)",
    "min(2px,,3px)",
    "system('echo no')",
    "min(2px, max(3px, 4px)"
  )) {
    expect_error(gc_length(invalid), class = "ggcarto_error")
  }
  expect_error(
    resolve_length("clamp(10px, 2px, 1px)", context),
    class = "ggcarto_error"
  )
  expect_error(gc_length(Inf), class = "ggcarto_error")
  expect_error(new_layout_context(-1, 20), class = "ggcarto_error")
})

test_that("lengths reject invalid units, sources and expressions", {
  condition <- expect_error(gc_length(5, "furlong"), class = "ggcarto_error")
  expect_identical(condition$property, "unit")
  condition <- expect_error(gc_length(list(1)), class = "ggcarto_error")
  expect_identical(condition$property, "length")
  expect_error(
    gc_length("min(1px), (2px)"),
    "Unbalanced",
    class = "ggcarto_error"
  )
  expect_error(gc_clamp("auto", 10, 20), "auto", class = "ggcarto_error")
  expect_error(
    resolve_length(10, new_layout_context(10, 10), "z"),
    "axis must be x or y",
    class = "ggcarto_error"
  )
})

test_that("automatic lengths print as auto", {
  expect_output(print(gc_length("auto")), "auto")
})
