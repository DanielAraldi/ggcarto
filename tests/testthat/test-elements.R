test_that("native extractors return graphics and absence is explicit", {
  plot <- example_plot() + ggplot2::facet_wrap(~cyl) + ggplot2::theme_bw()
  for (type in native_types) {
    expect_s3_class(gc_get_element(plot, type), "gc_element")
    expect_s3_class(gc_get_element(plot, type)$content, "grob")
  }
  empty <- ggplot2::ggplot()
  expect_error(
    gc_get_element(empty, "legend"),
    class = "ggcarto_missing_element"
  )
  expect_null(gc_get_element(empty, "title", missing = "null"))
  expect_error(
    gc_get_element(type = "title"),
    class = "ggcarto_unsupported_source"
  )
  expect_error(
    gc_get_element(plot, "unknown"),
    class = "ggcarto_missing_extractor"
  )
  panels <- gc_get_element(plot, "panel")
  expect_s3_class(panels$content, "gtable")
  expect_length(panels$content$grobs, 3)
  expect_false(inherits(
    gc_get_element(plot, "panel", which = 2)$content,
    "gtable"
  ))
  expect_error(
    gc_get_element(plot, "panel", which = 9),
    class = "ggcarto_error"
  )
})

test_that("removal and styles do not mutate sources or other elements", {
  plot <- example_plot()
  source_state <- function(plot) {
    list(
      data = plot$data,
      theme = plot$theme,
      labels = plot$labels,
      mapping = plot$mapping
    )
  }
  original <- source_state(plot)
  removed <- gc_without(plot, c("title", "legend", "x_axis_title"))
  expect_identical(source_state(plot), original)
  expect_null(gc_get_element(removed, "title", missing = "null"))
  expect_null(gc_get_element(removed, "legend", missing = "null"))
  title <- gc_get_element(
    plot,
    "title",
    style = list(color = "blue", font_size = "18pt")
  )
  text <- text_grobs(prepared(title))[[1L]]
  expect_identical(text$gp$col, "blue")
  expect_identical(text$gp$fontsize, 18)
  expect_identical(source_state(plot), original)
  for (type in c("legend", "panel", "x_axis")) {
    before <- text_grobs(gc_get_element(plot, type)$content)
    after <- text_grobs(gc_get_element(title$source, type)$content)
    expect_identical(
      unname(lapply(before, function(grob) grob$gp)),
      unname(lapply(after, function(grob) grob$gp))
    )
  }
  expect_error(
    gc_get_element(plot, "title", style = list(fill = "red")),
    class = "ggcarto_error"
  )
})

test_that("themes and responsive typography persist without device state", {
  caller <- grDevices::dev.cur()
  devices <- grDevices::dev.list()
  for (theme in list(
    ggplot2::theme_minimal(),
    ggplot2::theme_bw(),
    ggplot2::theme_void()
  )) {
    plot <- example_plot() +
      theme +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", colour = "red")
      )
    title <- gc_get_element(plot, "title")
    text <- text_grobs(prepared(title))[[1L]]
    expect_identical(text$gp$col, "red")
    expect_identical(unname(text$gp$font), 2L)
    changed <- text_grobs(prepared(gc_style(title, color = "blue")))[[1L]]
    expect_identical(unname(changed$gp$font), 2L)
  }
  title <- gc_get_element(
    example_plot(),
    "title",
    style = list(font_size = "clamp(8pt, 2vmin, 18pt)")
  )
  original <- title
  expect_identical(text_grobs(prepared(title, 256, 256))[[1]]$gp$fontsize, 8)
  expect_identical(
    text_grobs(prepared(title, 1920, 1080))[[1]]$gp$fontsize,
    16.2
  )
  expect_identical(title, original)
  expect_identical(grDevices::dev.cur(), caller)
  expect_identical(grDevices::dev.list(), devices)
})

test_that("custom registries are explicit and independent", {
  registry <- gc_register_element(
    "badge",
    extractor = function(plot) grid::textGrob(plot),
    can_extract = is.character
  )
  badge <- gc_get_element("Hello", "badge", registry = registry)
  expect_identical(badge$content$label, "Hello")
  expect_error(
    gc_get_element("Hello", "badge"),
    class = "ggcarto_missing_extractor"
  )
  expect_error(
    gc_get_element(1, "badge", registry = registry),
    class = "ggcarto_unsupported_source"
  )
  expect_s3_class(
    gc_get_element(grid::rectGrob(), "north_arrow")$content,
    "grob"
  )
  bad <- gc_register_element("bad", extractor = function(plot) 1)
  expect_error(
    gc_get_element("x", "bad", registry = bad),
    class = "ggcarto_invalid_extractor"
  )
})

test_that("style aliases and opacity preserve the original graphical content", {
  original <- gc_text("Styled label", col = "red", fontface = "bold")
  unchanged <- original
  node <- gc_style(original)
  expect_identical(gc_style(node), node)
  styled <- gc_style(
    node,
    colour = "blue",
    alpha = 0.8,
    opacity = 0.25,
    font_face = "italic",
    line_height = 1.6
  )
  content <- prepared(styled)
  expect_identical(content$gp$col, "blue")
  expect_identical(content$gp$alpha, 0.25)
  expect_identical(unname(content$gp$font), 3L)
  expect_identical(content$gp$lineheight, 1.6)
  expect_null(styled$style$colour)
  expect_identical(original, unchanged)
  expect_identical(node$content, original)
})

test_that("styles reject ambiguous names and invalid opacity or lengths", {
  label <- gc_text("Label")
  expect_error(gc_style(label, "blue"), class = "ggcarto_error")
  expect_error(
    # nolint next: duplicate_argument_linter.
    gc_style(label, color = "red", color = "blue"),
    class = "ggcarto_error"
  )
  expect_error(
    gc_style(label, color = "red", colour = "blue"),
    class = "ggcarto_error"
  )
  for (property in c("alpha", "opacity")) {
    for (value in c(-0.1, 1.1, Inf, NA_real_)) {
      expect_error(
        do.call(
          gc_style,
          c(list(label), stats::setNames(list(value), property))
        ),
        class = "ggcarto_error"
      )
    }
  }
  expect_error(gc_style(label, font_size = "auto"), class = "ggcarto_error")
  styled <- gc_style(label, font_size = "min(-1px, 10px)")
  condition <- tryCatch(gc_measure(styled), ggcarto_error = identity)
  expect_s3_class(condition, "ggcarto_error")
  expect_identical(condition$property, "font_size")
  legend <- gc_get_element(example_plot(), "legend")
  expect_error(
    gc_style(legend, legend.direction = "diagonal"),
    class = "ggcarto_error"
  )
})

test_that("style callbacks receive overrides without changing the registry", {
  registry <- gc_register_element(
    "custom_label",
    extract = function(source) gc_text(source),
    style = function(element, style, context) {
      gc_text(element$source, fontsize = style$label_size, col = style$color)
    }
  )
  original <- registry
  element <- gc_get_element("Badge", "custom_label", registry = registry)
  styled <- gc_style(element, label_size = 18, color = "blue")
  content <- prepared(styled)
  expect_identical(content$label, "Badge")
  expect_identical(content$gp$fontsize, 18)
  expect_identical(content$gp$col, "blue")
  expect_identical(registry, original)
  expect_identical(element$style, list())
})

test_that("legend spacer tracks do not stretch its intrinsic dimensions", {
  legend <- gc_get_element(example_plot(), "legend")
  small <- gc_measure(legend, list(width = 400, height = 300))
  large <- gc_measure(legend, list(width = 800, height = 600))
  expect_equal(
    small$intrinsic,
    large$intrinsic,
    tolerance = testthat::testthat_tolerance()
  )
  expect_lt(small$width, 200)
  expect_lt(small$height, 200)
})

test_that("styles work on viewports and custom measurers see styled content", {
  viewport <- gc_viewport()
  styled <- gc_style(viewport, background = "white", padding = "2mm")
  expect_null(viewport$background)
  expect_identical(styled$background, "white")
  expect_error(gc_style(viewport, font_size = 18), class = "ggcarto_error")
  registry <- gc_register_element(
    "label",
    extract = function(plot) grid::textGrob(plot),
    measure = function(element, context) measure_grob(element$content, context)
  )
  element <- gc_get_element("Hello", "label", registry = registry)
  expect_gt(
    gc_measure(gc_style(element, font_size = "30pt"))$height,
    gc_measure(element)$height
  )
  old_theme <- ggplot2::theme_set(ggplot2::theme_minimal(base_size = 10))
  on.exit(ggplot2::theme_set(old_theme))
  title <- gc_get_element(example_plot(), "title")
  before <- text_grobs(prepared(title))[[1]]$gp
  ggplot2::theme_set(ggplot2::theme_bw(base_size = 30))
  expect_identical(text_grobs(prepared(title))[[1]]$gp, before)
})

test_that("registration and lookup validate registries, types and callbacks", {
  expect_error(
    gc_register_element(""),
    "nonempty string",
    class = "ggcarto_error"
  )
  expect_error(
    gc_register_element("custom", registry = list()),
    "gc_registry",
    class = "ggcarto_error"
  )
  expect_error(
    gc_register_element("custom", extract = 1),
    "Extraction callbacks",
    class = "ggcarto_error"
  )
  expect_error(
    gc_register_element("custom", extract = identity, style = 1),
    "Optional callbacks",
    class = "ggcarto_error"
  )
  plot <- example_plot()
  expect_error(
    gc_get_element(plot, "title", registry = list()),
    "gc_registry",
    class = "ggcarto_error"
  )
  expect_error(
    gc_get_element(plot, 1),
    "type must be a string",
    class = "ggcarto_error"
  )
})

test_that("native extraction handles grobs, positions and void themes", {
  plot <- example_plot()
  expect_length(find_grobs(ggplot2::ggplotGrob(plot), "^no-such-grob$"), 0L)
  rectangle <- grid::rectGrob()
  expect_identical(gc_get_element(rectangle, "title")$content, rectangle)
  placed <- gc_get_element(plot, "title", position = list(left = 12))
  expect_identical(format_length(placed$left), "12px")
  void <- plot + ggplot2::theme_void()
  expect_null(gc_get_element(void, "panel_background", missing = "null"))
})

test_that("gc_without rejects unsupported sources, types and panels", {
  plot <- example_plot()
  expect_error(gc_without(1, "title"), class = "ggcarto_unsupported_source")
  expect_error(
    gc_without(plot, "bogus"),
    "Unknown element",
    class = "ggcarto_error"
  )
  expect_error(
    gc_without(plot, "panel"),
    "data panels",
    class = "ggcarto_error"
  )
})

test_that("theme styles reach every text and background element key", {
  plot <- example_plot() +
    ggplot2::facet_wrap(~am) +
    ggplot2::labs(x = "Weight", y = "Mileage") +
    ggplot2::theme_bw()
  text_types <- c(
    "caption",
    "tag",
    "x_axis",
    "y_axis",
    "x_axis_title",
    "y_axis_title",
    "strip"
  )
  for (type in text_types) {
    styled <- style_source(plot, type, list(color = "red"))
    for (key in theme_keys(type)) {
      expect_identical(
        ggplot2::calc_element(key, styled$theme)$colour,
        "red",
        info = key
      )
    }
  }
  for (type in c("plot_background", "panel_background")) {
    styled <- style_source(
      plot,
      type,
      list(fill = "yellow", line_width = 96 / 25.4)
    )
    element <- ggplot2::calc_element(theme_keys(type), styled$theme)
    expect_identical(element$fill, "yellow")
    expect_equal(
      element$linewidth,
      1,
      tolerance = testthat::testthat_tolerance()
    )
  }
  expect_identical(theme_keys("panel"), character())
  styled <- style_source(
    plot,
    "legend",
    list(legend.direction = "horizontal", legend.key_width = 48)
  )
  expect_identical(styled$theme$legend.direction, "horizontal")
  expect_identical(
    grid::convertUnit(
      styled$theme$legend.key.width,
      "inches",
      valueOnly = TRUE
    ),
    0.5
  )
  background <- gc_get_element(plot, "plot_background")
  expect_no_error(gc_style(background, fill = "yellow", line_width = 2))
  condition <- expect_error(
    gc_style(background, font_size = 2),
    class = "ggcarto_error"
  )
  expect_identical(condition$property, "style")
})

test_that("panels without a background grob have no panel background", {
  local_mocked_bindings(find_grobs = function(grob, pattern) list())
  expect_null(
    gc_get_element(example_plot(), "panel_background", missing = "null")
  )
})
