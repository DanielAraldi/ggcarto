north_arrow_designs <- c(
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
)

north_arrow_gallery <- function(styled = FALSE) {
  children <- list()
  for (index in seq_along(north_arrow_designs)) {
    design <- north_arrow_designs[[index]]
    column <- (index - 1) %% 4
    row <- (index - 1) %/% 4
    arguments <- if (styled) {
      list(
        angle = 25,
        fill = "#197C80",
        fill_secondary = "#F4CD68",
        col = "#293438",
        lwd = 0.8,
        fontsize = 13,
        fontface = "italic"
      )
    } else {
      list()
    }
    children <- c(
      children,
      list(
        gc_place(
          do.call(gc_north_arrow, c(list(design = design), arguments)),
          left = column * 150 + 45,
          top = row * 170 + 12,
          width = 60,
          height = 102
        ),
        gc_place(
          gc_text(design, fontsize = 10),
          left = column * 150,
          top = row * 170 + 126,
          width = 150,
          height = 24
        )
      )
    )
  }
  gc_viewport(children, width = 600, height = 510, background = "white")
}

test_that("all twelve north arrow presets are lazy native grobs", {
  caller <- grDevices::dev.cur()
  devices <- grDevices::dev.list()
  expect_identical(eval(formals(gc_north_arrow)$design), north_arrow_designs)
  bodies <- list()
  for (design in north_arrow_designs) {
    arrow <- gc_north_arrow(design, name = "north")
    expect_s3_class(arrow, "gTree")
    expect_identical(arrow$name, "north")
    symbol <- arrow$children$symbol
    expect_equal(symbol$vp$angle, 0)
    expect_identical(symbol$childrenOrder, c("body", "label"))
    expect_identical(symbol$children$label$label, "N")
    expect_gt(length(symbol$children$body$children), 0)
    expect_true(all(vapply(
      symbol$children$body$children,
      grid::is.grob,
      logical(1)
    )))
    bodies[[design]] <- symbol$children$body
  }
  expect_length(unique(bodies), 12)
  expect_identical(
    gc_north_arrow()$children$symbol$children,
    gc_north_arrow("classic")$children$symbol$children
  )
  expect_identical(grDevices::dev.cur(), caller)
  expect_identical(grDevices::dev.list(), devices)
})

test_that("all presets support styling and independently positioned rotated labels", {
  group <- grid::gpar(alpha = 0.7, col = "red", lwd = 4, linejoin = "bevel")
  label_style <- grid::gpar(col = "blue", fontface = "italic", fontsize = 15)
  original_group <- group
  original_label <- label_style
  viewport <- grid::viewport(x = 0.4, angle = 5)
  for (design in north_arrow_designs) {
    arrow <- gc_north_arrow(
      design,
      angle = -30,
      label = "North",
      col = "black",
      fill = "green",
      fill_secondary = "yellow",
      lwd = 0.8,
      fontsize = 13,
      fontface = "plain",
      fontfamily = "serif",
      label_x = gc_unit(12, "mm"),
      label_y = 0.88,
      gp = group,
      label_gp = label_style,
      vp = viewport
    )
    expect_identical(arrow$vp, viewport)
    expect_equal(arrow$gp$alpha, 0.7)
    expect_equal(arrow$gp$lwd, 0.8)
    expect_equal(arrow$gp$col, "black")
    expect_equal(arrow$gp$linejoin, "bevel")
    symbol <- arrow$children$symbol
    expect_equal(symbol$vp$angle, -30)
    label <- symbol$children$label
    expect_identical(label$label, "North")
    expect_identical(label$x, gc_unit(12, "mm"))
    expect_identical(label$y, gc_unit(0.88, "npc"))
    expect_equal(label$gp$fontsize, 15)
    expect_equal(label$gp$col, "blue")
    expect_equal(label$gp$fontfamily, "serif")
    expect_equal(unname(label$gp$font), 3L)
    fills <- vapply(
      symbol$children$body$children,
      function(child) {
        as.character(child$gp$fill %||% NA_character_)
      },
      character(1)
    )
    expect_true(
      all(is.na(fills) | fills %in% c("green", "yellow")),
      info = design
    )
    for (hidden in list(NULL, "")) {
      expect_identical(
        gc_north_arrow(design, label = hidden)$children$symbol$childrenOrder,
        "body"
      )
    }
  }
  expect_identical(group, original_group)
  expect_identical(label_style, original_label)
})

test_that("minimal arrows accept the full native arrow specification", {
  tip <- grid::arrow(
    length = gc_unit(5, "mm"),
    angle = 20,
    ends = "both",
    type = "open"
  )
  arrow <- gc_north_arrow("minimal", arrow = tip)
  shaft <- arrow$children$symbol$children$body$children$shaft
  expect_identical(shaft$arrow, tip)
  expect_equal(as.numeric(shaft$x0), 0.5)
  expect_equal(as.numeric(shaft$y1), 0.72)
  expect_null(
    gc_north_arrow(
      "minimal",
      arrow = NULL
    )$children$symbol$children$body$children$shaft$arrow
  )
})

test_that("pennant uses a right triangular flag without an arrowhead", {
  symbol <- gc_north_arrow("pennant", angle = 25, fill = "red")$children$symbol
  body <- symbol$children$body
  expect_identical(body$childrenOrder, c("mast", "flag", "base"))
  flag <- body$children$flag
  expect_s3_class(flag, "polygon")
  coordinates <- cbind(as.numeric(flag$x), as.numeric(flag$y))
  expect_equal(coordinates, cbind(c(0.5, 0.87, 0.5), c(0.8, 0.8, 0.5)))
  expect_equal(
    sum(
      (coordinates[2, ] - coordinates[1, ]) *
        (coordinates[3, ] - coordinates[1, ])
    ),
    0
  )
  expect_equal(
    coordinates[1, ],
    c(
      as.numeric(body$children$mast$x)[[2]],
      as.numeric(body$children$mast$y)[[2]]
    )
  )
  expect_identical(flag$gp$fill, "red")
  expect_identical(symbol$children$label$label, "N")
  expect_equal(symbol$vp$angle, 25)
})

test_that("custom bodies retain their geometry, styles and viewports", {
  child <- grid::polygonGrob(
    c(0.2, 0.5, 0.8),
    c(0.2, 0.8, 0.2),
    name = "custom",
    gp = grid::gpar(fill = "red", col = "blue"),
    vp = grid::viewport(width = 0.8)
  )
  original <- child
  for (children in list(list(child), grid::gList(child))) {
    arrow <- gc_north_arrow(children = children, angle = 45, col = "black")
    expect_identical(arrow$children$symbol$children$body$children$custom, child)
    expect_equal(arrow$children$symbol$vp$angle, 45)
  }
  expect_identical(child, original)
  expect_length(
    gc_north_arrow(children = list())$children$symbol$children$body$children,
    0
  )
  expect_no_error(gc_north_arrow(
    col = NA,
    fill = NA,
    fill_secondary = "transparent"
  ))
})

test_that("invalid north arrow inputs fail at construction", {
  for (design in list(
    "unknown",
    "cla",
    NA_character_,
    NULL,
    1,
    c("classic", "bold")
  )) {
    expect_error(gc_north_arrow(design), class = "ggcarto_error")
  }
  for (property in c("angle", "lwd", "fontsize", "label_x", "label_y")) {
    for (value in list(NA_real_, Inf, "bad", numeric(), c(1, 2))) {
      expect_error(
        do.call(gc_north_arrow, stats::setNames(list(value), property)),
        class = "ggcarto_error"
      )
    }
  }
  for (property in c("lwd", "fontsize")) {
    for (value in c(0, -1)) {
      expect_error(
        do.call(gc_north_arrow, stats::setNames(list(value), property)),
        class = "ggcarto_error"
      )
    }
  }
  for (label in list(NA_character_, 1, c("N", "S"))) {
    expect_error(gc_north_arrow(label = label), class = "ggcarto_error")
  }
  for (property in c("col", "fill", "fill_secondary")) {
    for (colour in list(
      "not-a-colour",
      character(),
      c("red", "blue"),
      list("red")
    )) {
      expect_error(
        do.call(gc_north_arrow, stats::setNames(list(colour), property)),
        class = "ggcarto_error"
      )
    }
  }
  expect_error(
    gc_north_arrow(label_x = gc_unit(c(1, 2), "mm")),
    class = "ggcarto_error"
  )
  expect_error(
    gc_north_arrow(label_y = gc_unit(NA_real_, "npc")),
    class = "ggcarto_error"
  )
  expect_error(gc_north_arrow(arrow = TRUE), class = "ggcarto_error")
  expect_error(
    gc_north_arrow(children = list(NULL)),
    class = "ggcarto_unsupported_source"
  )
  expect_error(gc_north_arrow(children = 1), class = "ggcarto_error")
  expect_error(gc_north_arrow(gp = list("red")), class = "ggcarto_error")
  expect_error(
    gc_north_arrow(label_gp = list(col = "red", col = "blue")),
    class = "ggcarto_error"
  )
})

test_that("every preset integrates with extraction and explicit layout sizes", {
  grDevices::pdf(NULL, width = 6, height = 4)
  on.exit(grDevices::dev.off(), add = TRUE)
  for (design in north_arrow_designs) {
    arrow <- gc_north_arrow(design)
    original <- arrow
    element <- gc_get_element(arrow, "north_arrow", width = 40, height = 68)
    expect_identical(element$content, arrow)
    scene <- gc_viewport(list(gc_place(element, right = 8, top = 8)), padding = 4)
    expect_no_warning(layout <- gc_render(scene))
    expect_equal(
      layout$root$children[[1]]$box[c("width", "height")],
      c(width = 40, height = 68)
    )
    grid::grid.force()
    expect_true("body" %in% grid::grid.ls(print = FALSE)$name)
    expect_identical(arrow, original)
  }
})

test_that("all presets have distinct nonblank raster output at different sizes", {
  skip_if_not_installed("ragg")
  caller <- grDevices::dev.cur()
  devices <- grDevices::dev.list()
  capture <- function(arrow, width, height) {
    image <- ragg::agg_capture(width = width, height = height, res = 96)
    on.exit(grDevices::dev.off())
    gc_render(gc_place(arrow, left = 8, right = 8, top = 8, bottom = 8))
    image()
  }
  for (size in list(c(56, 84), c(96, 152))) {
    images <- lapply(north_arrow_designs, function(design) {
      image <- capture(
        gc_north_arrow(design, label = NULL),
        size[[1]],
        size[[2]]
      )
      expect_gt(
        sum(tolower(image) != "white" & tolower(image) != "#ffffff"),
        20
      )
      rotated <- capture(
        gc_north_arrow(design, label = NULL, angle = 25),
        size[[1]],
        size[[2]]
      )
      expect_false(identical(image, rotated), info = design)
      recoloured <- capture(
        gc_north_arrow(design, label = NULL, col = "red", fill = "red"),
        size[[1]],
        size[[2]]
      )
      expect_false(identical(image, recoloured), info = design)
      image
    })
    expect_length(unique(images), 12)
  }
  native <- gc_template(
    grid::segmentsGrob(
      x0 = 0.5,
      x1 = 0.5,
      y0 = 0.14,
      y1 = 0.72,
      arrow = grid::arrow(length = gc_unit(3, "mm"), type = "closed"),
      gp = grid::gpar(col = "#203C43", fill = "#203C43", lwd = 1.5)
    ),
    gc_text(
      "N",
      x = 0.5,
      y = 0.92,
      fontsize = 11,
      fontface = "bold",
      col = "#203C43"
    ),
    vp = grid::viewport(angle = 17)
  )
  expect_identical(
    capture(gc_north_arrow("minimal", angle = 17), 80, 120),
    capture(native, 80, 120)
  )
  expect_identical(grDevices::dev.cur(), caller)
  expect_identical(grDevices::dev.list(), devices)
})

test_that("north arrow galleries have stable default and customized output", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("svglite")
  writer <- function(plot, file, title) {
    svglite::svglite(file, width = 600 / 96, height = 510 / 96)
    on.exit(grDevices::dev.off())
    grid::grid.draw(plot)
  }
  for (styled in c(FALSE, TRUE)) {
    vdiffr::expect_doppelganger(
      if (styled) "north-arrows-customized" else "north-arrows-default",
      gc_as_grob(north_arrow_gallery(styled)),
      writer = writer
    )
  }
})
