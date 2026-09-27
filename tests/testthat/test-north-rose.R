rose_designs <- c(
  "classic",
  "eight_point",
  "sixteen_point",
  "thirty_two_point",
  "stellar",
  "concentric",
  "compass",
  "nautical",
  "minimal",
  "geometric",
  "ornamental",
  "asymmetric"
)

capture_rose <- function(rose, size = c(160, 160)) {
  image <- ragg::agg_capture(width = size[[1]], height = size[[2]], res = 96)
  on.exit(grDevices::dev.off())
  l_render(l_place(rose, width = "100%", height = "100%"))
  image()
}

rose_gallery <- function(...) {
  children <- list()
  for (index in seq_along(rose_designs)) {
    column <- (index - 1) %% 4
    row <- (index - 1) %/% 4
    children <- c(
      children,
      list(
        l_place(
          l_north_rose(rose_designs[[index]], ...),
          left = paste0(column * 25, "%"),
          top = paste0(row * 100 / 3 + 5, "%"),
          width = "25%",
          height = "27%"
        ),
        l_place(
          l_text(rose_designs[[index]], fontsize = 10),
          x = paste0(column * 25 + 12.5, "%"),
          y = paste0(row * 100 / 3 + 3, "%"),
          anchor = "center",
          width = "25%",
          height = "5%"
        )
      )
    )
  }
  l_viewport(children, background = "white")
}

test_that("all twelve compass roses construct without drawing or opening devices", {
  caller <- grDevices::dev.cur()
  devices <- grDevices::dev.list()
  for (design in rose_designs) {
    rose <- l_north_rose(design, name = "test-rose")
    expect_s3_class(rose, "gTree")
    expect_true(
      length(rose$children$rose$children$body$children) > 0,
      info = design
    )
    expect_identical(rose, l_north_rose(design, name = "test-rose"))
  }
  expect_identical(grDevices::dev.cur(), caller)
  expect_identical(grDevices::dev.list(), devices)
})

test_that("point-count designs contain the requested number of split tips", {
  for (count in c(4, 8, 16, 32)) {
    design <- rose_designs[[match(count, c(4, 8, 16, 32))]]
    body <- l_north_rose(design)$children$rose$children$body
    expect_length(body$children, count * 2)
    expect_equal(
      as.numeric(body$children[[1]]$y)[[3]],
      0.5 + if (count == 4) 0.33 else 0.34
    )
  }
  for (count in c(8, 16, 32)) {
    body <- l_north_rose("stellar", points = count)$children$rose$children$body
    expect_length(body$children, count * 2)
  }
})

test_that("every design renders nonblank and distinct through lplot at two sizes", {
  skip_if_not_installed("ragg")
  for (size in list(c(120, 120), c(240, 160))) {
    images <- lapply(rose_designs, function(design) {
      capture_rose(l_north_rose(design, labels = FALSE), size)
    })
    for (index in seq_along(images)) {
      expect_true(
        any(!tolower(images[[index]]) %in% c("white", "#ffffff")),
        info = rose_designs[[index]]
      )
    }
    expect_length(unique(images), 12)
  }
})

test_that("specialized designs retain their defining geometry", {
  expected <- list(
    concentric = c("outer-ring", "middle-ring", "inner-ring", "ring-divisions"),
    compass = c(
      "case",
      "dial",
      "dial-ticks",
      "cardinal-ticks",
      "needle-01-primary",
      "pivot"
    ),
    nautical = c(
      "rhumb-lines",
      "outer-ring",
      "inner-ring",
      "hub-ring",
      "north-mark"
    ),
    minimal = c("fine-rays", "north-tip", "center"),
    geometric = c("diamond-01", "diamond-08", "center-diamond"),
    ornamental = c(
      "bead-01",
      "bead-08",
      "petal-01",
      "petal-08",
      "jewel-ring",
      "jewel"
    ),
    asymmetric = c(
      "intercardinal-01",
      "intercardinal-04",
      "cardinal-01-primary"
    )
  )
  for (design in names(expected)) {
    body <- l_north_rose(design)$children$rose$children$body
    expect_true(all(expected[[design]] %in% body$childrenOrder), info = design)
  }
  for (design in rose_designs) {
    body <- l_north_rose(design)$children$rose$children$body
    for (shape in body$children) {
      if (inherits(shape, "polygon")) {
        expect_true(all(is.finite(as.numeric(shape$x))))
        expect_true(all(is.finite(as.numeric(shape$y))))
        expect_true(all(as.numeric(shape$x) > 0 & as.numeric(shape$x) < 1))
        expect_true(all(as.numeric(shape$y) > 0 & as.numeric(shape$y) < 1))
      }
    }
  }
  mark <- l_north_rose("nautical")$children$rose$children$body$children[[
    "north-mark"
  ]]
  expect_true(all(as.numeric(mark$y) > 0.8))
})

test_that("labels run clockwise from north and can be translated or omitted", {
  for (design in rose_designs) {
    labels <- l_north_rose(design)$children$rose$children$labels
    expected <- if (design %in% c("classic", "minimal", "compass")) {
      c("N", "E", "S", "W")
    } else {
      c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
    }
    expect_identical(labels$label, expected)
    expect_equal(
      as.numeric(labels$x)[match(c("N", "E", "S", "W"), expected)],
      c(0.5, 0.925, 0.5, 0.075)
    )
    expect_equal(
      as.numeric(labels$y)[match(c("N", "E", "S", "W"), expected)],
      c(0.925, 0.5, 0.075, 0.5)
    )
    for (omit in list(FALSE, NULL)) {
      expect_identical(
        l_north_rose(design, labels = omit)$children$rose$childrenOrder,
        "body"
      )
    }
  }
  for (count in c(4, 8, 16, 32)) {
    labels <- as.character(seq_len(count))
    expect_identical(
      l_north_rose(labels = labels)$children$rose$children$labels$label,
      labels
    )
  }
  labels <- c("N", "L", "S", "")
  rose <- l_north_rose(labels = labels)
  expect_identical(rose$children$rose$children$labels$label, labels)
  expect_identical(
    rose$children$rose$children$body,
    l_north_rose()$children$rose$children$body
  )
})

test_that("styling and outer viewports are preserved without input mutation", {
  gp <- grid::gpar(col = "red", lwd = 4, alpha = 0.8, lty = "dashed")
  label_gp <- grid::gpar(col = "blue", fontface = "italic", fontsize = 14)
  viewport <- grid::viewport(angle = 10, name = "outer-rose")
  original <- list(gp, label_gp, viewport)
  rose <- l_north_rose(
    col = "black",
    fill = "red",
    fill_secondary = "gold",
    lwd = 2,
    fontsize = 9,
    fontface = "bold",
    fontfamily = "mono",
    angle = 25,
    gp = gp,
    label_gp = label_gp,
    vp = viewport,
    name = "styled-rose"
  )
  expect_identical(list(gp, label_gp, viewport), original)
  expect_identical(rose$vp, viewport)
  expect_identical(rose$name, "styled-rose")
  expect_equal(rose$gp$col, "black")
  expect_equal(rose$gp$lwd, 2)
  expect_equal(rose$gp$alpha, 0.8)
  expect_equal(rose$gp$lty, "dashed")
  expect_equal(rose$children$rose$vp$angle, 25)
  expect_identical(rose$children$rose$vp$width, grid::unit(1, "snpc"))
  expect_identical(rose$children$rose$vp$height, grid::unit(1, "snpc"))
  labels <- rose$children$rose$children$labels
  expect_equal(labels$gp$col, "blue")
  expect_equal(labels$gp$fontsize, 14)
  expect_equal(unname(labels$gp$font), 3L)
  expect_equal(labels$gp$fontfamily, "mono")
  body <- rose$children$rose$children$body
  expect_equal(body$children[[1]]$gp$fill, "red")
  expect_equal(body$children[[2]]$gp$fill, "gold")
  expect_equal(
    l_north_rose("minimal", lwd = 2)$children$rose$children$body$gp$lwd,
    1.3
  )
  expect_equal(
    l_north_rose("ornamental")$children$rose$children$labels$gp$fontfamily,
    "serif"
  )
  expect_equal(
    l_north_rose(
      label_gp = list(font = 4)
    )$children$rose$children$labels$gp$font,
    4
  )
})

test_that("malformed rose settings produce errors at construction", {
  invalid <- list(
    design = list(
      "unknown",
      "clas",
      1,
      NA_character_,
      character(),
      c("classic", "minimal")
    ),
    angle = list(NA, Inf, NaN, "90", c(0, 90), NULL),
    points = list(4, 12, 16.5, "16", NA, c(8, 16)),
    labels = list(
      NA,
      TRUE[FALSE],
      character(),
      "N",
      c("N", "E", "S", NA_character_),
      4
    ),
    col = list(NULL, "not-a-colour", c("red", "blue"), list("red")),
    fill = list("invalid", Inf),
    fill_secondary = list("invalid"),
    lwd = list(0, -1, Inf, NA, "1"),
    fontsize = list(0, -1, Inf, NA),
    fontfamily = list(NA_character_, c("serif", "mono"), 1),
    gp = list("red", list("red"), list(col = "red", col = "blue")),
    label_gp = list("red", list("red"), list(col = "red", col = "blue"))
  )
  for (property in names(invalid)) {
    for (value in invalid[[property]]) {
      expect_error(
        do.call(l_north_rose, stats::setNames(list(value), property)),
        class = "lplot_error"
      )
    }
  }
  expect_error(l_north_rose(fontface = "invalid"))
  expect_no_error(l_north_rose(col = NA, fill = NA, fill_secondary = NA))
  expect_no_error(l_north_rose(
    col = 1,
    fill = "transparent",
    fill_secondary = "#12345678"
  ))
})

test_that("all rose presets integrate with semantic extraction and layout", {
  registry <- l_registry()
  expect_true("north_rose" %in% names(registry))
  expect_error(
    l_get_element(ggplot2::ggplot(), "north_rose"),
    class = "lplot_unsupported_source"
  )
  for (design in rose_designs) {
    rose <- l_north_rose(design)
    original <- rose
    element <- l_get_element(
      rose,
      "north_rose",
      width = 120,
      height = 120,
      metadata = list(design = design)
    )
    expect_identical(element$content, rose)
    expect_identical(element$type, "north_rose")
    expect_identical(element$metadata$design, design)
    scene <- l_viewport(list(l_place(element, right = 10, top = 10)))
    layout <- l_resolve(scene, width = 400, height = 240)
    expect_equal(
      layout$root$children[[1]]$box,
      c(x = 270, y = 10, width = 120, height = 120)
    )
    expect_true(grid::is.grob(prepared(element)))
    expect_identical(rose, original)
  }
})

test_that("roses keep square geometry in wide and tall rendering boxes", {
  skip_if_not_installed("ragg")
  caller <- grDevices::dev.cur()
  devices <- grDevices::dev.list()
  for (design in rose_designs) {
    rose <- l_north_rose(design, labels = FALSE)
    original <- rose
    square <- capture_rose(rose)
    wide <- capture_rose(rose, c(260, 160))
    tall <- capture_rose(rose, c(160, 260))
    expect_identical(wide[, 51:210], square)
    expect_identical(tall[51:210, ], square)
    expect_true(all(
      tolower(wide[, c(1:50, 211:260)]) %in% c("white", "#ffffff")
    ))
    expect_true(all(
      tolower(tall[c(1:50, 211:260), ]) %in% c("white", "#ffffff")
    ))
    expect_identical(rose, original)
  }
  expect_identical(grDevices::dev.cur(), caller)
  expect_identical(grDevices::dev.list(), devices)
})

test_that("colours and rotation change the rendered geometry of every preset", {
  skip_if_not_installed("ragg")
  for (design in rose_designs) {
    original <- capture_rose(l_north_rose(design, labels = FALSE))
    rotated <- capture_rose(l_north_rose(design, labels = FALSE, angle = 25))
    coloured <- capture_rose(l_north_rose(
      design,
      labels = FALSE,
      col = "#A63748",
      fill = "#197C80",
      fill_secondary = "#F4CD68"
    ))
    expect_false(identical(original, rotated), info = design)
    expect_false(identical(original, coloured), info = design)
    fill_only <- capture_rose(l_north_rose(
      design,
      labels = FALSE,
      fill = "#197C80"
    ))
    expect_false(identical(original, fill_only), info = design)
  }
})

test_that("default and customized roses have stable visual galleries", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("svglite")
  writer <- function(plot, file, title) {
    svglite::svglite(file, width = 8, height = 6)
    on.exit(grDevices::dev.off())
    grid::grid.draw(plot)
  }
  vdiffr::expect_doppelganger(
    "north-rose-defaults",
    l_as_grob(rose_gallery()),
    writer = writer
  )
  vdiffr::expect_doppelganger(
    "north-rose-customized",
    l_as_grob(rose_gallery(
      angle = 15,
      fill = "#197C80",
      fill_secondary = "#F4CD68",
      col = "#293438",
      fontsize = 10,
      label_gp = list(col = "#A63748")
    )),
    writer = writer
  )
})

test_that("compass roses export as native SVG without changing the caller device", {
  skip_if_not_installed("svglite")
  directory <- tempfile("north-rose-export-")
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  caller <- grDevices::dev.cur()
  rose <- l_north_rose(
    "eight_point",
    angle = 15,
    labels = c("N", "L", "S", "O")
  )
  original <- rose
  path <- l_save(
    l_place(rose, width = "100%", height = "100%"),
    type = "svg",
    dir = directory,
    filename = "rose",
    width = 180,
    height = 180
  )
  expect_true(file.exists(path))
  contents <- paste(readLines(path), collapse = "\n")
  expect_match(contents, "<polygon", fixed = TRUE)
  expect_match(contents, ">N</text>", fixed = TRUE)
  expect_match(contents, ">L</text>", fixed = TRUE)
  expect_identical(rose, original)
  expect_identical(grDevices::dev.cur(), caller)
})
