cartographic_plot <- function(crs = 32119, width = 100000, height = 50000) {
  extent <- sf::st_bbox(
    c(xmin = 0, ymin = 0, xmax = width, ymax = height),
    crs = sf::st_crs(crs)
  )
  ggplot2::ggplot() +
    ggplot2::geom_sf(data = sf::st_as_sfc(extent), fill = "#95CEC0") +
    ggplot2::coord_sf(expand = FALSE, datum = NA) +
    ggplot2::theme_void()
}

test_that("frames preserve the built panel extent and aspect across devices", {
  skip_if_not_installed("sf")
  plot <- cartographic_plot()
  original <- plot
  frame <- gc_frame(
    plot,
    padding = 10,
    border = list(width = 2, color = "black")
  )
  expect_s3_class(frame, "gc_frame")
  expect_equal(as.numeric(frame$map_context$extent), c(0, 0, 100000, 50000))
  expect_equal(frame$map_context$crs$epsg, 32119)
  for (size in list(c(800, 300), c(300, 800))) {
    expect_no_warning(layout <- gc_resolve(frame, size[[1]], size[[2]]))
    panel <- layout$root$children[[1]]
    expect_equal(panel$box[["width"]] / panel$box[["height"]], 2)
    expect_lte(panel$box[["width"]], size[[1]] - 24)
    expect_lte(panel$box[["height"]], size[[2]] - 24)
    expect_identical(panel$node$overflow, "hidden")
  }
  expect_identical(plot, original)
})

test_that("frames use coord_sf limits and expansion instead of the data bounds", {
  skip_if_not_installed("sf")
  plot <- cartographic_plot()
  plot$coordinates <- ggplot2::coord_sf(
    xlim = c(10000, 60000),
    ylim = c(5000, 25000),
    expand = TRUE,
    datum = NA
  )
  frame <- gc_frame(plot)
  panel <- ggplot2::ggplot_build(plot)$layout$panel_params[[1]]
  expect_equal(
    as.numeric(frame$map_context$extent),
    c(
      panel$x_range[[1]],
      panel$y_range[[1]],
      panel$x_range[[2]],
      panel$y_range[[2]]
    )
  )
  expect_error(gc_frame(ggplot2::ggplot()), "coord_sf", class = "ggcarto_error")
  expect_error(gc_frame(plot, overlays = gc_text("invalid")), "overlays")
})

test_that("scale bars bind projected distances to the actual panel at every size", {
  skip_if_not_installed("sf")
  scale <- gc_scale_bar(50, "km", segments = 2, left = 8, bottom = 8)
  frame <- gc_frame(cartographic_plot(), overlays = list(scale), padding = 10)
  original <- frame
  for (size in list(c(800, 400), c(400, 800))) {
    expect_no_warning(layout <- gc_resolve(frame, size[[1]], size[[2]]))
    panel <- layout$root$children[[1]]
    bar <- panel$children[[2]]
    expect_equal(bar$box[["width"]] / panel$box[["width"]], 0.5)
    expect_equal(bar$scale_bar$distance_m, 50000)
    expect_identical(bar$content$children[[4]]$label, "25")
    expect_identical(bar$content$children[[5]]$label, "50 km")
  }
  expect_identical(frame, original)
})

test_that("scale bars convert CRS units and select automatic distances", {
  skip_if_not_installed("sf")
  plot <- cartographic_plot(2264, 100000 * 3937 / 1200, 50000 * 3937 / 1200)
  frame <- gc_frame(plot, overlays = list(gc_scale_bar(50, "km", segments = 2)))
  panel <- gc_resolve(frame, 800, 400)$root$children[[1]]
  expect_equal(panel$children[[2]]$box[["width"]] / panel$box[["width"]], 0.5)
  expect_equal(panel$children[[2]]$scale_bar$extent_width_m, 100000)
  frame <- gc_frame(
    cartographic_plot(),
    overlays = list(
      gc_scale_bar(
        max_fraction = 0.3,
        segments = 2,
        design = "ticks",
        subdivisions = 2
      )
    )
  )
  bar <- gc_resolve(frame, 800, 400)$root$children[[1]]$children[[2]]
  expect_equal(bar$scale_bar$distance, 20)
  expect_equal(bar$scale_bar$width, 160)
  expect_length(bar$content$children$`scale-ticks`$x0, 5)
  frame <- gc_frame(
    cartographic_plot(),
    overlays = list(gc_scale_bar(10, "mi", segments = 1))
  )
  bar <- gc_resolve(frame, 800, 400)$root$children[[1]]$children[[2]]
  expect_equal(bar$scale_bar$distance_m, 16093.44)
})

test_that("scale bars reject unbound, geographic and metrically unsafe layouts", {
  skip_if_not_installed("sf")
  expect_error(gc_resolve(gc_scale_bar()), class = "ggcarto_unbound_scale")
  plot <- cartographic_plot(4326, 1, 1)
  expect_error(
    gc_resolve(gc_frame(plot, overlays = list(gc_scale_bar()))),
    class = "ggcarto_unsupported_crs"
  )
  for (scale in list(
    gc_place(gc_scale_bar(), width = 100),
    gc_place(gc_scale_bar(), max_width = 50),
    gc_scale_bar(collision = "shrink")
  )) {
    expect_error(
      gc_resolve(gc_frame(cartographic_plot(), overlays = list(scale))),
      class = "ggcarto_scale_constraint"
    )
  }
  expect_error(
    gc_resolve(gc_frame(cartographic_plot(), overlays = list(gc_scale_bar(200)))),
    class = "ggcarto_scale_distance"
  )
  expect_error(gc_scale_bar(-1), class = "ggcarto_error")
  expect_error(gc_scale_bar(unit = "degrees"), class = "ggcarto_error")
  expect_error(gc_scale_bar(segments = 1.5), class = "ggcarto_error")
  expect_error(gc_scale_bar(max_fraction = 2), class = "ggcarto_error")
})

test_that("locator insets highlight the main extent without changing either plot", {
  skip_if_not_installed("sf")
  overview <- cartographic_plot()
  main <- cartographic_plot(width = 20000, height = 10000)
  original <- main
  inset <- gc_inset(overview, reference = main)
  expect_s3_class(inset, "gc_inset")
  expect_equal(sf::st_bbox(inset$footprint), gc_frame(main)$map_context$extent)
  expect_gt(nrow(sf::st_coordinates(inset$footprint)), 4)
  frame <- gc_frame(main, overlays = list(gc_place(inset, right = 10, top = 10)))
  expect_no_warning(layout <- gc_resolve(frame, 800, 400))
  panel <- layout$root$children[[1]]
  secondary <- panel$children[[2]]
  expect_equal(secondary$box[["width"]] / panel$box[["width"]], 0.3)
  expect_equal(secondary$box[["width"]] / secondary$box[["height"]], 2)
  expect_identical(main, original)
  expect_equal(as.numeric(inset$map_context$extent), c(0, 0, 100000, 50000))
  expect_error(
    gc_frame(overview, overlays = list(inset)),
    class = "ggcarto_inset_reference"
  )
})

test_that("detail insets highlight their footprint in the main map and own their scale", {
  skip_if_not_installed("sf")
  main <- cartographic_plot()
  detail <- cartographic_plot(width = 20000, height = 10000)
  inset <- gc_inset(
    detail,
    reference = gc_frame(main),
    mode = "detail",
    width = "40%",
    overlays = list(gc_scale_bar(10, "km", segments = 1))
  )
  frame <- gc_frame(
    main,
    overlays = list(
      gc_place(inset, right = 10, top = 10),
      gc_scale_bar(50, "km", segments = 2)
    )
  )
  expect_no_warning(layout <- gc_resolve(frame, 1000, 500))
  children <- layout$root$children[[1]]$children
  expect_length(children, 4)
  expect_identical(children[[2]]$content$name, "inset-footprint")
  expect_false(collision_obstacle(children[[2]]))
  secondary <- children[[3]]$children[[1]]
  expect_equal(secondary$children[[2]]$scale_bar$extent_width_m, 20000)
  expect_equal(children[[4]]$scale_bar$extent_width_m, 100000)
})

test_that("inset footprints transform between CRSs without retraining the map", {
  skip_if_not_installed("sf")
  main <- cartographic_plot(width = 20000, height = 10000)
  overview <- cartographic_plot()
  overview$coordinates <- ggplot2::coord_sf(
    crs = 3857,
    expand = FALSE,
    datum = NA
  )
  before <- map_panel_context(overview)
  inset <- gc_inset(overview, reference = main)
  expect_equal(sf::st_crs(inset$footprint)$epsg, 3857)
  expect_identical(inset$map_context, before)
  expected <- sf::st_transform(
    sf::st_as_sfc(gc_frame(main)$map_context$extent),
    3857
  )
  expect_equal(
    sf::st_coordinates(inset$footprint)[1, 1:2],
    sf::st_coordinates(expected)[1, 1:2]
  )
  expect_null(gc_inset(overview, reference = main, highlight = FALSE)$footprint)
  expect_error(
    gc_inset(overview, reference = main, mode = "other"),
    class = "ggcarto_error"
  )
})

test_that("scale decoration and automatic height do not change calibrated distance", {
  skip_if_not_installed("sf")
  scale <- gc_scale_bar(
    50,
    "km",
    segments = 2,
    height = "auto",
    padding = 4,
    border = list(width = 2, color = "black")
  )
  frame <- gc_frame(cartographic_plot(), overlays = list(scale))
  expect_no_warning(layout <- gc_resolve(frame, 800, 400))
  bar <- layout$root$children[[1]]$children[[2]]
  expect_equal(content_box(bar)[["width"]], 400)
  expect_equal(bar$box[["width"]], 412)
  expect_gt(bar$box[["height"]], 30)
  expect_error(gc_scale_bar(col = "not-a-colour"), class = "ggcarto_error")
  expect_error(gc_scale_bar(fill = Inf), class = "ggcarto_error")
  expect_error(gc_scale_bar(fontfamily = NA), class = "ggcarto_error")
  cramped <- gc_frame(
    cartographic_plot(),
    overlays = list(gc_scale_bar(1, segments = 4))
  )
  expect_warning(gc_resolve(cramped, 800, 400), class = "ggcarto_scale_labels")
})

test_that("scale avoidance moves the bar without changing its metric length", {
  skip_if_not_installed("sf")
  obstacle <- gc_place(gc_rect(), left = 0, bottom = 0, width = 400, height = 40)
  scale <- gc_scale_bar(50, segments = 2, collision = "avoid")
  frame <- gc_frame(cartographic_plot(), overlays = list(obstacle, scale))
  expect_no_warning(layout <- gc_resolve(frame, 800, 400))
  bar <- layout$root$children[[1]]$children[[3]]
  expect_equal(bar$scale, 1)
  expect_equal(bar$box[["width"]], 400)
  expect_equal(bar$box[["y"]], 0)
})

test_that("antimeridian-crossing inset footprints are rejected", {
  skip_if_not_installed("sf")
  source <- list(
    extent = sf::st_bbox(
      c(xmin = 170, ymin = 0, xmax = 190, ymax = 10),
      crs = sf::st_crs(4326)
    ),
    crs = sf::st_crs(4326)
  )
  expect_error(
    map_footprint(source, list(crs = sf::st_crs(3857))),
    class = "ggcarto_inset_projection"
  )
})

cartographic_scene <- function(mode = "locator") {
  counties <- sf::st_transform(
    sf::st_read(
      system.file("shape/nc.shp", package = "sf"),
      quiet = TRUE
    ),
    32119
  )
  overview <- ggplot2::ggplot(counties) +
    ggplot2::geom_sf(fill = "#95CEC0", colour = "white", linewidth = 0.3) +
    ggplot2::coord_sf(expand = FALSE, datum = NA) +
    ggplot2::theme_void()
  detail <- overview
  detail$coordinates <- ggplot2::coord_sf(
    crs = 32119,
    xlim = c(580000, 820000),
    ylim = c(130000, 290000),
    expand = FALSE,
    datum = NA
  )
  main <- if (mode == "locator") detail else overview
  secondary <- if (mode == "locator") overview else detail
  inset <- gc_inset(
    secondary,
    reference = main,
    mode = mode,
    width = "34%",
    background = "white",
    border = list(color = "#718A90", width = 1)
  )
  gc_frame(
    main,
    overlays = list(
      gc_place(inset, right = 10, top = 10),
      gc_scale_bar(
        if (mode == "locator") 50 else 200,
        "km",
        segments = 2,
        left = 12,
        bottom = 8,
        background = "white",
        padding = 4
      )
    ),
    padding = 16,
    background = "white"
  )
}

test_that("cartographic frames and insets have stable responsive visual output", {
  skip_if_not_installed("sf")
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("svglite")
  for (mode in c("locator", "detail")) {
    for (size in list(c(900, 600), c(500, 800))) {
      writer <- function(plot, file, title) {
        svglite::svglite(file, width = size[[1]] / 96, height = size[[2]] / 96)
        on.exit(grDevices::dev.off())
        grid::grid.draw(plot)
      }
      vdiffr::expect_doppelganger(
        paste(mode, paste(size, collapse = "x"), sep = "-"),
        gc_as_grob(cartographic_scene(mode)),
        writer = writer
      )
    }
  }
})

test_that("cartographic composition exports natively and preserves the caller", {
  skip_if_not_installed("sf")
  skip_if_not_installed("svglite")
  directory <- tempfile("cartographic-export-")
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  caller <- grDevices::dev.cur()
  scene <- cartographic_scene()
  original <- scene
  expect_no_warning(
    path <- gc_save(
      scene,
      type = "svg",
      dir = directory,
      filename = "map",
      width = 900,
      height = 600
    )
  )
  contents <- paste(readLines(path), collapse = "\n")
  expect_match(contents, "50 km", fixed = TRUE)
  expect_match(contents, "<polygon", fixed = TRUE)
  expect_identical(scene, original)
  expect_identical(grDevices::dev.cur(), caller)
})
