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
  expect_identical(as.numeric(frame$map_context$extent), c(0, 0, 100000, 50000))
  expect_identical(frame$map_context$crs$epsg, 32119L)
  for (size in list(c(800, 300), c(300, 800))) {
    expect_no_warning(layout <- gc_resolve(frame, size[[1]], size[[2]]))
    panel <- layout$root$children[[1]]
    expect_identical(panel$box[["width"]] / panel$box[["height"]], 2)
    expect_lte(panel$box[["width"]], size[[1]] - 24)
    expect_lte(panel$box[["height"]], size[[2]] - 24)
    expect_identical(panel$node$overflow, "hidden")
  }
  expect_identical(plot, original)
})

test_that("frames use coord_sf limits and expansion, not the data bounds", {
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
  expect_identical(
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

test_that("scale bars bind geodesic distances to the panel at every size", {
  skip_if_not_installed("sf")
  scale <- gc_scale_bar(50, "km", segments = 2, left = 8, bottom = 8)
  frame <- gc_frame(cartographic_plot(), overlays = list(scale), padding = 10)
  original <- frame
  fractions <- numeric()
  for (size in list(c(800, 400), c(400, 800))) {
    expect_no_warning(layout <- gc_resolve(frame, size[[1]], size[[2]]))
    panel <- layout$root$children[[1]]
    bar <- panel$children[[2]]
    fractions <- c(fractions, bar$box[["width"]] / panel$box[["width"]])
    expect_equal(fractions[[length(fractions)]], 0.5, tolerance = 1e-3)
    expect_identical(bar$scale_bar$distance_m, 50000)
    expect_identical(bar$content$children[[4]]$label, "25")
    expect_identical(bar$content$children[[5]]$label, "50 km")
  }
  expect_identical(fractions[[1]], fractions[[2]])
  expect_identical(frame, original)
})

test_that("scale bars convert CRS units and select automatic distances", {
  skip_if_not_installed("sf")
  plot <- cartographic_plot(2264, 100000 * 3937 / 1200, 50000 * 3937 / 1200)
  frame <- gc_frame(plot, overlays = list(gc_scale_bar(50, "km", segments = 2)))
  panel <- gc_resolve(frame, 800, 400)$root$children[[1]]
  expect_equal(
    panel$children[[2]]$box[["width"]] / panel$box[["width"]],
    0.5,
    tolerance = 1e-3
  )
  expect_equal(
    panel$children[[2]]$scale_bar$extent_width_m,
    100000,
    tolerance = 1e-3
  )
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
  expect_identical(bar$scale_bar$distance, 20)
  expect_equal(bar$scale_bar$width, 160, tolerance = 1e-3)
  expect_length(bar$content$children$`scale-ticks`$x0, 5)
  frame <- gc_frame(
    cartographic_plot(),
    overlays = list(gc_scale_bar(10, "mi", segments = 1))
  )
  bar <- gc_resolve(frame, 800, 400)$root$children[[1]]$children[[2]]
  expect_identical(bar$scale_bar$distance_m, 16093.44)
})

test_that("scale bars reject unbound and unsafe metric layouts", {
  skip_if_not_installed("sf")
  expect_error(gc_resolve(gc_scale_bar()), class = "ggcarto_unbound_scale")
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
    gc_resolve(gc_frame(
      cartographic_plot(),
      overlays = list(gc_scale_bar(200))
    )),
    class = "ggcarto_scale_distance"
  )
  expect_error(gc_scale_bar(-1), class = "ggcarto_error")
  expect_error(gc_scale_bar(unit = "degrees"), class = "ggcarto_error")
  expect_error(gc_scale_bar(segments = 1.5), class = "ggcarto_error")
  expect_error(gc_scale_bar(max_fraction = 2), class = "ggcarto_error")
})

test_that("locator insets highlight the main extent without changing plots", {
  skip_if_not_installed("sf")
  overview <- cartographic_plot()
  main <- cartographic_plot(width = 20000, height = 10000)
  original <- main
  inset <- gc_inset(overview, reference = main)
  expect_s3_class(inset, "gc_inset")
  expect_identical(
    sf::st_bbox(inset$footprint),
    gc_frame(main)$map_context$extent
  )
  expect_gt(nrow(sf::st_coordinates(inset$footprint)), 4)
  frame <- gc_frame(
    main,
    overlays = list(gc_place(inset, right = 10, top = 10))
  )
  expect_no_warning(layout <- gc_resolve(frame, 800, 400))
  panel <- layout$root$children[[1]]
  secondary <- panel$children[[2]]
  expect_identical(secondary$box[["width"]] / panel$box[["width"]], 0.3)
  expect_identical(secondary$box[["width"]] / secondary$box[["height"]], 2)
  expect_identical(main, original)
  expect_identical(as.numeric(inset$map_context$extent), c(0, 0, 100000, 50000))
  expect_error(
    gc_frame(overview, overlays = list(inset)),
    class = "ggcarto_inset_reference"
  )
})

test_that("detail insets highlight their footprint and own their scale", {
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
  expect_equal(
    secondary$children[[2]]$scale_bar$extent_width_m,
    20000,
    tolerance = 1e-3
  )
  expect_equal(
    children[[4]]$scale_bar$extent_width_m,
    100000,
    tolerance = 1e-3
  )
})

test_that("inset footprints transform between CRSs without retraining", {
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
  expect_identical(sf::st_crs(inset$footprint)$epsg, 3857L)
  expect_identical(inset$map_context, before)
  expected <- sf::st_transform(
    sf::st_as_sfc(gc_frame(main)$map_context$extent),
    3857
  )
  expect_identical(
    sf::st_coordinates(inset$footprint)[1, 1:2],
    sf::st_coordinates(expected)[1, 1:2]
  )
  expect_null(gc_inset(overview, reference = main, highlight = FALSE)$footprint)
  expect_error(
    gc_inset(overview, reference = main, mode = "other"),
    class = "ggcarto_error"
  )
})

test_that("scale decoration and auto height keep the calibrated distance", {
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
  expect_equal(content_box(bar)[["width"]], 400, tolerance = 1e-3)
  expect_equal(
    bar$box[["width"]] - content_box(bar)[["width"]],
    12,
    tolerance = 1e-12
  )
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
  obstacle <- gc_place(
    gc_rect(),
    left = 0,
    bottom = 0,
    width = 400,
    height = 40
  )
  scale <- gc_scale_bar(50, segments = 2, collision = "avoid")
  frame <- gc_frame(cartographic_plot(), overlays = list(obstacle, scale))
  expect_no_warning(layout <- gc_resolve(frame, 800, 400))
  bar <- layout$root$children[[1]]$children[[3]]
  expect_identical(bar$scale, 1)
  expect_equal(bar$box[["width"]], 400, tolerance = 1e-3)
  expect_identical(bar$box[["y"]], 0)
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

test_that("frames and insets have stable responsive visual output", {
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

test_that("cartographic composition exports natively, preserving the caller", {
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

test_that("frames reject faceted maps and maps without a known CRS", {
  skip_if_not_installed("sf")
  points <- sf::st_sf(
    group = c("a", "b"),
    geometry = sf::st_sfc(
      sf::st_point(c(0, 0)),
      sf::st_point(c(1000, 1000)),
      crs = 32119
    )
  )
  faceted <- ggplot2::ggplot(points) +
    ggplot2::geom_sf() +
    ggplot2::coord_sf() +
    ggplot2::facet_wrap(~group)
  condition <- expect_error(gc_frame(faceted), class = "ggcarto_error")
  expect_identical(condition$property, "plot")
  unknown <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1)))
    ) +
    ggplot2::coord_sf()
  expect_error(gc_frame(unknown), "known CRS", class = "ggcarto_error")
})

test_that("scale bars ignore CRS units and reject unknown designs", {
  skip_if_not_installed("sf")
  frame <- gc_frame(
    cartographic_plot(2314, 300000, 150000),
    overlays = list(gc_scale_bar(segments = 2))
  )
  bar <- gc_resolve(frame, 800, 400)$root$children[[1]]$children[[2]]
  expect_true(is.finite(bar$scale_bar$extent_width_m))
  expect_gt(bar$scale_bar$width, 0)
  condition <- expect_error(
    gc_scale_bar(design = "dots"),
    class = "ggcarto_error"
  )
  expect_identical(condition$property, "design")
})

test_that("inset footprints reject antimeridian and projection failures", {
  skip_if_not_installed("sf")
  crossing <- list(
    extent = sf::st_bbox(c(xmin = 3e6, ymin = 0, xmax = 4e6, ymax = 1e6)),
    crs = sf::st_crs(3832)
  )
  expect_error(
    map_footprint(crossing, list(crs = sf::st_crs(3857))),
    "antimeridian",
    class = "ggcarto_inset_projection"
  )
  source <- list(
    extent = sf::st_bbox(c(xmin = 1e5, ymin = 0, xmax = 3e5, ymax = 1e5)),
    crs = sf::st_crs(32631)
  )
  expect_error(
    map_footprint(source, list(crs = sf::NA_crs_)),
    "cannot be transformed",
    class = "ggcarto_inset_projection"
  )
  expect_error(
    map_footprint(
      source,
      list(crs = sf::st_crs("+proj=ortho +lat_0=0 +lon_0=-120"))
    ),
    "outside the target projection",
    class = "ggcarto_inset_projection"
  )
  shifted <- sf::st_crs("+proj=longlat +pm=180 +datum=WGS84 +no_defs")
  expect_error(
    map_footprint(source, list(crs = shifted)),
    "antimeridian",
    class = "ggcarto_inset_projection"
  )
})

test_that("insets require a logical highlight flag", {
  skip_if_not_installed("sf")
  condition <- expect_error(
    gc_inset(cartographic_plot(), highlight = NA),
    class = "ggcarto_error"
  )
  expect_identical(condition$property, "highlight")
})

test_that("geodesic distances follow the WGS 84 ellipsoid", {
  degrees <- function(d, m, s) d + m / 60 + s / 3600
  expect_equal(
    geodesic_distance(
      degrees(144, 25, 29.52440),
      -degrees(37, 57, 3.72030),
      degrees(143, 55, 35.38390),
      -degrees(37, 39, 10.15610)
    ),
    54972.271,
    tolerance = 1e-8
  )
  expect_equal(
    geodesic_distance(0, 0, 1, 0),
    6378137 * pi / 180,
    tolerance = 1e-12
  )
  expect_identical(geodesic_distance(10, 20, 10, 20), 0)
  expect_identical(
    geodesic_distance(179.9, 0, -179.9, 0),
    geodesic_distance(0, 0, 0.2, 0)
  )
  expect_identical(geodesic_distance(NA, 0, 1, 0), NA_real_)
  expect_identical(geodesic_distance(0, 0, 179.9, 0.1), NA_real_)
})

parallel_length <- function(latitude, longitudes) {
  eccentricity2 <- wgs84_flattening * (2 - wgs84_flattening)
  latitude <- latitude * pi / 180
  wgs84_semi_major *
    cos(latitude) /
    sqrt(1 - eccentricity2 * sin(latitude)^2) *
    longitudes *
    pi /
    180
}

geographic_scale <- function(ylim, distance = 100) {
  plot <- cartographic_plot(4326, 10, 2)
  plot$coordinates <- ggplot2::coord_sf(
    xlim = c(0, 10),
    ylim = ylim,
    expand = FALSE,
    datum = NA
  )
  frame <- gc_frame(
    plot,
    overlays = list(gc_scale_bar(distance, "km", segments = 1))
  )
  panel <- gc_resolve(frame, 800, 400)$root$children[[1]]
  list(
    fraction = panel$children[[2]]$box[["width"]] / panel$box[["width"]],
    scale_bar = panel$children[[2]]$scale_bar
  )
}

test_that("scale bars measure ground distance on geographic maps", {
  skip_if_not_installed("sf")
  equator <- geographic_scale(c(-1, 1))
  expect_equal(
    equator$scale_bar$extent_width_m,
    6378137 * 10 * pi / 180,
    tolerance = 1e-9
  )
  expect_equal(
    equator$fraction,
    100000 / (6378137 * 10 * pi / 180),
    tolerance = 1e-9
  )
  expect_equal(
    equator$scale_bar$reference,
    c(longitude = 5, latitude = 0),
    tolerance = 1e-12
  )
  for (latitude in c(1, 60)) {
    scale <- geographic_scale(latitude + c(-1, 1))
    parallel <- parallel_length(latitude, 10)
    expect_equal(scale$scale_bar$extent_width_m, parallel, tolerance = 1e-6)
    expect_equal(scale$fraction, 100000 / parallel, tolerance = 1e-6)
  }
})

test_that("scale bars correct projection distortion in Web Mercator", {
  skip_if_not_installed("sf")
  centre <- sf::sf_project("OGC:CRS84", "EPSG:3857", cbind(0, 60))[1, ]
  extent <- sf::st_bbox(
    c(
      xmin = centre[[1]] - 250000,
      ymin = centre[[2]] - 100000,
      xmax = centre[[1]] + 250000,
      ymax = centre[[2]] + 100000
    ),
    crs = sf::st_crs(3857)
  )
  plot <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = sf::st_as_sfc(extent)) +
    ggplot2::coord_sf(expand = FALSE, datum = NA)
  layout <- gc_resolve(
    gc_frame(plot, overlays = list(gc_scale_bar(50, "km", segments = 1))),
    800,
    400
  )
  panel <- layout$root$children[[1]]
  bar <- panel$children[[2]]
  projected_fraction <- 50000 / 500000
  expect_equal(
    bar$box[["width"]] / panel$box[["width"]],
    projected_fraction / cos(60 * pi / 180),
    tolerance = 5e-3
  )
  expect_equal(bar$scale_bar$extent_width_m, 250000, tolerance = 5e-3)
})

test_that("scale bars measure within the projection domain", {
  skip_if_not_installed("sf")
  world <- list(
    extent = sf::st_bbox(
      c(xmin = -1.8e7, ymin = -9e6, xmax = 1.8e7, ymax = 9e6)
    ),
    crs = sf::st_crs("ESRI:54030")
  )
  profile <- scale_ground_profile(world)
  expect_identical(profile$extent_length, NA_real_)
  expect_lt(max(profile$half_widths), 1.8e7)
  expect_gt(max(profile$lengths), 3.5e7)
  scale <- scale_bar_distance(
    list(distance = NULL, unit = "km", max_fraction = 0.25),
    world,
    list(id = "scale")
  )
  expect_identical(scale$distance, 10000)
  expect_error(
    scale_bar_distance(
      list(distance = 45000, unit = "km", max_fraction = 0.25),
      world,
      list(id = "scale")
    ),
    class = "ggcarto_scale_distance"
  )
  outside <- list(
    extent = sf::st_bbox(c(xmin = 7e6, ymin = 0, xmax = 8e6, ymax = 1e6)),
    crs = sf::st_crs("+proj=ortho +lat_0=0 +lon_0=0")
  )
  expect_error(
    scale_ground_profile(outside),
    "outside the projection domain",
    class = "ggcarto_scale_projection"
  )
  edge <- list(
    extent = sf::st_bbox(
      c(xmin = 5378000, ymin = -1e5, xmax = 7378000, ymax = 1e5)
    ),
    crs = outside$crs
  )
  expect_error(
    scale_ground_profile(edge),
    "cannot be measured",
    class = "ggcarto_scale_projection"
  )
  expect_error(
    suppressWarnings(scale_geographic(cbind(0, 0), sf::NA_crs_)),
    "cannot be transformed",
    class = "ggcarto_scale_projection"
  )
})

test_that("scale profiles interpolate within their bounds", {
  from <- c(0, 1, 2)
  to <- c(0, 10, 20)
  expect_identical(scale_interpolate(from, to, -1), 0)
  expect_identical(scale_interpolate(from, to, 5), 20)
  expect_identical(scale_interpolate(from, to, 1.5), 15)
  expect_identical(scale_interpolate(c(0, 1, 1), to, 1), 10)
})

test_that("automatic scales reject maps without measurable ground length", {
  skip_if_not_installed("sf")
  local_mocked_bindings(
    scale_ground_profile = function(map) {
      list(
        half_widths = c(0, 1),
        lengths = c(0, 0),
        extent_half_width = 1,
        extent_length = 0,
        reference = c(0, 0)
      )
    }
  )
  expect_error(
    scale_bar_distance(
      list(distance = NULL, unit = "km", max_fraction = 0.25),
      list(),
      list(id = "scale")
    ),
    "No positive ground distance",
    class = "ggcarto_scale_distance"
  )
})
