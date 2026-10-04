test_that("true north uses the symbol convention on known projections", {
  skip_if_not_installed("sf")
  expect_equal(gc_north_angle(4326, c(-48, -23)), 0, tolerance = 1e-7)
  expect_equal(gc_north_angle(3857, c(12, 55)), 0, tolerance = 1e-7)
  expect_equal(gc_north_angle(3413, c(-45, 75)), 0, tolerance = 1e-7)
  expect_equal(gc_north_angle(3413, c(0, 75)), 45, tolerance = 1e-6)
  expect_equal(gc_north_angle(3413, c(-90, 75)), -45, tolerance = 1e-6)
  expect_equal(abs(gc_north_angle(3413, c(135, 75))), 180, tolerance = 1e-6)
})

test_that("projected sf points and bbox centers produce reusable scalar angles", {
  skip_if_not_installed("sf")
  point <- sf::st_sfc(sf::st_point(c(0, 75)), crs = 4326)
  projected <- sf::st_transform(point, 3413)
  original <- projected
  angle <- gc_north_angle(sf::st_crs(3413), projected)
  expect_equal(angle, 45, tolerance = 1e-6)
  expect_equal(gc_north_angle(3413, sf::st_sf(geometry = projected)), angle)
  coordinates <- sf::st_coordinates(projected)[1, ]
  extent <- sf::st_bbox(
    c(
      xmin = unname(coordinates[[1]]) - 10000,
      ymin = unname(coordinates[[2]]) - 10000,
      xmax = unname(coordinates[[1]]) + 10000,
      ymax = unname(coordinates[[2]]) + 10000
    ),
    crs = sf::st_crs(3413)
  )
  expect_equal(gc_north_angle(extent), angle)
  expect_identical(projected, original)
  expect_type(angle, "double")
  expect_length(angle, 1)
  expect_equal(gc_north_arrow(angle = angle)$children$symbol$vp$angle, angle)
  expect_equal(gc_north_rose(angle = angle)$children$rose$vp$angle, angle)
})

test_that("plots and frames use their displayed extent without opening devices", {
  skip_if_not_installed("sf")
  area <- sf::st_as_sfc(sf::st_bbox(
    c(
      xmin = 580000,
      ymin = 130000,
      xmax = 820000,
      ymax = 290000
    ),
    crs = sf::st_crs(32119)
  ))
  plot <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = area) +
    ggplot2::coord_sf(expand = FALSE, datum = NA)
  frame <- gc_frame(plot)
  device <- grDevices::dev.cur()
  state <- sf::sf_use_s2()
  original <- frame
  expect_equal(gc_north_angle(plot), gc_north_angle(frame))
  expect_equal(gc_north_angle(frame), gc_north_angle(frame$map_context$extent))
  expect_equal(
    gc_north_angle(frame, c(-79, 35)),
    gc_north_angle(32119, c(-79, 35))
  )
  expect_identical(frame, original)
  expect_identical(grDevices::dev.cur(), device)
  expect_identical(sf::sf_use_s2(), state)
})

test_that("north angle rejects unknown references and degenerate locations", {
  skip_if_not_installed("sf")
  expect_error(gc_north_angle(4326), "explicit at", class = "ggcarto_error")
  expect_error(gc_north_angle(sf::st_crs(NA), c(0, 0)), class = "ggcarto_error")
  expect_error(gc_north_angle(4978, c(0, 0)), class = "ggcarto_error")
  expect_error(
    gc_north_angle(4326, matrix(c(0, 0), nrow = 1)),
    class = "ggcarto_error"
  )
  for (location in list(c(0, 90), c(0, -90), c(181, 0), c(0, NA), 5)) {
    expect_error(gc_north_angle(4326, location), class = "ggcarto_error")
  }
  for (step in c(0, -1, Inf, 2)) {
    expect_error(gc_north_angle(4326, c(0, 0), step), class = "ggcarto_error")
  }
  expect_error(
    gc_north_angle(4326, sf::st_sfc(sf::st_point(c(0, 0)))),
    "known CRS"
  )
  expect_error(
    gc_north_angle(4326, sf::st_sfc(sf::st_point(), crs = 4326)),
    "nonempty"
  )
  expect_error(
    gc_north_angle(
      4326,
      sf::st_sfc(sf::st_multipoint(rbind(c(0, 0), c(1, 1))), crs = 4326)
    ),
    "POINT"
  )
  expect_error(
    gc_north_angle(
      4326,
      sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1)), crs = 4326)
    ),
    "one"
  )
})

test_that("local meridian tangents are stable across step sizes and hemispheres", {
  skip_if_not_installed("sf")
  expect_equal(gc_north_angle(3031, c(45, -75)), -45, tolerance = 1e-6)
  expect_equal(gc_north_angle(3031, c(-45, -75)), 45, tolerance = 1e-6)
  east <- gc_north_angle(32631, c(6, 45))
  west <- gc_north_angle(32631, c(0, 45))
  expect_gt(east, 2)
  expect_lt(east, 2.2)
  expect_equal(east, -west, tolerance = 1e-6)
  for (step in c(0.001, 0.00001)) {
    expect_equal(gc_north_angle(32631, c(6, 45), step), east, tolerance = 1e-6)
  }
  expect_error(
    gc_north_angle(3413, c(0, 75), step = 1e-20),
    class = "ggcarto_north_projection"
  )
  orthographic <- "+proj=ortho +lat_0=0 +lon_0=0 +datum=WGS84 +units=m"
  expect_error(
    gc_north_angle(orthographic, c(180, 0)),
    class = "ggcarto_north_projection"
  )
  bound <- "+proj=utm +zone=31 +ellps=intl +towgs84=-87,-98,-121 +units=m"
  expect_true(is.finite(gc_north_angle(bound, c(6, 45))))
})

test_that("insets use their own displayed center rather than the main reference", {
  skip_if_not_installed("sf")
  area <- sf::st_as_sfc(sf::st_bbox(
    c(xmin = -60, ymin = 65, xmax = 15, ymax = 80),
    crs = sf::st_crs(4326)
  ))
  overview <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = area) +
    ggplot2::coord_sf(crs = 3413, expand = FALSE, datum = NA)
  detail <- overview
  detail$coordinates <- ggplot2::coord_sf(
    crs = 3413,
    default_crs = 4326,
    xlim = c(-15, 0),
    ylim = c(70, 75),
    expand = FALSE,
    datum = NA
  )
  inset <- gc_inset(overview, reference = detail)
  expect_equal(gc_north_angle(inset), gc_north_angle(overview))
  expect_gt(abs(gc_north_angle(inset) - gc_north_angle(detail)), 1)
})

test_that("north angle examples have stable visual output", {
  skip_if_not_installed("sf")
  skip_if_not_installed("vdiffr")
  example <- new.env(parent = baseenv())
  grDevices::pdf(NULL, width = 8, height = 6)
  on.exit(grDevices::dev.off())
  capture.output(sys.source(
    system.file("examples", "functions", "gc_north_angle.R", package = "ggcarto"),
    envir = example
  ))
  vdiffr::expect_doppelganger(
    "wake-county-true-north",
    gc_as_grob(example$scene)
  )
})
