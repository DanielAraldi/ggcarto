if (!requireNamespace("sf", quietly = TRUE)) {
  stop("Install the optional sf package to run this cartographic example.")
}

counties <- sf::st_transform(
  sf::st_read(
    system.file("shape/nc.shp", package = "sf"),
    quiet = TRUE
  ),
  32119
)
plot <- ggplot2::ggplot(counties) +
  ggplot2::geom_sf(fill = "#95CEC0", colour = "white", linewidth = 0.3) +
  ggplot2::coord_sf(expand = FALSE, datum = NA) +
  ggplot2::theme_void()

result <- lplot::l_scale_bar(
  distance = 200,
  unit = "km",
  segments = 2,
  subdivisions = 2,
  design = "bar",
  left = 12,
  bottom = 8,
  background = "white",
  padding = 4
)

lplot::l_render(lplot::l_frame(
  plot,
  overlays = list(result),
  padding = 16,
  background = "white"
))
