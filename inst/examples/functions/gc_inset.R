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
overview <- ggplot2::ggplot(counties) +
  ggplot2::geom_sf(fill = "#95CEC0", colour = "white", linewidth = 0.3) +
  ggplot2::coord_sf(expand = FALSE, datum = NA) +
  ggplot2::theme_void()
main <- overview
main$coordinates <- ggplot2::coord_sf(
  crs = 32119,
  xlim = c(580000, 820000),
  ylim = c(130000, 290000),
  expand = FALSE,
  datum = NA
)

result <- lplot::l_inset(
  overview,
  reference = main,
  mode = "locator",
  width = "34%",
  background = "white",
  border = list(color = "#718A90", width = 1)
)

lplot::l_render(lplot::l_frame(
  main,
  overlays = list(lplot::l_place(result, left = 10, top = 10)),
  padding = 16,
  background = "white"
))
