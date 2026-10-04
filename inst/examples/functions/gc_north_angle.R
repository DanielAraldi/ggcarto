if (!requireNamespace("sf", quietly = TRUE)) {
  stop("Install the optional sf package to calculate true north.")
}

counties <- sf::st_read(
  system.file("shape/nc.shp", package = "sf"),
  quiet = TRUE
)
area <- sf::st_transform(counties[counties$NAME == "Wake", ], 32119)
plot <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = area, fill = "#95CEC0", colour = "#203C43") +
  ggplot2::coord_sf(crs = 32119, datum = NA, expand = FALSE) +
  ggplot2::theme_void()

result <- ggcarto::gc_north_angle(plot)
print(result)

arrow <- ggcarto::gc_north_arrow(
  "minimal",
  angle = result,
  name = "true-north-arrow"
)
rose <- ggcarto::gc_north_rose(
  "eight_point",
  angle = result,
  name = "true-north-rose"
)
scene <- ggcarto::gc_frame(
  plot,
  overlays = list(
    ggcarto::gc_place(arrow, left = 8, top = 8, width = 80, height = 80),
    ggcarto::gc_place(rose, right = 8, bottom = 8, width = 80, height = 80)
  ),
  padding = 16,
  background = "white"
)
ggcarto::gc_render(scene)
