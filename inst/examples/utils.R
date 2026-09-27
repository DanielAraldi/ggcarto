map_extent <- function(counties) {
  bounds <- sf::st_bbox(counties)
  padding <- c(
    diff(bounds[c("xmin", "xmax")]),
    diff(bounds[c("ymin", "ymax")])
  ) *
    0.04
  bounds[c("xmin", "ymin")] <- bounds[c("xmin", "ymin")] - padding
  bounds[c("xmax", "ymax")] <- bounds[c("xmax", "ymax")] + padding
  bounds
}

map_label_template <- function(
  title,
  subtitle,
  accent = "#197C80",
  background = "#F0F6F5"
) {
  lplot::l_template(
    lplot::l_rect(fill = background, col = NA, name = "background"),
    lplot::l_rect(
      x = 0,
      just = "left",
      width = lplot::l_unit(4, "pt"),
      fill = accent,
      col = NA,
      name = "accent"
    ),
    lplot::l_text(
      title,
      x = lplot::l_unit(14, "pt"),
      y = 0.65,
      just = "left",
      fontsize = 12,
      fontface = "bold",
      col = accent,
      name = "title"
    ),
    lplot::l_text(
      subtitle,
      x = lplot::l_unit(14, "pt"),
      y = 0.28,
      just = "left",
      fontsize = 8,
      col = "#50666C",
      name = "subtitle"
    )
  )
}

map_source <- function(
  counties,
  extent,
  field = "BIR74",
  title = "Carolina do Norte"
) {
  counties$births <- counties[[field]]
  mapping <- do.call(ggplot2::aes, list(fill = quote(births)))
  ggplot2::ggplot(counties) +
    ggplot2::geom_sf(mapping, colour = "#FFFFFF", linewidth = 0.3) +
    ggplot2::scale_fill_gradientn(
      colours = c("#EEF3CF", "#95CEC0", "#2B8E98", "#194E70"),
      limits = c(
        0,
        ceiling(max(c(counties$BIR74, counties$BIR79)) / 10000) * 10000
      ),
      breaks = c(0, 20000, 40000),
      labels = c("0", "20 mil", "40 mil"),
      name = "Nascimentos",
      guide = ggplot2::guide_colourbar(
        direction = "horizontal",
        title.position = "top",
        barwidth = lplot::l_unit(42, "mm"),
        barheight = lplot::l_unit(2.5, "mm")
      )
    ) +
    ggplot2::coord_sf(
      crs = sf::st_crs(counties),
      default_crs = sf::st_crs(counties),
      datum = NA,
      xlim = as.numeric(extent[c("xmin", "xmax")]),
      ylim = as.numeric(extent[c("ymin", "ymax")]),
      expand = FALSE
    ) +
    ggplot2::labs(
      title = title,
      subtitle = paste("Nascimentos por condado |", field)
    ) +
    ggplot2::theme_void(base_size = 10) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", colour = "#203C43"),
      plot.subtitle = ggplot2::element_text(colour = "#50666C"),
      legend.position = "bottom",
      legend.text = ggplot2::element_text(size = 8),
      legend.title = ggplot2::element_text(size = 9),
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      panel.background = ggplot2::element_rect(fill = "#EDF3F5", colour = NA),
      plot.margin = ggplot2::margin(0, 0, 0, 0)
    )
}

map_frame <- function(plot, distance_m = 200000, overlays = list()) {
  lplot::l_frame(
    plot,
    overlays = c(
      list(
        lplot::l_scale_bar(
          distance_m / 1000,
          "km",
          segments = 1,
          subdivisions = 2,
          design = "ticks",
          fontsize = 7,
          height = 30,
          left = 12,
          bottom = 8,
          z_index = 10
        )
      ),
      overlays
    )
  )
}

map_sheet <- function(plot, frame, heading = "Carolina do Norte") {
  title <- lplot::l_get_element(
    plot,
    "title",
    style = list(font_size = "clamp(12pt, 2.5vmin, 20pt)")
  )
  subtitle <- lplot::l_get_element(
    plot,
    "subtitle",
    style = list(font_size = "clamp(8pt, 1.4vmin, 10pt)")
  )
  legend <- lplot::l_get_element(plot, "legend")
  credit <- lplot::l_get_element(
    lplot::l_text(
      "Fonte: NAD83 / NC (m)\nElaboração: Daniel Sansão Araldi",
      fontsize = 7,
      col = "#50666C"
    ),
    "credits"
  )
  lplot::l_viewport(
    list(
      lplot::l_place(title, left = 0, top = 0, z_index = 20),
      lplot::l_place(subtitle, left = 0, top = 34, z_index = 20),
      lplot::l_place(
        lplot::l_viewport(list(frame)),
        left = 0,
        right = 0,
        top = 68,
        bottom = 98
      ),
      lplot::l_place(legend, x = "50%", bottom = 28, anchor = "bottom-center"),
      lplot::l_place(credit, x = "50%", bottom = 4, anchor = "bottom-center")
    ),
    width = 1000,
    height = 650,
    padding = 16,
    background = "#FFFFFF",
    metadata = list(heading = heading)
  )
}

map_north_arrow <- function(extent) {
  center <- sf::st_sfc(
    sf::st_point(c(
      mean(as.numeric(extent[c("xmin", "xmax")])),
      mean(as.numeric(extent[c("ymin", "ymax")]))
    )),
    crs = sf::st_crs(extent)
  )
  lonlat <- sf::st_coordinates(sf::st_transform(center, 4326))[1, ]
  north <- sf::st_sfc(sf::st_point(unname(lonlat + c(0, 0.05))), crs = 4326)
  direction <- as.numeric(
    sf::st_coordinates(sf::st_transform(north, sf::st_crs(extent))) -
      sf::st_coordinates(center)
  )
  angle <- atan2(direction[[2]], direction[[1]]) * 180 / pi - 90
  arrow <- lplot::l_north_arrow(
    design = "minimal",
    angle = angle,
    label_gp = list(col = "black")
  )
  lplot::l_get_element(
    arrow,
    "north_arrow",
    width = 40,
    height = 68,
    metadata = list(angle = angle, reference = lonlat)
  )
}

map_locator <- function(counties, reference) {
  extent <- map_extent(counties)
  plot <- ggplot2::ggplot(counties) +
    ggplot2::geom_sf(fill = "#D7E2E3", colour = "white", linewidth = 0.15) +
    ggplot2::coord_sf(
      crs = sf::st_crs(counties),
      datum = NA,
      expand = FALSE,
      xlim = as.numeric(extent[c("xmin", "xmax")]),
      ylim = as.numeric(extent[c("ymin", "ymax")])
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.border = ggplot2::element_rect(
        fill = NA,
        colour = "#718A90",
        linewidth = 0.4
      )
    )
  lplot::l_place(
    lplot::l_inset(
      plot,
      reference = reference,
      mode = "locator",
      width = "34%",
      background = "white",
      highlight_lwd = 2
    ),
    left = 10,
    top = 10,
    z_index = 30
  )
}
