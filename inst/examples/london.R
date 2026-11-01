examples_root <- system.file("examples", package = "ggcarto")
sys.source(file.path(examples_root, "data", "london.R"), envir = environment())
rm(examples_root)

london_breaks <- c(0, 50, 100, 250, 500, 1000)

london_palette <- list(
  background = "#D9D9D9",
  density = c("#FBF0B3", "#F6D07E", "#F1A35B", "#E45D3C", "#C42B2B"),
  border = "#2B2B2B",
  river = "#4F81B3",
  text = "#1A1A1A"
)

london_labels <- function(breaks) {
  values <- format(breaks, big.mark = ",", trim = TRUE, scientific = FALSE)
  paste(values[-length(values)], "to", values[-1L])
}

london_map <- function(data) {
  extent <- data$extent
  density <- data$density
  density$density <- pmin(density$density, max(london_breaks) - 1e-6)
  levels <- london_labels(london_breaks)
  aes <- function(...) do.call(ggplot2::aes, list(...))
  ggplot2::ggplot() +
    ggplot2::geom_contour_filled(
      data = density,
      aes(
        x = quote(x),
        y = quote(y),
        z = quote(density),
        fill = quote(ggplot2::after_stat(level))
      ),
      breaks = london_breaks
    ) +
    ggplot2::geom_sf(
      data = data$mask,
      fill = london_palette$background,
      colour = NA
    ) +
    ggplot2::geom_sf(
      data = data$boroughs,
      fill = NA,
      colour = london_palette$border,
      linewidth = 0.3
    ) +
    ggplot2::geom_sf(
      data = data$thames,
      colour = london_palette$river,
      linewidth = 1.4,
      lineend = "round"
    ) +
    ggplot2::scale_fill_manual(
      values = london_palette$density,
      labels = levels,
      drop = FALSE,
      name = "Crimes per km²",
      guide = ggplot2::guide_legend(
        keywidth = ggcarto::gc_unit(7, "mm"),
        keyheight = ggcarto::gc_unit(4.4, "mm")
      )
    ) +
    ggplot2::coord_sf(
      crs = sf::st_crs(london_crs),
      default_crs = sf::st_crs(london_crs),
      datum = NA,
      xlim = as.numeric(extent[c("xmin", "xmax")]),
      ylim = as.numeric(extent[c("ymin", "ymax")]),
      expand = FALSE
    ) +
    ggplot2::theme_void(base_size = 10) +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(
        fill = london_palette$background,
        colour = NA
      ),
      legend.title = ggplot2::element_text(
        size = 13,
        colour = london_palette$text,
        margin = ggplot2::margin(0, 0, 4, 0)
      ),
      legend.text = ggplot2::element_text(
        size = 10.5,
        colour = london_palette$text
      ),
      legend.key.spacing.y = ggcarto::gc_unit(0, "pt"),
      legend.background = ggplot2::element_blank(),
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      plot.margin = ggplot2::margin(0, 0, 0, 0)
    )
}

map_london_scene <- function(data = london_data()) {
  plot <- london_map(data)
  title <- ggcarto::gc_text(
    "Crimes in Greater London\nOctober 2015",
    x = 0,
    just = "left",
    fontsize = 20,
    lineheight = 1.15,
    col = london_palette$text
  )
  north <- ggcarto::gc_get_element(
    ggcarto::gc_north_arrow(
      design = "classic",
      angle = ggcarto::gc_north_angle(data$extent),
      col = london_palette$text,
      fontsize = 9,
      fontface = "plain"
    ),
    "north_arrow",
    width = 28,
    height = 56
  )
  scale <- ggcarto::gc_scale_bar(
    20,
    "km",
    segments = 4,
    design = "bar",
    col = london_palette$text,
    fill = london_palette$text,
    fill_secondary = "white",
    lwd = 0.6,
    fontsize = 7.5,
    height = 28,
    left = 14,
    bottom = 12,
    id = "scale_bar"
  )
  frame <- ggcarto::gc_frame(
    plot,
    overlays = list(
      ggcarto::gc_place(title, left = 14, top = 12, z_index = 20, id = "title"),
      ggcarto::gc_place(
        ggcarto::gc_get_element(plot, "legend"),
        right = 40,
        bottom = 16,
        z_index = 20,
        id = "legend"
      ),
      ggcarto::gc_place(north, left = 22, bottom = 50, z_index = 20),
      scale
    )
  )
  ggcarto::gc_viewport(
    list(ggcarto::gc_place(frame, left = 0, right = 0, top = 0, bottom = 0)),
    width = 1054,
    height = 818,
    background = london_palette$background,
    metadata = list(
      crs = london_crs,
      crimes = data$crimes,
      source = "Tennekes (2018), Journal of Statistical Software 84(6)"
    )
  )
}
