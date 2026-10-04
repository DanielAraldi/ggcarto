terrain_scene <- function() {
  elevation <- expand.grid(
    easting = seq_len(nrow(datasets::volcano)) * 10,
    northing = seq_len(ncol(datasets::volcano)) * 10
  )
  elevation$height <- as.vector(datasets::volcano)
  mapping <- do.call(
    ggplot2::aes,
    list(x = quote(easting), y = quote(northing), fill = quote(height))
  )
  plot <- ggplot2::ggplot(elevation, mapping) +
    ggplot2::geom_raster() +
    ggplot2::coord_equal(expand = FALSE) +
    ggplot2::scale_fill_gradientn(
      colours = c("#E0F3DB", "#A8DDB5", "#43A2CA", "#0868AC", "#084081"),
      guide = ggplot2::guide_colourbar(
        barheight = ggcarto::gc_unit(22, "mm"),
        barwidth = ggcarto::gc_unit(2.5, "mm")
      )
    ) +
    ggplot2::labs(
      title = "Maunga Whau",
      fill = "Elevation (m)",
      x = "Local easting (m)",
      y = "Local northing (m)"
    ) +
    ggplot2::theme_minimal(base_size = 8) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold")
    )
  title <- ggcarto::gc_get_element(
    plot,
    "title",
    style = list(color = "#173B34", font_size = "clamp(10pt, 2.5vmin, 16pt)")
  )
  legend <- ggcarto::gc_get_element(
    plot,
    "legend",
    style = list(background = "#FFFFFFEE", padding = "3px", font_size = "8pt")
  )
  main <- ggcarto::gc_viewport(
    list(
      ggcarto::gc_place(
        ggcarto::gc_without(plot, c("title", "legend")),
        left = 0,
        right = 0,
        top = 28,
        bottom = 0
      ),
      ggcarto::gc_place(
        title,
        x = "50%",
        top = 4,
        anchor = "top-center",
        z_index = 20
      ),
      ggcarto::gc_place(legend, right = 4, top = 36, z_index = 10)
    ),
    padding = 6,
    background = "white"
  )
  profile_mapping <- do.call(ggplot2::aes, list(x = quote(height)))
  profile <- ggplot2::ggplot(elevation, profile_mapping) +
    ggplot2::geom_histogram(binwidth = 5, fill = "#32856A", colour = "white") +
    ggplot2::labs(
      title = "Elevation\nprofile",
      x = "Elevation (m)",
      y = "Grid cells"
    ) +
    ggplot2::theme_minimal(base_size = 8)
  note <- ggcarto::gc_get_element(
    ggcarto::gc_text(
      "Data:\ndatasets::volcano\n10 m local grid",
      just = "left",
      x = 0,
      fontsize = 8,
      col = "#4F5C58"
    ),
    "credits"
  )
  sidebar <- ggcarto::gc_viewport(
    list(
      ggcarto::gc_place(profile, left = 0, right = 0, top = 4, height = "65%"),
      ggcarto::gc_place(note, left = 4, bottom = 8)
    ),
    padding = 6,
    background = "#F1F5F2",
    border = list(color = "#CCD8D0", width = "1px")
  )
  ggcarto::gc_join(
    list(
      ggcarto::gc_place(
        main,
        left = "2%",
        top = "3%",
        width = "50%",
        height = "94%"
      ),
      ggcarto::gc_place(
        sidebar,
        left = "54%",
        top = "3%",
        width = "44%",
        height = "94%"
      )
    ),
    width = 1080,
    height = 420,
    background = "#F9FAF9"
  )
}
