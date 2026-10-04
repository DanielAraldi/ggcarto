visual_scene <- function(theme = ggplot2::theme_minimal()) {
  plot <- example_plot() + theme
  title <- gc_get_element(
    plot,
    "title",
    style = list(font_size = "clamp(10pt, 2.5vmin, 20pt)")
  )
  legend <- gc_get_element(
    plot,
    "legend",
    style = list(background = "white", padding = 4, border = "#CCCCCC")
  )
  gc_viewport(
    list(
      gc_place(
        gc_without(plot, c("title", "legend", "subtitle", "tag")),
        left = 0,
        right = 0,
        top = 40,
        bottom = 0
      ),
      gc_place(title, top = 4, x = "50%", anchor = "top-center", z_index = 20),
      gc_place(legend, top = 56, right = 12, z_index = 10)
    ),
    background = "white",
    padding = 8
  )
}

example_plot <- function() {
  mapping <- do.call(
    ggplot2::aes,
    list(x = quote(wt), y = quote(mpg), colour = quote(factor(cyl)))
  )
  ggplot2::ggplot(mtcars, mapping) +
    ggplot2::geom_point() +
    ggplot2::labs(
      title = "Efficiency",
      subtitle = "Weight and mileage",
      caption = "Source: mtcars",
      tag = "A"
    ) +
    ggplot2::theme_minimal()
}

prepared <- function(element, width = 800, height = 600) {
  context <- new_layout_context(width, height)
  with_grid_context(context, prepare_content(element, context))
}

text_grobs <- function(grob) {
  if (inherits(grob, "text")) {
    return(list(grob))
  }
  children <- if (inherits(grob, "gtable")) {
    grob$grobs
  } else {
    as.list(grob$children)
  }
  unlist(lapply(children, text_grobs), recursive = FALSE)
}

collision_scene <- function(policy = "avoid") {
  fixed <- gc_place(
    grid::rectGrob(),
    width = 60,
    height = 30,
    top = 0,
    right = 0,
    id = "fixed"
  )
  moving <- gc_place(
    grid::rectGrob(),
    width = 60,
    height = 30,
    anchor = "top-right",
    collision = policy,
    id = "moving"
  )
  gc_viewport(list(fixed, moving), width = 200, height = 120)
}
