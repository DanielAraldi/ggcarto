original <- ggcarto::gc_text("Titulo do mapa", fontsize = 10)

result <- ggcarto::gc_style(
  original,
  color = "#194E70",
  font_size = "20pt",
  font_face = "bold",
  background = "#EEF3CF",
  padding = 12
)

ggcarto::gc_render(result)
