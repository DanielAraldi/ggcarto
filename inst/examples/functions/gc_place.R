rectangle <- ggcarto::gc_rect(fill = "#95CEC0", col = "#194E70")

result <- ggcarto::gc_place(
  rectangle,
  left = "10%",
  top = "15%",
  width = "50%",
  height = 80,
  id = "positioned_rectangle"
)

ggcarto::gc_render(result)
